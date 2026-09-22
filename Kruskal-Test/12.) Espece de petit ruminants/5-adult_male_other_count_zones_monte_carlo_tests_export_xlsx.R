# Adult male "Other" count vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: adult_male_other_count_zones_monte_carlo_results.xlsx

packages <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "writexl")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install packages first: install.packages(c(",
       paste(sprintf('"%s"', missing), collapse = ", "), "))")
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(stringi)
  library(writexl)
})

DATA_FILE <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "adult_male_other_count_zones_monte_carlo_results.xlsx"
B <- 10000L                 # Use 100000L for the final analysis if desired
ALPHA <- 0.05
SEED <- 20260916L

COUNT_COL <-
  "12.) Effectif et composition du cheptel/Nombre de mâle adultes Autre____"
COMMUNE_COL <-
  "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

normalize_header <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |>
    str_to_lower() |>
    str_squish()
  str_replace_all(x, "[^a-z0-9]", "")
}

resolve_column <- function(expected, headers, terms, label) {
  if (expected %in% headers) return(expected)
  nh <- normalize_header(headers)
  hit <- which(nh == normalize_header(expected))
  if (length(hit) == 1L) {
    message(label, " matched after normalization to: ", headers[hit])
    return(headers[hit])
  }
  nt <- normalize_header(terms)
  hit <- which(vapply(nh, function(h) {
    all(vapply(nt, function(t) str_detect(h, fixed(t)), logical(1)))
  }, logical(1)))
  if (length(hit) == 1L) {
    message(label, " matched flexibly to: ", headers[hit])
    return(headers[hit])
  }
  candidates <- headers[str_detect(nh, "effectifcomposition|maleadult|autre")]
  stop(label, " column could not be identified uniquely. Candidate headers: ",
       paste(candidates, collapse = " | "))
}

normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |>
    str_to_lower() |>
    str_squish()
  x <- str_replace_all(x, "[’'`-]", "") |>
    str_replace_all("[^a-z0-9]", "")
  recode(x,
         "toribossito" = "tori",
         "dassazoume" = "dassa",
         "dassazounme" = "dassa",
         .default = x)
}

if (!file.exists(DATA_FILE)) stop("File not found: ", DATA_FILE, ". Current folder: ", getwd())
if (!file.exists(ZONES_FILE)) stop("File not found: ", ZONES_FILE, ". Current folder: ", getwd())

raw <- read_excel(DATA_FILE, 1, col_types = "text", .name_repair = "minimal")
zraw <- read_excel(ZONES_FILE, 1, col_types = "text", .name_repair = "unique")

COUNT_COL <- resolve_column(
  COUNT_COL, names(raw),
  c("Effectif et composition du cheptel", "Nombre de mâle adultes", "Autre"),
  "Adult-male Other count"
)
COMMUNE_COL <- resolve_column(
  COMMUNE_COL, names(raw),
  c("CARACTERISTIQUES", "UNITE", "ELEVAGE", "Commune"),
  "Commune"
)

required_zone_cols <- c("Vegetation zones", "Phytogeographic zones", "District")
if (!all(required_zone_cols %in% names(zraw))) {
  stop("zones.xlsx must contain: ", paste(required_zone_cols, collapse = ", "))
}

zones <- zraw |>
  transmute(
    vegetation_zone = str_squish(str_replace_all(`Vegetation zones`, "\\u00A0", " ")),
    phytogeo_zone = str_squish(str_replace_all(`Phytogeographic zones`, "\\u00A0", " ")),
    district = str_squish(str_replace_all(District, "\\u00A0", " "))
  ) |>
  fill(vegetation_zone, phytogeo_zone) |>
  filter(!is.na(district), district != "",
         !str_detect(str_to_lower(vegetation_zone), "^total")) |>
  mutate(commune_key = normalize_key(district)) |>
  select(commune_key, vegetation_zone, phytogeo_zone) |>
  distinct(commune_key, .keep_all = TRUE)

parse_number <- function(x) {
  x <- str_squish(as.character(x))
  x[x == ""] <- NA_character_
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

data <- raw |>
  transmute(
    row_id = row_number(),
    commune_raw = .data[[COMMUNE_COL]],
    commune_key = normalize_key(.data[[COMMUNE_COL]]),
    count_raw = str_squish(.data[[COUNT_COL]]),
    count = parse_number(.data[[COUNT_COL]])
  ) |>
  left_join(zones, by = "commune_key") |>
  mutate(
    blank = is.na(count_raw) | count_raw == "",
    nonnumeric = !blank & is.na(count),
    negative = !is.na(count) & count < 0,
    noninteger = !is.na(count) & count >= 0 & abs(count - round(count)) > 1e-9,
    valid_count = !is.na(count) & count >= 0 & abs(count - round(count)) <= 1e-9
  )

kw_h_from_ranks <- function(ranks, group_id, group_sizes, tie_correction) {
  rank_sums <- rowsum(ranks, group_id, reorder = FALSE)[, 1]
  n <- length(ranks)
  h <- (12 / (n * (n + 1))) * sum((rank_sums^2) / group_sizes) - 3 * (n + 1)
  h / tie_correction
}

run_mc_test <- function(zone_var, label, seed_offset = 0L) {
  d <- data |>
    filter(valid_count, !is.na(.data[[zone_var]]), .data[[zone_var]] != "") |>
    transmute(value = count, zone = factor(.data[[zone_var]])) |>
    droplevels()

  n <- nrow(d)
  k <- nlevels(d$zone)
  if (n == 0L || k < 2L || length(unique(d$value)) < 2L) {
    reason <- if (n == 0L) "No complete valid observations." else if (k < 2L) "Only one zone category represented." else "All valid counts are identical."
    return(list(
      summary = data.frame(
        Comparison = label, N = n, Groups = k,
        Observed_Kruskal_Wallis_H = NA_real_, Degrees_of_freedom = NA_real_,
        Monte_Carlo_permutations = B, Monte_Carlo_p_value = NA_real_,
        Monte_Carlo_SE = NA_real_, Monte_Carlo_95CI_low = NA_real_,
        Monte_Carlo_95CI_high = NA_real_, Epsilon_squared = NA_real_,
        Alpha = ALPHA, Decision = "No statistical test performed",
        Recommendation = reason, stringsAsFactors = FALSE
      ),
      descriptive = data.frame(Note = reason)
    ))
  }

  ranks <- rank(d$value, ties.method = "average")
  group_id <- as.integer(d$zone)
  group_sizes <- tabulate(group_id, nbins = k)
  ties <- table(d$value)
  tie_correction <- 1 - sum(ties^3 - ties) / (n^3 - n)
  observed_h <- kw_h_from_ranks(ranks, group_id, group_sizes, tie_correction)

  set.seed(SEED + seed_offset)
  extreme <- 0L
  progress_points <- unique(pmax(1L, round(seq(B / 10, B, length.out = 10))))
  for (b in seq_len(B)) {
    perm_h <- kw_h_from_ranks(sample(ranks, n, replace = FALSE),
                            group_id, group_sizes, tie_correction)
    if (perm_h >= observed_h - 1e-12) extreme <- extreme + 1L
    if (b %in% progress_points) message(label, ": ", b, "/", B, " permutations completed")
  }

  p_mc <- (extreme + 1) / (B + 1)
  mc_se <- sqrt(p_mc * (1 - p_mc) / (B + 1))
  eps2 <- max(0, (observed_h - k + 1) / (n - k))

  desc <- d |>
    group_by(zone) |>
    summarise(
      N = n(), Mean = mean(value), SD = sd(value), Median = median(value),
      Q1 = quantile(value, 0.25), Q3 = quantile(value, 0.75),
      Minimum = min(value), Maximum = max(value),
      Zero_count = sum(value == 0), Zero_percent = 100 * mean(value == 0),
      .groups = "drop"
    )

  list(
    summary = data.frame(
      Comparison = label, N = n, Groups = k,
      Observed_Kruskal_Wallis_H = observed_h, Degrees_of_freedom = k - 1,
      Monte_Carlo_permutations = B, Monte_Carlo_p_value = p_mc,
      Monte_Carlo_SE = mc_se,
      Monte_Carlo_95CI_low = max(0, p_mc - 1.96 * mc_se),
      Monte_Carlo_95CI_high = min(1, p_mc + 1.96 * mc_se),
      Epsilon_squared = eps2, Alpha = ALPHA,
      Decision = ifelse(p_mc < ALPHA,
                        "Reject H0: distributions differ by zone",
                        "Do not reject H0: no evidence of a zone difference"),
      Recommendation = "Report the Monte Carlo permutation p-value based on the tie-corrected Kruskal-Wallis H statistic.",
      stringsAsFactors = FALSE
    ),
    descriptive = desc
  )
}

veg <- run_mc_test("vegetation_zone", "Adult male Other count x vegetation zone", 0L)
phyto <- run_mc_test("phytogeo_zone", "Adult male Other count x phytogeographic zone", 100L)

quality <- data.frame(
  Metric = c("Total records", "Valid nonnegative integer counts", "Zero counts",
             "Blank responses", "Nonnumeric responses", "Negative values",
             "Noninteger nonnegative values", "Unmatched communes"),
  Value = c(nrow(data), sum(data$valid_count), sum(data$valid_count & data$count == 0),
            sum(data$blank), sum(data$nonnumeric), sum(data$negative),
            sum(data$noninteger), sum(is.na(data$vegetation_zone)))
)

notes <- data.frame(
  Parameter = c("Variable type", "Primary test", "Permutation count", "Zero handling",
                "Null hypothesis", "Effect size"),
  Assessment = c(
    "Quantitative discrete count variable.",
    "Monte Carlo permutation test using the tie-corrected Kruskal-Wallis H statistic.",
    paste0(B, " finite permutations; change B to 100000L for the final analysis if needed."),
    "Zero is valid and means no adult male animals in the Other category.",
    "The count distribution is identical across geographical zones.",
    "Epsilon-squared based on the observed Kruskal-Wallis statistic."
  )
)

sheets <- list(
  Summary = bind_rows(veg$summary, phyto$summary),
  Veg_descriptive = veg$descriptive,
  Phyto_descriptive = phyto$descriptive,
  Data_quality = quality,
  Method_notes = notes
)

writexl::write_xlsx(sheets, path = OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, winslash = "/", mustWork = FALSE))
