# Young female Djallonke animals (>6 months) vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: young_female_djallonke_over6months_zones_monte_carlo_results.xlsx

packages <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "writexl")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop(
    "Install missing packages first: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
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
OUTPUT_FILE <- "young_female_djallonke_over6months_zones_monte_carlo_results.xlsx"
ALPHA <- 0.05
B <- 10000L
SEED <- 20260916L

COUNT_COL <- "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Djallonke"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

normalize_header <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |>
    str_to_lower() |>
    str_squish()
  str_replace_all(x, "[^a-z0-9]", "")
}

resolve_column <- function(expected, headers, required_terms, label) {
  if (expected %in% headers) return(expected)
  normalized_headers <- normalize_header(headers)
  exact_hit <- which(normalized_headers == normalize_header(expected))
  if (length(exact_hit) == 1L) {
    message(label, " matched after normalization to: ", headers[exact_hit])
    return(headers[exact_hit])
  }
  terms <- normalize_header(required_terms)
  flexible_hit <- which(vapply(
    normalized_headers,
    function(h) all(vapply(terms, function(term) grepl(term, h, fixed = TRUE), logical(1))),
    logical(1)
  ))
  if (length(flexible_hit) == 1L) {
    message(label, " matched flexibly to: ", headers[flexible_hit])
    return(headers[flexible_hit])
  }
  candidates <- headers[grepl("jeunes|femelles|djallonke|effectif", normalized_headers)]
  stop(label, " could not be identified uniquely. Candidate headers: ", paste(candidates, collapse = " | "))
}

normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |>
    str_to_lower() |>
    str_squish()
  x <- str_replace_all(x, "[’'`-]", "") |>
    str_replace_all("[^a-z0-9]", "")
  recode(
    x,
    "toribossito" = "tori",
    "dassazoume" = "dassa",
    "dassazounme" = "dassa",
    .default = x
  )
}

if (!file.exists(DATA_FILE)) stop("File not found: ", DATA_FILE, ". Current folder: ", getwd())
if (!file.exists(ZONES_FILE)) stop("File not found: ", ZONES_FILE, ". Current folder: ", getwd())

raw <- read_excel(DATA_FILE, sheet = 1, col_types = "text", .name_repair = "minimal")
zraw <- read_excel(ZONES_FILE, sheet = 1, col_types = "text", .name_repair = "unique")

COUNT_COL <- resolve_column(
  COUNT_COL, names(raw),
  c("Effectif et composition du cheptel", "jeunes", "6 mois", "femelles", "Djallonke"),
  "Young female Djallonke count"
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
  filter(!is.na(district), district != "", !str_detect(str_to_lower(vegetation_zone), "^total")) |>
  mutate(commune_key = normalize_key(district)) |>
  select(commune_key, vegetation_zone, phytogeo_zone) |>
  distinct(commune_key, .keep_all = TRUE)

parse_count <- function(x) {
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
    count_value = parse_count(.data[[COUNT_COL]])
  ) |>
  left_join(zones, by = "commune_key") |>
  mutate(
    blank = is.na(count_raw) | count_raw == "",
    nonnumeric = !blank & is.na(count_value),
    negative = !is.na(count_value) & count_value < 0,
    noninteger = !is.na(count_value) & count_value >= 0 & abs(count_value - round(count_value)) > 1e-8,
    valid_count = !is.na(count_value) & count_value >= 0 & abs(count_value - round(count_value)) <= 1e-8,
    count_value = ifelse(valid_count, round(count_value), NA_real_)
  )

kw_h_from_ranks <- function(ranks, groups, tie_correction) {
  n <- length(ranks)
  group_factor <- droplevels(factor(groups))
  rank_sums <- rowsum(ranks, group_factor, reorder = FALSE)[, 1]
  group_sizes <- as.numeric(table(group_factor))
  h <- (12 / (n * (n + 1))) * sum((rank_sums^2) / group_sizes) - 3 * (n + 1)
  h / tie_correction
}

run_monte_carlo <- function(zone_var, label, seed_offset = 0L) {
  d <- data |>
    filter(valid_count, !is.na(.data[[zone_var]]), .data[[zone_var]] != "") |>
    transmute(value = count_value, zone = droplevels(factor(.data[[zone_var]])))

  n <- nrow(d)
  k <- nlevels(d$zone)
  if (n == 0L || k < 2L || length(unique(d$value)) < 2L) {
    reason <- if (n == 0L) "No complete valid observations." else if (k < 2L) "Only one zone category represented." else "Only one count value observed."
    return(list(
      summary = data.frame(
        Comparison = label, N = n, Groups = k, Observed_KW_H = NA_real_,
        Degrees_of_freedom = ifelse(k >= 2L, k - 1L, NA_integer_),
        Permutations = B, Monte_Carlo_p_value = NA_real_, Monte_Carlo_SE = NA_real_,
        Monte_Carlo_95CI_low = NA_real_, Monte_Carlo_95CI_high = NA_real_,
        Epsilon_squared = NA_real_, Alpha = ALPHA,
        Decision = "No statistical test performed", Recommendation = reason,
        stringsAsFactors = FALSE
      ),
      descriptive = data.frame(Note = reason)
    ))
  }

  ranks <- rank(d$value, ties.method = "average")
  tie_sizes <- table(d$value)
  tie_correction <- 1 - sum(tie_sizes^3 - tie_sizes) / (n^3 - n)
  if (!is.finite(tie_correction) || tie_correction <= 0) {
    stop(label, ": invalid tie correction because all values are identical.")
  }

  observed_h <- kw_h_from_ranks(ranks, d$zone, tie_correction)
  extreme_count <- 0L
  set.seed(SEED + seed_offset)
  progress_points <- unique(pmax(1L, floor(seq(0.1, 1, 0.1) * B)))

  for (b in seq_len(B)) {
    permuted_h <- kw_h_from_ranks(sample(ranks, length(ranks), replace = FALSE), d$zone, tie_correction)
    if (permuted_h >= observed_h - 1e-12) extreme_count <- extreme_count + 1L
    if (b %in% progress_points) message(label, ": ", b, "/", B, " permutations completed")
  }

  p_value <- (extreme_count + 1) / (B + 1)
  mc_se <- sqrt(p_value * (1 - p_value) / (B + 1))
  ci_low <- max(0, p_value - 1.96 * mc_se)
  ci_high <- min(1, p_value + 1.96 * mc_se)
  epsilon_squared <- max(0, (observed_h - k + 1) / (n - k))

  descriptive <- d |>
    group_by(zone) |>
    summarise(
      N = n(), Mean = mean(value), SD = sd(value), Median = median(value),
      Q1 = quantile(value, 0.25), Q3 = quantile(value, 0.75),
      Minimum = min(value), Maximum = max(value),
      Zero_count = sum(value == 0), Zero_percent = 100 * mean(value == 0),
      .groups = "drop"
    ) |>
    rename(Zone = zone)

  list(
    summary = data.frame(
      Comparison = label, N = n, Groups = k, Observed_KW_H = observed_h,
      Degrees_of_freedom = k - 1L, Permutations = B,
      Monte_Carlo_p_value = p_value, Monte_Carlo_SE = mc_se,
      Monte_Carlo_95CI_low = ci_low, Monte_Carlo_95CI_high = ci_high,
      Epsilon_squared = epsilon_squared, Alpha = ALPHA,
      Decision = ifelse(p_value < ALPHA, "Reject H0: distributions differ by zone", "Do not reject H0: no evidence of a zone difference"),
      Recommendation = "Report the Monte Carlo permutation p-value based on the tie-corrected Kruskal-Wallis H statistic.",
      stringsAsFactors = FALSE
    ),
    descriptive = descriptive
  )
}

vegetation <- run_monte_carlo(
  "vegetation_zone",
  "Young female Djallonke count (>6 months) x vegetation zone",
  0L
)
phytogeo <- run_monte_carlo(
  "phytogeo_zone",
  "Young female Djallonke count (>6 months) x phytogeographic zone",
  100L
)

quality <- data.frame(
  Metric = c(
    "Total records", "Valid nonnegative integer counts", "Zero counts",
    "Blank responses", "Nonnumeric responses", "Negative values",
    "Noninteger nonnegative values", "Unmatched communes"
  ),
  Value = c(
    nrow(data), sum(data$valid_count), sum(data$count_value == 0, na.rm = TRUE),
    sum(data$blank), sum(data$nonnumeric), sum(data$negative),
    sum(data$noninteger), sum(is.na(data$vegetation_zone))
  ),
  stringsAsFactors = FALSE
)

notes <- data.frame(
  Parameter = c("Variable type", "Primary test", "Permutation statistic", "Permutations", "Zero counts", "Effect size", "Interpretation"),
  Assessment = c(
    "Quantitative discrete count variable.",
    "Monte Carlo permutation test for differences among zone distributions.",
    "Tie-corrected Kruskal-Wallis H statistic.",
    paste0(B, " random permutations; finite loop with progress reports."),
    "Zero is retained as a valid count.",
    "Epsilon-squared.",
    "The test evaluates whether the count distributions differ among zones."
  ),
  stringsAsFactors = FALSE
)

sheets <- list(
  Summary = bind_rows(vegetation$summary, phytogeo$summary),
  Veg_descriptive = vegetation$descriptive,
  Phyto_descriptive = phytogeo$descriptive,
  Data_quality = quality,
  Method_notes = notes
)

writexl::write_xlsx(sheets, path = OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, winslash = "/", mustWork = FALSE))
