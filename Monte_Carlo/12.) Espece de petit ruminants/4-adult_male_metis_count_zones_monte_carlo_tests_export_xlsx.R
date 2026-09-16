# Adult male Metis count vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: adult_male_metis_count_zones_monte_carlo_results.xlsx

packages <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "writexl")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
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
OUTPUT_FILE <- "adult_male_metis_count_zones_monte_carlo_results.xlsx"
COUNT_COL <- "12.) Effectif et composition du cheptel/Nombre de mâle adultes Metis"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
B <- 10000L
SEED <- 20260916L
ALPHA <- 0.05

if (!file.exists(DATA_FILE)) stop("File not found: ", DATA_FILE, ". Current folder: ", getwd())
if (!file.exists(ZONES_FILE)) stop("File not found: ", ZONES_FILE, ". Current folder: ", getwd())

normalize_header <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(str_squish(x))
  str_replace_all(x, "[^a-z0-9]", "")
}

resolve_column <- function(expected, headers, required_terms, label) {
  if (expected %in% headers) return(expected)
  normalized_headers <- normalize_header(headers)
  exact_hits <- which(normalized_headers == normalize_header(expected))
  if (length(exact_hits) == 1L) {
    message(label, " matched after normalization to: ", headers[exact_hits])
    return(headers[exact_hits])
  }
  normalized_terms <- normalize_header(required_terms)
  flexible_hits <- which(vapply(
    normalized_headers,
    function(h) all(vapply(normalized_terms, function(term) grepl(term, h, fixed = TRUE), logical(1))),
    logical(1)
  ))
  if (length(flexible_hits) == 1L) {
    message(label, " matched flexibly to: ", headers[flexible_hits])
    return(headers[flexible_hits])
  }
  candidates <- headers[grepl("effectif|composition|male|metis|commune", normalized_headers)]
  stop(
    label, " could not be identified uniquely. Candidate headers: ",
    paste(candidates, collapse = " | ")
  )
}

normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(str_squish(x))
  x <- str_replace_all(x, "[’'`-]", "")
  x <- str_replace_all(x, "[^a-z0-9]", "")
  recode(
    x,
    "toribossito" = "tori",
    "dassazoume" = "dassa",
    "dassazounme" = "dassa",
    .default = x
  )
}

raw <- read_excel(DATA_FILE, sheet = 1, col_types = "text", .name_repair = "minimal")
zraw <- read_excel(ZONES_FILE, sheet = 1, col_types = "text", .name_repair = "unique")

COUNT_COL <- resolve_column(
  COUNT_COL, names(raw),
  c("Effectif et composition du cheptel", "Nombre de mâle adultes", "Metis"),
  "Adult-male Metis count"
)
COMMUNE_COL <- resolve_column(
  COMMUNE_COL, names(raw),
  c("CARACTERISTIQUES", "UNITE", "ELEVAGE", "Commune"),
  "Commune"
)

required_zone_columns <- c("Vegetation zones", "Phytogeographic zones", "District")
if (!all(required_zone_columns %in% names(zraw))) {
  stop("zones.xlsx must contain: ", paste(required_zone_columns, collapse = ", "))
}

zones <- zraw |>
  transmute(
    vegetation_zone = str_squish(str_replace_all(`Vegetation zones`, "\\u00A0", " ")),
    phytogeo_zone = str_squish(str_replace_all(`Phytogeographic zones`, "\\u00A0", " ")),
    district = str_squish(str_replace_all(District, "\\u00A0", " "))
  ) |>
  fill(vegetation_zone, phytogeo_zone) |>
  filter(
    !is.na(district), district != "",
    !is.na(vegetation_zone), vegetation_zone != "",
    !str_detect(str_to_lower(vegetation_zone), "^total")
  ) |>
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
    valid_count = !blank & !nonnumeric & !negative & !noninteger
  )

kw_h_from_ranks <- function(ranks, group_index, group_sizes, n, tie_correction) {
  rank_sums <- numeric(length(group_sizes))
  for (g in seq_along(group_sizes)) {
    rank_sums[g] <- sum(ranks[group_index == g])
  }
  h_uncorrected <- (12 / (n * (n + 1))) * sum((rank_sums^2) / group_sizes) - 3 * (n + 1)
  h_uncorrected / tie_correction
}

run_monte_carlo <- function(zone_var, label, seed_offset = 0L) {
  d <- data |>
    filter(valid_count, !is.na(.data[[zone_var]]), .data[[zone_var]] != "") |>
    transmute(value = count_value, zone = factor(.data[[zone_var]])) |>
    droplevels()

  n <- nrow(d)
  k <- nlevels(d$zone)
  if (n == 0L || k < 2L) {
    reason <- if (n == 0L) "No complete valid observations." else "Only one zone category is represented."
    return(list(
      summary = data.frame(
        Comparison = label, N = n, Groups = k, Observed_KW_H = NA_real_,
        Degrees_of_freedom = NA_integer_, Permutations = B,
        Extreme_permutations = NA_integer_, Monte_Carlo_p_value = NA_real_,
        Monte_Carlo_SE = NA_real_, Monte_Carlo_95CI_low = NA_real_,
        Monte_Carlo_95CI_high = NA_real_, Epsilon_squared = NA_real_,
        Alpha = ALPHA, Decision = "No statistical test performed",
        Recommendation = reason, stringsAsFactors = FALSE
      ),
      descriptive = data.frame(Note = reason, stringsAsFactors = FALSE)
    ))
  }

  ranks <- rank(d$value, ties.method = "average")
  group_index <- as.integer(d$zone)
  group_sizes <- tabulate(group_index, nbins = k)
  tie_sizes <- as.numeric(table(d$value))
  tie_correction <- 1 - sum(tie_sizes^3 - tie_sizes) / (n^3 - n)
  if (!is.finite(tie_correction) || tie_correction <= 0) {
    reason <- "Not testable because all valid count values are identical."
    return(list(
      summary = data.frame(
        Comparison = label, N = n, Groups = k, Observed_KW_H = 0,
        Degrees_of_freedom = k - 1L, Permutations = B,
        Extreme_permutations = NA_integer_, Monte_Carlo_p_value = NA_real_,
        Monte_Carlo_SE = NA_real_, Monte_Carlo_95CI_low = NA_real_,
        Monte_Carlo_95CI_high = NA_real_, Epsilon_squared = 0,
        Alpha = ALPHA, Decision = "No statistical test performed",
        Recommendation = reason, stringsAsFactors = FALSE
      ),
      descriptive = d |>
        group_by(zone) |>
        summarise(N = n(), Mean = mean(value), SD = sd(value), Median = median(value),
                  Q1 = as.numeric(quantile(value, 0.25)), Q3 = as.numeric(quantile(value, 0.75)),
                  Minimum = min(value), Maximum = max(value), Zero_count = sum(value == 0),
                  Zero_percent = 100 * mean(value == 0), .groups = "drop")
    ))
  }

  observed_h <- kw_h_from_ranks(ranks, group_index, group_sizes, n, tie_correction)
  set.seed(SEED + seed_offset)
  extreme <- 0L
  checkpoints <- unique(pmax(1L, round(seq(0.1, 1, by = 0.1) * B)))
  checkpoint_index <- 1L

  for (b in seq_len(B)) {
    permuted_ranks <- sample(ranks, size = n, replace = FALSE)
    permuted_h <- kw_h_from_ranks(permuted_ranks, group_index, group_sizes, n, tie_correction)
    if (permuted_h >= observed_h - 1e-12) extreme <- extreme + 1L
    if (checkpoint_index <= length(checkpoints) && b == checkpoints[checkpoint_index]) {
      message(label, ": ", b, "/", B, " permutations completed")
      checkpoint_index <- checkpoint_index + 1L
    }
  }

  p_mc <- (extreme + 1) / (B + 1)
  mc_se <- sqrt(p_mc * (1 - p_mc) / (B + 1))
  ci_low <- max(0, p_mc - 1.96 * mc_se)
  ci_high <- min(1, p_mc + 1.96 * mc_se)
  epsilon_sq <- max(0, (observed_h - k + 1) / (n - k))

  descriptive <- d |>
    group_by(zone) |>
    summarise(
      N = n(), Mean = mean(value), SD = sd(value), Median = median(value),
      Q1 = as.numeric(quantile(value, 0.25)), Q3 = as.numeric(quantile(value, 0.75)),
      Minimum = min(value), Maximum = max(value), Zero_count = sum(value == 0),
      Zero_percent = 100 * mean(value == 0), .groups = "drop"
    )

  summary <- data.frame(
    Comparison = label, N = n, Groups = k, Observed_KW_H = observed_h,
    Degrees_of_freedom = k - 1L, Permutations = B,
    Extreme_permutations = extreme, Monte_Carlo_p_value = p_mc,
    Monte_Carlo_SE = mc_se, Monte_Carlo_95CI_low = ci_low,
    Monte_Carlo_95CI_high = ci_high, Epsilon_squared = epsilon_sq,
    Alpha = ALPHA,
    Decision = ifelse(p_mc < ALPHA, "Reject H0: distributions differ by zone",
                      "Do not reject H0: no evidence of a zone difference"),
    Recommendation = "Report the Monte Carlo permutation p-value based on the Kruskal-Wallis H statistic.",
    stringsAsFactors = FALSE
  )
  list(summary = summary, descriptive = descriptive)
}

veg <- run_monte_carlo(
  "vegetation_zone",
  "Adult male Metis count x vegetation zone",
  0L
)
phyto <- run_monte_carlo(
  "phytogeo_zone",
  "Adult male Metis count x phytogeographic zone",
  100L
)

summary_results <- bind_rows(veg$summary, phyto$summary)
quality <- data.frame(
  Metric = c(
    "Total records", "Valid nonnegative integer counts", "Zero counts",
    "Blank responses", "Nonnumeric responses", "Negative counts",
    "Noninteger nonnegative values", "Unmatched communes"
  ),
  Value = c(
    nrow(data), sum(data$valid_count, na.rm = TRUE),
    sum(data$valid_count & data$count_value == 0, na.rm = TRUE),
    sum(data$blank, na.rm = TRUE), sum(data$nonnumeric, na.rm = TRUE),
    sum(data$negative, na.rm = TRUE), sum(data$noninteger, na.rm = TRUE),
    sum(is.na(data$vegetation_zone) | data$vegetation_zone == "", na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

notes <- data.frame(
  Parameter = c(
    "Variable type", "Primary test", "Permutation statistic", "Permutations",
    "Treatment of zero", "Invalid values", "Null hypothesis", "Effect size"
  ),
  Assessment = c(
    "Quantitative discrete count variable.",
    "Monte Carlo permutation test.",
    "Kruskal-Wallis H statistic calculated from ranks with tie correction.",
    paste0(B, " finite permutations; progress is printed every 10%."),
    "Zero is valid and means no adult male Metis animals.",
    "Blank, nonnumeric, negative, and noninteger values are excluded and reported.",
    "The count distribution is the same across geographical zones.",
    "Epsilon-squared based on the observed Kruskal-Wallis statistic."
  ),
  stringsAsFactors = FALSE
)

sheets <- list(
  Summary = summary_results,
  Veg_descriptive = veg$descriptive,
  Phyto_descriptive = phyto$descriptive,
  Data_quality = quality,
  Method_notes = notes
)

writexl::write_xlsx(sheets, path = OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, winslash = "/", mustWork = FALSE))
