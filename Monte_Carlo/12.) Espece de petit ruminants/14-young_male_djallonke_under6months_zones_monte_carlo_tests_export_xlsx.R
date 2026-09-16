# Young male Djallonke count under 6 months vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: young_male_djallonke_under6months_zones_monte_carlo_results.xlsx

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
OUTPUT_FILE <- "young_male_djallonke_under6months_zones_monte_carlo_results.xlsx"
COUNT_COL <- "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) mâle_Djallonke"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
ALPHA <- 0.05
B <- 10000L
SEED <- 20260916L

if (!file.exists(DATA_FILE)) stop("File not found: ", DATA_FILE, ". Current folder: ", getwd())
if (!file.exists(ZONES_FILE)) stop("File not found: ", ZONES_FILE, ". Current folder: ", getwd())

normalize_header <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(str_squish(x))
  str_replace_all(x, "[^a-z0-9]", "")
}

resolve_column <- function(expected, headers, terms, label) {
  if (expected %in% headers) return(expected)
  normalized_headers <- normalize_header(headers)
  exact_hit <- which(normalized_headers == normalize_header(expected))
  if (length(exact_hit) == 1L) {
    message(label, " matched after normalization to: ", headers[exact_hit])
    return(headers[exact_hit])
  }
  normalized_terms <- normalize_header(terms)
  flexible_hit <- which(vapply(
    normalized_headers,
    function(h) all(vapply(normalized_terms, function(term) grepl(term, h, fixed = TRUE), logical(1))),
    logical(1)
  ))
  if (length(flexible_hit) == 1L) {
    message(label, " matched flexibly to: ", headers[flexible_hit])
    return(headers[flexible_hit])
  }
  candidates <- headers[grepl("effectif|composition|petits|djallonke|commune", normalized_headers)]
  stop(label, " could not be identified uniquely. Candidate headers: ", paste(candidates, collapse = " | "))
}

normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(str_squish(x))
  x <- str_replace_all(x, "[’'`-]", "")
  x <- str_replace_all(x, "[^a-z0-9]", "")
  recode(x,
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
  c("Effectif et composition du cheptel", "Nbre de petits", "6 mois", "male", "Djallonke"),
  "Young male Djallonke count under 6 months"
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

clean_numeric_text <- function(x) {
  x <- str_squish(as.character(x))
  x <- str_replace_all(x, "\\u00A0", "")
  x <- str_replace_all(x, ",", ".")
  x
}

analysis_data <- raw |>
  transmute(
    row_id = row_number(),
    commune_raw = .data[[COMMUNE_COL]],
    commune_key = normalize_key(.data[[COMMUNE_COL]]),
    count_raw = str_squish(.data[[COUNT_COL]]),
    count_clean = clean_numeric_text(.data[[COUNT_COL]]),
    count_value = suppressWarnings(as.numeric(count_clean))
  ) |>
  left_join(zones, by = "commune_key") |>
  mutate(
    blank_count = is.na(count_raw) | count_raw == "",
    nonnumeric_count = !blank_count & is.na(count_value),
    negative_count = !is.na(count_value) & count_value < 0,
    noninteger_count = !is.na(count_value) & count_value >= 0 & abs(count_value - round(count_value)) > 1e-8,
    valid_count = !blank_count & !nonnumeric_count & !negative_count & !noninteger_count
  )

kw_h_from_ranks <- function(ranks, group_index, group_sizes, n, tie_correction) {
  rank_sums <- rowsum(ranks, group_index, reorder = FALSE)[, 1]
  h_uncorrected <- (12 / (n * (n + 1))) * sum((rank_sums^2) / group_sizes) - 3 * (n + 1)
  h_uncorrected / tie_correction
}

run_monte_carlo <- function(zone_var, label, seed_offset = 0L) {
  d <- analysis_data |>
    filter(valid_count, !is.na(.data[[zone_var]]), .data[[zone_var]] != "") |>
    transmute(value = count_value, zone = factor(.data[[zone_var]])) |>
    droplevels()

  n <- nrow(d)
  k <- nlevels(d$zone)
  if (n == 0L || k < 2L) {
    reason <- if (n == 0L) "No valid complete observations." else "Only one geographical group is represented."
    return(list(
      summary = data.frame(
        Comparison = label, N = n, Groups = k, Observed_KW_H = NA_real_,
        Degrees_of_freedom = NA_integer_, Permutations = B,
        Monte_Carlo_p_value = NA_real_, Monte_Carlo_SE = NA_real_,
        Monte_Carlo_95CI_low = NA_real_, Monte_Carlo_95CI_high = NA_real_,
        Epsilon_squared = NA_real_, Alpha = ALPHA,
        Decision = "No statistical test performed", Recommendation = reason,
        stringsAsFactors = FALSE
      ),
      descriptive = data.frame(Note = reason, stringsAsFactors = FALSE)
    ))
  }

  ranks <- rank(d$value, ties.method = "average")
  tie_sizes <- as.numeric(table(d$value))
  tie_correction <- 1 - sum(tie_sizes^3 - tie_sizes) / (n^3 - n)
  if (!is.finite(tie_correction) || tie_correction <= 0) {
    reason <- "Not testable because all valid count values are identical."
    return(list(
      summary = data.frame(
        Comparison = label, N = n, Groups = k, Observed_KW_H = 0,
        Degrees_of_freedom = k - 1L, Permutations = B,
        Monte_Carlo_p_value = NA_real_, Monte_Carlo_SE = NA_real_,
        Monte_Carlo_95CI_low = NA_real_, Monte_Carlo_95CI_high = NA_real_,
        Epsilon_squared = 0, Alpha = ALPHA,
        Decision = "No statistical test performed", Recommendation = reason,
        stringsAsFactors = FALSE
      ),
      descriptive = d |>
        group_by(zone) |>
        summarise(N = n(), Mean = mean(value), SD = sd(value), Median = median(value),
                  Q1 = quantile(value, 0.25), Q3 = quantile(value, 0.75),
                  Minimum = min(value), Maximum = max(value), Zero_count = sum(value == 0),
                  Zero_percent = 100 * mean(value == 0), .groups = "drop")
    ))
  }

  group_index <- as.integer(d$zone)
  group_sizes <- as.numeric(table(d$zone))
  observed_h <- kw_h_from_ranks(ranks, group_index, group_sizes, n, tie_correction)

  set.seed(SEED + seed_offset)
  extreme_count <- 0L
  progress_step <- max(1L, B %/% 10L)
  tolerance <- sqrt(.Machine$double.eps)

  for (b in seq_len(B)) {
    permuted_ranks <- sample(ranks, size = n, replace = FALSE)
    permuted_h <- kw_h_from_ranks(permuted_ranks, group_index, group_sizes, n, tie_correction)
    if (permuted_h >= observed_h - tolerance) extreme_count <- extreme_count + 1L
    if (b %% progress_step == 0L || b == B) {
      message(label, ": ", b, "/", B, " permutations completed")
    }
  }

  p_mc <- (extreme_count + 1) / (B + 1)
  mc_se <- sqrt(p_mc * (1 - p_mc) / (B + 1))
  ci_low <- max(0, p_mc - 1.96 * mc_se)
  ci_high <- min(1, p_mc + 1.96 * mc_se)
  epsilon_squared <- max(0, (observed_h - k + 1) / (n - k))

  descriptive <- d |>
    group_by(zone) |>
    summarise(
      N = n(), Mean = mean(value), SD = sd(value), Median = median(value),
      Q1 = quantile(value, 0.25), Q3 = quantile(value, 0.75),
      Minimum = min(value), Maximum = max(value),
      Zero_count = sum(value == 0), Zero_percent = 100 * mean(value == 0),
      .groups = "drop"
    )

  summary <- data.frame(
    Comparison = label, N = n, Groups = k, Observed_KW_H = observed_h,
    Degrees_of_freedom = k - 1L, Permutations = B,
    Monte_Carlo_p_value = p_mc, Monte_Carlo_SE = mc_se,
    Monte_Carlo_95CI_low = ci_low, Monte_Carlo_95CI_high = ci_high,
    Epsilon_squared = epsilon_squared, Alpha = ALPHA,
    Decision = ifelse(p_mc < ALPHA,
      "Reject H0: count distributions differ across zones",
      "Do not reject H0: no evidence of different count distributions"
    ),
    Recommendation = "Report the Monte Carlo permutation p-value and median (IQR) by zone.",
    stringsAsFactors = FALSE
  )

  list(summary = summary, descriptive = descriptive)
}

veg <- run_monte_carlo(
  "vegetation_zone",
  "Young male Djallonke count under 6 months x vegetation zone",
  0L
)
phyto <- run_monte_carlo(
  "phytogeo_zone",
  "Young male Djallonke count under 6 months x phytogeographic zone",
  100L
)

summary_results <- bind_rows(veg$summary, phyto$summary)

data_quality <- data.frame(
  Metric = c(
    "Total records", "Valid nonnegative integer counts", "Zero counts",
    "Positive counts", "Blank responses", "Nonnumeric responses",
    "Negative values", "Noninteger values", "Unmatched communes"
  ),
  Value = c(
    nrow(analysis_data), sum(analysis_data$valid_count),
    sum(analysis_data$valid_count & analysis_data$count_value == 0, na.rm = TRUE),
    sum(analysis_data$valid_count & analysis_data$count_value > 0, na.rm = TRUE),
    sum(analysis_data$blank_count), sum(analysis_data$nonnumeric_count),
    sum(analysis_data$negative_count, na.rm = TRUE),
    sum(analysis_data$noninteger_count, na.rm = TRUE),
    sum(is.na(analysis_data$vegetation_zone))
  ),
  stringsAsFactors = FALSE
)

method_notes <- data.frame(
  Parameter = c(
    "Variable type", "Valid values", "Primary test", "Permutation statistic",
    "Permutations", "P-value formula", "Effect size", "Reporting"
  ),
  Assessment = c(
    "Quantitative discrete count variable.",
    "Nonnegative integers; zero is valid.",
    "Monte Carlo permutation test for equality of distributions across zones.",
    "Tie-corrected Kruskal-Wallis H statistic.",
    paste0(B, " finite permutations with progress reported every 10%."),
    "(number of permuted H values at least as large as observed H + 1) / (B + 1).",
    "Epsilon-squared based on the observed Kruskal-Wallis H statistic.",
    "Report median, interquartile range, Monte Carlo p-value, and epsilon-squared."
  ),
  stringsAsFactors = FALSE
)

sheets <- list(
  Summary = summary_results,
  Veg_descriptive = veg$descriptive,
  Phyto_descriptive = phyto$descriptive,
  Data_quality = data_quality,
  Method_notes = method_notes
)

writexl::write_xlsx(sheets, path = OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, mustWork = FALSE))
