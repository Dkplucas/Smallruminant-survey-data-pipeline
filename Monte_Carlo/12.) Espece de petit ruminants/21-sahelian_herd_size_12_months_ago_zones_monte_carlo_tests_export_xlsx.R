# Sahelian herd size 12 months ago vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: sahelian_herd_size_12_months_ago_zones_monte_carlo_results.xlsx

packages <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "writexl")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop(
    "Install packages first: install.packages(c(",
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
OUTPUT_FILE <- "sahelian_herd_size_12_months_ago_zones_monte_carlo_results.xlsx"
COUNT_COL <- "12.) Effectif et composition du cheptel/Effectif il y a 12 mois_Saheliens"
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
  normalized_expected <- normalize_header(expected)
  exact_normalized <- which(normalized_headers == normalized_expected)
  if (length(exact_normalized) == 1L) {
    message(label, " matched after normalization to: ", headers[exact_normalized])
    return(headers[exact_normalized])
  }
  normalized_terms <- normalize_header(required_terms)
  hits <- which(vapply(
    normalized_headers,
    function(h) all(vapply(normalized_terms, function(term) grepl(term, h, fixed = TRUE), logical(1))),
    logical(1)
  ))
  if (length(hits) == 1L) {
    message(label, " matched flexibly to: ", headers[hits])
    return(headers[hits])
  }
  candidates <- headers[grepl("effectifilya12mois|sahel", normalized_headers)]
  stop(
    label, " could not be identified uniquely. Candidate headers: ",
    if (length(candidates)) paste(candidates, collapse = " | ") else "none"
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
  COUNT_COL,
  names(raw),
  c("Effectif et composition du cheptel", "Effectif il y a 12 mois", "Saheliens"),
  "Sahelian herd-size variable"
)
COMMUNE_COL <- resolve_column(
  COMMUNE_COL,
  names(raw),
  c("CARACTERISTIQUES", "UNITE", "ELEVAGE", "Commune"),
  "Commune variable"
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
    !is.na(vegetation_zone),
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
    commune_raw = as.character(.data[[COMMUNE_COL]]),
    commune_key = normalize_key(.data[[COMMUNE_COL]]),
    count_raw = str_squish(as.character(.data[[COUNT_COL]])),
    count = parse_count(.data[[COUNT_COL]])
  ) |>
  left_join(zones, by = "commune_key") |>
  mutate(
    blank = is.na(count_raw) | count_raw == "",
    nonnumeric = !blank & is.na(count),
    negative = !is.na(count) & count < 0,
    noninteger = !is.na(count) & count >= 0 & abs(count - round(count)) > 1e-9,
    valid_count = !is.na(count) & count >= 0 & abs(count - round(count)) <= 1e-9,
    count = ifelse(valid_count, round(count), NA_real_)
  )

kw_h_from_ranks <- function(ranks, groups, tie_correction) {
  n <- length(ranks)
  group_factor <- droplevels(factor(groups))
  group_sizes <- as.numeric(table(group_factor))
  rank_sums <- as.numeric(rowsum(ranks, group_factor, reorder = FALSE))
  h_uncorrected <- 12 / (n * (n + 1)) * sum((rank_sums^2) / group_sizes) - 3 * (n + 1)
  h_uncorrected / tie_correction
}

run_monte_carlo <- function(zone_var, comparison_label, seed_offset = 0L) {
  d <- data |>
    filter(valid_count, !is.na(.data[[zone_var]]), .data[[zone_var]] != "") |>
    transmute(value = count, zone = droplevels(factor(.data[[zone_var]])))

  n <- nrow(d)
  k <- nlevels(d$zone)
  if (n == 0L || k < 2L || length(unique(d$value)) < 2L) {
    reason <- if (n == 0L) {
      "No complete valid observations."
    } else if (k < 2L) {
      "Only one geographical-zone category is represented."
    } else {
      "The count variable has no observed variation."
    }
    return(data.frame(
      Comparison = comparison_label, N = n, Groups = k,
      Observed_Kruskal_Wallis_H = NA_real_, Degrees_of_freedom = NA_integer_,
      Permutations = 0L, Extreme_permutations = NA_integer_,
      Monte_Carlo_p_value = NA_real_, Monte_Carlo_SE = NA_real_,
      Monte_Carlo_95CI_low = NA_real_, Monte_Carlo_95CI_high = NA_real_,
      Epsilon_squared = NA_real_, Alpha = ALPHA,
      Decision = "No statistical test performed", Recommendation = reason,
      stringsAsFactors = FALSE
    ))
  }

  ranks <- rank(d$value, ties.method = "average")
  tie_sizes <- as.numeric(table(d$value))
  tie_correction <- 1 - sum(tie_sizes^3 - tie_sizes) / (n^3 - n)
  if (!is.finite(tie_correction) || tie_correction <= 0) stop("Invalid tie correction.")

  observed_h <- kw_h_from_ranks(ranks, d$zone, tie_correction)
  set.seed(SEED + seed_offset)
  extreme_count <- 0L
  progress_points <- unique(pmax(1L, floor(seq(0.1, 1, by = 0.1) * B)))

  for (b in seq_len(B)) {
    permuted_h <- kw_h_from_ranks(sample(ranks, size = n, replace = FALSE), d$zone, tie_correction)
    if (permuted_h >= observed_h - 1e-12) extreme_count <- extreme_count + 1L
    if (b %in% progress_points) {
      message(comparison_label, ": ", b, "/", B, " permutations completed")
    }
  }

  p_value <- (extreme_count + 1) / (B + 1)
  mc_se <- sqrt(p_value * (1 - p_value) / (B + 1))
  ci_low <- max(0, p_value - 1.96 * mc_se)
  ci_high <- min(1, p_value + 1.96 * mc_se)
  epsilon_squared <- max(0, (observed_h - k + 1) / (n - k))

  data.frame(
    Comparison = comparison_label, N = n, Groups = k,
    Observed_Kruskal_Wallis_H = observed_h, Degrees_of_freedom = k - 1L,
    Permutations = B, Extreme_permutations = extreme_count,
    Monte_Carlo_p_value = p_value, Monte_Carlo_SE = mc_se,
    Monte_Carlo_95CI_low = ci_low, Monte_Carlo_95CI_high = ci_high,
    Epsilon_squared = epsilon_squared, Alpha = ALPHA,
    Decision = ifelse(p_value < ALPHA, "Reject H0: distributions differ by zone", "Do not reject H0: no evidence of a zone difference"),
    Recommendation = "Report the Monte Carlo permutation p-value based on the tie-corrected Kruskal-Wallis H statistic.",
    stringsAsFactors = FALSE
  )
}

describe_by_zone <- function(zone_var) {
  data |>
    filter(valid_count, !is.na(.data[[zone_var]]), .data[[zone_var]] != "") |>
    group_by(zone = .data[[zone_var]]) |>
    summarise(
      N = n(), Mean = mean(count), SD = sd(count), Median = median(count),
      Q1 = quantile(count, 0.25, names = FALSE),
      Q3 = quantile(count, 0.75, names = FALSE),
      Minimum = min(count), Maximum = max(count),
      Zero_count = sum(count == 0), Zero_percent = 100 * mean(count == 0),
      .groups = "drop"
    )
}

veg_result <- run_monte_carlo(
  "vegetation_zone",
  "Sahelian herd size 12 months ago x vegetation zone",
  0L
)
phyto_result <- run_monte_carlo(
  "phytogeo_zone",
  "Sahelian herd size 12 months ago x phytogeographic zone",
  100L
)

summary_results <- bind_rows(veg_result, phyto_result)
veg_descriptive <- describe_by_zone("vegetation_zone")
phyto_descriptive <- describe_by_zone("phytogeo_zone")

quality <- data.frame(
  Metric = c(
    "Total records", "Valid nonnegative integer counts", "Zero counts",
    "Blank responses", "Nonnumeric responses", "Negative values",
    "Noninteger nonnegative values", "Unmatched communes"
  ),
  Value = c(
    nrow(data), sum(data$valid_count), sum(data$count == 0, na.rm = TRUE),
    sum(data$blank), sum(data$nonnumeric), sum(data$negative),
    sum(data$noninteger), sum(is.na(data$vegetation_zone))
  ),
  stringsAsFactors = FALSE
)

notes <- data.frame(
  Parameter = c("Variable type", "Primary test", "Permutation statistic", "Permutations", "Zero treatment", "Effect size", "Null hypothesis"),
  Assessment = c(
    "Quantitative discrete count variable.",
    "Monte Carlo permutation test.",
    "Tie-corrected Kruskal-Wallis H statistic.",
    paste0(B, " finite permutations per geographical comparison."),
    "Zero is retained as a valid count.",
    "Epsilon-squared.",
    "The distribution of the count is identical across geographical zones."
  ),
  stringsAsFactors = FALSE
)

sheets <- list(
  Summary = summary_results,
  Veg_descriptive = veg_descriptive,
  Phyto_descriptive = phyto_descriptive,
  Data_quality = quality,
  Method_notes = notes
)

write_xlsx(sheets, path = OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, winslash = "/", mustWork = FALSE))
