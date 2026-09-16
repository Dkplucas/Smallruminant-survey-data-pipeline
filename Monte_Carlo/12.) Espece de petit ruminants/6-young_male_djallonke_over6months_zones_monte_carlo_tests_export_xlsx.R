# Young male Djallonke count (>6 months) vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: young_male_djallonke_over6months_zones_monte_carlo_results.xlsx

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
OUTPUT_FILE <- "young_male_djallonke_over6months_zones_monte_carlo_results.xlsx"
B <- 10000L
SEED <- 20260916L
ALPHA <- 0.05

COUNT_COL <- "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) mâles_Djallonke"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

normalize_header <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(str_squish(x))
  str_replace_all(x, "[^a-z0-9]", "")
}

resolve_column <- function(expected, headers, terms, label) {
  if (expected %in% headers) return(expected)

  normalized_headers <- normalize_header(headers)
  normalized_expected <- normalize_header(expected)
  exact_normalized <- which(normalized_headers == normalized_expected)

  if (length(exact_normalized) == 1L) {
    message(label, " matched after normalization to: ", headers[exact_normalized])
    return(headers[exact_normalized])
  }

  normalized_terms <- normalize_header(terms)
  flexible_hits <- which(vapply(
    normalized_headers,
    function(header) all(vapply(
      normalized_terms,
      function(term) str_detect(header, fixed(term)),
      logical(1)
    )),
    logical(1)
  ))

  if (length(flexible_hits) == 1L) {
    message(label, " matched flexibly to: ", headers[flexible_hits])
    return(headers[flexible_hits])
  }

  candidates <- headers[str_detect(
    normalized_headers,
    "effectifcomposition|jeunes|6mois|males|djallonke"
  )]
  stop(
    label,
    " column could not be identified uniquely. Candidate headers: ",
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

if (!file.exists(DATA_FILE)) stop("File not found: ", DATA_FILE, ". Current folder: ", getwd())
if (!file.exists(ZONES_FILE)) stop("File not found: ", ZONES_FILE, ". Current folder: ", getwd())

raw <- read_excel(DATA_FILE, sheet = 1, col_types = "text", .name_repair = "minimal")
zraw <- read_excel(ZONES_FILE, sheet = 1, col_types = "text", .name_repair = "unique")

COUNT_COL <- resolve_column(
  COUNT_COL,
  names(raw),
  c("Effectif et composition du cheptel", "Nbre de jeunes", "6 mois", "mâles", "Djallonke"),
  "Young male Djallonke count"
)
COMMUNE_COL <- resolve_column(
  COMMUNE_COL,
  names(raw),
  c("CARACTERISTIQUES", "UNITE", "ELEVAGE", "Commune"),
  "Commune"
)

required_zone_columns <- c("Vegetation zones", "Phytogeographic zones", "District")
missing_zone_columns <- setdiff(required_zone_columns, names(zraw))
if (length(missing_zone_columns) > 0L) {
  stop("Missing columns in zones.xlsx: ", paste(missing_zone_columns, collapse = ", "))
}

zones <- zraw |>
  transmute(
    vegetation_zone = str_squish(str_replace_all(`Vegetation zones`, "\\u00A0", " ")),
    phytogeo_zone = str_squish(str_replace_all(`Phytogeographic zones`, "\\u00A0", " ")),
    district = str_squish(str_replace_all(District, "\\u00A0", " "))
  ) |>
  fill(vegetation_zone, phytogeo_zone) |>
  filter(
    !is.na(district),
    district != "",
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
    count = parse_count(.data[[COUNT_COL]])
  ) |>
  left_join(zones, by = "commune_key") |>
  mutate(
    blank = is.na(count_raw) | count_raw == "",
    nonnumeric = !blank & is.na(count),
    negative = !is.na(count) & count < 0,
    noninteger = !is.na(count) & count >= 0 & abs(count - round(count)) > 1e-8,
    valid_count = !is.na(count) & count >= 0 & abs(count - round(count)) <= 1e-8,
    count = ifelse(valid_count, round(count), NA_real_)
  )

kw_h_from_ranks <- function(ranks, group_id, group_sizes, tie_correction) {
  rank_sums <- rowsum(ranks, group_id, reorder = FALSE)
  n <- length(ranks)
  h_uncorrected <- (12 / (n * (n + 1))) * sum((rank_sums[, 1]^2) / group_sizes) - 3 * (n + 1)
  if (!is.finite(tie_correction) || tie_correction <= 0) return(NA_real_)
  h_uncorrected / tie_correction
}

run_monte_carlo <- function(zone_var, label) {
  d <- data |>
    filter(
      valid_count,
      !is.na(.data[[zone_var]]),
      .data[[zone_var]] != ""
    ) |>
    transmute(value = count, zone = factor(.data[[zone_var]])) |>
    droplevels()

  n <- nrow(d)
  k <- nlevels(d$zone)

  if (n == 0L || k < 2L) {
    reason <- if (n == 0L) "No complete valid observations." else "Only one zone category is represented."
    summary <- data.frame(
      Comparison = label,
      N = n,
      Groups = k,
      Observed_Kruskal_Wallis_H = NA_real_,
      Degrees_of_freedom = NA_integer_,
      Monte_Carlo_permutations = B,
      Extreme_permutations = NA_integer_,
      Monte_Carlo_p_value = NA_real_,
      Monte_Carlo_SE = NA_real_,
      Monte_Carlo_95CI_low = NA_real_,
      Monte_Carlo_95CI_high = NA_real_,
      Epsilon_squared = NA_real_,
      Alpha = ALPHA,
      Decision = "No statistical test performed",
      Recommendation = paste("Not testable:", reason),
      stringsAsFactors = FALSE
    )
    return(list(summary = summary, descriptive = data.frame(Note = reason)))
  }

  ranks <- rank(d$value, ties.method = "average")
  group_id <- as.integer(d$zone)
  group_sizes <- tabulate(group_id, nbins = k)
  tie_sizes <- as.numeric(table(d$value))
  tie_correction <- 1 - sum(tie_sizes^3 - tie_sizes) / (n^3 - n)
  observed_h <- kw_h_from_ranks(ranks, group_id, group_sizes, tie_correction)

  if (!is.finite(observed_h)) {
    summary <- data.frame(
      Comparison = label, N = n, Groups = k,
      Observed_Kruskal_Wallis_H = NA_real_, Degrees_of_freedom = k - 1L,
      Monte_Carlo_permutations = B, Extreme_permutations = NA_integer_,
      Monte_Carlo_p_value = NA_real_, Monte_Carlo_SE = NA_real_,
      Monte_Carlo_95CI_low = NA_real_, Monte_Carlo_95CI_high = NA_real_,
      Epsilon_squared = NA_real_, Alpha = ALPHA,
      Decision = "No statistical test performed",
      Recommendation = "Not testable: all valid counts are identical or tie correction is undefined.",
      stringsAsFactors = FALSE
    )
  } else {
    set.seed(SEED + ifelse(zone_var == "phytogeo_zone", 100L, 0L))
    extreme_count <- 0L
    progress_step <- max(1L, B %/% 10L)

    for (b in seq_len(B)) {
      permuted_ranks <- sample(ranks, size = n, replace = FALSE)
      permuted_h <- kw_h_from_ranks(permuted_ranks, group_id, group_sizes, tie_correction)
      if (is.finite(permuted_h) && permuted_h >= observed_h - 1e-12) {
        extreme_count <- extreme_count + 1L
      }
      if (b %% progress_step == 0L || b == B) {
        message(label, ": ", b, "/", B, " permutations completed")
      }
    }

    p_mc <- (extreme_count + 1) / (B + 1)
    mc_se <- sqrt(p_mc * (1 - p_mc) / (B + 1))
    ci_low <- max(0, p_mc - 1.96 * mc_se)
    ci_high <- min(1, p_mc + 1.96 * mc_se)
    epsilon_squared <- max(0, (observed_h - k + 1) / (n - k))

    summary <- data.frame(
      Comparison = label,
      N = n,
      Groups = k,
      Observed_Kruskal_Wallis_H = observed_h,
      Degrees_of_freedom = k - 1L,
      Monte_Carlo_permutations = B,
      Extreme_permutations = extreme_count,
      Monte_Carlo_p_value = p_mc,
      Monte_Carlo_SE = mc_se,
      Monte_Carlo_95CI_low = ci_low,
      Monte_Carlo_95CI_high = ci_high,
      Epsilon_squared = epsilon_squared,
      Alpha = ALPHA,
      Decision = ifelse(p_mc < ALPHA, "Reject H0: distributions differ by zone", "Do not reject H0: no evidence of a difference by zone"),
      Recommendation = "Report the Monte Carlo permutation p-value based on the Kruskal-Wallis H statistic.",
      stringsAsFactors = FALSE
    )
  }

  descriptive <- d |>
    group_by(zone) |>
    summarise(
      N = n(),
      Mean = mean(value),
      SD = ifelse(n() > 1L, sd(value), NA_real_),
      Median = median(value),
      Q1 = as.numeric(quantile(value, 0.25, names = FALSE)),
      Q3 = as.numeric(quantile(value, 0.75, names = FALSE)),
      Minimum = min(value),
      Maximum = max(value),
      Zero_count = sum(value == 0),
      Zero_percent = 100 * mean(value == 0),
      .groups = "drop"
    )

  list(summary = summary, descriptive = descriptive)
}

veg <- run_monte_carlo(
  "vegetation_zone",
  "Young male Djallonke count (>6 months) x vegetation zone"
)
phyto <- run_monte_carlo(
  "phytogeo_zone",
  "Young male Djallonke count (>6 months) x phytogeographic zone"
)

summary_results <- bind_rows(veg$summary, phyto$summary)

data_quality <- data.frame(
  Metric = c(
    "Total records",
    "Valid nonnegative integer counts",
    "Zero counts",
    "Positive counts",
    "Blank responses",
    "Nonnumeric responses",
    "Negative counts",
    "Noninteger nonnegative values",
    "Unmatched communes"
  ),
  Value = c(
    nrow(data),
    sum(data$valid_count, na.rm = TRUE),
    sum(data$valid_count & data$count == 0, na.rm = TRUE),
    sum(data$valid_count & data$count > 0, na.rm = TRUE),
    sum(data$blank, na.rm = TRUE),
    sum(data$nonnumeric, na.rm = TRUE),
    sum(data$negative, na.rm = TRUE),
    sum(data$noninteger, na.rm = TRUE),
    sum(is.na(data$vegetation_zone) | data$vegetation_zone == "", na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

method_notes <- data.frame(
  Parameter = c(
    "Variable type",
    "Primary test",
    "Permutation statistic",
    "Permutations",
    "P-value formula",
    "Zero treatment",
    "Invalid values",
    "Effect size",
    "Progress reporting"
  ),
  Assessment = c(
    "Quantitative discrete count variable.",
    "Monte Carlo permutation test for differences in count distributions among zones.",
    "Tie-corrected Kruskal-Wallis H statistic calculated from ranks.",
    paste(B, "permutations per geographical comparison."),
    "(number of permuted H values >= observed H + 1) / (B + 1).",
    "Zero is retained as a valid count.",
    "Blank, nonnumeric, negative, and noninteger values are excluded and reported.",
    "Epsilon-squared derived from the observed Kruskal-Wallis H statistic.",
    "A progress message is printed after every 10% of permutations."
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

write_xlsx(sheets, path = OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, winslash = "/", mustWork = FALSE))
