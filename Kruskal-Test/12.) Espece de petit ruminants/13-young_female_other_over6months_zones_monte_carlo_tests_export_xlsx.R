# Young female animals in the Other category (>6 months) vs geographical zones
# Inputs: data7.xlsx and zones.xlsx
# Output: young_female_other_over6months_zones_monte_carlo_results.xlsx

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
OUTPUT_FILE <- "young_female_other_over6months_zones_monte_carlo_results.xlsx"
COUNT_COL <- "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Autre"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

ALPHA <- 0.05
B <- 10000L             # Change to 100000L for the final analysis if desired.
SEED <- 20260916L
TOL <- 1e-12

normalize_header <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(x)
  x <- str_squish(x)
  str_replace_all(x, "[^a-z0-9]", "")
}

resolve_column <- function(expected, headers, required_terms, label) {
  if (expected %in% headers) return(expected)

  normalized_headers <- normalize_header(headers)
  normalized_expected <- normalize_header(expected)
  exact_hits <- which(normalized_headers == normalized_expected)

  if (length(exact_hits) == 1L) {
    message(label, " matched after normalization to: ", headers[exact_hits])
    return(headers[exact_hits])
  }

  normalized_terms <- normalize_header(required_terms)
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
    "effectifetcompositionducheptel|nbredejeunes|femellesautre"
  )]

  stop(
    label,
    " could not be identified uniquely. Candidate headers: ",
    paste(candidates, collapse = " | ")
  )
}

normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(x)
  x <- str_squish(x)
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
  c("Effectif et composition du cheptel", "Nbre de jeunes", "6 mois", "femelles", "Autre"),
  "Young-female Other count"
)
COMMUNE_COL <- resolve_column(
  COMMUNE_COL,
  names(raw),
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
    !is.na(district),
    district != "",
    !is.na(vegetation_zone),
    !str_detect(str_to_lower(vegetation_zone), "^total")
  ) |>
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
    count_value = parse_number(.data[[COUNT_COL]])
  ) |>
  left_join(zones, by = "commune_key") |>
  mutate(
    blank_value = is.na(count_raw) | count_raw == "",
    nonnumeric_value = !blank_value & is.na(count_value),
    negative_value = !is.na(count_value) & count_value < 0,
    noninteger_value = !is.na(count_value) & count_value >= 0 & abs(count_value - round(count_value)) > TOL,
    valid_count = !is.na(count_value) & count_value >= 0 & abs(count_value - round(count_value)) <= TOL,
    count_value = ifelse(valid_count, round(count_value), NA_real_)
  )

kw_h_from_ranks <- function(ranks, group_index, group_sizes, tie_correction) {
  rank_sums <- rowsum(ranks, group_index, reorder = FALSE)
  n <- length(ranks)
  h_uncorrected <- (12 / (n * (n + 1))) * sum((rank_sums[, 1]^2) / group_sizes) - 3 * (n + 1)
  if (!is.finite(tie_correction) || tie_correction <= 0) return(NA_real_)
  h_uncorrected / tie_correction
}

monte_carlo_kw <- function(zone_var, comparison_label, seed_offset = 0L) {
  d <- data |>
    filter(
      valid_count,
      !is.na(.data[[zone_var]]),
      .data[[zone_var]] != ""
    ) |>
    transmute(value = count_value, zone = factor(.data[[zone_var]])) |>
    droplevels()

  n <- nrow(d)
  k <- nlevels(d$zone)

  if (n == 0L || k < 2L) {
    reason <- if (n == 0L) "No complete valid observations." else "Only one geographical group is represented."
    summary <- data.frame(
      Comparison = comparison_label,
      N = n,
      Groups = k,
      Observed_Kruskal_Wallis_H = NA_real_,
      Degrees_of_freedom = NA_real_,
      Permutations = B,
      Extreme_permutations = NA_integer_,
      Monte_Carlo_p_value = NA_real_,
      Monte_Carlo_SE = NA_real_,
      Monte_Carlo_95CI_low = NA_real_,
      Monte_Carlo_95CI_high = NA_real_,
      Epsilon_squared = NA_real_,
      Alpha = ALPHA,
      Decision = "No statistical test performed",
      Recommendation = reason,
      stringsAsFactors = FALSE
    )
    return(list(summary = summary, descriptive = data.frame(Note = reason)))
  }

  if (length(unique(d$value)) < 2L) {
    reason <- "All valid counts are identical; the permutation test is not informative."
    summary <- data.frame(
      Comparison = comparison_label,
      N = n,
      Groups = k,
      Observed_Kruskal_Wallis_H = 0,
      Degrees_of_freedom = k - 1L,
      Permutations = B,
      Extreme_permutations = NA_integer_,
      Monte_Carlo_p_value = NA_real_,
      Monte_Carlo_SE = NA_real_,
      Monte_Carlo_95CI_low = NA_real_,
      Monte_Carlo_95CI_high = NA_real_,
      Epsilon_squared = 0,
      Alpha = ALPHA,
      Decision = "No statistical test performed",
      Recommendation = reason,
      stringsAsFactors = FALSE
    )
  } else {
    ranks <- rank(d$value, ties.method = "average")
    group_index <- as.integer(d$zone)
    group_sizes <- tabulate(group_index, nbins = k)
    tie_sizes <- as.numeric(table(d$value))
    tie_correction <- 1 - sum(tie_sizes^3 - tie_sizes) / (n^3 - n)
    observed_h <- kw_h_from_ranks(ranks, group_index, group_sizes, tie_correction)

    set.seed(SEED + seed_offset)
    extreme_count <- 0L
    progress_step <- max(1L, B %/% 10L)

    for (b in seq_len(B)) {
      permuted_ranks <- sample(ranks, size = n, replace = FALSE)
      permuted_h <- kw_h_from_ranks(permuted_ranks, group_index, group_sizes, tie_correction)
      if (is.finite(permuted_h) && permuted_h >= observed_h - TOL) {
        extreme_count <- extreme_count + 1L
      }
      if (b %% progress_step == 0L || b == B) {
        message(comparison_label, ": ", b, "/", B, " permutations completed")
      }
    }

    p_value <- (extreme_count + 1) / (B + 1)
    mc_se <- sqrt(p_value * (1 - p_value) / (B + 1))
    ci_low <- max(0, p_value - 1.96 * mc_se)
    ci_high <- min(1, p_value + 1.96 * mc_se)
    epsilon_sq <- max(0, (observed_h - k + 1) / (n - k))

    summary <- data.frame(
      Comparison = comparison_label,
      N = n,
      Groups = k,
      Observed_Kruskal_Wallis_H = observed_h,
      Degrees_of_freedom = k - 1L,
      Permutations = B,
      Extreme_permutations = extreme_count,
      Monte_Carlo_p_value = p_value,
      Monte_Carlo_SE = mc_se,
      Monte_Carlo_95CI_low = ci_low,
      Monte_Carlo_95CI_high = ci_high,
      Epsilon_squared = epsilon_sq,
      Alpha = ALPHA,
      Decision = ifelse(
        p_value < ALPHA,
        "Reject H0: count distributions differ across zones",
        "Do not reject H0: no evidence of different count distributions"
      ),
      Recommendation = "Report the Monte Carlo permutation p-value and zone-specific median [Q1-Q3].",
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
      Q1 = quantile(value, 0.25, names = FALSE),
      Q3 = quantile(value, 0.75, names = FALSE),
      Minimum = min(value),
      Maximum = max(value),
      Zero_count = sum(value == 0),
      Zero_percent = 100 * mean(value == 0),
      .groups = "drop"
    )

  list(summary = summary, descriptive = descriptive)
}

vegetation_result <- monte_carlo_kw(
  "vegetation_zone",
  "Young female Other count (>6 months) x vegetation zone",
  0L
)

phytogeo_result <- monte_carlo_kw(
  "phytogeo_zone",
  "Young female Other count (>6 months) x phytogeographic zone",
  100L
)

summary_results <- bind_rows(
  vegetation_result$summary,
  phytogeo_result$summary
)

data_quality <- data.frame(
  Metric = c(
    "Total records",
    "Valid nonnegative integer counts",
    "Valid zero counts",
    "Valid positive counts",
    "Blank responses",
    "Nonnumeric responses",
    "Negative values",
    "Noninteger nonnegative values",
    "Unmatched communes"
  ),
  Value = c(
    nrow(data),
    sum(data$valid_count, na.rm = TRUE),
    sum(data$valid_count & data$count_value == 0, na.rm = TRUE),
    sum(data$valid_count & data$count_value > 0, na.rm = TRUE),
    sum(data$blank_value, na.rm = TRUE),
    sum(data$nonnumeric_value, na.rm = TRUE),
    sum(data$negative_value, na.rm = TRUE),
    sum(data$noninteger_value, na.rm = TRUE),
    sum(is.na(data$vegetation_zone) | is.na(data$phytogeo_zone))
  ),
  stringsAsFactors = FALSE
)

method_notes <- data.frame(
  Parameter = c(
    "Variable type",
    "Target variable",
    "Primary method",
    "Permutation statistic",
    "Number of permutations",
    "Monte Carlo p-value",
    "Treatment of zero",
    "Invalid counts",
    "Effect size",
    "Reporting"
  ),
  Assessment = c(
    "Quantitative discrete count variable.",
    COUNT_COL,
    "Monte Carlo permutation test for equality of count distributions across zones.",
    "Tie-corrected Kruskal-Wallis H statistic computed from ranks.",
    as.character(B),
    "(number of permuted H statistics >= observed H + 1) / (B + 1).",
    "Zero is valid and means no young female animals in the Other category older than six months.",
    "Blank, nonnumeric, negative, and noninteger values are excluded and reported.",
    "Epsilon-squared based on the observed Kruskal-Wallis H statistic.",
    "Report median [Q1-Q3], Monte Carlo p-value, permutation count, and epsilon-squared."
  ),
  stringsAsFactors = FALSE
)

sheets <- list(
  Summary = summary_results,
  Veg_descriptive = vegetation_result$descriptive,
  Phyto_descriptive = phytogeo_result$descriptive,
  Data_quality = data_quality,
  Method_notes = method_notes
)

write_xlsx(sheets, path = OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, mustWork = FALSE))
