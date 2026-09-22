# Adult male Sahelian count vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: adult_male_sahelian_count_zones_monte_carlo_results.xlsx

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
OUTPUT_FILE <- "adult_male_sahelian_count_zones_monte_carlo_results.xlsx"

ALPHA <- 0.05
B <- 10000L       # Use 100000L for the final analysis if desired
SEED <- 20260916L

COUNT_COL <-
  "12.) Effectif et composition du cheptel/Nombre de mâle adultes Sahelien"
COMMUNE_COL <-
  "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

if (!file.exists(DATA_FILE)) stop("File not found: ", DATA_FILE, ". Current folder: ", getwd())
if (!file.exists(ZONES_FILE)) stop("File not found: ", ZONES_FILE, ". Current folder: ", getwd())

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
  exact_normalized <- which(normalized_headers == normalized_expected)

  if (length(exact_normalized) == 1L) {
    message(label, " matched after normalization to: ", headers[exact_normalized])
    return(headers[exact_normalized])
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

  candidates <- headers[str_detect(
    normalized_headers,
    "effectifetcompositionducheptel|maleadultes|sahelien"
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

raw <- read_excel(DATA_FILE, sheet = 1, col_types = "text", .name_repair = "minimal")
zraw <- read_excel(ZONES_FILE, sheet = 1, col_types = "text", .name_repair = "unique")

COUNT_COL <- resolve_column(
  COUNT_COL,
  names(raw),
  c("Effectif et composition du cheptel", "Nombre de mâle adultes", "Sahelien"),
  "Adult-male Sahelian count"
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
    !str_detect(str_to_lower(vegetation_zone), "^total")
  ) |>
  mutate(commune_key = normalize_key(district)) |>
  select(commune_key, district, vegetation_zone, phytogeo_zone) |>
  distinct(commune_key, .keep_all = TRUE)

data <- raw |>
  transmute(
    row_id = row_number(),
    commune_raw = .data[[COMMUNE_COL]],
    commune_key = normalize_key(.data[[COMMUNE_COL]]),
    count_raw = str_squish(.data[[COUNT_COL]]),
    count_value = suppressWarnings(as.numeric(str_replace_all(count_raw, ",", ".")))
  ) |>
  left_join(zones, by = "commune_key") |>
  mutate(
    blank_count = is.na(count_raw) | count_raw == "",
    nonnumeric_count = !blank_count & is.na(count_value),
    negative_count = !is.na(count_value) & count_value < 0,
    noninteger_count = !is.na(count_value) & count_value >= 0 & abs(count_value - round(count_value)) > 1e-9,
    valid_count = !blank_count & !nonnumeric_count & !negative_count & !noninteger_count
  )

kw_h_from_ranks <- function(ranks, group_id, group_sizes, tie_correction) {
  rank_sums <- rowsum(ranks, group_id, reorder = FALSE)
  n <- length(ranks)
  h_uncorrected <- (12 / (n * (n + 1))) * sum((rank_sums[, 1]^2) / group_sizes) - 3 * (n + 1)
  h_uncorrected / tie_correction
}

run_monte_carlo_test <- function(zone_var, label, seed_offset = 0L) {
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
    reason <- if (n == 0L) {
      "No complete valid observations."
    } else {
      "Only one geographical-zone category is represented."
    }

    summary <- data.frame(
      Comparison = label,
      N = n,
      Groups = k,
      Observed_Kruskal_Wallis_H = NA_real_,
      Degrees_of_freedom = NA_real_,
      Monte_Carlo_permutations = B,
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

    return(list(
      summary = summary,
      descriptive = data.frame(Note = reason, stringsAsFactors = FALSE)
    ))
  }

  if (length(unique(d$value)) < 2L) {
    reason <- "All valid counts are identical; no between-zone difference can be tested."
    summary <- data.frame(
      Comparison = label,
      N = n,
      Groups = k,
      Observed_Kruskal_Wallis_H = 0,
      Degrees_of_freedom = k - 1,
      Monte_Carlo_permutations = B,
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
    descriptive <- d |>
      group_by(zone) |>
      summarise(
        N = n(), Mean = mean(value), SD = sd(value), Median = median(value),
        Q1 = quantile(value, 0.25), Q3 = quantile(value, 0.75),
        Minimum = min(value), Maximum = max(value),
        Zero_count = sum(value == 0), Zero_percent = 100 * mean(value == 0),
        .groups = "drop"
      )
    return(list(summary = summary, descriptive = descriptive))
  }

  ranks <- rank(d$value, ties.method = "average")
  group_id <- as.integer(d$zone)
  group_sizes <- tabulate(group_id, nbins = k)
  tie_sizes <- as.numeric(table(d$value))
  tie_correction <- 1 - sum(tie_sizes^3 - tie_sizes) / (n^3 - n)

  observed_h <- kw_h_from_ranks(ranks, group_id, group_sizes, tie_correction)
  extreme_count <- 0L
  progress_step <- max(1L, B %/% 10L)

  set.seed(SEED + seed_offset)
  for (b in seq_len(B)) {
    permuted_h <- kw_h_from_ranks(sample(ranks, n, replace = FALSE), group_id, group_sizes, tie_correction)
    if (permuted_h >= observed_h - 1e-12) extreme_count <- extreme_count + 1L

    if (b %% progress_step == 0L || b == B) {
      message(label, ": ", b, "/", B, " permutations completed")
    }
  }

  p_value <- (extreme_count + 1) / (B + 1)
  mc_se <- sqrt(p_value * (1 - p_value) / (B + 1))
  ci_low <- max(0, p_value - 1.96 * mc_se)
  ci_high <- min(1, p_value + 1.96 * mc_se)
  epsilon_squared <- max(0, (observed_h - k + 1) / (n - k))

  summary <- data.frame(
    Comparison = label,
    N = n,
    Groups = k,
    Observed_Kruskal_Wallis_H = observed_h,
    Degrees_of_freedom = k - 1,
    Monte_Carlo_permutations = B,
    Extreme_permutations = extreme_count,
    Monte_Carlo_p_value = p_value,
    Monte_Carlo_SE = mc_se,
    Monte_Carlo_95CI_low = ci_low,
    Monte_Carlo_95CI_high = ci_high,
    Epsilon_squared = epsilon_squared,
    Alpha = ALPHA,
    Decision = ifelse(
      p_value < ALPHA,
      "Reject H0: count distributions differ across zones",
      "Do not reject H0: no evidence of a zone difference"
    ),
    Recommendation = "Report the Monte Carlo permutation p-value based on the Kruskal-Wallis H statistic.",
    stringsAsFactors = FALSE
  )

  descriptive <- d |>
    group_by(zone) |>
    summarise(
      N = n(),
      Mean = mean(value),
      SD = sd(value),
      Median = median(value),
      Q1 = quantile(value, 0.25),
      Q3 = quantile(value, 0.75),
      Minimum = min(value),
      Maximum = max(value),
      Zero_count = sum(value == 0),
      Zero_percent = 100 * mean(value == 0),
      .groups = "drop"
    )

  list(summary = summary, descriptive = descriptive)
}

veg <- run_monte_carlo_test(
  "vegetation_zone",
  "Adult male Sahelian count x vegetation zone",
  0L
)

phyto <- run_monte_carlo_test(
  "phytogeo_zone",
  "Adult male Sahelian count x phytogeographic zone",
  1000L
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
    "Noninteger counts",
    "Unmatched communes"
  ),
  Value = c(
    nrow(data),
    sum(data$valid_count),
    sum(data$valid_count & data$count_value == 0, na.rm = TRUE),
    sum(data$valid_count & data$count_value > 0, na.rm = TRUE),
    sum(data$blank_count),
    sum(data$nonnumeric_count),
    sum(data$negative_count, na.rm = TRUE),
    sum(data$noninteger_count, na.rm = TRUE),
    sum(is.na(data$vegetation_zone))
  ),
  stringsAsFactors = FALSE
)

method_notes <- data.frame(
  Parameter = c(
    "Variable type",
    "Primary method",
    "Null hypothesis",
    "Permutations",
    "Monte Carlo p-value",
    "Treatment of zero",
    "Invalid observations",
    "Effect size"
  ),
  Assessment = c(
    "Quantitative discrete count variable.",
    "Monte Carlo permutation test using the Kruskal-Wallis H statistic.",
    "The count distribution is identical across geographical zones.",
    paste0(B, " random permutations; increase B to 100000 for the final analysis if required."),
    "Calculated as (extreme permutations + 1) / (B + 1).",
    "Zero is valid and means no adult male Sahelian animals.",
    "Blank, nonnumeric, negative, and noninteger values are excluded.",
    "Epsilon-squared based on the observed Kruskal-Wallis H statistic."
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
message("Results exported to: ", normalizePath(OUTPUT_FILE, winslash = "/", mustWork = FALSE))
