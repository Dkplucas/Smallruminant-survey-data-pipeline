# Monte Carlo permutation tests for Djallonke herd size 12 months ago by zones
# Inputs: data7.xlsx and zones.xlsx
# Output: djallonke_herd_size_12_months_ago_zones_monte_carlo_results.xlsx

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
OUTPUT_FILE <- "djallonke_herd_size_12_months_ago_zones_monte_carlo_results.xlsx"
ALPHA <- 0.05
B <- 10000L                  # Change to 100000L for the final analysis
SEED <- 20260916L

COUNT_COL <- "12.) Effectif et composition du cheptel/Effectif il y a 12 mois_Djallonke"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

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

  terms <- normalize_header(required_terms)
  hits <- which(vapply(
    normalized_headers,
    function(h) all(vapply(terms, function(term) str_detect(h, fixed(term)), logical(1))),
    logical(1)
  ))

  if (length(hits) == 1L) {
    message(label, " matched flexibly to: ", headers[hits])
    return(headers[hits])
  }

  candidates <- headers[str_detect(normalized_headers, "effectifilya12mois|12mois|djallonke")]
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
  COUNT_COL,
  names(raw),
  c("Effectif et composition du cheptel", "Effectif il y a 12 mois", "Djallonke"),
  "Djallonke herd size 12 months ago"
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
    valid_count = !is.na(count) & count >= 0 & abs(count - round(count)) <= 1e-8
  )

kw_h_from_ranks <- function(ranks, group_index, group_sizes, tie_correction) {
  rank_sums <- rowsum(ranks, group_index, reorder = FALSE)[, 1]
  n <- length(ranks)
  h <- (12 / (n * (n + 1))) * sum((rank_sums^2) / group_sizes) - 3 * (n + 1)
  h / tie_correction
}

run_monte_carlo <- function(zone_var, label, seed_offset = 0L) {
  d <- data |>
    filter(valid_count, !is.na(.data[[zone_var]]), .data[[zone_var]] != "") |>
    transmute(value = count, zone = factor(.data[[zone_var]])) |>
    droplevels()

  n <- nrow(d)
  k <- nlevels(d$zone)

  if (n == 0L || k < 2L || length(unique(d$value)) < 2L) {
    reason <- if (n == 0L) {
      "No complete valid observations."
    } else if (k < 2L) {
      "Only one geographical group is represented."
    } else {
      "All valid counts are identical; no between-zone test is possible."
    }

    return(list(
      summary = data.frame(
        Comparison = label, N = n, Groups = k,
        Observed_Kruskal_Wallis_H = NA_real_, Degrees_of_freedom = NA_real_,
        Permutations = 0L, Extreme_permutations = NA_integer_,
        Monte_Carlo_p_value = NA_real_, Monte_Carlo_SE = NA_real_,
        Monte_Carlo_95CI_low = NA_real_, Monte_Carlo_95CI_high = NA_real_,
        Epsilon_squared = NA_real_, Alpha = ALPHA,
        Decision = "No statistical test performed",
        Recommendation = reason,
        stringsAsFactors = FALSE
      ),
      descriptive = data.frame(Note = reason, stringsAsFactors = FALSE)
    ))
  }

  ranks <- rank(d$value, ties.method = "average")
  group_index <- as.integer(d$zone)
  group_sizes <- tabulate(group_index, nbins = k)
  tie_sizes <- table(d$value)
  tie_correction <- 1 - sum(tie_sizes^3 - tie_sizes) / (n^3 - n)

  observed_h <- kw_h_from_ranks(ranks, group_index, group_sizes, tie_correction)
  extreme_count <- 0L
  progress_points <- unique(pmax(1L, round(seq(0.1, 1, by = 0.1) * B)))

  set.seed(SEED + seed_offset)
  for (b in seq_len(B)) {
    permuted_ranks <- sample(ranks, size = n, replace = FALSE)
    permuted_h <- kw_h_from_ranks(permuted_ranks, group_index, group_sizes, tie_correction)
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
    ) |>
    rename(Zone = zone)

  summary <- data.frame(
    Comparison = label,
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
    Epsilon_squared = epsilon_squared,
    Alpha = ALPHA,
    Decision = ifelse(
      p_value < ALPHA,
      "Reject H0: count distributions differ across zones",
      "Do not reject H0: no evidence of different count distributions"
    ),
    Recommendation = "Report the Monte Carlo permutation p-value and median [Q1-Q3] by zone.",
    stringsAsFactors = FALSE
  )

  list(summary = summary, descriptive = descriptive)
}

veg <- run_monte_carlo(
  "vegetation_zone",
  "Djallonke herd size 12 months ago x vegetation zone",
  0L
)
phyto <- run_monte_carlo(
  "phytogeo_zone",
  "Djallonke herd size 12 months ago x phytogeographic zone",
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
    "Negative values",
    "Noninteger nonnegative values",
    "Unmatched communes"
  ),
  Value = c(
    nrow(data),
    sum(data$valid_count),
    sum(data$valid_count & data$count == 0),
    sum(data$valid_count & data$count > 0),
    sum(data$blank),
    sum(data$nonnumeric),
    sum(data$negative),
    sum(data$noninteger),
    sum(is.na(data$vegetation_zone))
  ),
  stringsAsFactors = FALSE
)

method_notes <- data.frame(
  Parameter = c(
    "Variable type", "Primary test", "Permutation statistic",
    "Null hypothesis", "Permutations", "P-value correction",
    "Zero counts", "Invalid observations", "Effect size", "Runtime"
  ),
  Assessment = c(
    "Quantitative discrete count variable.",
    "Monte Carlo permutation test for differences among geographical groups.",
    "Tie-corrected Kruskal-Wallis H statistic.",
    "The count distribution is exchangeable and does not differ across zones.",
    paste0(B, " random permutations for each geographical classification."),
    "P=(extreme+1)/(B+1).",
    "Zero is retained as a valid count.",
    "Blank, nonnumeric, negative, and noninteger values are excluded.",
    "Epsilon-squared based on the observed Kruskal-Wallis H statistic.",
    "The loop is finite and progress is printed every 10 percent."
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
