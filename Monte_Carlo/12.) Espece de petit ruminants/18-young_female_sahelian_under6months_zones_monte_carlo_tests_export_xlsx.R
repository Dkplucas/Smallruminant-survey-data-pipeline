# Young female Sahelian count under 6 months vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: young_female_sahelian_under6months_zones_monte_carlo_results.xlsx

packages <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "writexl")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install packages first: install.packages(c(",
       paste(sprintf('"%s"', missing_packages), collapse = ", "), "))")
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
OUTPUT_FILE <- "young_female_sahelian_under6months_zones_monte_carlo_results.xlsx"
COUNT_COL <- "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Sahelien"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
ALPHA <- 0.05
B <- 10000L
SEED <- 20260916L

normalize_header <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |> str_to_lower() |> str_squish()
  str_replace_all(x, "[^a-z0-9]", "")
}

normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |> str_to_lower() |> str_squish()
  x <- str_replace_all(x, "[’'`-]", "") |> str_replace_all("[^a-z0-9]", "")
  recode(x,
         "toribossito" = "tori",
         "dassazoume" = "dassa",
         "dassazounme" = "dassa",
         .default = x)
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
  hits <- which(vapply(normalized_headers, function(h) {
    all(vapply(terms, function(term) str_detect(h, fixed(term)), logical(1)))
  }, logical(1)))
  if (length(hits) == 1L) {
    message(label, " matched flexibly to: ", headers[hits])
    return(headers[hits])
  }
  candidates <- headers[str_detect(normalized_headers, "effectif|petits|femelle|sahelien")]
  stop(label, " could not be identified uniquely. Candidate headers: ",
       paste(candidates, collapse = " | "))
}

if (!file.exists(DATA_FILE)) stop("File not found: ", normalizePath(DATA_FILE, mustWork = FALSE))
if (!file.exists(ZONES_FILE)) stop("File not found: ", normalizePath(ZONES_FILE, mustWork = FALSE))

raw <- read_excel(DATA_FILE, sheet = 1, col_types = "text", .name_repair = "minimal")
zraw <- read_excel(ZONES_FILE, sheet = 1, col_types = "text", .name_repair = "unique")

COUNT_COL <- resolve_column(
  COUNT_COL, names(raw),
  c("Effectif et composition du cheptel", "Nbre de petits", "6 mois", "femelle", "Sahelien"),
  "Young female Sahelian count under 6 months"
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
  filter(!is.na(district), district != "",
         !str_detect(str_to_lower(coalesce(vegetation_zone, "")), "^total")) |>
  mutate(commune_key = normalize_key(district)) |>
  select(commune_key, vegetation_zone, phytogeo_zone) |>
  distinct(commune_key, .keep_all = TRUE)

clean_numeric <- function(x) {
  x <- str_squish(as.character(x))
  x[x == ""] <- NA_character_
  suppressWarnings(as.numeric(str_replace_all(x, ",", ".")))
}

data <- raw |>
  transmute(
    row_id = row_number(),
    commune_raw = .data[[COMMUNE_COL]],
    commune_key = normalize_key(.data[[COMMUNE_COL]]),
    count_raw = str_squish(.data[[COUNT_COL]]),
    count = clean_numeric(.data[[COUNT_COL]])
  ) |>
  left_join(zones, by = "commune_key") |>
  mutate(
    blank = is.na(count_raw) | count_raw == "",
    nonnumeric = !blank & is.na(count),
    negative = !is.na(count) & count < 0,
    noninteger = !is.na(count) & count >= 0 & abs(count - round(count)) > 1e-9,
    valid_count = !is.na(count) & count >= 0 & abs(count - round(count)) <= 1e-9
  )

kw_h_from_ranks <- function(ranks, groups, tie_correction) {
  n <- length(ranks)
  sums <- rowsum(ranks, groups, reorder = FALSE)[, 1]
  sizes <- as.numeric(table(groups))
  h <- 12 / (n * (n + 1)) * sum((sums^2) / sizes) - 3 * (n + 1)
  h / tie_correction
}

run_monte_carlo <- function(zone_var, label) {
  d <- data |>
    filter(valid_count, !is.na(.data[[zone_var]]), .data[[zone_var]] != "") |>
    transmute(value = count, zone = factor(.data[[zone_var]])) |>
    droplevels()

  n <- nrow(d)
  k <- nlevels(d$zone)
  if (n == 0L || k < 2L) {
    reason <- if (n == 0L) "No valid complete observations." else "Only one zone category is represented."
    summary <- data.frame(
      Comparison = label, N = n, Groups = k, Observed_H = NA_real_,
      Degrees_of_freedom = NA_integer_, Permutations = 0L,
      Monte_Carlo_p_value = NA_real_, Monte_Carlo_SE = NA_real_,
      Monte_Carlo_95CI_low = NA_real_, Monte_Carlo_95CI_high = NA_real_,
      Epsilon_squared = NA_real_, Alpha = ALPHA,
      Decision = "No statistical test performed", Recommendation = reason
    )
    return(list(summary = summary, descriptive = data.frame(Note = reason)))
  }

  ranks <- rank(d$value, ties.method = "average")
  ties <- table(d$value)
  tie_correction <- 1 - sum(ties^3 - ties) / (n^3 - n)
  if (!is.finite(tie_correction) || tie_correction <= 0) {
    reason <- "Not testable: all valid count values are identical."
    summary <- data.frame(
      Comparison = label, N = n, Groups = k, Observed_H = 0,
      Degrees_of_freedom = k - 1L, Permutations = 0L,
      Monte_Carlo_p_value = NA_real_, Monte_Carlo_SE = NA_real_,
      Monte_Carlo_95CI_low = NA_real_, Monte_Carlo_95CI_high = NA_real_,
      Epsilon_squared = 0, Alpha = ALPHA,
      Decision = "No statistical test performed", Recommendation = reason
    )
    return(list(summary = summary, descriptive = data.frame(Note = reason)))
  }

  observed_h <- kw_h_from_ranks(ranks, d$zone, tie_correction)
  set.seed(SEED + ifelse(zone_var == "phytogeo_zone", 100L, 0L))
  extreme <- 0L
  progress_step <- max(1L, B %/% 10L)
  for (b in seq_len(B)) {
    permuted_h <- kw_h_from_ranks(sample(ranks, replace = FALSE), d$zone, tie_correction)
    if (permuted_h >= observed_h - 1e-12) extreme <- extreme + 1L
    if (b %% progress_step == 0L || b == B) {
      message(label, ": ", b, "/", B, " permutations completed")
    }
  }

  p <- (extreme + 1) / (B + 1)
  se <- sqrt(p * (1 - p) / (B + 1))
  ci_low <- max(0, p - 1.96 * se)
  ci_high <- min(1, p + 1.96 * se)
  epsilon2 <- max(0, (observed_h - k + 1) / (n - k))

  summary <- data.frame(
    Comparison = label, N = n, Groups = k, Observed_H = observed_h,
    Degrees_of_freedom = k - 1L, Permutations = B,
    Monte_Carlo_p_value = p, Monte_Carlo_SE = se,
    Monte_Carlo_95CI_low = ci_low, Monte_Carlo_95CI_high = ci_high,
    Epsilon_squared = epsilon2, Alpha = ALPHA,
    Decision = ifelse(p < ALPHA,
                      "Reject H0: distributions differ among zones",
                      "Do not reject H0: no evidence of distributional differences"),
    Recommendation = "Report the Monte Carlo permutation p-value based on the tie-corrected Kruskal-Wallis H statistic."
  )

  descriptive <- d |>
    group_by(zone) |>
    summarise(
      N = n(), Mean = mean(value), SD = sd(value), Median = median(value),
      Q1 = quantile(value, 0.25, names = FALSE),
      Q3 = quantile(value, 0.75, names = FALSE),
      Minimum = min(value), Maximum = max(value),
      Zero_count = sum(value == 0), Zero_percent = 100 * mean(value == 0),
      .groups = "drop"
    )

  list(summary = summary, descriptive = descriptive)
}

veg <- run_monte_carlo(
  "vegetation_zone",
  "Young female Sahelian count under 6 months x vegetation zone"
)
phyto <- run_monte_carlo(
  "phytogeo_zone",
  "Young female Sahelian count under 6 months x phytogeographic zone"
)

summary_results <- bind_rows(veg$summary, phyto$summary)
quality <- data.frame(
  Metric = c(
    "Total records", "Valid nonnegative integer counts", "Zero counts",
    "Blank responses", "Nonnumeric responses", "Negative counts",
    "Noninteger nonnegative values", "Unmatched communes"
  ),
  Value = c(
    nrow(data), sum(data$valid_count), sum(data$valid_count & data$count == 0),
    sum(data$blank), sum(data$nonnumeric), sum(data$negative),
    sum(data$noninteger), sum(is.na(data$vegetation_zone))
  )
)
notes <- data.frame(
  Parameter = c("Variable type", "Primary test", "Permutation statistic", "Permutations",
                "Zero treatment", "Invalid values", "Effect size"),
  Assessment = c(
    "Quantitative discrete count variable.",
    "Monte Carlo permutation test across geographical groups.",
    "Tie-corrected Kruskal-Wallis H statistic.",
    paste0(B, " finite permutations with progress reported every 10%."),
    "Zero is retained as a valid count.",
    "Blank, nonnumeric, negative, and noninteger values are excluded from testing.",
    "Epsilon-squared based on the observed Kruskal-Wallis statistic."
  )
)

sheets <- list(
  Summary = summary_results,
  Veg_descriptive = veg$descriptive,
  Phyto_descriptive = phyto$descriptive,
  Data_quality = quality,
  Method_notes = notes
)

writexl::write_xlsx(sheets, path = OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, mustWork = FALSE))
