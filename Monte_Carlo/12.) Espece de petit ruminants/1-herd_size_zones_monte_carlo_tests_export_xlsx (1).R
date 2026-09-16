# Monte Carlo permutation tests for total herd size vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: herd_size_zones_monte_carlo_results.xlsx

packages <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "writexl")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  stop("Install missing packages first: install.packages(c(",
       paste(sprintf('"%s"', missing_packages), collapse = ", "), "))")
}

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(stringi); library(writexl)
})

DATA_FILE <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "herd_size_zones_monte_carlo_results.xlsx"
ALPHA <- 0.05
B <- 10000L  # Increase to 100000L for the final analysis if desired
SEED <- 20260916L

HERD_SIZE_COL <- "12.) Effectif et composition du cheptel/Effectif total du troupeau"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

normalize_header <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(str_squish(x))
  str_replace_all(x, "[^a-z0-9]", "")
}

resolve_column <- function(expected, headers, required_terms, label) {
  if (expected %in% headers) return(expected)
  nh <- normalize_header(headers)
  hit <- which(nh == normalize_header(expected))
  if (length(hit) == 1L) return(headers[hit])
  terms <- normalize_header(required_terms)
  hit <- which(vapply(nh, function(h) all(vapply(terms, function(z) grepl(z, h, fixed = TRUE), logical(1))), logical(1)))
  if (length(hit) == 1L) return(headers[hit])
  candidates <- headers[grepl("effectif|troupeau|commune", nh)]
  stop(label, " could not be identified uniquely. Candidates: ", paste(candidates, collapse = " | "))
}

normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(str_squish(x))
  x <- str_replace_all(x, "[’'`-]", "")
  x <- str_replace_all(x, "[^a-z0-9]", "")
  recode(x, "toribossito" = "tori", "dassazoume" = "dassa", "dassazounme" = "dassa", .default = x)
}

parse_number_safe <- function(x) {
  x <- str_squish(str_replace_all(as.character(x), "\\u00A0", " "))
  x <- str_replace_all(x, "\\s", "")
  x <- str_replace_all(x, ",", ".")
  x <- str_replace_all(x, "[^0-9.\\-]", "")
  suppressWarnings(as.numeric(x))
}

if (!file.exists(DATA_FILE)) stop("File not found: ", DATA_FILE, ". Current folder: ", getwd())
if (!file.exists(ZONES_FILE)) stop("File not found: ", ZONES_FILE, ". Current folder: ", getwd())

raw <- read_excel(DATA_FILE, sheet = 1, col_types = "text", .name_repair = "minimal")
zraw <- read_excel(ZONES_FILE, sheet = 1, col_types = "text", .name_repair = "unique")

HERD_SIZE_COL <- resolve_column(HERD_SIZE_COL, names(raw), c("Effectif et composition du cheptel", "Effectif total du troupeau"), "Total herd size")
COMMUNE_COL <- resolve_column(COMMUNE_COL, names(raw), c("UNITE D ELEVAGE", "Commune"), "Commune")

required_zone_columns <- c("Vegetation zones", "Phytogeographic zones", "District")
if (!all(required_zone_columns %in% names(zraw))) stop("zones.xlsx must contain: ", paste(required_zone_columns, collapse = ", "))

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

data <- raw |>
  transmute(
    row_id = row_number(),
    commune_raw = .data[[COMMUNE_COL]],
    commune_key = normalize_key(.data[[COMMUNE_COL]]),
    herd_size_raw = .data[[HERD_SIZE_COL]],
    herd_size = parse_number_safe(.data[[HERD_SIZE_COL]])
  ) |>
  left_join(zones, by = "commune_key") |>
  mutate(
    invalid_numeric = !is.na(herd_size_raw) & str_squish(herd_size_raw) != "" & is.na(herd_size),
    nonpositive_value = !is.na(herd_size) & herd_size <= 0,
    noninteger_value = !is.na(herd_size) & abs(herd_size - round(herd_size)) > 1e-8,
    valid_herd_size = ifelse(!is.na(herd_size) & herd_size > 0, herd_size, NA_real_)
  )

describe_by_zone <- function(zone_var) {
  data |>
    filter(!is.na(valid_herd_size), !is.na(.data[[zone_var]]), .data[[zone_var]] != "") |>
    group_by(Zone = .data[[zone_var]]) |>
    summarise(
      N = n(), Mean = mean(valid_herd_size), SD = sd(valid_herd_size),
      Median = median(valid_herd_size), Q1 = quantile(valid_herd_size, .25),
      Q3 = quantile(valid_herd_size, .75), Minimum = min(valid_herd_size),
      Maximum = max(valid_herd_size), .groups = "drop"
    )
}

run_tests <- function(zone_var, label) {
  d <- data |>
    filter(!is.na(valid_herd_size), !is.na(.data[[zone_var]]), .data[[zone_var]] != "") |>
    transmute(value = valid_herd_size, zone = factor(.data[[zone_var]])) |>
    droplevels()

  n <- nrow(d)
  k <- nlevels(d$zone)

  if (n == 0L || k < 2L || length(unique(d$value)) < 2L || n <= k) {
    reason <- if (n == 0L) "No valid complete observations." else if (k < 2L) "Only one geographical zone is represented." else if (n <= k) "Too few observations for the number of groups." else "All valid herd-size values are identical."
    return(list(
      summary = data.frame(
        Comparison = label, N = n, Groups = k, Primary_test = "Not testable",
        Observed_Kruskal_Wallis_H = NA_real_, Degrees_of_freedom = NA_real_,
        Monte_Carlo_permutations = NA_integer_, Monte_Carlo_p_value = NA_real_,
        Monte_Carlo_SE = NA_real_, Monte_Carlo_95CI_low = NA_real_,
        Monte_Carlo_95CI_high = NA_real_, Alpha = ALPHA,
        Decision = "No statistical test performed", Epsilon_squared = NA_real_,
        Recommendation = reason, stringsAsFactors = FALSE
      ), pairwise = data.frame(Note = "Not available", stringsAsFactors = FALSE)
    ))
  }

  # Compute ranks and tie correction once. This is mathematically equivalent to
  # calling kruskal.test() for every permutation, but much faster.
  ranks <- rank(d$value, ties.method = "average")
  group_id <- as.integer(d$zone)
  group_sizes <- tabulate(group_id, nbins = k)
  tie_sizes <- table(d$value)
  tie_correction <- 1 - sum(tie_sizes^3 - tie_sizes) / (n^3 - n)

  kw_from_ranks <- function(r) {
    rank_sums <- rowsum(r, group_id, reorder = FALSE)[, 1]
    h_uncorrected <- 12 / (n * (n + 1)) * sum((rank_sums^2) / group_sizes) - 3 * (n + 1)
    h_uncorrected / tie_correction
  }

  observed_h <- kw_from_ranks(ranks)
  observed_kw <- kruskal.test(value ~ zone, data = d)

  set.seed(SEED + ifelse(zone_var == "phytogeo_zone", 100L, 0L))
  extreme_count <- 0L
  progress_every <- max(1L, floor(B / 10L))

  for (b in seq_len(B)) {
    permuted_h <- kw_from_ranks(sample(ranks, size = n, replace = FALSE))
    if (permuted_h >= observed_h - sqrt(.Machine$double.eps)) {
      extreme_count <- extreme_count + 1L
    }
    if (b %% progress_every == 0L || b == B) {
      message(label, ": ", b, "/", B, " permutations completed")
    }
  }

  monte_carlo_p <- (extreme_count + 1) / (B + 1)
  monte_carlo_se <- sqrt(monte_carlo_p * (1 - monte_carlo_p) / (B + 1))
  ci_low <- max(0, monte_carlo_p - 1.96 * monte_carlo_se)
  ci_high <- min(1, monte_carlo_p + 1.96 * monte_carlo_se)
  epsilon_squared <- max(0, (observed_h - k + 1) / (n - k))

  summary <- data.frame(
    Comparison = label, N = n, Groups = k,
    Primary_test = "Monte Carlo permutation test based on Kruskal-Wallis H",
    Observed_Kruskal_Wallis_H = observed_h,
    R_kruskal_test_H_check = as.numeric(observed_kw$statistic),
    Degrees_of_freedom = as.numeric(observed_kw$parameter),
    Monte_Carlo_permutations = B, Monte_Carlo_p_value = monte_carlo_p,
    Monte_Carlo_SE = monte_carlo_se, Monte_Carlo_95CI_low = ci_low,
    Monte_Carlo_95CI_high = ci_high, Alpha = ALPHA,
    Decision = ifelse(monte_carlo_p < ALPHA, "Reject H0: herd-size distributions differ across zones", "Do not reject H0: no distributional difference detected"),
    Epsilon_squared = epsilon_squared,
    Recommendation = "Report the Monte Carlo permutation p-value and observed Kruskal-Wallis H statistic.",
    stringsAsFactors = FALSE
  )

  list(summary = summary, pairwise = data.frame(Note = "Pairwise tests were not requested.", stringsAsFactors = FALSE))
}

veg <- run_tests("vegetation_zone", "Total herd size x vegetation zone")
phyto <- run_tests("phytogeo_zone", "Total herd size x phytogeographic zone")

quality <- data.frame(
  Metric = c("Total records", "Valid positive herd sizes", "Blank herd-size responses", "Invalid numeric values", "Zero or negative values", "Non-integer positive values", "Unmatched communes"),
  Value = c(nrow(data), sum(!is.na(data$valid_herd_size)), sum(is.na(data$herd_size_raw) | str_squish(data$herd_size_raw) == ""),
            sum(data$invalid_numeric), sum(data$nonpositive_value), sum(data$noninteger_value), sum(is.na(data$vegetation_zone) | is.na(data$phytogeo_zone))),
  stringsAsFactors = FALSE
)

notes <- data.frame(
  Parameter = c("Variable type", "Primary analysis", "Permutation principle", "Number of simulations", "P-value correction", "Effect size", "Validity rule"),
  Assessment = c(
    "Quantitative discrete count variable, not a qualitative variable.",
    "Monte Carlo permutation test using the Kruskal-Wallis H statistic.",
    "Observed herd sizes are randomly reassigned to the existing zone memberships under the null hypothesis of identical distributions.",
    "10,000 random permutations are used by default for each zone analysis; B can be increased to 100,000 for the final run.",
    "The Monte Carlo p-value is calculated as (extreme permutations + 1)/(B + 1), preventing a zero p-value.",
    "Epsilon-squared based on the observed Kruskal-Wallis statistic.",
    "Only positive numeric herd sizes with matched zones are analyzed; zero, negative, blank, and nonnumeric entries are excluded and reported."
  ), stringsAsFactors = FALSE
)

sheets <- list(
  Summary = bind_rows(veg$summary, phyto$summary),
  Veg_descriptive = describe_by_zone("vegetation_zone"),
  Veg_pairwise = veg$pairwise,
  Phyto_descriptive = describe_by_zone("phytogeo_zone"),
  Phyto_pairwise = phyto$pairwise,
  Data_quality = quality,
  Method_notes = notes
)

write_xlsx(sheets, OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, winslash = "/", mustWork = FALSE))
