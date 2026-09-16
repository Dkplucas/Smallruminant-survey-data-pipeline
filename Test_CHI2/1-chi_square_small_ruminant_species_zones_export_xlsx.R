# Small-ruminant species vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: chi_square_results_small_ruminant_species_zones.xlsx

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
OUTPUT_FILE <- "chi_square_results_small_ruminant_species_zones.xlsx"
ALPHA <- 0.05
B <- 100000L
SEED <- 20260916L

SPECIES_COL <- "12.) Espece de petit ruminants: 1=Caprins, 2=Ovins, 3=Ovins et Caprins"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

normalize_header <- function(x) {
  x <- stringr::str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringr::str_to_lower(x)
  x <- stringr::str_squish(x)
  stringr::str_replace_all(x, "[^a-z0-9]", "")
}

resolve_column <- function(expected, headers, required_terms, label) {
  if (expected %in% headers) return(expected)

  normalized_headers <- normalize_header(headers)
  normalized_expected <- normalize_header(expected)
  exact_normalized_hits <- which(normalized_headers == normalized_expected)

  if (length(exact_normalized_hits) == 1L) {
    message(label, " matched after normalization to: ", headers[exact_normalized_hits])
    return(headers[exact_normalized_hits])
  }

  normalized_terms <- normalize_header(required_terms)
  flexible_hits <- which(vapply(
    normalized_headers,
    function(header) {
      all(vapply(
        normalized_terms,
        function(term) stringr::str_detect(header, stringr::fixed(term)),
        logical(1)
      ))
    },
    logical(1)
  ))

  if (length(flexible_hits) == 1L) {
    message(label, " matched flexibly to: ", headers[flexible_hits])
    return(headers[flexible_hits])
  }

  candidates <- headers[stringr::str_detect(
    normalized_headers,
    "petitruminant|caprins|ovins"
  )]

  stop(
    label,
    " column could not be identified uniquely. Candidate headers: ",
    paste(candidates, collapse = " | ")
  )
}

normalize_key <- function(x) {
  x <- stringr::str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringr::str_to_lower(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_replace_all(x, "[’'`-]", "")
  x <- stringr::str_replace_all(x, "[^a-z0-9]", "")
  dplyr::recode(
    x,
    "toribossito" = "tori",
    "dassazoume" = "dassa",
    "dassazounme" = "dassa",
    .default = x
  )
}

if (!file.exists(DATA_FILE)) {
  stop("data7.xlsx was not found in the current working directory: ", getwd())
}
if (!file.exists(ZONES_FILE)) {
  stop("zones.xlsx was not found in the current working directory: ", getwd())
}

raw <- readxl::read_excel(
  DATA_FILE,
  sheet = 1,
  col_types = "text",
  .name_repair = "minimal"
)

zraw <- readxl::read_excel(
  ZONES_FILE,
  sheet = 1,
  col_types = "text",
  .name_repair = "unique"
)

SPECIES_COL <- resolve_column(
  SPECIES_COL,
  names(raw),
  c("Espece de petit ruminants", "Caprins", "Ovins et Caprins"),
  "Small-ruminant species"
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
  stop(
    "Missing required column(s) in zones.xlsx: ",
    paste(missing_zone_columns, collapse = ", ")
  )
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

data <- raw |>
  transmute(
    row_id = row_number(),
    commune_raw = .data[[COMMUNE_COL]],
    commune_key = normalize_key(.data[[COMMUNE_COL]]),
    species_code = str_squish(.data[[SPECIES_COL]])
  ) |>
  left_join(zones, by = "commune_key") |>
  mutate(
    invalid_code = !is.na(species_code) & species_code != "" &
      !species_code %in% c("1", "2", "3"),
    small_ruminant_species = factor(
      ifelse(species_code %in% c("1", "2", "3"), species_code, NA_character_),
      levels = c("1", "2", "3"),
      labels = c("Goats", "Sheep", "Sheep and goats"),
      ordered = FALSE
    )
  )

run_test <- function(zone_var, label) {
  d <- data |>
    filter(
      !is.na(small_ruminant_species),
      !is.na(.data[[zone_var]]),
      .data[[zone_var]] != ""
    ) |>
    droplevels()

  observed <- table(d$small_ruminant_species, d[[zone_var]])
  n <- sum(observed)
  nr <- nrow(observed)
  nc <- ncol(observed)
  df_chi <- (nr - 1L) * (nc - 1L)

  if (n == 0L || nr < 2L || nc < 2L) {
    reason <- if (n == 0L) {
      "No complete valid observations."
    } else if (nr < 2L) {
      "Only one small-ruminant species category was observed."
    } else {
      "Only one geographical-zone category was observed."
    }

    summary_row <- data.frame(
      Comparison = label,
      N = n,
      Rows = nr,
      Columns = nc,
      Degrees_of_freedom = ifelse(df_chi > 0L, df_chi, NA_real_),
      Pearson_chi_square = NA_real_,
      Asymptotic_p_value = NA_real_,
      Minimum_expected = NA_real_,
      Cells_expected_below_5 = NA_real_,
      Percent_expected_below_5 = NA_real_,
      Cells_expected_below_1 = NA_real_,
      Selected_test = "Not testable",
      Selected_p_value = NA_real_,
      Monte_Carlo_B = NA_integer_,
      Monte_Carlo_p_value = NA_real_,
      Alpha = ALPHA,
      Decision = "No statistical test performed",
      Cramers_V = NA_real_,
      Recommendation = reason,
      stringsAsFactors = FALSE
    )

    return(list(
      summary = summary_row,
      observed = observed,
      expected = NULL,
      percent = NULL,
      stdres = NULL
    ))
  }

  pearson <- suppressWarnings(chisq.test(observed, correct = FALSE))
  expected <- pearson$expected
  cells_below_5 <- sum(expected < 5)
  cells_below_1 <- sum(expected < 1)
  percent_below_5 <- 100 * cells_below_5 / length(expected)
  pearson_valid <- cells_below_1 == 0L && percent_below_5 <= 20

  set.seed(SEED + ifelse(zone_var == "phytogeo_zone", 100L, 0L))
  monte_carlo <- suppressWarnings(
    chisq.test(observed, simulate.p.value = TRUE, B = B)
  )

  selected_p <- if (pearson_valid) pearson$p.value else monte_carlo$p.value
  selected_test <- if (pearson_valid) {
    "Pearson chi-square (asymptotic)"
  } else {
    paste0("Pearson chi-square with Monte Carlo p-value (B=", B, ")")
  }

  denominator <- n * min(nr - 1L, nc - 1L)
  cramers_v <- if (denominator > 0) {
    sqrt(as.numeric(pearson$statistic) / denominator)
  } else {
    NA_real_
  }

  summary_row <- data.frame(
    Comparison = label,
    N = n,
    Rows = nr,
    Columns = nc,
    Degrees_of_freedom = df_chi,
    Pearson_chi_square = as.numeric(pearson$statistic),
    Asymptotic_p_value = pearson$p.value,
    Minimum_expected = min(expected),
    Cells_expected_below_5 = cells_below_5,
    Percent_expected_below_5 = percent_below_5,
    Cells_expected_below_1 = cells_below_1,
    Selected_test = selected_test,
    Selected_p_value = selected_p,
    Monte_Carlo_B = B,
    Monte_Carlo_p_value = monte_carlo$p.value,
    Alpha = ALPHA,
    Decision = ifelse(
      selected_p < ALPHA,
      "Reject H0: association detected",
      "Do not reject H0: no association detected"
    ),
    Cramers_V = cramers_v,
    Recommendation = ifelse(
      pearson_valid,
      "Pearson expected-count conditions are acceptable.",
      "Expected counts are sparse; report the Monte Carlo p-value."
    ),
    stringsAsFactors = FALSE
  )

  list(
    summary = summary_row,
    observed = observed,
    expected = expected,
    percent = prop.table(observed, margin = 2) * 100,
    stdres = pearson$stdres
  )
}

matrix_df <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(data.frame(Note = "Not available", stringsAsFactors = FALSE))
  }

  m <- as.matrix(x)
  nr <- nrow(m)
  nc <- ncol(m)
  if (is.null(nr) || is.null(nc) || nr < 1L || nc < 1L) {
    return(data.frame(Note = "Not available", stringsAsFactors = FALSE))
  }

  row_labels <- rownames(m)
  if (is.null(row_labels)) row_labels <- as.character(seq_len(nr))
  column_labels <- colnames(m)
  if (is.null(column_labels)) column_labels <- paste0("Column_", seq_len(nc))

  out <- data.frame(Response = row_labels, stringsAsFactors = FALSE, check.names = FALSE)
  for (j in seq_len(nc)) {
    out[[column_labels[j]]] <- unname(m[, j])
  }
  out
}

safe_round <- function(x, digits) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  round(x, digits)
}

veg <- run_test(
  "vegetation_zone",
  "Small-ruminant species x vegetation zone"
)
phyto <- run_test(
  "phytogeo_zone",
  "Small-ruminant species x phytogeographic zone"
)

summary_results <- bind_rows(veg$summary, phyto$summary)

quality <- data.frame(
  Metric = c(
    "Total records",
    "Valid species codes 1/2/3",
    "Goats (1)",
    "Sheep (2)",
    "Sheep and goats (3)",
    "Blank responses",
    "Invalid response codes",
    "Unmatched communes"
  ),
  Value = c(
    nrow(data),
    sum(!is.na(data$small_ruminant_species)),
    sum(data$species_code == "1", na.rm = TRUE),
    sum(data$species_code == "2", na.rm = TRUE),
    sum(data$species_code == "3", na.rm = TRUE),
    sum(is.na(data$species_code) | data$species_code == ""),
    sum(data$invalid_code),
    sum(is.na(data$vegetation_zone))
  ),
  stringsAsFactors = FALSE
)

notes <- data.frame(
  Parameter = c(
    "Variable type",
    "Coding",
    "Treatment of codes",
    "Null hypothesis",
    "Expected-count rule",
    "Monte Carlo",
    "Effect size"
  ),
  Assessment = c(
    "Standalone nominal qualitative variable with three categories.",
    "1=Goats; 2=Sheep; 3=Sheep and goats.",
    "All three codes are substantive categories and are retained.",
    "Small-ruminant species and geographical zone are independent.",
    "No expected count below 1 and no more than 20% below 5.",
    "100,000 simulations; selected when Pearson expected-count conditions fail.",
    "Cramer's V."
  ),
  stringsAsFactors = FALSE
)

sheets <- list()
sheets[["Summary"]] <- summary_results
sheets[["Veg_observed"]] <- matrix_df(veg$observed)
sheets[["Veg_expected"]] <- matrix_df(safe_round(veg$expected, 4))
sheets[["Veg_percent"]] <- matrix_df(safe_round(veg$percent, 2))
sheets[["Veg_std_residuals"]] <- matrix_df(safe_round(veg$stdres, 4))
sheets[["Phyto_observed"]] <- matrix_df(phyto$observed)
sheets[["Phyto_expected"]] <- matrix_df(safe_round(phyto$expected, 4))
sheets[["Phyto_percent"]] <- matrix_df(safe_round(phyto$percent, 2))
sheets[["Phyto_std_residuals"]] <- matrix_df(safe_round(phyto$stdres, 4))
sheets[["Data_quality"]] <- quality
sheets[["Method_notes"]] <- notes

writexl::write_xlsx(sheets, path = OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, winslash = "/", mustWork = FALSE))
