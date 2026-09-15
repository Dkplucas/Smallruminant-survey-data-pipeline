# VERIFIED: "Pas de distiction" mortality-comparison modality vs zones
# Inputs: data7.xlsx and zones.xlsx
# Output: chi_square_results_djallonke_more_mortality_cases_no_distinction_zones.xlsx

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
OUTPUT_FILE <- "chi_square_results_djallonke_more_mortality_cases_no_distinction_zones.xlsx"
ALPHA <- 0.05
B <- 100000L
SEED <- 20260915L

TARGET_COL <- paste0(
  "11.) Suivi sanitaire/",
  "Enregistrez-vous plus cas de mortalité chez les chèvres de races Djallonké ",
  "que chez les métis et autres races dans votre élevage ? /Pas de distiction"
)

COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

normalize_header <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(x)
  x <- str_squish(x)
  str_replace_all(x, "[^a-z0-9]", "")
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
    "mortalite|djallonke|distiction|distinction"
  )]
  stop(
    label,
    " column could not be identified uniquely. Candidate headers: ",
    paste(candidates, collapse = " | ")
  )
}

if (!file.exists(DATA_FILE)) stop("File not found: ", DATA_FILE, ". Current folder: ", getwd())
if (!file.exists(ZONES_FILE)) stop("File not found: ", ZONES_FILE, ". Current folder: ", getwd())

raw <- read_excel(DATA_FILE, sheet = 1, col_types = "text", .name_repair = "minimal")
zraw <- read_excel(ZONES_FILE, sheet = 1, col_types = "text", .name_repair = "unique")

TARGET_COL <- resolve_column(
  TARGET_COL,
  names(raw),
  c("Suivi sanitaire", "mortalité", "Djallonké", "métis", "Pas de distiction"),
  "Mortality no-distinction modality"
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
  select(commune_key, vegetation_zone, phytogeo_zone) |>
  distinct(commune_key, .keep_all = TRUE)

data <- raw |>
  transmute(
    row_id = row_number(),
    commune_raw = .data[[COMMUNE_COL]],
    commune_key = normalize_key(.data[[COMMUNE_COL]]),
    response_code = str_squish(.data[[TARGET_COL]])
  ) |>
  left_join(zones, by = "commune_key") |>
  mutate(
    invalid_code = !is.na(response_code) & response_code != "" &
      !response_code %in% c("0", "1"),
    response = factor(
      ifelse(response_code %in% c("0", "1"), response_code, NA_character_),
      levels = c("0", "1"),
      labels = c("Not selected", "Pas de distiction selected")
    )
  )

run_test <- function(zone_var, comparison_label) {
  d <- data |>
    filter(
      !is.na(response),
      !is.na(.data[[zone_var]]),
      .data[[zone_var]] != ""
    ) |>
    droplevels()

  observed <- table(d$response, d[[zone_var]])
  n <- sum(observed)
  nr <- nrow(observed)
  nc <- ncol(observed)
  df_chi <- (nr - 1L) * (nc - 1L)

  if (n == 0L || nr < 2L || nc < 2L) {
    reason <- if (n == 0L) {
      "No complete valid observations."
    } else if (nr < 2L) {
      "Only one response category was observed."
    } else {
      "Only one zone category was observed."
    }

    summary_row <- data.frame(
      Comparison = comparison_label,
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

    percentages <- if (n > 0L && nc > 0L) prop.table(observed, margin = 2) * 100 else NULL
    return(list(
      summary = summary_row,
      observed = observed,
      expected = NULL,
      percent = percentages,
      stdres = NULL
    ))
  }

  pearson <- suppressWarnings(chisq.test(observed, correct = FALSE))
  expected <- pearson$expected
  below_5 <- sum(expected < 5)
  below_1 <- sum(expected < 1)
  percent_below_5 <- 100 * below_5 / length(expected)
  pearson_valid <- below_1 == 0L && percent_below_5 <= 20

  set.seed(SEED + ifelse(zone_var == "phytogeo_zone", 100L, 0L))
  monte_carlo <- suppressWarnings(chisq.test(observed, simulate.p.value = TRUE, B = B))

  if (pearson_valid) {
    selected_test <- "Pearson chi-square"
    selected_p <- pearson$p.value
    recommendation <- "Pearson expected-count conditions are acceptable."
  } else {
    selected_test <- paste0("Pearson chi-square with Monte Carlo p-value (B=", B, ")")
    selected_p <- monte_carlo$p.value
    recommendation <- "Sparse expected counts: report the Monte Carlo p-value."
  }

  denominator <- n * min(nr - 1L, nc - 1L)
  cramers_v <- if (denominator > 0) {
    sqrt(as.numeric(pearson$statistic) / denominator)
  } else {
    NA_real_
  }

  summary_row <- data.frame(
    Comparison = comparison_label,
    N = n,
    Rows = nr,
    Columns = nc,
    Degrees_of_freedom = df_chi,
    Pearson_chi_square = as.numeric(pearson$statistic),
    Asymptotic_p_value = pearson$p.value,
    Minimum_expected = min(expected),
    Cells_expected_below_5 = below_5,
    Percent_expected_below_5 = percent_below_5,
    Cells_expected_below_1 = below_1,
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
    Recommendation = recommendation,
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
  if (is.null(x) || length(x) == 0L || is.null(dim(x)) || length(dim(x)) != 2L) {
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
  col_labels <- colnames(m)
  if (is.null(col_labels)) col_labels <- paste0("Column_", seq_len(nc))

  out <- data.frame(Response = row_labels, stringsAsFactors = FALSE, check.names = FALSE)
  for (j in seq_len(nc)) out[[col_labels[j]]] <- unname(m[, j])
  out
}

safe_round <- function(x, digits) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  if (!is.numeric(x)) return(x)
  round(x, digits)
}

veg <- run_test(
  "vegetation_zone",
  "Pas de distiction mortality response x vegetation zone"
)
phyto <- run_test(
  "phytogeo_zone",
  "Pas de distiction mortality response x phytogeographic zone"
)

summary_results <- bind_rows(veg$summary, phyto$summary)

data_quality <- data.frame(
  Metric = c(
    "Total records",
    "Valid 0/1 responses",
    "Not selected (0)",
    "Pas de distiction selected (1)",
    "Blank responses",
    "Invalid response codes",
    "Unmatched communes"
  ),
  Value = c(
    nrow(data),
    sum(!is.na(data$response)),
    sum(data$response_code == "0", na.rm = TRUE),
    sum(data$response_code == "1", na.rm = TRUE),
    sum(is.na(data$response_code) | data$response_code == ""),
    sum(data$invalid_code, na.rm = TRUE),
    sum(is.na(data$vegetation_zone) | is.na(data$phytogeo_zone))
  ),
  stringsAsFactors = FALSE
)

method_notes <- data.frame(
  Parameter = c(
    "Variable type",
    "Coding",
    "Null hypothesis",
    "Expected-count rule",
    "Monte Carlo",
    "Effect size",
    "Spelling"
  ),
  Assessment = c(
    "Standalone binary qualitative modality.",
    "0=modality not selected; 1=Pas de distiction selected.",
    "The response modality and geographical zone are independent.",
    "No expected count below 1 and no more than 20% below 5.",
    "100,000 simulations; selected when Pearson expected-count conditions fail.",
    "Cramer's V.",
    "The source spelling 'Pas de distiction' is intentionally retained."
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
sheets[["Data_quality"]] <- data_quality
sheets[["Method_notes"]] <- method_notes

writexl::write_xlsx(sheets, path = OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, winslash = "/", mustWork = FALSE))
