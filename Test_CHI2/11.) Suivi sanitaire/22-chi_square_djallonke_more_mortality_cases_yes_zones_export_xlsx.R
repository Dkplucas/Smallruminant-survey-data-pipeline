# Disease-related mortality reported more often in Djallonke goats (Oui modality)
# Comparisons: vegetation zone and phytogeographic zone
# Inputs: data7.xlsx and zones.xlsx
# Output: chi_square_results_djallonke_more_mortality_cases_yes_zones.xlsx

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
OUTPUT_FILE <- "chi_square_results_djallonke_more_mortality_cases_yes_zones.xlsx"
ALPHA <- 0.05
B <- 100000L
SEED <- 20260915L

CHILD_COL <- paste0(
  "11.) Suivi sanitaire/",
  "Enregistrez-vous plus cas de mortalité chez les chèvres de races Djallonké ",
  "que chez les métis et autres races dans votre élevage ? /Oui"
)

COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

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
    "suivisanitaire|mortalite|djallonke|metis"
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
    "ndali" = "ndali",
    "kpomasse" = "kpomasse",
    .default = x
  )
}

if (!file.exists(DATA_FILE)) stop("File not found: ", DATA_FILE, ". Set the working directory correctly.")
if (!file.exists(ZONES_FILE)) stop("File not found: ", ZONES_FILE, ". Set the working directory correctly.")

raw <- read_excel(DATA_FILE, sheet = 1, col_types = "text", .name_repair = "minimal")
zraw <- read_excel(ZONES_FILE, sheet = 1, col_types = "text", .name_repair = "unique")

CHILD_COL <- resolve_column(
  CHILD_COL,
  names(raw),
  c("Suivi sanitaire", "plus cas de mortalité", "Djallonké", "métis", "Oui"),
  "Djallonke mortality-comparison Oui modality"
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
    child_code = str_squish(.data[[CHILD_COL]])
  ) |>
  left_join(zones, by = "commune_key") |>
  mutate(
    invalid_code = !is.na(child_code) & child_code != "" & !child_code %in% c("0", "1"),
    response = factor(
      ifelse(child_code %in% c("0", "1"), child_code, NA_character_),
      levels = c("0", "1"),
      labels = c("Oui not selected", "Oui selected")
    )
  )

empty_summary <- function(label, n, nr, nc, reason) {
  data.frame(
    Comparison = label,
    N = n,
    Rows = nr,
    Columns = nc,
    Cell_count = nr * nc,
    Degrees_of_freedom = ifelse(nr >= 2L && nc >= 2L, (nr - 1L) * (nc - 1L), NA_real_),
    Pearson_chi_square = NA_real_,
    Asymptotic_p_value = NA_real_,
    Minimum_expected = NA_real_,
    Cells_expected_below_5 = NA_real_,
    Percent_expected_below_5 = NA_real_,
    Cells_expected_below_1 = NA_real_,
    Asymptotic_chi_square_valid = "NO",
    Selected_test = "Not testable",
    Selected_p_value = NA_real_,
    Monte_Carlo_B = NA_integer_,
    Monte_Carlo_p_value = NA_real_,
    Monte_Carlo_SE = NA_real_,
    Monte_Carlo_95CI_low = NA_real_,
    Monte_Carlo_95CI_high = NA_real_,
    Alpha = ALPHA,
    Decision = "No statistical test performed",
    Cramers_V = NA_real_,
    Recommendation = reason,
    stringsAsFactors = FALSE
  )
}

run_test <- function(zone_var, label) {
  d <- data |>
    filter(
      !is.na(response),
      !is.na(.data[[zone_var]]),
      .data[[zone_var]] != ""
    ) |>
    droplevels()

  obs <- table(d$response, d[[zone_var]])
  n <- sum(obs)
  nr <- nrow(obs)
  nc <- ncol(obs)

  if (n == 0L || nr < 2L || nc < 2L) {
    reason <- if (n == 0L) {
      "No complete valid observations."
    } else if (nr < 2L) {
      "Only one response category is observed."
    } else {
      "Only one zone category is represented."
    }
    return(list(
      summary = empty_summary(label, n, nr, nc, reason),
      observed = obs,
      expected = NULL,
      percent = if (n > 0L && nc > 0L) prop.table(obs, margin = 2) * 100 else NULL,
      stdres = NULL
    ))
  }

  pearson <- suppressWarnings(chisq.test(obs, correct = FALSE))
  expected <- pearson$expected
  n_below_5 <- sum(expected < 5)
  n_below_1 <- sum(expected < 1)
  pct_below_5 <- 100 * n_below_5 / length(expected)
  valid <- n_below_1 == 0L && pct_below_5 <= 20

  set.seed(SEED + ifelse(zone_var == "phytogeo_zone", 100L, 0L))
  mc <- suppressWarnings(chisq.test(obs, simulate.p.value = TRUE, B = B))
  mc_se <- sqrt(mc$p.value * (1 - mc$p.value) / (B + 1))
  mc_low <- max(0, mc$p.value - 1.96 * mc_se)
  mc_high <- min(1, mc$p.value + 1.96 * mc_se)

  selected_p <- if (valid) pearson$p.value else mc$p.value
  selected_test <- if (valid) {
    "Pearson chi-square (asymptotic p-value)"
  } else {
    paste0("Pearson chi-square with Monte Carlo p-value (B=", B, ")")
  }

  cramer_v <- sqrt(
    as.numeric(pearson$statistic) /
      (n * min(nr - 1L, nc - 1L))
  )

  summary <- data.frame(
    Comparison = label,
    N = n,
    Rows = nr,
    Columns = nc,
    Cell_count = length(expected),
    Degrees_of_freedom = (nr - 1L) * (nc - 1L),
    Pearson_chi_square = as.numeric(pearson$statistic),
    Asymptotic_p_value = pearson$p.value,
    Minimum_expected = min(expected),
    Cells_expected_below_5 = n_below_5,
    Percent_expected_below_5 = pct_below_5,
    Cells_expected_below_1 = n_below_1,
    Asymptotic_chi_square_valid = ifelse(valid, "YES", "NO"),
    Selected_test = selected_test,
    Selected_p_value = selected_p,
    Monte_Carlo_B = B,
    Monte_Carlo_p_value = mc$p.value,
    Monte_Carlo_SE = mc_se,
    Monte_Carlo_95CI_low = mc_low,
    Monte_Carlo_95CI_high = mc_high,
    Alpha = ALPHA,
    Decision = ifelse(
      selected_p < ALPHA,
      "Reject H0: association detected",
      "Do not reject H0: no association detected"
    ),
    Cramers_V = cramer_v,
    Recommendation = ifelse(
      valid,
      "Report the ordinary Pearson chi-square p-value.",
      "Report the Monte Carlo p-value because expected-count conditions fail."
    ),
    stringsAsFactors = FALSE
  )

  list(
    summary = summary,
    observed = obs,
    expected = expected,
    percent = prop.table(obs, margin = 2) * 100,
    stdres = pearson$stdres
  )
}

safe_round <- function(x, digits) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  round(x, digits)
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

  rn <- rownames(m)
  if (is.null(rn)) rn <- as.character(seq_len(nr))
  cn <- colnames(m)
  if (is.null(cn)) cn <- paste0("Zone_", seq_len(nc))

  out <- data.frame(Response = rn, stringsAsFactors = FALSE, check.names = FALSE)
  for (j in seq_len(nc)) out[[cn[j]]] <- unname(m[, j])
  out
}

veg <- run_test(
  "vegetation_zone",
  "More mortality cases in Djallonke goats (Oui modality) x vegetation zone"
)
phyto <- run_test(
  "phytogeo_zone",
  "More mortality cases in Djallonke goats (Oui modality) x phytogeographic zone"
)

summary_results <- bind_rows(veg$summary, phyto$summary)

quality <- data.frame(
  Metric = c(
    "Total records",
    "Valid 0/1 responses",
    "Oui not selected (0)",
    "Oui selected (1)",
    "Blank responses",
    "Invalid response codes",
    "Unmatched communes"
  ),
  Value = c(
    nrow(data),
    sum(!is.na(data$response)),
    sum(data$child_code == "0", na.rm = TRUE),
    sum(data$child_code == "1", na.rm = TRUE),
    sum(is.na(data$child_code) | data$child_code == ""),
    sum(data$invalid_code),
    sum(is.na(data$vegetation_zone) | is.na(data$phytogeo_zone))
  ),
  stringsAsFactors = FALSE
)

notes <- data.frame(
  Parameter = c(
    "Variable type",
    "Coding",
    "Null hypothesis",
    "Expected-count rule",
    "Monte Carlo rule",
    "Effect size",
    "Independence prerequisite",
    "Complex survey caution"
  ),
  Assessment = c(
    "Standalone binary qualitative modality.",
    "0=Oui modality not selected; 1=Oui modality selected.",
    "Selection of the Oui modality is independent of geographical zone.",
    "No expected count below 1 and no more than 20% of expected counts below 5.",
    "100,000 simulations; selected when the Pearson expected-count rule fails.",
    "Cramer's V.",
    "Each livestock unit must contribute one independent observation to each table.",
    "If sampling weights, strata, or village clusters were used, consider a Rao-Scott survey-adjusted chi-square test."
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
