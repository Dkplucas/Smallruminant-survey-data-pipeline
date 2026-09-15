# Weaned animals deworming option vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: chi_square_results_dewormed_animals_weaned_zones.xlsx

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
OUTPUT_FILE <- "chi_square_results_dewormed_animals_weaned_zones.xlsx"
ALPHA <- 0.05
B <- 100000L
SEED <- 20260914L

DEWORMED_WEANED_ANIMALS_COL <-
  "11.) Suivi sanitaire/Quels sont les animaux déparasités ? /3= Sevrés"
PARENT_DEWORMING_YES_COL <-
  "11.) Suivi sanitaire/Déparasitez-vous vos animaux ? /Oui"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

normalize_header <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |>
    str_to_lower() |>
    str_squish()
  str_replace_all(x, "[^a-z0-9]", "")
}

resolve_column <- function(expected, headers, terms, label) {
  if (expected %in% headers) return(expected)

  normalized_headers <- normalize_header(headers)
  normalized_expected <- normalize_header(expected)
  hits <- which(normalized_headers == normalized_expected)

  if (length(hits) == 1L) {
    message(label, " matched after normalization to: ", headers[hits])
    return(headers[hits])
  }

  normalized_terms <- normalize_header(terms)
  hits <- which(vapply(
    normalized_headers,
    function(header) all(vapply(
      normalized_terms,
      function(term) str_detect(header, fixed(term)),
      logical(1)
    )),
    logical(1)
  ))

  if (length(hits) == 1L) {
    message(label, " matched flexibly to: ", headers[hits])
    return(headers[hits])
  }

  candidates <- headers[str_detect(
    normalized_headers,
    "suivisanitaire|maladiesrecurrentes|diarhee"
  )]

  stop(
    label,
    " column could not be identified uniquely. Candidate headers: ",
    paste(candidates, collapse = " | ")
  )
}

normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |>
    str_to_lower() |>
    str_squish()
  x <- str_replace_all(x, "[’'`-]", "") |>
    str_replace_all("[^a-z0-9]", "")
  recode(
    x,
    "toribossito" = "tori",
    "dassazoume" = "dassa",
    "dassazounme" = "dassa",
    .default = x
  )
}

if (!file.exists(DATA_FILE)) stop("File not found: ", DATA_FILE)
if (!file.exists(ZONES_FILE)) stop("File not found: ", ZONES_FILE)

raw <- read_excel(DATA_FILE, 1, col_types = "text", .name_repair = "minimal")
zraw <- read_excel(ZONES_FILE, 1, col_types = "text", .name_repair = "unique")

DEWORMED_WEANED_ANIMALS_COL <- resolve_column(
  DEWORMED_WEANED_ANIMALS_COL,
  names(raw),
  c("Suivi sanitaire", "animaux déparasités", "Sevrés"),
  "Weaned-animals deworming option"
)

PARENT_DEWORMING_YES_COL <- resolve_column(
  PARENT_DEWORMING_YES_COL,
  names(raw),
  c("Suivi sanitaire", "Déparasitez-vous vos animaux", "Oui"),
  "Parent deworming Yes modality"
)

COMMUNE_COL <- resolve_column(
  COMMUNE_COL,
  names(raw),
  c("CARACTERISTIQUES", "UNITE", "ELEVAGE", "Commune"),
  "Commune"
)

required_zone_columns <- c("Vegetation zones", "Phytogeographic zones", "District")
missing_zone_columns <- setdiff(required_zone_columns, names(zraw))
if (length(missing_zone_columns)) {
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

data <- raw |>
  transmute(
    row_id = row_number(),
    commune_raw = .data[[COMMUNE_COL]],
    commune_key = normalize_key(.data[[COMMUNE_COL]]),
    weaned_animals_code = str_squish(.data[[DEWORMED_WEANED_ANIMALS_COL]]),
    parent_deworming_yes_code = str_squish(.data[[PARENT_DEWORMING_YES_COL]])
  ) |>
  left_join(zones, by = "commune_key") |>
  mutate(
    invalid_child_code = !is.na(weaned_animals_code) & weaned_animals_code != "" & !weaned_animals_code %in% c("0", "1"),
    invalid_parent_code = !is.na(parent_deworming_yes_code) & parent_deworming_yes_code != "" & !parent_deworming_yes_code %in% c("0", "1"),
    eligible = parent_deworming_yes_code == "1",
    ineligible_positive = !eligible & weaned_animals_code == "1",
    weaned_animals = factor(
      ifelse(eligible & weaned_animals_code %in% c("0", "1"), weaned_animals_code, NA_character_),
      levels = c("0", "1"),
      labels = c("No", "Yes")
    )
  )

run_test <- function(zone_var, label) {
  d <- data |>
    filter(
      !is.na(weaned_animals),
      !is.na(.data[[zone_var]]),
      .data[[zone_var]] != ""
    ) |>
    droplevels()

  observed <- table(d$weaned_animals, d[[zone_var]])
  n <- sum(observed)
  nr <- nrow(observed)
  nc <- ncol(observed)
  df_chi <- (nr - 1) * (nc - 1)

  if (n == 0L || nr < 2L || nc < 2L) {
    reason <- if (n == 0L) {
      "No complete valid observations."
    } else if (nr < 2L) {
      "Only one response category observed."
    } else {
      "Only one zone category observed."
    }

    summary_row <- data.frame(
      Comparison = label,
      N = n,
      Rows = nr,
      Columns = nc,
      Degrees_of_freedom = ifelse(df_chi > 0, df_chi, NA),
      Pearson_chi_square = NA_real_,
      Asymptotic_p_value = NA_real_,
      Minimum_expected = NA_real_,
      Percent_expected_below_5 = NA_real_,
      Selected_test = "Not testable",
      Selected_p_value = NA_real_,
      Monte_Carlo_B = NA_integer_,
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
  percent_below_5 <- 100 * sum(expected < 5) / length(expected)
  pearson_valid <- all(expected >= 1) && percent_below_5 <= 20

  if (pearson_valid) {
    selected_p <- pearson$p.value
    selected_test <- "Pearson chi-square"
    monte_carlo_b <- NA_integer_
  } else {
    set.seed(SEED + ifelse(zone_var == "phytogeo_zone", 100L, 0L))
    monte_carlo <- suppressWarnings(
      chisq.test(observed, simulate.p.value = TRUE, B = B)
    )
    selected_p <- monte_carlo$p.value
    selected_test <- "Monte Carlo chi-square"
    monte_carlo_b <- B
  }

  cramers_v <- sqrt(
    as.numeric(pearson$statistic) /
      (n * min(nr - 1, nc - 1))
  )

  summary_row <- data.frame(
    Comparison = label,
    N = n,
    Rows = nr,
    Columns = nc,
    Degrees_of_freedom = df_chi,
    Pearson_chi_square = as.numeric(pearson$statistic),
    Asymptotic_p_value = pearson$p.value,
    Minimum_expected = min(expected),
    Percent_expected_below_5 = percent_below_5,
    Selected_test = selected_test,
    Selected_p_value = selected_p,
    Monte_Carlo_B = monte_carlo_b,
    Alpha = ALPHA,
    Decision = ifelse(
      selected_p < ALPHA,
      "Reject H0: association detected",
      "Do not reject H0: no association detected"
    ),
    Cramers_V = cramers_v,
    Recommendation = ifelse(
      pearson_valid,
      "Pearson assumptions acceptable.",
      "Sparse expected counts: Monte Carlo p-value selected."
    ),
    stringsAsFactors = FALSE
  )

  list(
    summary = summary_row,
    observed = observed,
    expected = expected,
    percent = prop.table(observed, 2) * 100,
    stdres = pearson$stdres
  )
}

matrix_df <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(data.frame(Note = "Not available", stringsAsFactors = FALSE))
  }

  out <- as.data.frame.matrix(x, stringsAsFactors = FALSE)
  out <- data.frame(
    Response = rownames(out),
    out,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

safe_round <- function(x, digits) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  round(x, digits)
}

veg <- run_test(
  "vegetation_zone",
  "Weaned animals selected for deworming x vegetation zone"
)
phyto <- run_test(
  "phytogeo_zone",
  "Weaned animals selected for deworming x phytogeographic zone"
)

summary_results <- bind_rows(veg$summary, phyto$summary)

quality <- data.frame(
  Metric = c("Total records", "Eligible respondents selecting deworming Yes", "Valid eligible binary responses", "Weaned animals not selected (0)", "Weaned animals selected (1)", "Blank child responses", "Invalid child codes", "Invalid parent codes", "Ineligible positive responses", "Unmatched communes"),
  Value = c(nrow(data), sum(data$eligible, na.rm = TRUE), sum(!is.na(data$weaned_animals)), sum(data$eligible & data$weaned_animals_code == "0", na.rm = TRUE), sum(data$eligible & data$weaned_animals_code == "1", na.rm = TRUE), sum(is.na(data$weaned_animals_code) | data$weaned_animals_code == ""), sum(data$invalid_child_code, na.rm = TRUE), sum(data$invalid_parent_code, na.rm = TRUE), sum(data$ineligible_positive, na.rm = TRUE), sum(is.na(data$vegetation_zone))),
  stringsAsFactors = FALSE
)

notes <- data.frame(
  Parameter = c("Variable type", "Parent eligibility", "Coding", "Null hypothesis", "Expected-count rule", "Monte Carlo", "Effect size"),
  Assessment = c("Conditional binary qualitative multiple-response option.", "Eligible only when the parent deworming Yes modality equals 1.", "0=Weaned animals not selected; 1=Weaned animals selected.", "Selection of the weaned-animals deworming option and geographical zone are independent among eligible respondents.", "No expected count below 1 and no more than 20% below 5.", "100,000 simulations, selected when Pearson expected-count conditions fail.", "Cramer's V."),
  stringsAsFactors = FALSE
)

sheets <- list(
  Summary = summary_results,
  Veg_observed = matrix_df(veg$observed),
  Veg_expected = matrix_df(safe_round(veg$expected, 4)),
  Veg_percent = matrix_df(safe_round(veg$percent, 2)),
  Veg_std_residuals = matrix_df(safe_round(veg$stdres, 4)),
  Phyto_observed = matrix_df(phyto$observed),
  Phyto_expected = matrix_df(safe_round(phyto$expected, 4)),
  Phyto_percent = matrix_df(safe_round(phyto$percent, 2)),
  Phyto_std_residuals = matrix_df(safe_round(phyto$stdres, 4)),
  Data_quality = quality,
  Method_notes = notes
)

write_xlsx(sheets, OUTPUT_FILE)
message("Created: ", OUTPUT_FILE)
