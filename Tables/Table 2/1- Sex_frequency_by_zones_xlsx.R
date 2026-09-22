# Sex frequencies by vegetation and phytogeographic zones in one Excel workbook.
# Required packages: readxl, dplyr, and writexl. Base R table(), chisq.test(),
# and fisher.test() are used for cross-tabulations and independence tests.

required_packages <- c("readxl", "dplyr", "writexl")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Install missing package(s): ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(writexl)
})

DATA_FILE <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "sex_frequency_by_zones.xlsx"

SEX_COLUMN <-
  "I.- IDENTIFICATION DU CHEF DE MENAGE /Sexe: 1=Masculin, 2=Feminin"
COMMUNE_COLUMN <-
  "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
ALPHA <- 0.05

if (!file.exists(DATA_FILE)) stop("File not found: ", DATA_FILE)
if (!file.exists(ZONES_FILE)) stop("File not found: ", ZONES_FILE)

survey_data <- read_excel(
  DATA_FILE, sheet = 1, col_types = "text", .name_repair = "minimal"
)
zone_file <- read_excel(
  ZONES_FILE, sheet = 1, col_types = "text", .name_repair = "unique"
)

if (!all(c(SEX_COLUMN, COMMUNE_COLUMN) %in% names(survey_data))) {
  stop("The required Sexe or Commune column is missing from data7.xlsx.")
}
if (!all(c("Vegetation zones", "Phytogeographic zones", "District") %in% names(zone_file))) {
  stop("zones.xlsx must contain Vegetation zones, Phytogeographic zones, and District.")
}
if (nrow(survey_data) != 211L) {
  stop("Expected 211 farmer records, but readxl imported ", nrow(survey_data), ".")
}

normalize_commune_key <- function(x) {
  x <- as.character(x)
  x <- gsub("\u00A0", " ", x, fixed = TRUE)
  x <- gsub("[’‘`´]", "'", x)
  x <- trimws(x)
  x <- gsub("[[:space:]]+", " ", x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]", "", x)
  x[x %in% c("dassazoume", "dassazounme")] <- "dassa"
  x[x == "toribossito"] <- "tori"
  x
}

# Reuse the established district-to-zone mapping unchanged.
zone_lookup <- data.frame(
  district = c(
    "Toffo", "Zogbodomey", "Dogbo", "Athiémé",
    "Kpomasse", "Tori", "Kétou", "Bohicon", "Agbangninzoun",
    "Djidja", "Dassa", "Glazoué", "N'dali", "Tchaourou", "Savè"
  ),
  vegetation_zone = c(rep("Guineo-Congolaise", 9), rep("Guineo-Soudanienne", 6)),
  phytogeographic_zone = c(
    rep("Vallée de l'Ouémé (VOZ)", 4), rep("Plateau", 5),
    rep("Zou", 2), rep("Borgou-Sud", 4)
  ),
  stringsAsFactors = FALSE
) |>
  mutate(commune_key = normalize_commune_key(district)) |>
  select(commune_key, district, vegetation_zone, phytogeographic_zone)

analysis_data <- survey_data |>
  transmute(
    survey_row = row_number() + 1L,
    commune_original = .data[[COMMUNE_COLUMN]],
    commune_key = normalize_commune_key(.data[[COMMUNE_COLUMN]]),
    sex_code = suppressWarnings(as.integer(trimws(.data[[SEX_COLUMN]])))
  ) |>
  left_join(zone_lookup, by = "commune_key") |>
  mutate(
    sex = factor(sex_code, levels = c(1L, 2L), labels = c("Male", "Female"))
  )

unmatched_records <- analysis_data |> filter(is.na(district))
invalid_sex_records <- analysis_data |> filter(is.na(sex))

if (nrow(unmatched_records) > 0L) {
  cat("Unmatched Commune values:\n")
  print(unmatched_records |> count(commune_original, commune_key, name = "n"), n = Inf)
}
if (nrow(invalid_sex_records) > 0L) {
  cat("Invalid Sexe values:\n")
  print(invalid_sex_records |> count(sex_code, name = "n"), n = Inf)
}
if (nrow(unmatched_records) > 0L || nrow(invalid_sex_records) > 0L) {
  stop("Resolve unmatched Commune or invalid Sexe values before analysis.")
}

make_frequency_table <- function(data, zone_variable) {
  data |>
    count(zone = .data[[zone_variable]], sex, name = "count", .drop = FALSE) |>
    group_by(zone) |>
    mutate(zone_total = sum(count), percentage = 100 * count / zone_total) |>
    ungroup() |>
    arrange(zone, sex)
}

run_independence_test <- function(data, zone_variable, comparison_label) {
  contingency_table <- table(data[[zone_variable]], data$sex)
  chi_result <- suppressWarnings(chisq.test(contingency_table, correct = FALSE))

  if (any(chi_result$expected < 5)) {
    fisher_result <- fisher.test(contingency_table)
    data.frame(
      comparison = comparison_label,
      test_used = "Fisher's exact test",
      statistic = if (!is.null(fisher_result$estimate)) unname(fisher_result$estimate) else NA_real_,
      statistic_name = if (!is.null(fisher_result$estimate)) "odds ratio" else NA_character_,
      degrees_of_freedom = NA_real_,
      p_value = fisher_result$p.value,
      minimum_expected_count = min(chi_result$expected),
      alpha = ALPHA,
      decision = ifelse(fisher_result$p.value < ALPHA, "Reject H0", "Do not reject H0"),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      comparison = comparison_label,
      test_used = "Pearson chi-square test",
      statistic = unname(chi_result$statistic),
      statistic_name = "X-squared",
      degrees_of_freedom = unname(chi_result$parameter),
      p_value = chi_result$p.value,
      minimum_expected_count = min(chi_result$expected),
      alpha = ALPHA,
      decision = ifelse(chi_result$p.value < ALPHA, "Reject H0", "Do not reject H0"),
      stringsAsFactors = FALSE
    )
  }
}

vegetation_frequency <- make_frequency_table(analysis_data, "vegetation_zone")
phytogeo_frequency <- make_frequency_table(analysis_data, "phytogeographic_zone")

test_results <- bind_rows(
  run_independence_test(analysis_data, "vegetation_zone", "Sex by vegetation zone"),
  run_independence_test(analysis_data, "phytogeographic_zone", "Sex by phytogeographic zone")
)

data_quality <- data.frame(
  measure = c(
    "Total rows read", "Rows with valid Sexe value", "Rows matched to both zones",
    "Unmatched Commune rows", "Invalid/missing Sexe rows"
  ),
  value = c(
    nrow(analysis_data), sum(!is.na(analysis_data$sex)),
    sum(!is.na(analysis_data$district)), nrow(unmatched_records),
    nrow(invalid_sex_records)
  ),
  stringsAsFactors = FALSE
)

method_notes <- data.frame(
  item = c("Sex coding", "Percentages", "Test selection", "Alpha"),
  detail = c(
    "1 = Male; 2 = Female.",
    "Percentages are calculated within each zone and sum to 100% per zone.",
    "Pearson chi-square is used when all expected counts are at least 5; otherwise Fisher's exact test is used.",
    as.character(ALPHA)
  ),
  stringsAsFactors = FALSE
)

cat("\nDATA-QUALITY CHECK\n")
print(data_quality)
cat("\nTEST RESULTS\n")
print(test_results)

write_xlsx(
  list(
    Vegetation_frequency = vegetation_frequency,
    Phytogeo_frequency = phytogeo_frequency,
    Test_results = test_results,
    Data_quality = data_quality,
    Method_notes = method_notes
  ),
  path = OUTPUT_FILE
)

cat("\nExcel workbook exported to: ", normalizePath(OUTPUT_FILE, mustWork = FALSE), "\n", sep = "")
