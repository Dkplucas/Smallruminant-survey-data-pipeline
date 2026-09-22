# Total herd size by district for the goat-farmer survey.
# Required packages: readxl, dplyr, and writexl.
# The script normalizes Commune names, sums farmers' herd sizes within each
# district, checks all records, and exports an Excel workbook.

required_packages <- c("readxl", "dplyr", "writexl")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the missing package(s): install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(writexl)
})

DATA_FILE <- "data7.xlsx"
OUTPUT_FILE <- "total_herd_size_by_district.xlsx"

COMMUNE_COLUMN <-
  "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

HERD_SIZE_COLUMN <-
  "12.) Effectif et composition du cheptel/Effectif total du troupeau"

if (!file.exists(DATA_FILE)) {
  stop("File not found: ", DATA_FILE, ". Current folder: ", getwd())
}

survey_data <- read_excel(
  DATA_FILE,
  sheet = 1,
  col_types = "text",
  .name_repair = "minimal"
)

required_columns <- c(COMMUNE_COLUMN, HERD_SIZE_COLUMN)
missing_columns <- setdiff(required_columns, names(survey_data))

if (length(missing_columns) > 0L) {
  stop("Missing required column(s): ", paste(missing_columns, collapse = " | "))
}

if (nrow(survey_data) != 211L) {
  stop(
    "Expected 211 farmer records, but readxl imported ",
    nrow(survey_data),
    ". Re-save data7.xlsx in Excel or use the repaired workbook."
  )
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

district_dictionary <- data.frame(
  commune_key = c(
    "agbangninzoun", "athieme", "bohicon", "dassa", "djidja", "dogbo",
    "glazoue", "ketou", "kpomasse", "ndali", "save", "tchaourou",
    "toffo", "tori", "zogbodomey"
  ),
  district = c(
    "Agbangninzoun", "Athiémé", "Bohicon", "Dassa", "Djidja", "Dogbo",
    "Glazoué", "Kétou", "Kpomasse", "N'dali", "Savè", "Tchaourou",
    "Toffo", "Tori", "Zogbodomey"
  ),
  stringsAsFactors = FALSE
)

analysis_data <- survey_data |>
  transmute(
    survey_row = row_number() + 1L,
    commune_original = .data[[COMMUNE_COLUMN]],
    commune_key = normalize_commune_key(.data[[COMMUNE_COLUMN]]),
    herd_size_raw = .data[[HERD_SIZE_COLUMN]],
    herd_size = suppressWarnings(
      as.numeric(gsub(",", ".", trimws(.data[[HERD_SIZE_COLUMN]]), fixed = TRUE))
    )
  ) |>
  left_join(district_dictionary, by = "commune_key")

unmatched_records <- analysis_data |> filter(is.na(district))
invalid_herd_sizes <- analysis_data |>
  filter(
    is.na(herd_size) |
      herd_size <= 0 |
      abs(herd_size - round(herd_size)) > 1e-9
  )

cat("\nDATA-QUALITY CHECK\n")
cat("Total rows read:                 ", nrow(analysis_data), "\n", sep = "")
cat("Rows matched to a district:     ", sum(!is.na(analysis_data$district)), "\n", sep = "")
cat("Unmatched/unclassified rows:     ", nrow(unmatched_records), "\n", sep = "")
cat("Invalid herd-size rows:          ", nrow(invalid_herd_sizes), "\n", sep = "")

if (nrow(unmatched_records) > 0L) {
  cat("\nActual unmatched Commune values:\n")
  print(
    unmatched_records |>
      count(commune_original, commune_key, name = "n") |>
      arrange(desc(n), commune_original),
    n = Inf
  )
}

if (nrow(unmatched_records) > 0L || nrow(invalid_herd_sizes) > 0L) {
  stop("Resolve unmatched Commune or invalid herd-size values before aggregation.")
}

herd_totals_by_district <- analysis_data |>
  group_by(district) |>
  summarise(
    total_herd_size = sum(herd_size),
    n_farmers = n(),
    .groups = "drop"
  ) |>
  arrange(desc(total_herd_size), district)

grand_total <- data.frame(
  measure = c("Grand total herd size", "Grand total farmers"),
  value = c(sum(analysis_data$herd_size), nrow(analysis_data)),
  stringsAsFactors = FALSE
)

data_quality <- data.frame(
  measure = c(
    "Total rows read", "Rows matched to a district",
    "Unmatched/unclassified rows", "Invalid herd-size rows",
    "Observed districts"
  ),
  value = c(
    nrow(analysis_data), sum(!is.na(analysis_data$district)),
    nrow(unmatched_records), nrow(invalid_herd_sizes),
    n_distinct(analysis_data$district)
  ),
  stringsAsFactors = FALSE
)

if (sum(herd_totals_by_district$total_herd_size) != grand_total$value[1]) {
  stop("District herd totals do not equal the whole-column herd-size sum.")
}
if (sum(herd_totals_by_district$n_farmers) != grand_total$value[2]) {
  stop("District farmer counts do not sum to 211.")
}

cat("\nTOTAL HERD SIZE BY DISTRICT\n")
print(herd_totals_by_district, n = Inf)
cat("\nGrand total herd size: ", grand_total$value[1], "\n", sep = "")
cat("Grand total farmers:   ", grand_total$value[2], "\n", sep = "")

write_xlsx(
  list(
    District_totals = herd_totals_by_district,
    Grand_total = grand_total,
    Data_quality = data_quality
  ),
  path = OUTPUT_FILE
)

cat("\nExcel workbook exported to: ", normalizePath(OUTPUT_FILE, mustWork = FALSE), "\n", sep = "")
