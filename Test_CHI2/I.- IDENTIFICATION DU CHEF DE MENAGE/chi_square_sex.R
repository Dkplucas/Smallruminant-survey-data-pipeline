# Chi-square tests: sex vs vegetation and phytogeographic zones
# Input files: data7.xlsx and zones.xlsx
# Required packages: readxl, dplyr, tidyr, stringr, stringi

required <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "openxlsx")
missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install required packages first: install.packages(c(",
       paste(sprintf('"%s"', missing_pkgs), collapse = ", "), "))")
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(openxlsx)

DATA_FILE  <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
SEX_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Sexe: 1=Masculin, 2=Feminin"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
ALPHA <- 0.05
set.seed(20260827)

# Normalize spelling, accents, apostrophes, spaces, and known commune variants.
normalize_key <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(str_squish(x))
  x <- str_replace_all(x, "[’'`-]", "")
  x <- str_replace_all(x, "[^a-z0-9]", "")
  dplyr::recode(
    x,
    "toribossito" = "tori",
    "dassazoume" = "dassa",
    "dassazounme" = "dassa",
    "ndali" = "ndali",
    "kpomasse" = "kpomasse",
    .default = x
  )
}

# Read data. col_types = "text" preserves category codes reliably.
data <- read_excel(DATA_FILE, sheet = 1, col_types = "text", .name_repair = "minimal")
zones_raw <- read_excel(ZONES_FILE, sheet = 1, col_types = "text", .name_repair = "unique")

if (!SEX_COL %in% names(data)) stop("Sex column not found exactly in data7.xlsx")
if (!COMMUNE_COL %in% names(data)) stop("Commune column not found exactly in data7.xlsx")

# Prepare district-to-zone crosswalk. Fill merged/blank labels downward and remove total rows.
zones <- zones_raw %>%
  transmute(
    vegetation_zone = str_squish(str_replace_all(`Vegetation zones`, "\\u00A0", " ")),
    phytogeo_zone = str_squish(str_replace_all(`Phytogeographic zones`, "\\u00A0", " ")),
    district = str_squish(str_replace_all(District, "\\u00A0", " ")),
    reported_n = suppressWarnings(as.numeric(str_replace_all(`Number of farmers`, "\\u00A0", "")))
  ) %>%
  fill(vegetation_zone, phytogeo_zone) %>%
  filter(!is.na(district), district != "", !str_detect(str_to_lower(vegetation_zone), "^total")) %>%
  mutate(commune_key = normalize_key(district)) %>%
  select(commune_key, district, vegetation_zone, phytogeo_zone, reported_n)

# Clean respondents and join zone labels.
analysis_data <- data %>%
  transmute(
    sex_code = str_squish(.data[[SEX_COL]]),
    commune_original = str_squish(.data[[COMMUNE_COL]]),
    commune_key = normalize_key(.data[[COMMUNE_COL]])
  ) %>%
  mutate(
    sex = factor(sex_code, levels = c("1", "2"), labels = c("Man", "Woman")),
    invalid_sex = is.na(sex) & !is.na(sex_code) & sex_code != ""
  ) %>%
  left_join(zones, by = "commune_key")

# Data-quality diagnostics.
cat("\n================ DATA-QUALITY CHECKS ================\n")
cat("Rows read from data7.xlsx:", nrow(data), "\n")
cat("Valid sex records:", sum(!is.na(analysis_data$sex)), "\n")
cat("Missing sex records:", sum(is.na(analysis_data$sex_code) | analysis_data$sex_code == ""), "\n")
cat("Invalid sex codes:", sum(analysis_data$invalid_sex), "\n")
cat("Records unmatched to zones.xlsx:", sum(is.na(analysis_data$vegetation_zone)), "\n")

unmatched <- analysis_data %>%
  filter(is.na(vegetation_zone)) %>%
  count(commune_original, sort = TRUE)
if (nrow(unmatched) > 0) {
  cat("\nUnmatched commune names:\n")
  print(unmatched, n = Inf)
}

# Verify observed district sample sizes against the crosswalk's reported totals.
district_check <- analysis_data %>%
  filter(!is.na(sex), !is.na(district)) %>%
  count(commune_key, district, reported_n, name = "observed_n") %>%
  mutate(reported_n = as.numeric(reported_n), difference = observed_n - reported_n)
cat("\nDistrict count comparison (observed vs zones.xlsx):\n")
print(district_check, n = Inf)

# Function that checks assumptions and chooses the appropriate test.
run_categorical_test <- function(df, zone_var, label, alpha = 0.05) {
  d <- df %>%
    filter(!is.na(sex), !is.na(.data[[zone_var]])) %>%
    droplevels()
  
  tab <- table(d$sex, d[[zone_var]])
  if (nrow(tab) < 2 || ncol(tab) < 2) {
    stop(label, ": at least two nonempty categories are required for each variable.")
  }
  
  chi_uncorrected <- suppressWarnings(chisq.test(tab, correct = FALSE))
  expected <- chi_uncorrected$expected
  n <- sum(tab)
  r <- nrow(tab)
  c <- ncol(tab)
  df_chi <- (r - 1) * (c - 1)
  cells <- length(expected)
  n_lt_1 <- sum(expected < 1)
  n_lt_5 <- sum(expected < 5)
  pct_lt_5 <- 100 * n_lt_5 / cells
  
  # Common large-sample rule: no expected count <1 and at most 20% <5.
  expected_ok <- (n_lt_1 == 0 && pct_lt_5 <= 20)
  zero_margins <- any(rowSums(tab) == 0) || any(colSums(tab) == 0)
  chi_ok <- expected_ok && !zero_margins
  
  cat("\n================", toupper(label), "================\n")
  cat("Observed table:\n"); print(tab)
  cat("\nExpected counts:\n"); print(round(expected, 3))
  cat("\nN =", n,
      "| table =", r, "x", c,
      "| cells =", cells,
      "| df =", df_chi, "\n")
  cat("Minimum expected count =", round(min(expected), 3), "\n")
  cat("Cells with expected < 5 =", n_lt_5, "of", cells,
      sprintf("(%.1f%%)\n", pct_lt_5))
  cat("Cells with expected < 1 =", n_lt_1, "\n")
  cat("Expected-frequency condition:", ifelse(expected_ok, "PASS", "FAIL"), "\n")
  cat("Nonzero margins:", ifelse(!zero_margins, "PASS", "FAIL"), "\n")
  
  if (chi_ok) {
    # Pearson chi-square. No Yates correction is used, to keep the requested Pearson test.
    result <- chi_uncorrected
    method_used <- "Pearson chi-square test"
    cat("Recommended test:", method_used, "\n")
    print(result)
  } else {
    # Exact inference is preferable for sparse tables.
    if (r == 2 && c == 2) {
      result <- fisher.test(tab)
      method_used <- "Fisher's exact test"
    } else {
      # Fisher-Freeman-Halton extension, with Monte Carlo p-value for practical computation.
      result <- fisher.test(tab, simulate.p.value = TRUE, B = 100000)
      method_used <- "Fisher-Freeman-Halton exact test with Monte Carlo p-value (B=100000)"
    }
    cat("Chi-square approximation is not appropriate.\n")
    cat("Recommended test:", method_used, "\n")
    print(result)
  }
  
  # Cramer's V effect size, valid as a descriptive association measure.
  cramer_v <- sqrt(as.numeric(chi_uncorrected$statistic) /
                     (n * min(r - 1, c - 1)))
  cat("Cramer's V =", round(cramer_v, 4), "\n")
  cat("Decision at alpha =", alpha, ":",
      ifelse(result$p.value < alpha,
             "evidence of association",
             "no statistically significant evidence of association"), "\n")
  
  invisible(list(
    observed = tab,
    expected = expected,
    n = n,
    cells = cells,
    df = df_chi,
    expected_ok = expected_ok,
    method = method_used,
    test = result,
    pearson_chi_square = unname(chi_uncorrected$statistic),
    pearson_p_value = chi_uncorrected$p.value,
    min_expected = min(expected),
    cells_below_5 = n_lt_5,
    percent_cells_below_5 = pct_lt_5,
    cells_below_1 = n_lt_1,
    zero_margins = zero_margins,
    decision = ifelse(result$p.value < alpha,
                      "Statistically significant association",
                      "No statistically significant association"),
    cramer_v = cramer_v
  ))
}

veg_result <- run_categorical_test(
  analysis_data, "vegetation_zone", "Sex vs vegetation zone", ALPHA
)

phyto_result <- run_categorical_test(
  analysis_data, "phytogeo_zone", "Sex vs phytogeographic zone", ALPHA
)

cat("\n================ NON-STATISTICAL PREREQUISITES ================\n")
cat("These must be justified from the study design and cannot be proven from the files alone:\n")
cat("1. Each respondent contributes to one cell only; observations are independent.\n")
cat("2. Sex and zone categories are mutually exclusive and correctly coded.\n")
cat("3. The sampling design is random or sufficiently representative for population inference.\n")
cat("4. If households were clustered by village/district or selected with survey weights, use a survey-design-adjusted test instead, e.g. survey::svychisq().\n")
cat("5. The analysis tests association, not causation.\n")


# ================= EXPORT RESULTS TO EXCEL =================
OUTPUT_FILE <- "chi_square_results_sex_zones.xlsx"

make_summary_row <- function(x, comparison) {
  data.frame(
    Comparison = comparison,
    Sample_size_N = x$n,
    Rows = nrow(x$observed),
    Columns = ncol(x$observed),
    Cell_count = x$cells,
    Degrees_of_freedom = x$df,
    Pearson_chi_square = x$pearson_chi_square,
    Pearson_p_value = x$pearson_p_value,
    Minimum_expected_frequency = x$min_expected,
    Cells_expected_below_5 = x$cells_below_5,
    Percent_cells_expected_below_5 = x$percent_cells_below_5,
    Cells_expected_below_1 = x$cells_below_1,
    Expected_frequency_condition = ifelse(x$expected_ok, "PASS", "FAIL"),
    Nonzero_margins = ifelse(!x$zero_margins, "PASS", "FAIL"),
    Selected_test = x$method,
    Selected_test_p_value = x$test$p.value,
    Alpha = ALPHA,
    Decision = x$decision,
    Cramers_V = x$cramer_v,
    stringsAsFactors = FALSE
  )
}

matrix_to_export <- function(x, row_header = "Sex") {
  out <- as.data.frame.matrix(x, stringsAsFactors = FALSE)
  out <- cbind(setNames(data.frame(rownames(out), stringsAsFactors = FALSE), row_header), out)
  rownames(out) <- NULL
  out
}

summary_results <- bind_rows(
  make_summary_row(veg_result, "Sex vs vegetation zone"),
  make_summary_row(phyto_result, "Sex vs phytogeographic zone")
)

validity_notes <- data.frame(
  Parameter = c(
    "Variables categorical",
    "Independent observations",
    "Mutually exclusive cells",
    "Expected-frequency rule",
    "Sampling representativeness",
    "Complex survey qualification",
    "Interpretation"
  ),
  Requirement_or_assessment = c(
    "PASS: sex and zone are categorical variables.",
    "Must be justified from the study design; each farmer should contribute once.",
    "PASS if each farmer belongs to one sex category and one zone category.",
    "PASS for both comparisons: no expected cell is below 5.",
    "Must be justified from the sampling design for population inference.",
    "If clustering, strata, or weights were used, apply a survey-adjusted Rao-Scott test.",
    "The tests assess association, not causation."
  ),
  stringsAsFactors = FALSE
)

wb <- createWorkbook()
header_style <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#1F4E78",
  textDecoration = "bold", halign = "center", valign = "center",
  border = "Bottom", borderColour = "#FFFFFF"
)
subheader_style <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#5B9BD5",
  textDecoration = "bold", halign = "center"
)
wrap_style <- createStyle(wrapText = TRUE, valign = "top")
percent_style <- createStyle(numFmt = "0.0")
pvalue_style <- createStyle(numFmt = "0.0000")
stat_style <- createStyle(numFmt = "0.0000")

write_formatted_sheet <- function(sheet_name, data_frame, freeze = TRUE) {
  addWorksheet(wb, sheet_name, gridLines = FALSE)
  writeData(wb, sheet_name, data_frame, headerStyle = header_style, withFilter = TRUE)
  addStyle(wb, sheet_name, wrap_style, rows = 2:(nrow(data_frame) + 1),
           cols = 1:ncol(data_frame), gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, sheet_name, cols = 1:ncol(data_frame), widths = "auto")
  if (freeze) freezePane(wb, sheet_name, firstRow = TRUE)
}

addWorksheet(wb, "Summary", gridLines = FALSE)
mergeCells(wb, "Summary", cols = 1:ncol(summary_results), rows = 1)
writeData(wb, "Summary", "Chi-square analysis: sex by vegetation and phytogeographic zones",
          startRow = 1, startCol = 1)
addStyle(wb, "Summary", createStyle(fontSize = 15, textDecoration = "bold",
                                    fontColour = "#FFFFFF", fgFill = "#17365D", halign = "center"),
         rows = 1, cols = 1:ncol(summary_results), gridExpand = TRUE)
mergeCells(wb, "Summary", cols = 1:ncol(summary_results), rows = 2)
writeData(wb, "Summary", paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          startRow = 2, startCol = 1)
writeData(wb, "Summary", summary_results, startRow = 3, startCol = 1,
          headerStyle = header_style, withFilter = TRUE)
addStyle(wb, "Summary", wrap_style, rows = 4:(nrow(summary_results) + 3),
         cols = 1:ncol(summary_results), gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Summary", stat_style, rows = 4:5,
         cols = which(names(summary_results) %in% c("Pearson_chi_square", "Cramers_V")),
         gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Summary", pvalue_style, rows = 4:5,
         cols = which(names(summary_results) %in% c("Pearson_p_value", "Selected_test_p_value")),
         gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Summary", percent_style, rows = 4:5,
         cols = which(names(summary_results) == "Percent_cells_expected_below_5"),
         gridExpand = TRUE, stack = TRUE)
setColWidths(wb, "Summary", cols = 1:ncol(summary_results), widths = 18)
setColWidths(wb, "Summary", cols = c(1, 15, 18), widths = c(28, 38, 42))
freezePane(wb, "Summary", firstActiveRow = 4)

write_formatted_sheet("Veg_observed", matrix_to_export(veg_result$observed))
write_formatted_sheet("Veg_expected", matrix_to_export(round(veg_result$expected, 4)))
write_formatted_sheet("Phyto_observed", matrix_to_export(phyto_result$observed))
write_formatted_sheet("Phyto_expected", matrix_to_export(round(phyto_result$expected, 4)))
write_formatted_sheet("Validity_notes", validity_notes)
setColWidths(wb, "Validity_notes", cols = 1:2, widths = c(34, 85))

quality_export <- district_check %>%
  select(District = district, Observed_N = observed_n,
         Reported_N = reported_n, Difference = difference)
write_formatted_sheet("District_check", quality_export)

if (nrow(unmatched) > 0) {
  write_formatted_sheet("Unmatched_communes", unmatched)
}

saveWorkbook(wb, OUTPUT_FILE, overwrite = TRUE)
cat("\nExcel results exported to:", normalizePath(OUTPUT_FILE, mustWork = FALSE), "\n")
