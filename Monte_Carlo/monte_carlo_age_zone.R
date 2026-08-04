#!/usr/bin/env Rscript
# Monte carlo chisq.test(..., simulate.p.value = TRUE, B = 20000) because the Expected count 5 for the Chi2 was >5

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(openxlsx)
})

find_column <- function(df, pattern) {
  idx <- grep(pattern, names(df), ignore.case = TRUE)
  if (length(idx) == 0) {
    stop(sprintf("Could not find a column matching pattern: %s", pattern), call. = FALSE)
  }
  names(df)[idx[1]]
}

normalize_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- str_trim(x)
  x <- str_replace_all(x, "[\\s\\u00A0]+", " ")
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", " ")
  x <- str_squish(x)
  x
}

fill_forward <- function(x) {
  out <- as.character(x)
  last_value <- NA_character_
  for (i in seq_along(out)) {
    if (!is.na(out[i]) && str_trim(out[i]) != "") {
      last_value <- out[i]
    } else {
      out[i] <- last_value
    }
  }
  out
}

prepare_workbook <- function(output_file) {
  if (file.exists(output_file)) {
    file.remove(output_file)
  }

  wb <- createWorkbook()
  addWorksheet(wb, "Observed_contingency_tables")
  addWorksheet(wb, "Expected_frequencies")
  addWorksheet(wb, "Summary_statistics")

  saveWorkbook(wb, output_file, overwrite = TRUE)
  wb
}

run_monte_carlo_test <- function(data, zone_col, output_file, workbook) {
  tab <- table(data$AgeGroup, data[[zone_col]])
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0]

  if (nrow(tab) < 2 || ncol(tab) < 2) {
    stop(sprintf("Not enough categories to run a Monte Carlo test for %s", zone_col), call. = FALSE)
  }

  test <- chisq.test(tab, simulate.p.value = TRUE, B = 20000)

  cat("\n=== Monte Carlo test: Age group vs", zone_col, "===\n")
  print(tab)
  cat("\nChi-square statistic:", round(test$statistic, 3), "\n")
  cat("Monte Carlo p-value:", format.pval(test$p.value, digits = 4), "\n")
  cat("\nExpected frequencies:\n")
  print(round(test$expected, 2))

  observed_df <- as.data.frame.matrix(tab)
  observed_df <- cbind(AgeGroup = rownames(observed_df), observed_df)
  rownames(observed_df) <- NULL

  expected_df <- as.data.frame.matrix(round(test$expected, 2))
  expected_df <- cbind(AgeGroup = rownames(expected_df), expected_df)
  rownames(expected_df) <- NULL

  summary_lines <- c(
    paste0("=== Monte Carlo test: Age group vs ", zone_col, " ==="),
    paste0("Chi-square statistic: ", round(test$statistic, 3)),
    paste0("Monte Carlo p-value: ", format(round(test$p.value, 4), nsmall = 4))
  )

  observed_sheet <- "Observed_contingency_tables"
  expected_sheet <- "Expected_frequencies"

  writeData(workbook, sheet = observed_sheet, x = cbind(Comparison = paste("AgeGroup vs", zone_col), observed_df), startRow = 1, startCol = 1)
  writeData(workbook, sheet = expected_sheet, x = cbind(Comparison = paste("AgeGroup vs", zone_col), expected_df), startRow = 1, startCol = 1)

  saveWorkbook(workbook, output_file, overwrite = TRUE)
  cat("Saved results to", output_file, "\n")

  list(summary = summary_lines)
}

script_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", script_args, value = TRUE)
if (length(file_arg) > 0) {
  base_dir <- dirname(normalizePath(sub("^--file=", "", file_arg), winslash = "/", mustWork = TRUE))
} else {
  base_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

data_path <- file.path(base_dir, "data8.xlsx")
zones_path <- file.path(base_dir, "zones.xlsx")
output_file <- file.path(base_dir, "age_zone_monte_carlo_results.xlsx")
if (file.exists(output_file)) {
  output_file <- file.path(base_dir, paste0("age_zone_monte_carlo_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
}

wb <- prepare_workbook(output_file)
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Reading survey data from", data_path, "\n")
survey_raw <- read_excel(data_path, sheet = 1)

age_col <- find_column(survey_raw, "catégorie d'âge|categorie d'age|catégorie d.age")
commune_col <- find_column(survey_raw, "commune")

survey <- survey_raw %>%
  select(all_of(age_col), all_of(commune_col)) %>%
  rename(AgeCategory = !!age_col, Commune = !!commune_col) %>%
  mutate(
    AgeCategory = as.character(AgeCategory),
    Commune = as.character(Commune),
    AgeGroup = case_when(
      str_detect(str_to_lower(AgeCategory), "> 50|>50|50ans|1") ~ ">50",
      str_detect(str_to_lower(AgeCategory), "20 a 30|20-30|20 to 30|2") ~ "20-30",
      str_detect(str_to_lower(AgeCategory), "30 a 50|30-50|30 to 50|3") ~ "30-50",
      TRUE ~ str_squish(AgeCategory)
    )
  )

cat("Reading zone data from", zones_path, "\n")
zones_raw <- read_excel(zones_path, sheet = 1)

veg_col <- find_column(zones_raw, "vegetation")
phyto_col <- find_column(zones_raw, "phytogeographic|phytogeographique")
district_col <- find_column(zones_raw, "district")

zones <- zones_raw %>%
  select(all_of(c(veg_col, phyto_col, district_col))) %>%
  rename(VegetationZone = !!veg_col, PhytogeographicZone = !!phyto_col, District = !!district_col) %>%
  mutate(
    District = as.character(District),
    VegetationZone = as.character(VegetationZone),
    PhytogeographicZone = as.character(PhytogeographicZone)
  ) %>%
  mutate(
    VegetationZone = if_else(is.na(VegetationZone) | VegetationZone %in% c("NA", ""), NA_character_, VegetationZone),
    PhytogeographicZone = if_else(is.na(PhytogeographicZone) | PhytogeographicZone %in% c("NA", ""), NA_character_, PhytogeographicZone),
    District = if_else(is.na(District) | District %in% c("NA", ""), NA_character_, District)
  )

zones$VegetationZone <- fill_forward(zones$VegetationZone)
zones$PhytogeographicZone <- fill_forward(zones$PhytogeographicZone)

zones <- zones %>%
  mutate(
    District = str_squish(District),
    DistrictClean = normalize_text(District)
  ) %>%
  filter(!str_detect(str_to_lower(District), "^total"))

survey <- survey %>%
  mutate(
    Commune = str_squish(Commune),
    CommuneClean = normalize_text(Commune)
  )

zones_join <- zones %>%
  select(DistrictClean, VegetationZone, PhytogeographicZone) %>%
  distinct(DistrictClean, .keep_all = TRUE)

joined <- survey %>%
  left_join(zones_join, by = c("CommuneClean" = "DistrictClean")) %>%
  filter(!is.na(AgeGroup))

cat("\nMatched rows after joining communes to zones:", nrow(joined), "\n")
cat("Unmatched rows:", sum(is.na(joined$VegetationZone) | is.na(joined$PhytogeographicZone)), "\n")

if (sum(!is.na(joined$VegetationZone)) == 0 || sum(!is.na(joined$PhytogeographicZone)) == 0) {
  stop("No usable zone matches were found after joining the survey and zone tables.", call. = FALSE)
}

veg_summary <- run_monte_carlo_test(joined %>% filter(!is.na(VegetationZone)), "VegetationZone", output_file, wb)
phyto_summary <- run_monte_carlo_test(joined %>% filter(!is.na(PhytogeographicZone)), "PhytogeographicZone", output_file, wb)

summary_text <- c(veg_summary$summary, "", phyto_summary$summary)
writeData(wb, sheet = "Summary_statistics", x = data.frame(Result = summary_text), startRow = 1, startCol = 1)
saveWorkbook(wb, output_file, overwrite = TRUE)
