#!/usr/bin/env Rscript
# Chi-square test: Niveau d'instruction vs VegetationZone / PhytogeographicZone
# Self-contained (chi2_shared.R does not exist / run_variable_analysis was never defined)

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

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg), winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

base_dir <- get_script_dir()

# NOTE: Test_CHI2/data7.xlsx is a stale/corrupted copy; read the source of truth instead.
survey_file <- "C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning/data7.xlsx"
zones_file <- file.path(base_dir, "zones.xlsx")
output_file <- file.path(base_dir, "education_zone_chisq_results.xlsx")

cat("Reading survey file:", survey_file, "\n")
cat("Reading zones file:", zones_file, "\n")

survey_raw <- read_excel(survey_file)
zones_raw <- read_excel(zones_file)

education_col <- find_column(survey_raw, "Niveau.*instruction")
commune_col <- find_column(survey_raw, "Commune")

veg_col <- find_column(zones_raw, "vegetation")
phyto_col <- find_column(zones_raw, "phytogeographic|phytogeographique")
district_col <- find_column(zones_raw, "district")

survey <- survey_raw %>%
  select(all_of(education_col), all_of(commune_col)) %>%
  rename(EducationLevel = !!education_col, Commune = !!commune_col) %>%
  mutate(
    EducationLevel = as.character(EducationLevel),
    Commune = as.character(Commune),
    EducationLevel = str_trim(EducationLevel)
  )

zones <- zones_raw %>%
  select(all_of(c(veg_col, phyto_col, district_col))) %>%
  rename(VegetationZone = !!veg_col, PhytogeographicZone = !!phyto_col, District = !!district_col) %>%
  mutate(
    VegetationZone = as.character(VegetationZone),
    PhytogeographicZone = as.character(PhytogeographicZone),
    District = as.character(District)
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
    District = str_trim(District),
    DistrictClean = normalize_text(District)
  ) %>%
  filter(!str_detect(str_to_lower(District), "^total"))

survey <- survey %>%
  mutate(
    Commune = str_trim(Commune),
    CommuneClean = normalize_text(Commune)
  )

# Tori-Bossito is an arrondissement of Tori commune; count it as Tori for the zone join
survey <- survey %>%
  mutate(CommuneClean = if_else(CommuneClean == "tori bossito", "tori", CommuneClean))

zone_lookup <- zones %>%
  select(DistrictClean, VegetationZone, PhytogeographicZone) %>%
  distinct(DistrictClean, .keep_all = TRUE)

joined <- survey %>%
  left_join(zone_lookup, by = c("CommuneClean" = "DistrictClean"))

cat("Total survey rows:", nrow(survey), "\n")
cat("Matched rows:", sum(!is.na(joined$VegetationZone) & !is.na(joined$PhytogeographicZone)), "\n")
cat("Unmatched rows:", sum(is.na(joined$VegetationZone) | is.na(joined$PhytogeographicZone)), "\n")

run_test <- function(data, zone_col) {
  zone_sym <- sym(zone_col)
  data <- data %>% filter(!is.na(!!zone_sym), !is.na(EducationLevel))
  tab <- table(data$EducationLevel, data[[zone_col]])
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0]

  if (nrow(tab) < 2 || ncol(tab) < 2) {
    stop(sprintf("Not enough categories to run a chi-square test for %s", zone_col), call. = FALSE)
  }

  test <- chisq.test(tab)

  list(
    table = tab,
    expected = round(test$expected, 2),
    statistic = round(test$statistic, 3),
    p_value = round(test$p.value, 4),
    zone_col = zone_col
  )
}

results <- list(
  veg = run_test(joined, "VegetationZone"),
  phyto = run_test(joined, "PhytogeographicZone")
)

wb <- createWorkbook()
addWorksheet(wb, "Observed_contingency_tables")
addWorksheet(wb, "Expected_frequencies")
addWorksheet(wb, "Summary_statistics")

write_block <- function(sheet, result, start_row) {
  obs_df <- as.data.frame.matrix(result$table)
  obs_df <- cbind(EducationLevel = rownames(obs_df), obs_df)
  rownames(obs_df) <- NULL
  writeData(wb, sheet = sheet, x = data.frame(Comparison = paste("Niveau d'instruction vs", result$zone_col)), startRow = start_row, startCol = 1)
  writeData(wb, sheet = sheet, x = obs_df, startRow = start_row + 1, startCol = 1)
}

write_block("Observed_contingency_tables", results$veg, 1)
write_block("Observed_contingency_tables", results$phyto, nrow(as.data.frame.matrix(results$veg$table)) + 4)

write_block_expected <- function(sheet, result, start_row) {
  exp_df <- as.data.frame.matrix(result$expected)
  exp_df <- cbind(EducationLevel = rownames(exp_df), exp_df)
  rownames(exp_df) <- NULL
  writeData(wb, sheet = sheet, x = data.frame(Comparison = paste("Niveau d'instruction vs", result$zone_col)), startRow = start_row, startCol = 1)
  writeData(wb, sheet = sheet, x = exp_df, startRow = start_row + 1, startCol = 1)
}

write_block_expected("Expected_frequencies", results$veg, 1)
write_block_expected("Expected_frequencies", results$phyto, nrow(as.data.frame.matrix(results$veg$expected)) + 4)

summary_text <- c(
  paste0("=== Chi-square test: Niveau d'instruction vs VegetationZone ==="),
  paste0("Chi-square statistic: ", results$veg$statistic),
  paste0("Chi-square p-value: ", format(results$veg$p_value, nsmall = 4)),
  "",
  paste0("=== Chi-square test: Niveau d'instruction vs PhytogeographicZone ==="),
  paste0("Chi-square statistic: ", results$phyto$statistic),
  paste0("Chi-square p-value: ", format(results$phyto$p_value, nsmall = 4))
)

writeData(wb, sheet = "Summary_statistics", x = data.frame(Result = summary_text), startRow = 1, startCol = 1)

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Saved results to", output_file, "\n")

