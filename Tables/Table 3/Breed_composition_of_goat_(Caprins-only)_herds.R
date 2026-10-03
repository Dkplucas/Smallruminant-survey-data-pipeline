# =============================================================================
# Breed composition of goat (Caprins-only) herds, by phytogeographic zone
# =============================================================================
# What this script does:
#   1. Reads the raw survey export (labels version) and the zones lookup table
#   2. Excludes the training population (Abomey-Calavi + any blank-commune rows)
#   3. Reconstructs the TRUE species classification from the raw Ovins/Caprins
#      checkboxes (NOT the pre-derived "Espece de petit ruminants" field, which
#      was found to be miscoded)
#   4. Keeps only "Caprins only" households (Caprins=1 & Ovins=0) -> 60 farmers
#   5. Normalizes commune names and joins to the phytogeographic zone lookup
#   6. Classifies each herd's breed composition into one of three categories:
#        - "Local breed(s) only (Djallonke/Sahelien)"
#        - "Mixed (local + Metis/Autre)"
#        - "Metis/Autre only"
#      based on whether local-breed and/or Metis/Autre counts are present
#      anywhere in that farmer's breed x sex x age columns
#   7. Builds the counts/percentages table by phytogeographic zone
#   8. Tests the association between breed composition and zone: the standard
#      chi-square test cannot run as-is (the "Local only" column is a
#      structural zero - 0 farmers in every zone - which produces a
#      division-by-zero expected frequency), so that empty column is dropped
#      before testing, and a Monte Carlo permutation p-value is reported
#      alongside the asymptotic one since expected cell counts are small
#   9. Exports everything to a formatted Excel workbook
#
# Required packages: readxl, dplyr, stringr, stringi, openxlsx, tidyr
# =============================================================================

library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(openxlsx)
library(tidyr)

# ---- File paths (edit these to match your local paths) --------------------
LABELS_PATH <- "Questionnaire_caracterisation_pratiques_de_croisements_-_latest_version_-_labels_-_2026-08-25-07-38-57.xlsx"
ZONES_PATH  <- "zones.xlsx"
OUTPUT_PATH <- "goat_breed_composition_by_zone.xlsx"
MC_PERMUTATIONS <- 20000
SEED <- 42

# ---- Column name constants (exact headers from the raw export) ------------
COMMUNE_COL <- "II- CARACTERISTIQUES DE L\u2019UNITE D\u2019ELEVAGE (UE) /Commune"
OVINS_COL   <- "II- CARACTERISTIQUES DE L\u2019UNITE D\u2019ELEVAGE (UE) /Quelles sont les esp\u00e8ces animales que vous \u00e9levez?/1=Ovins"
CAPRINS_COL <- "II- CARACTERISTIQUES DE L\u2019UNITE D\u2019ELEVAGE (UE) /Quelles sont les esp\u00e8ces animales que vous \u00e9levez?/2=Caprins"

# Breed columns grouped by breed (summed across every sex/age tier that exists
# for that breed - adult male, young male, female >=6mo, baby male, baby female)
BREED_COLS <- list(
  Djallonke = c(
    "12.) Effectif et composition du cheptel/Nombre de m\u00e2le adultes Djallonke",
    "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) m\u00e2les_Djallonke",
    "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Djallonke",
    "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) m\u00e2le_Djallonke",
    "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Djallonke"
  ),
  Sahelien = c(
    "12.) Effectif et composition du cheptel/Nombre de m\u00e2le adultes Sahelien",
    "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) m\u00e2les_Sahelien",
    "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femellesSahelienne",
    "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) m\u00e2le_Sahelien",
    "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Sahelien"
  ),
  Metis = c(
    "12.) Effectif et composition du cheptel/Nombre de m\u00e2le adultes Metis",
    "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) m\u00e2les_Metis",
    "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Metis",
    "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) m\u00e2le_Metis",
    "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Metis"
  ),
  Autre = c(
    "12.) Effectif et composition du cheptel/Nombre de m\u00e2le adultes Autre____",
    "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) m\u00e2les_Autre",
    "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Autre"
  )
)

# ---- Helper: normalize a commune/district name into a matching key --------
normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(str_squish(x))
  x <- str_replace_all(x, "[\u2019'`-]", "")
  x <- str_replace_all(x, "[^a-z0-9]", "")
  recode(x,
         "toribossito"  = "tori",
         "dassazoume"   = "dassa",
         "dassazounme"  = "dassa",
         .default = x)
}

# =============================================================================
# 1. Read data
# =============================================================================
raw   <- read_excel(LABELS_PATH, col_types = "text")
zones <- read_excel(ZONES_PATH,  col_types = "text")

# =============================================================================
# 2. Exclude the training population (Abomey-Calavi + blank commune)
# =============================================================================
raw_clean <- raw %>%
  filter(!is.na(.data[[COMMUNE_COL]]), .data[[COMMUNE_COL]] != "Abomey-Calavi")

# =============================================================================
# 3. Reconstruct species from the raw Ovins/Caprins checkboxes
# =============================================================================
raw_clean <- raw_clean %>%
  mutate(
    ovins_flag   = as.numeric(.data[[OVINS_COL]]),
    caprins_flag = as.numeric(.data[[CAPRINS_COL]]),
    species = case_when(
      ovins_flag == 1 & caprins_flag == 0 ~ "Ovins only",
      ovins_flag == 0 & caprins_flag == 1 ~ "Caprins only",
      ovins_flag == 1 & caprins_flag == 1 ~ "Mixed/Both",
      TRUE ~ "Unknown"
    )
  )

# =============================================================================
# 4. Keep Caprins-only households (expected: 60 farmers)
# =============================================================================
goats <- raw_clean %>% filter(species == "Caprins only")
cat("Caprins-only farmers:", nrow(goats), "\n")

# =============================================================================
# 5. Normalize commune and join to the phytogeographic zone lookup
# =============================================================================
zones_valid <- zones %>%
  fill(`Vegetation zones`, `Phytogeographic zones`) %>%
  filter(!is.na(District), District != "") %>%
  mutate(key = normalize_key(District))

goats <- goats %>%
  mutate(key = normalize_key(.data[[COMMUNE_COL]])) %>%
  left_join(zones_valid %>% select(key, `Phytogeographic zones`), by = "key")

n_unmatched <- sum(is.na(goats$`Phytogeographic zones`))
cat("Unmatched to a zone:", n_unmatched, "(should be 0)\n")

zone_order <- c("Vallee de l\u2019Oueme (VOZ)", "Plateau", "Borgou-Sud", "Zou")
goats$`Phytogeographic zones` <- factor(goats$`Phytogeographic zones`, levels = zone_order)

# =============================================================================
# 6. Classify breed composition per farmer
# =============================================================================
for (breed in names(BREED_COLS)) {
  cols <- BREED_COLS[[breed]]
  goats[[paste0("has_", breed)]] <- rowSums(
    sapply(cols, function(c) replace_na(as.numeric(goats[[c]]), 0))
  ) > 0
}

goats <- goats %>%
  mutate(
    has_local  = has_Djallonke | has_Sahelien,
    has_exotic = has_Metis | has_Autre,
    breed_composition = case_when(
      has_local  & !has_exotic ~ "Local breed(s) only (Djallonke/Sahelien)",
      has_exotic & !has_local  ~ "Metis/Autre only",
      has_local  & has_exotic  ~ "Mixed (local + Metis/Autre)",
      TRUE ~ "Unclassified (no breed recorded)"
    ),
    breed_composition = factor(
      breed_composition,
      levels = c("Local breed(s) only (Djallonke/Sahelien)",
                 "Mixed (local + Metis/Autre)",
                 "Metis/Autre only",
                 "Unclassified (no breed recorded)")
    )
  )

cat("\nBreed composition breakdown (overall):\n")
print(table(goats$breed_composition))

# =============================================================================
# 7. Build the counts / percentages table by zone
# =============================================================================
counts_table <- table(goats$`Phytogeographic zones`, goats$breed_composition)
pct_table    <- prop.table(counts_table, margin = 1) * 100

n_per_zone <- table(goats$`Phytogeographic zones`)

cat("\n===== Counts by zone =====\n"); print(counts_table)
cat("\n===== Percent within zone =====\n"); print(round(pct_table, 1))

# Tidy version for export: one row per zone, columns = n + each category's %
breed_summary <- goats %>%
  count(`Phytogeographic zones`, breed_composition, .drop = FALSE) %>%
  group_by(`Phytogeographic zones`) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  pivot_wider(id_cols = `Phytogeographic zones`,
              names_from = breed_composition,
              values_from = c(n, pct)) %>%
  left_join(as.data.frame(n_per_zone) %>% rename(`Phytogeographic zones` = Var1, N_farmers = Freq),
            by = "Phytogeographic zones")

# =============================================================================
# 8. Chi-square test: drop the structural-zero column first, then run a
#    Monte Carlo permutation test alongside the asymptotic chi-square
# =============================================================================
tab_for_test <- goats %>%
  filter(breed_composition != "Local breed(s) only (Djallonke/Sahelien)") %>%
  droplevels()

ct <- table(tab_for_test$`Phytogeographic zones`, tab_for_test$breed_composition)
chi_asym <- suppressWarnings(chisq.test(ct, correct = FALSE))

expected <- chi_asym$expected
pct_below5 <- 100 * sum(expected < 5) / length(expected)
n_below1   <- sum(expected < 1)

cat(sprintf("\nAsymptotic chi-square: chi2=%.4f, df=%d, p=%.4f\n",
            chi_asym$statistic, chi_asym$parameter, chi_asym$p.value))
cat(sprintf("Minimum expected count=%.3f, %% cells<5=%.1f%%, cells<1=%d\n",
            min(expected), pct_below5, n_below1))

set.seed(SEED)
chi_mc <- chisq.test(ct, correct = FALSE, simulate.p.value = TRUE, B = MC_PERMUTATIONS)
cat(sprintf("Monte Carlo p-value (%d permutations) = %.4f\n", MC_PERMUTATIONS, chi_mc$p.value))

test_summary <- tibble(
  Comparison = "Breed composition (Mixed vs Metis/Autre only) vs Phytogeographic zone",
  Note = "The 'Local breed(s) only' category was dropped before testing: it has 0 farmers in every zone (a structural zero), which makes the standard chi-square test's expected frequency undefined for that cell.",
  N = sum(ct),
  Groups_zone = nrow(ct),
  Groups_breed = ncol(ct),
  Chi_square = unname(chi_asym$statistic),
  df = unname(chi_asym$parameter),
  Asymptotic_p_value = chi_asym$p.value,
  Pct_cells_expected_below_5 = pct_below5,
  Monte_Carlo_p_value = chi_mc$p.value,
  Decision = if_else(chi_mc$p.value < 0.05, "Reject H0", "Do not reject H0")
)
print(test_summary)

# =============================================================================
# 9. Export to Excel
# =============================================================================
wb <- createWorkbook()
header_style <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2", halign = "center",
                            wrapText = TRUE, fontName = "Arial", fontSize = 10)
body_style   <- createStyle(fontName = "Arial", fontSize = 10)

add_sheet <- function(wb, name, df) {
  addWorksheet(wb, name)
  writeData(wb, name, df, headerStyle = header_style)
  addStyle(wb, name, body_style, rows = 2:(nrow(df) + 1), cols = 1:ncol(df), gridExpand = TRUE)
  freezePane(wb, name, firstRow = TRUE)
  setColWidths(wb, name, cols = 1:ncol(df), widths = "auto")
}

add_sheet(wb, "Breed_composition_by_zone", as.data.frame(breed_summary))
add_sheet(wb, "Chi_square_Monte_Carlo_test", as.data.frame(test_summary))

saveWorkbook(wb, OUTPUT_PATH, overwrite = TRUE)
cat("\nSaved:", OUTPUT_PATH, "\n")