# =============================================================================
# Sheep (Ovins-only) herd size vs zone - Kruskal-Wallis analysis
# =============================================================================
# What this script does:
#   1. Reads the raw survey export (labels version) and the zones lookup table
#   2. Excludes the training population (Abomey-Calavi + any blank-commune rows)
#   3. Reconstructs the TRUE species classification from the raw Ovins/Caprins
#      checkboxes (NOT the pre-derived "Espece de petit ruminants" field, which
#      was found to be miscoded)
#   4. Keeps only "Ovins only" households (Ovins=1 & Caprins=0)
#   5. Normalizes commune names and joins to the vegetation/phytogeographic
#      zone lookup table
#   6. Runs Kruskal-Wallis on total herd size vs vegetation zone, and vs
#      phytogeographic zone; runs pairwise post-hoc (Mann-Whitney, BH +
#      Bonferroni corrected) if the phytogeographic comparison is significant
#   7. Exports everything to a formatted Excel workbook
#
# Required packages: readxl, dplyr, stringr, stringi, openxlsx, purrr, tidyr
# =============================================================================

library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(openxlsx)
library(purrr)
library(tidyr)

# ---- File paths (edit these to match your local paths) --------------------
LABELS_PATH <- "Questionnaire_caracterisation_pratiques_de_croisements_-_latest_version_-_labels_-_2026-08-25-07-38-57.xlsx"
ZONES_PATH  <- "zones.xlsx"
OUTPUT_PATH <- "sheep_herd_size_kruskal_wallis.xlsx"

# ---- Column name constants (exact headers from the raw export) ------------
COMMUNE_COL <- "II- CARACTERISTIQUES DE L\u2019UNITE D\u2019ELEVAGE (UE) /Commune"
VILLAGE_COL <- "II- CARACTERISTIQUES DE L\u2019UNITE D\u2019ELEVAGE (UE) /Village/Localit\u00e9 : "
OVINS_COL   <- "II- CARACTERISTIQUES DE L\u2019UNITE D\u2019ELEVAGE (UE) /Quelles sont les esp\u00e8ces animales que vous \u00e9levez?/1=Ovins"
CAPRINS_COL <- "II- CARACTERISTIQUES DE L\u2019UNITE D\u2019ELEVAGE (UE) /Quelles sont les esp\u00e8ces animales que vous \u00e9levez?/2=Caprins"
TOTAL_COL   <- "12.) Effectif et composition du cheptel/Effectif total du troupeau"

# Herd composition (breed x sex x age) columns
COMP_COLS <- c(
  Adult_male_Djallonke        = "12.) Effectif et composition du cheptel/Nombre de m\u00e2le adultes Djallonke",
  Adult_male_Sahelien         = "12.) Effectif et composition du cheptel/Nombre de m\u00e2le adultes Sahelien",
  Adult_male_Metis            = "12.) Effectif et composition du cheptel/Nombre de m\u00e2le adultes Metis",
  Adult_male_Autre            = "12.) Effectif et composition du cheptel/Nombre de m\u00e2le adultes Autre____",
  Young_male_ge6mo_Djallonke  = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) m\u00e2les_Djallonke",
  Young_male_ge6mo_Sahelien   = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) m\u00e2les_Sahelien",
  Young_male_ge6mo_Metis      = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) m\u00e2les_Metis",
  Young_male_ge6mo_Autre      = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) m\u00e2les_Autre",
  Female_ge6mo_Djallonke      = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Djallonke",
  Female_ge6mo_Sahelien       = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femellesSahelienne",
  Female_ge6mo_Metis          = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Metis",
  Female_ge6mo_Autre          = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Autre",
  Baby_male_lt6mo_Djallonke   = "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) m\u00e2le_Djallonke",
  Baby_male_lt6mo_Sahelien    = "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) m\u00e2le_Sahelien",
  Baby_male_lt6mo_Metis       = "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) m\u00e2le_Metis",
  Baby_female_lt6mo_Djallonke = "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Djallonke",
  Baby_female_lt6mo_Sahelien  = "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Sahelien",
  Baby_female_lt6mo_Metis     = "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Metis"
)

# ---- Helper: normalize a commune/district name into a matching key --------
# Strips accents, lowercases, removes apostrophes/hyphens/whitespace, and
# collapses the known multi-word or spelling variants onto a single canonical
# key so it matches the corresponding key built from zones.xlsx's District
# column. (Same normalization used across the rest of this project's scripts.)
normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\u00A0", " ")   # non-breaking space -> space
  x <- stri_trans_general(x, "Latin-ASCII")               # strip accents
  x <- str_to_lower(str_squish(x))
  x <- str_replace_all(x, "[\u2019'`-]", "")               # drop apostrophes/hyphens
  x <- str_replace_all(x, "[^a-z0-9]", "")                 # drop remaining punctuation/spaces
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

cat("Rows after excluding training population:", nrow(raw_clean), "\n")

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

cat("\nSpecies breakdown:\n")
print(table(raw_clean$species, useNA = "ifany"))

# =============================================================================
# 4. Keep Ovins-only households
# =============================================================================
sheep <- raw_clean %>% filter(species == "Ovins only")
cat("\nOvins-only farmers:", nrow(sheep), "\n")

# =============================================================================
# 5. Normalize commune and join to vegetation/phytogeographic zones
# =============================================================================
zones_valid <- zones %>%
  fill(`Vegetation zones`, `Phytogeographic zones`) %>%
  filter(!is.na(District), District != "") %>%
  mutate(key = normalize_key(District))

sheep <- sheep %>%
  mutate(key = normalize_key(.data[[COMMUNE_COL]])) %>%
  left_join(zones_valid %>% select(key, `Vegetation zones`, `Phytogeographic zones`),
            by = "key")

n_unmatched <- sum(is.na(sheep$`Vegetation zones`))
cat("Unmatched to a zone:", n_unmatched, "(should be 0)\n")
if (n_unmatched > 0) {
  cat("Unmatched communes:\n")
  print(sheep %>% filter(is.na(`Vegetation zones`)) %>% pull(.data[[COMMUNE_COL]]) %>% unique())
}

# =============================================================================
# 6. Prepare herd size and composition columns
# =============================================================================
sheep <- sheep %>%
  mutate(total_herd_size = as.numeric(.data[[TOTAL_COL]])) %>%
  mutate(across(all_of(COMP_COLS), ~ replace_na(as.numeric(.x), 0), .names = "{.col}"))

# =============================================================================
# 7. Kruskal-Wallis: herd size vs vegetation zone
# =============================================================================
kw_veg <- kruskal.test(total_herd_size ~ `Vegetation zones`, data = sheep)

# =============================================================================
# 8. Kruskal-Wallis: herd size vs phytogeographic zone
# =============================================================================
sheep$`Phytogeographic zones` <- factor(sheep$`Phytogeographic zones`)
kw_phyto <- kruskal.test(total_herd_size ~ `Phytogeographic zones`, data = sheep)

kw_summary <- tibble(
  Comparison = c("Herd size vs Vegetation zone", "Herd size vs Phytogeographic zone"),
  N          = c(nrow(sheep), nrow(sheep)),
  Groups     = c(2, nlevels(sheep$`Phytogeographic zones`)),
  H_statistic = c(unname(kw_veg$statistic), unname(kw_phyto$statistic)),
  df          = c(unname(kw_veg$parameter), unname(kw_phyto$parameter)),
  p_value     = c(kw_veg$p.value, kw_phyto$p.value),
  Decision    = if_else(c(kw_veg$p.value, kw_phyto$p.value) < 0.05,
                        "Reject H0", "Do not reject H0")
)

cat("\n===== Kruskal-Wallis summary =====\n")
print(kw_summary)

# =============================================================================
# 9. Pairwise post-hoc (only if the phytogeographic comparison is significant)
# =============================================================================
pairwise_df <- tibble()
if (kw_phyto$p.value < 0.05) {
  zone_levels <- levels(sheep$`Phytogeographic zones`)
  combos <- combn(zone_levels, 2, simplify = FALSE)
  
  pairwise_df <- map_dfr(combos, function(pair) {
    a <- pair[1]; b <- pair[2]
    ga <- sheep$total_herd_size[sheep$`Phytogeographic zones` == a]
    gb <- sheep$total_herd_size[sheep$`Phytogeographic zones` == b]
    test <- wilcox.test(ga, gb, exact = FALSE)
    tibble(Zone_A = a, Zone_B = b, n_A = length(ga), n_B = length(gb),
           W = unname(test$statistic), p_raw = test$p.value)
  })
  
  m <- nrow(pairwise_df)
  pairwise_df <- pairwise_df %>%
    arrange(p_raw) %>%
    mutate(
      p_BH         = p.adjust(p_raw, method = "BH"),
      p_Bonferroni = p.adjust(p_raw, method = "bonferroni")
    )
  
  cat("\n===== Pairwise post-hoc (phytogeographic zone) =====\n")
  print(pairwise_df)
} else {
  cat("\nPhytogeographic comparison not significant - no post-hoc tests run.\n")
}

# =============================================================================
# 10. Descriptive statistics
# =============================================================================
desc_veg <- sheep %>%
  group_by(`Vegetation zones`) %>%
  summarise(count = n(), mean = mean(total_herd_size), median = median(total_herd_size),
            std = sd(total_herd_size), min = min(total_herd_size), max = max(total_herd_size),
            .groups = "drop")

desc_phyto <- sheep %>%
  group_by(`Phytogeographic zones`) %>%
  summarise(count = n(), mean = mean(total_herd_size), median = median(total_herd_size),
            std = sd(total_herd_size), min = min(total_herd_size), max = max(total_herd_size),
            .groups = "drop")

cat("\n===== Descriptives: vegetation zone =====\n"); print(desc_veg)
cat("\n===== Descriptives: phytogeographic zone =====\n"); print(desc_phyto)

# =============================================================================
# 11. Farmer-level detail and zone composition summary
# =============================================================================
farmer_level <- sheep %>%
  select(`Vegetation zones`, `Phytogeographic zones`, all_of(COMMUNE_COL), all_of(VILLAGE_COL),
         total_herd_size, all_of(names(COMP_COLS))) %>%
  rename(Vegetation_zone = `Vegetation zones`, Phytogeographic_zone = `Phytogeographic zones`,
         Commune = all_of(COMMUNE_COL), Village = all_of(VILLAGE_COL),
         Total_herd_size = total_herd_size) %>%
  arrange(Phytogeographic_zone, Commune)

zone_summary <- sheep %>%
  group_by(`Phytogeographic zones`) %>%
  summarise(N_farmers = n(), across(all_of(names(COMP_COLS)), sum),
            Total_herd_size_sum = sum(total_herd_size), .groups = "drop") %>%
  rename(Phytogeographic_zone = `Phytogeographic zones`)

# =============================================================================
# 12. Method notes
# =============================================================================
method_notes <- tibble(
  Item = c("Population", "Outcome variable", "Test used",
           "Result: Vegetation zone", "Result: Phytogeographic zone",
           "Post-hoc", "Caveat", "Herd composition detail"),
  Detail = c(
    paste0(nrow(sheep), " farmers who selected \"Ovins\" only and NOT \"Caprins\" on the species ",
           "checkbox question (reconstructed from the raw checkboxes, not the unreliable pre-derived ",
           "species field). Training population (Abomey-Calavi + blank-commune records) excluded."),
    "Effectif total du troupeau (total herd size), self-reported by the farmer.",
    "Kruskal-Wallis H test (non-parametric; sheep herd sizes are strongly right-skewed).",
    sprintf("H(%d) = %.3f, p = %.4f -> %s", kw_veg$parameter, kw_veg$statistic, kw_veg$p.value,
            if_else(kw_veg$p.value < 0.05, "Reject H0", "Do not reject H0")),
    sprintf("H(%d) = %.3f, p = %.4f -> %s", kw_phyto$parameter, kw_phyto$statistic, kw_phyto$p.value,
            if_else(kw_phyto$p.value < 0.05, "Reject H0", "Do not reject H0")),
    "Pairwise Mann-Whitney with BH and Bonferroni correction, run only because the phytogeographic comparison was significant with more than 2 groups.",
    "Small zones (e.g. Plateau) have low statistical power; check for outliers pulling means up in any zone with a high standard deviation.",
    "Female >=6 months combines young and adult females - the questionnaire never asked for adult females separately."
  )
)

# =============================================================================
# 13. Export to Excel
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

add_sheet(wb, "Kruskal_Wallis_Summary", kw_summary)
if (nrow(pairwise_df) > 0) add_sheet(wb, "Posthoc_Phyto_zone_pairwise", pairwise_df)
add_sheet(wb, "Descriptives_Vegetation_zone", desc_veg)
add_sheet(wb, "Descriptives_Phyto_zone", desc_phyto)
add_sheet(wb, "Farmer_level_detail", farmer_level)
add_sheet(wb, "Zone_composition_summary", zone_summary)

addWorksheet(wb, "Method_notes")
writeData(wb, "Method_notes", method_notes, headerStyle = header_style)
addStyle(wb, "Method_notes", createStyle(fontName = "Arial", fontSize = 10, wrapText = TRUE, valign = "top"),
         rows = 2:(nrow(method_notes) + 1), cols = 1:2, gridExpand = TRUE)
setColWidths(wb, "Method_notes", cols = 1, widths = 28)
setColWidths(wb, "Method_notes", cols = 2, widths = 110)

saveWorkbook(wb, OUTPUT_PATH, overwrite = TRUE)
cat("\nSaved:", OUTPUT_PATH, "\n")