# =============================================================================
# Ovins-only (sheep) herd composition by breed, sex, age and zone
# =============================================================================
# What this script does:
#   1. Reads the raw survey export (labels version) and the zones lookup table
#   2. Excludes the training population (Abomey-Calavi + any blank-commune rows)
#   3. Reconstructs the TRUE species classification from the raw Ovins/Caprins
#      checkboxes (NOT the pre-derived "Espece de petit ruminants" field, which
#      was found to be miscoded)
#   4. Keeps only "Ovins only" households (Ovins=1 & Caprins=0) -> 67 farmers
#   5. Normalizes commune names and joins to the phytogeographic zone lookup
#   6. Builds the herd composition table: head counts by breed x sex/age tier,
#      per phytogeographic zone, plus breed totals per zone
#   7. Exports a farmer-level detail sheet and the zone-level summary to Excel
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
OUTPUT_PATH <- "sheep_herd_composition_by_zone.xlsx"

# ---- Column name constants (exact headers from the raw export) ------------
COMMUNE_COL <- "II- CARACTERISTIQUES DE L\u2019UNITE D\u2019ELEVAGE (UE) /Commune"
VILLAGE_COL <- "II- CARACTERISTIQUES DE L\u2019UNITE D\u2019ELEVAGE (UE) /Village/Localit\u00e9 : "
OVINS_COL   <- "II- CARACTERISTIQUES DE L\u2019UNITE D\u2019ELEVAGE (UE) /Quelles sont les esp\u00e8ces animales que vous \u00e9levez?/1=Ovins"
CAPRINS_COL <- "II- CARACTERISTIQUES DE L\u2019UNITE D\u2019ELEVAGE (UE) /Quelles sont les esp\u00e8ces animales que vous \u00e9levez?/2=Caprins"
TOTAL_COL   <- "12.) Effectif et composition du cheptel/Effectif total du troupeau"

# Herd composition (breed x sex x age) columns, labeled to match the table
# presented earlier ("Adult male - Djallonke", "Female >=6mo - Metis", etc.)
# NOTE: these breed/sex/age columns are generic to "petits ruminants" and are
# NOT species-tagged per row - that is exactly why mixed-species households
# had to be excluded earlier. Restricting to Ovins-only households first is
# what makes these counts unambiguously about sheep.
COMP_COLS <- c(
  "Adult male_Djallonke"        = "12.) Effectif et composition du cheptel/Nombre de m\u00e2le adultes Djallonke",
  "Adult male_Sahelien"         = "12.) Effectif et composition du cheptel/Nombre de m\u00e2le adultes Sahelien",
  "Adult male_Metis"            = "12.) Effectif et composition du cheptel/Nombre de m\u00e2le adultes Metis",
  "Adult male_Autre"            = "12.) Effectif et composition du cheptel/Nombre de m\u00e2le adultes Autre____",
  "Young male >=6mo_Djallonke"  = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) m\u00e2les_Djallonke",
  "Young male >=6mo_Sahelien"   = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) m\u00e2les_Sahelien",
  "Young male >=6mo_Metis"      = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) m\u00e2les_Metis",
  "Young male >=6mo_Autre"      = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) m\u00e2les_Autre",
  "Female >=6mo_Djallonke"      = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Djallonke",
  "Female >=6mo_Sahelien"       = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femellesSahelienne",
  "Female >=6mo_Metis"          = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Metis",
  "Female >=6mo_Autre"          = "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Autre",
  "Baby male <6mo_Djallonke"    = "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) m\u00e2le_Djallonke",
  "Baby male <6mo_Sahelien"     = "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) m\u00e2le_Sahelien",
  "Baby male <6mo_Metis"        = "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) m\u00e2le_Metis",
  "Baby female <6mo_Djallonke"  = "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Djallonke",
  "Baby female <6mo_Sahelien"   = "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Sahelien",
  "Baby female <6mo_Metis"      = "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Metis"
)

# NOTE: "Female >=6mo" combines young and adult females. The questionnaire
# only asks for adult MALES separately from young males; females are only
# ever split into ">=6 months" and "<6 months", with no separate adult-female
# field anywhere in the source data (confirmed against the original
# questionnaire template).

# ---- Helper: normalize a commune/district name into a matching key --------
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
# 4. Keep Ovins-only households (expected: 67 farmers)
# =============================================================================
sheep <- raw_clean %>% filter(species == "Ovins only")
cat("\nOvins-only farmers:", nrow(sheep), "\n")

# =============================================================================
# 5. Normalize commune and join to the phytogeographic zone lookup
# =============================================================================
zones_valid <- zones %>%
  fill(`Vegetation zones`, `Phytogeographic zones`) %>%
  filter(!is.na(District), District != "") %>%
  mutate(key = normalize_key(District))

sheep <- sheep %>%
  mutate(key = normalize_key(.data[[COMMUNE_COL]])) %>%
  left_join(zones_valid %>% select(key, `Vegetation zones`, `Phytogeographic zones`),
            by = "key")

n_unmatched <- sum(is.na(sheep$`Phytogeographic zones`))
cat("Unmatched to a zone:", n_unmatched, "(should be 0)\n")
if (n_unmatched > 0) {
  cat("Unmatched communes:\n")
  print(sheep %>% filter(is.na(`Phytogeographic zones`)) %>% pull(.data[[COMMUNE_COL]]) %>% unique())
}

# =============================================================================
# 6. Clean the composition columns and total herd size
# =============================================================================
sheep <- sheep %>%
  mutate(Total_herd_size = as.numeric(.data[[TOTAL_COL]])) %>%
  mutate(across(all_of(COMP_COLS), ~ replace_na(as.numeric(.x), 0), .names = "{.col}")) %>%
  rename(!!!COMP_COLS)
  # COMP_COLS is a named vector: names = short labels (e.g. "Adult male_Djallonke"),
  # values = the real long column headers from the raw export. This rename()
  # call is what lets every later step below refer to the short names.

zone_order <- c("Vallee de l\u2019Oueme (VOZ)", "Plateau", "Borgou-Sud", "Zou")
sheep <- sheep %>% mutate(`Phytogeographic zones` = factor(`Phytogeographic zones`, levels = zone_order))

# =============================================================================
# 7. Zone-level composition summary (head counts, summed within each zone)
# =============================================================================
zone_summary <- sheep %>%
  group_by(`Phytogeographic zones`) %>%
  summarise(N_farmers = n(),
            across(all_of(names(COMP_COLS)), sum),
            Reported_total_herd_size = sum(Total_herd_size),
            .groups = "drop") %>%
  arrange(match(`Phytogeographic zones`, zone_order))

# ---- Breed totals per zone (all sexes/ages combined) -----------------------
breed_totals <- zone_summary %>%
  transmute(
    `Phytogeographic zones`,
    TOTAL_Djallonke = rowSums(select(zone_summary, ends_with("Djallonke"))),
    TOTAL_Sahelien   = rowSums(select(zone_summary, ends_with("Sahelien"))),
    TOTAL_Metis      = rowSums(select(zone_summary, ends_with("Metis"))),
    TOTAL_Autre      = rowSums(select(zone_summary, ends_with("Autre")))
  )

cat("\n===== Farmer counts by zone =====\n")
print(zone_summary %>% select(`Phytogeographic zones`, N_farmers))

cat("\n===== Breed totals by zone =====\n")
print(breed_totals)

# =============================================================================
# 8. Farmer-level detail (one row per farmer)
# =============================================================================
farmer_level <- sheep %>%
  select(`Vegetation zones`, `Phytogeographic zones`, all_of(COMMUNE_COL), all_of(VILLAGE_COL),
         Total_herd_size, all_of(names(COMP_COLS))) %>%
  rename(Vegetation_zone = `Vegetation zones`,
         Phytogeographic_zone = `Phytogeographic zones`,
         Commune = all_of(COMMUNE_COL),
         Village = all_of(VILLAGE_COL)) %>%
  arrange(Phytogeographic_zone, Commune)

# =============================================================================
# 9. Data-quality check: does the reported total match the sum of components?
# =============================================================================
sheep <- sheep %>%
  mutate(sum_components = rowSums(across(all_of(names(COMP_COLS)))),
         gap = Total_herd_size - sum_components)

n_mismatch <- sum(sheep$gap != 0)
cat(sprintf("\nFarmers where reported total != sum of breed/sex/age components: %d of %d\n",
            n_mismatch, nrow(sheep)))
if (n_mismatch > 0) {
  print(sheep %>% filter(gap != 0) %>%
          select(all_of(COMMUNE_COL), Total_herd_size, sum_components, gap))
}

# =============================================================================
# 10. Export to Excel
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

add_sheet(wb, "Zone_composition_summary", zone_summary)
add_sheet(wb, "Breed_totals_by_zone", breed_totals)
add_sheet(wb, "Farmer_level_detail", farmer_level)
add_sheet(wb, "Data_quality_gap_check", sheep %>%
            select(all_of(COMMUNE_COL), `Phytogeographic zones`, Total_herd_size, sum_components, gap) %>%
            rename(Commune = all_of(COMMUNE_COL)))

saveWorkbook(wb, OUTPUT_PATH, overwrite = TRUE)
cat("\nSaved:", OUTPUT_PATH, "\n")
