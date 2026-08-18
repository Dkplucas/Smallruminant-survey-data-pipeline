# Cleaning of III- 3.) Logement, matériel et équipements (UE)

library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)

# ============================================================================
# CONFIGURATION
# ============================================================================

COLS_TO_DELETE <- list(
  list(pattern = "décrire.*brievement|decrire.*brievement", label = "Décrire brievement")
)

COLS_TO_CLEAN <- list(
  list(pattern = "abri.*animaux.*saisons", new_name = "3.) Logement, matériel et équipements/Avez-vous un abri où vous gardez les animaux toutes les saisons ?: 1=Oui, 0=Non"),
  list(pattern = "compartimenté|compartimente|allotement", new_name = "3.) Logement, matériel et équipements/Si oui, est-ce compartimenté pour permettre un allotement des animaux de votre élevage ?: 1=Oui, 0=Non"),
  list(pattern = "mangeoires", new_name = "3.) Logement, matériel et équipements/Y-a-t-il de mangeoires dans l'habitat des animaux ?: 1=Oui, 0=Non"),
  list(pattern = "abreuvoirs", new_name = "3.) Logement, matériel et équipements/Y-a-t-il d'abreuvoirs dans l'habitat des animaux: 1=Oui, 0=Non"),
  list(pattern = "nettoyez.*régulièrement|nettoyez.*regulierement", new_name = "3.) Logement, matériel et équipements/Nettoyez-vous régulièrement le logement et les équipements d'élevage ?: 1=Oui, 0=Non")
)

# ============================================================================
# HELPERS
# ============================================================================

find_col <- function(df, pattern) {
  m <- names(df)[grep(pattern, names(df), ignore.case = TRUE)]
  if (length(m) == 0) NA else m[1]
}

find_cols <- function(df, pattern) {
  names(df)[grep(pattern, names(df), ignore.case = TRUE)]
}

delete_column <- function(raw, pattern, label) {
  cols <- find_cols(raw, pattern)
  if (length(cols) > 0) {
    cat("✓", label, "-", length(cols), "colonne(s) supprimée(s)\n")
    raw %>% select(-all_of(cols))
  } else {
    cat("⚠", label, "non trouvée\n")
    raw
  }
}

clean_yesno_col <- function(raw, pattern, new_name) {
  col <- find_col(raw, pattern)
  if (is.na(col)) return(raw)

  cat("\nColonne trouvée:", col, "\n")
  cat("Valeurs avant transformation :\n")
  print(table(raw[[col]], useNA = "ifany"))

  raw %>%
    rename(!!new_name := all_of(col)) %>%
    mutate(!!new_name := as.character(!!sym(new_name))) %>%
    mutate(!!new_name := str_trim(!!sym(new_name))) %>%
    mutate(!!new_name := case_when(
      is.na(!!sym(new_name)) | !!sym(new_name) == "" ~ "0",
      str_detect(str_to_lower(!!sym(new_name)), "^0=|non") ~ "0",
      str_detect(str_to_lower(!!sym(new_name)), "^1=|oui") ~ "1",
      TRUE ~ !!sym(new_name)
    )) %>%
    {
      cat("✓ Colonne transformée\n")
      cat("Valeurs après transformation :\n")
      print(table(.[[new_name]], useNA = "ifany"))
      .
    }
}

# ============================================================================
# WORKFLOW
# ============================================================================

cat("=== CLEANING III: LOGEMENT, MATERIEL ET EQUIPEMENTS ===\n\n")

# Load data
xlsx_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning/data2.xlsx"
raw <- read_excel(xlsx_path, col_names = TRUE)
cat("Fichier chargé :", nrow(raw), "lignes x", ncol(raw), "colonnes\n\n")

# Delete columns
cat("--- Suppression de colonnes ---\n")
for (spec in COLS_TO_DELETE) {
  raw <- delete_column(raw, spec$pattern, spec$label)
}
cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n\n")

# Clean yes/no columns
cat("--- Nettoyage des colonnes Oui/Non ---\n")
for (spec in COLS_TO_CLEAN) {
  raw <- clean_yesno_col(raw, spec$pattern, spec$new_name)
}

# Export
cat("\n=== EXPORT DES DONNEES NETTOYEES ===\n")
output_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning/data3.xlsx"
if (!require(openxlsx, quietly = TRUE)) {
  install.packages("openxlsx")
  library(openxlsx)
}
write.xlsx(raw, output_path, rowNames = FALSE, overwrite = TRUE)
cat("✓ Fichier sauvegardé :", output_path, "\n")
cat("Dimensions finales :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
