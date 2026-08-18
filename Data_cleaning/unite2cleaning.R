# Cleaning of II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (OPTIMIZED)
# Reduced from 343 to ~130 lines by consolidating repetition

library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)

# ============================================================================
# CONFIGURATION
# ============================================================================

COLS_TO_DELETE <- list(
  list(pattern = "coordonnées|coordonnees", label = "Coordonnées géographiques"),
  list(pattern = "latitude", label = "Latitude"),
  list(pattern = "longitude", label = "Longitude"),
  list(pattern = "altitude", label = "Altitude"),
  list(pattern = "precision", label = "Precision"),
  list(pattern = "si.*oui.*nom.*structure|nom.*structure", label = "Si Oui, nom de la structure"),
  list(pattern = "appui.*technique", label = "Types d'appuis technique"),
  list(pattern = "services.*offerts.*technicien", label = "Services offerts par le technicien"),
  list(pattern = "degré.*satisfaction|degree.*satisfaction", label = "Degré de satisfaction"),
  list(pattern = "3=.*autres.*à.*préciser|3=.*autres.*a.*preciser|autres.*à.*préciser|autres.*a.*preciser|autres.*préciser.*à|autres.*preciser.*a", label = "3= Autres (à préciser)"),
  list(pattern = "préciser.*autre|preciser.*autre", label = "Préciser autre"),
  list(pattern = "4=autre.*\\(préciser\\)|4=autre.*\\(preciser\\)|4=.*autre|main.*oeuvre.*autre.*préciser|main.*œuvre.*autre.*préciser|main.*oeuvre.*autre.*preciser|main.*œuvre.*autre.*preciser|préciser.*autre.*47|preciser.*autre.*47", label = "4=autre (préciser)"),
  list(pattern = "système.*élevage.*autre.*préciser|systeme.*elevage.*autre.*préciser|système.*élevage.*autre.*preciser|systeme.*elevage.*autre.*preciser|5=.*autre.*préciser|5=.*autre.*preciser", label = "5=Autre (préciser)"),
  list(pattern = "autres.*espèces.*préciser|autres.*especes.*preciser|espèces.*autres.*préciser|especes.*autres.*preciser", label = "Autres espèces élevées à préciser"),
  list(pattern = "races.*troupeau.*autre.*préciser|races.*troupeau.*autre.*preciser|préciser.*autre.*68|preciser.*autre.*68", label = "Préciser autre...68")
)

COLS_TO_CLEAN <- list(
  list(pattern = "appui.*structure.*encadrement", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Votre Unité d'élevage bénéficie-t-elle de l'appui d'une structure d'encadrement ?: 1=Oui, 0=Non", codes = c("0" = "non", "1" = "oui")),
  list(pattern = "etes-vous.*suivi|suivi.*technicien", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Etes-vous suivi par un technicien ?: 1=Oui, 0=Non", codes = c("0" = "non", "1" = "oui"))
)

COLS_TO_RENAME_AND_FILL <- list(
  list(pattern = "appui.*financier", old_pattern_desc = "Appui financier", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Types d'appuis offerts par la structure /2= Appui financier/0=Non, 1=Oui"),
  list(pattern = "appui.*structure.*autres|structure.*autres.*appui", old_pattern_desc = "Autres (à préciser)", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Types d'appuis offerts par la structure /3= Autres (à préciser)/0=Non, 1=Oui"),
  list(pattern = "nature.*technicien.*affilié|affilié.*nature.*technicien", old_pattern_desc = "Affilié à la structure", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Si Oui, préciser nature du technicien /1. Affilié à la structure/0=Non, 1=Oui"),
  list(pattern = "clientèle.*privée|clientele.*privee|privée.*clientèle|privee.*clientele", old_pattern_desc = "En clientèle privée", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Si Oui, préciser nature du technicien /2. En clientèle privée/0=Non, 1=Oui"),
  list(pattern = "agent.*cellule|cellule.*communale", old_pattern_desc = "Agent de la cellule communale de la zone", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Si Oui, préciser nature du technicien /3. Agent de la cellule communale de la zone/0=Non, 1=Oui"),
  list(pattern = "nature.*technicien.*autre|autre.*nature.*technicien", old_pattern_desc = "Autre", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Si Oui, préciser nature du technicien /4. Autre/0=Non, 1=Oui"),
  list(pattern = "main.*oeuvre.*familiale|main.*œuvre.*familiale|familiale.*main", old_pattern_desc = "Familiale", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Type de main d'œuvre /1= Familiale/0=Non, 1=Oui"),
  list(pattern = "main.*oeuvre.*salariale|main.*œuvre.*salariale|salariale.*main", old_pattern_desc = "Salariale permanente", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Type de main d'œuvre /2= Salariale permanente/0=Non, 1=Oui"),
  list(pattern = "main.*oeuvre.*occasionnelle|main.*œuvre.*occasionnelle|occasionnelle.*main", old_pattern_desc = "Salariale occasionnelle", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Type de main d'œuvre /3= Salariale occasionnelle/0=Non, 1=Oui"),
  list(pattern = "main.*oeuvre.*autre|main.*œuvre.*autre|autre.*main.*oeuvre|autre.*main.*œuvre", old_pattern_desc = "Autre (main d'œuvre)", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Type de main d'œuvre /4=autre/0=Non, 1=Oui"),
  list(pattern = "système.*élevage.*extensif|systeme.*elevage.*extensif|extensif.*système.*élevage|extensif.*systeme.*elevage", old_pattern_desc = "Extensif traditionnel", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Type de système d'élevage pratiqué /Extensif traditionnel/0=Non, 1=Oui"),
  list(pattern = "élevage.*marché|elevage.*marche|marché.*élevage|marche.*elevage", old_pattern_desc = "Elevage orienté vers le marché", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Type de système d'élevage pratiqué /Elevage orienté vers le marché/0=Non, 1=Oui"),
  list(pattern = "embouche.*périurbain|embouche.*periurbain|périurbain.*embouche|periurbain.*embouche", old_pattern_desc = "Embouche en milieu périurbain", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Type de système d'élevage pratiqué /Embouche en milieu périurbain/0=Non, 1=Oui"),
  list(pattern = "système.*élevage.*autre|systeme.*elevage.*autre|autre.*système.*élevage.*pratiqué|autre.*systeme.*elevage.*pratique", old_pattern_desc = "Autre (système d'élevage)", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Type de système d'élevage pratiqué /Autre/0=Non, 1=Oui"),
  list(pattern = "espèces.*animales.*ovins|especes.*animales.*ovins|ovins.*espèces.*animales|ovins.*especes.*animales", old_pattern_desc = "Ovins", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Quelles sont les espèces animales que vous élevez?/1=Ovins/0=Non, 1=Oui"),
  list(pattern = "espèces.*animales.*caprins|especes.*animales.*caprins|caprins.*espèces.*animales|caprins.*especes.*animales", old_pattern_desc = "Caprins", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Quelles sont les espèces animales que vous élevez?/2=Caprins/0=Non, 1=Oui"),
  list(pattern = "autres.*espèces.*animales|autres.*especes.*animales|espèces.*animales.*autres|especes.*animales.*autres", old_pattern_desc = "Autres espèces", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Quelles sont les espèces animales que vous élevez?/Autres espèces/0=Non, 1=Oui"),
  list(pattern = "races.*troupeau.*djallonké|races.*troupeau.*djallonke|djallonké.*races.*troupeau|djallonke.*races.*troupeau", old_pattern_desc = "Djallonké", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Races présentes dans le troupeau /1=Djallonké/0=Non, 1=Oui"),
  list(pattern = "races.*troupeau.*sahélienne|races.*troupeau.*sahelienne|sahélienne.*races.*troupeau|sahelienne.*races.*troupeau", old_pattern_desc = "Sahélienne", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Races présentes dans le troupeau /2=Sahélienne/0=Non, 1=Oui"),
  list(pattern = "races.*troupeau.*métis|races.*troupeau.*metis|métis.*races.*troupeau|metis.*races.*troupeau", old_pattern_desc = "Métis", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Races présentes dans le troupeau /3=Métis/0=Non, 1=Oui"),
  list(pattern = "races.*troupeau.*autres|autres.*races.*troupeau", old_pattern_desc = "Autres (races)", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Races présentes dans le troupeau /4=Autres/0=Non, 1=Oui"),
  list(pattern = "djallonke.*sahelien|djallonké.*sahélienne", old_pattern_desc = "Djallonke X Sahelien", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Si présence des Métis dans le troupeau, préciser de quel type de croisement il (s) est/sont issu(s)/Djallonke X Sahelien/0=Non, 1=Oui"),
  list(pattern = "sahelien.*djallonke|sahélienne.*djallonké", old_pattern_desc = "Sahelien X Djallonke", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Si présence des Métis dans le troupeau, préciser de quel type de croisement il (s) est/sont issu(s)/Sahelien X Djallonke/0=Non, 1=Oui"),
  list(pattern = "sahelien.*metis|sahélienne.*métis|sahelien.*métis|sahélienne.*metis", old_pattern_desc = "Sahelien X Metis", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Si présence des Métis dans le troupeau, préciser de quel type de croisement il (s) est/sont issu(s)/Sahelien X Metis/0=Non, 1=Oui"),
  list(pattern = "croisement.*metis.*sahelien|croisement.*métis.*sahélienne|croisement.*metis.*sahélienne|croisement.*métis.*sahelien", old_pattern_desc = "Metis X Sahelien", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Si présence des Métis dans le troupeau, préciser de quel type de croisement il (s) est/sont issu(s)/Metis X Sahelien/0=Non, 1=Oui"),
  list(pattern = "croisement.*metis.*djallonke|croisement.*métis.*djallonké|croisement.*metis.*djallonké|croisement.*métis.*djallonke", old_pattern_desc = "Metis X Djallonke", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Si présence des Métis dans le troupeau, préciser de quel type de croisement il (s) est/sont issu(s)/Metis X Djallonke/0=Non, 1=Oui"),
  list(pattern = "croisement.*metis.*metis|croisement.*métis.*métis|croisement.*metis.*métis|croisement.*métis.*metis", old_pattern_desc = "Metis X Metis", new_name = "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Si présence des Métis dans le troupeau, préciser de quel type de croisement il (s) est/sont issu(s)/Metis X Metis/0=Non, 1=Oui")
)

COLS_TO_FILL <- list(
  list(pattern = "type.*main|main.*oeuvre|main.*œuvre", label = "Type de main d'œuvre"),
  list(pattern = "type.*système|type.*systeme|système.*élevage|systeme.*elevage", label = "Type de système d'élevage"),
  list(pattern = "espèces.*animales|especes.*animales", label = "Espèces animales"),
  list(pattern = "races.*présentes|races.*presentes|races.*troupeau", label = "Races présentes")
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

log_status <- function(msg, data = NULL) {
  cat(msg, "\n")
  if (!is.null(data)) print(data)
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

  raw %>%
    rename(!!new_name := all_of(col)) %>%
    mutate(!!new_name := as.character(!!sym(new_name))) %>%
    mutate(!!new_name := str_trim(!!sym(new_name))) %>%
    mutate(!!new_name := case_when(
      is.na(!!sym(new_name)) | !!sym(new_name) == "" ~ "0",
      str_detect(str_to_lower(!!sym(new_name)), "^0=|non") ~ "0",
      str_detect(str_to_lower(!!sym(new_name)), "^1=|oui") ~ "1",
      TRUE ~ !!sym(new_name)
    ))
}

fill_empty_group <- function(raw, pattern, label) {
  cols <- find_cols(raw, pattern)
  if (length(cols) == 0) {
    cat("⚠", label, "- aucune colonne trouvée\n")
    return(raw)
  }

  cat("✓", label, "-", length(cols), "colonne(s) remplies\n")
  raw %>%
    mutate(across(all_of(cols),
      ~case_when(is.na(.) | . == "" | . == "NA" ~ "0", TRUE ~ as.character(.))))
}

rename_and_fill_col <- function(raw, pattern, new_name) {
  col <- find_col(raw, pattern)
  if (is.na(col)) return(raw)

  raw %>%
    rename(!!new_name := all_of(col)) %>%
    mutate(!!new_name := as.character(!!sym(new_name))) %>%
    mutate(!!new_name := case_when(
      is.na(!!sym(new_name)) | !!sym(new_name) == "" ~ "0",
      TRUE ~ !!sym(new_name)
    ))
}

# ============================================================================
# WORKFLOW
# ============================================================================

cat("=== CLEANING II: LIVESTOCK UNIT CHARACTERISTICS ===\n\n")

# Load data
xlsx_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning/data1.xlsx"
raw <- read_excel(xlsx_path, col_names = TRUE)
cat("Loaded:", nrow(raw), "rows ×", ncol(raw), "columns\n\n")

# Delete columns
cat("--- Deleting columns ---\n")
for (spec in COLS_TO_DELETE) {
  raw <- delete_column(raw, spec$pattern, spec$label)
}
cat("Dimensions:", nrow(raw), "rows ×", ncol(raw), "columns\n\n")

# Clean Yes/No columns
cat("--- Cleaning Yes/No columns ---\n")
for (spec in COLS_TO_CLEAN) {
  raw <- clean_yesno_col(raw, spec$pattern, spec$new_name)
  cat("✓", gsub(".*/", "", spec$new_name), "\n")
}

# Rename and fill columns
cat("\n--- Renaming and filling columns ---\n")
for (spec in COLS_TO_RENAME_AND_FILL) {
  raw <- rename_and_fill_col(raw, spec$pattern, spec$new_name)
  cat("✓", spec$old_pattern_desc, "renommée et remplie\n")
}

# Fill empty cells in multi-select groups
cat("\n--- Filling empty cells in multi-select groups ---\n")
for (spec in COLS_TO_FILL) {
  raw <- fill_empty_group(raw, spec$pattern, spec$label)
}

# Export
cat("\n=== EXPORT ===\n")
output_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning/data2.xlsx"
if (!require(openxlsx, quietly = TRUE)) {
  install.packages("openxlsx")
  library(openxlsx)
}
write.xlsx(raw, output_path, rowNames = FALSE, overwrite = TRUE)
cat("✓ Saved to:", output_path, "\n")
cat("Final dimensions:", nrow(raw), "rows ×", ncol(raw), "columns\n")
