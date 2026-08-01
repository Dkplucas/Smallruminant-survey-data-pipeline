#Cleaning of III- 3.) Logement, matériel et équipements (UE) 

# library needed
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)

#A. Import the data1 exported from the unite1cleaning.R to continue the cleaning
xlsx_path <- "c:/Users/lucas/Downloads/data2.xlsx"
raw <- read_excel(xlsx_path, col_names = TRUE)
cat("Fichier charge :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
raw <- read_excel("data2.xlsx")

#C. Data cleaning

#1. NETTOYAGE COLONNE "Avez-vous un abri où vous gardez les animaux toutes les saisons ?"
# Chercher la colonne
col_abri <- names(raw)[grep("abri.*animaux.*saisons", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_abri)) {
  cat("\nColonne trouvée:", col_abri, "\n")
  cat("Valeurs avant transformation :\n")
  print(table(raw[[col_abri]], useNA = "ifany"))
  
  col_name_new <- "3.) Logement, matériel et équipements/Avez-vous un abri où vous gardez les animaux toutes les saisons ?: 1=Oui, 0=Non"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_abri)) %>%
    mutate(!!col_name_new := as.character(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "^0=|non") ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "^1=|oui") ~ "1",
      TRUE ~ !!sym(col_name_new)
    ))
  
  cat("✓ Colonne transformée\n")
  cat("Valeurs après transformation :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Avez-vous un abri où vous gardez les animaux toutes les saisons ?' non trouvée\n")
}

#2.NETTOYAGE COLONNE "Est-ce compartimenté pour permettre un allotement"
# Chercher la colonne
col_compartimente <- names(raw)[grep("compartimenté|compartimente|allotement", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_compartimente)) {
  cat("\nColonne trouvée:", col_compartimente, "\n")
  cat("Valeurs avant transformation :\n")
  print(table(raw[[col_compartimente]], useNA = "ifany"))
  
  col_name_new <- "3.) Logement, matériel et équipements/Si oui, est-ce compartimenté pour permettre un allotement des animaux de votre élevage ?: 1=Oui, 0=Non"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_compartimente)) %>%
    mutate(!!col_name_new := as.character(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "^0=|non") ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "^1=|oui") ~ "1",
      TRUE ~ !!sym(col_name_new)
    ))
  
  cat("✓ Colonne transformée\n")
  cat("Valeurs après transformation :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Est-ce compartimenté pour permettre un allotement' non trouvée\n")
}

#3.SUPPRESSION COLONNE "Décrire brievement" 
# Chercher et supprimer la colonne
col_decrire <- names(raw)[grep("décrire.*brievement|decrire.*brievement", names(raw), ignore.case = TRUE)][1]

cat("\nSuppression de la colonne 'Décrire brievement'...\n")

if (!is.na(col_decrire)) {
  cat("Colonne trouvée:", col_decrire, "\n")
  raw <- raw %>% select(-all_of(col_decrire))
  cat("✓ Colonne supprimée avec succès\n")
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("⚠ Colonne 'Décrire brievement' non trouvée\n")
}

#4.NETTOYAGE COLONNE "Y-a-t-il de mangeoires dans l'habitat des animaux ?"
# Chercher la colonne
col_mangeoires <- names(raw)[grep("mangeoires|mangeoires", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_mangeoires)) {
  cat("\nColonne trouvée:", col_mangeoires, "\n")
  cat("Valeurs avant transformation :\n")
  print(table(raw[[col_mangeoires]], useNA = "ifany"))
  
  col_name_new <- "3.) Logement, matériel et équipements/Y-a-t-il de mangeoires dans l'habitat des animaux ?: 1=Oui, 0=Non"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_mangeoires)) %>%
    mutate(!!col_name_new := as.character(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "^0=|non") ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "^1=|oui") ~ "1",
      TRUE ~ !!sym(col_name_new)
    ))
  
  cat("✓ Colonne transformée\n")
  cat("Valeurs après transformation :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Y-a-t-il de mangeoires dans l'habitat des animaux ?' non trouvée\n")
}

#5.NETTOYAGE COLONNE "Y-a-t-il d'abreuvoirs dans l'habitat des animaux"
# Chercher la colonne
col_abreuvoirs <- names(raw)[grep("abreuvoirs", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_abreuvoirs)) {
  cat("\nColonne trouvée:", col_abreuvoirs, "\n")
  cat("Valeurs avant transformation :\n")
  print(table(raw[[col_abreuvoirs]], useNA = "ifany"))
  
  col_name_new <- "3.) Logement, matériel et équipements/Y-a-t-il d'abreuvoirs dans l'habitat des animaux: 1=Oui, 0=Non"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_abreuvoirs)) %>%
    mutate(!!col_name_new := as.character(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "^0=|non") ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "^1=|oui") ~ "1",
      TRUE ~ !!sym(col_name_new)
    ))
  
  cat("✓ Colonne transformée\n")
  cat("Valeurs après transformation :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Y-a-t-il d'abreuvoirs dans l'habitat des animaux' non trouvée\n")
}

#6. NETTOYAGE COLONNE "Nettoyez-vous régulièrement le logement et les équipements d'élevage ?" 
# Chercher la colonne
col_nettoyage <- names(raw)[grep("nettoyez.*régulièrement|nettoyez.*regulierement", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_nettoyage)) {
  cat("\nColonne trouvée:", col_nettoyage, "\n")
  cat("Valeurs avant transformation :\n")
  print(table(raw[[col_nettoyage]], useNA = "ifany"))
  
  col_name_new <- "3.) Logement, matériel et équipements/Nettoyez-vous régulièrement le logement et les équipements d'élevage ?: 1=Oui, 0=Non"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_nettoyage)) %>%
    mutate(!!col_name_new := as.character(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "^0=|non") ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "^1=|oui") ~ "1",
      TRUE ~ !!sym(col_name_new)
    ))
  
  cat("✓ Colonne transformée\n")
  cat("Valeurs après transformation :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Nettoyez-vous régulièrement le logement et les équipements d'élevage ?' non trouvée\n")
}

#7. Enreigistrement des donnees
cat("\n=== EXPORT DES DONNEES NETTOYEES ===\n")

# Exporter en Excel
output_path <- "c:/Users/lucas/Downloads/data3.xlsx"

# Essayer avec openxlsx (recommandé)
if (!require(openxlsx, quietly = TRUE)) {
  cat("Installation de openxlsx...\n")
  install.packages("openxlsx")
  library(openxlsx)
}

write.xlsx(raw, output_path, rowNames = FALSE, overwrite = TRUE)
cat("Fichier Excel sauvegardé :", output_path, "\n")
cat("Dimensions finales :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")