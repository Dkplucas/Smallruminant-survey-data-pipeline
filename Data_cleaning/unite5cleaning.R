#Cleaning of 6.) Caractéristiques des Chèvres Métissées (UE) 

# library needed
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)

#A. Import the data1 exported from the unite1cleaning.R to continue the cleaning
# Attention le dossier d'importation a changer veuiller a corriger en fontion de votre dossier de travail
getwd()
setwd("C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning")
xlsx_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning/data4.xlsx"
raw <- read_excel(xlsx_path, col_names = TRUE)
cat("Fichier charge :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
raw <- read_excel("data4.xlsx")

#C. Data cleaning
#1. NETTOYAGE COLONNE "Avez-vous observé des différences morphologiques..."
# Chercher la colonne
col_diff_morpho <- names(raw)[grep("avez-vous.*observé.*différences.*morphologiques|avez.*vous.*observe.*differences.*morphologiques", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_diff_morpho)) {
  cat("\nColonne trouvée:", col_diff_morpho, "\n")
  cat("Valeurs avant transformation :\n")
  print(table(raw[[col_diff_morpho]], useNA = "ifany"))
  
  col_name_new <- "6.) Caractéristiques des Chèvres Métissées/Avez-vous observé des différences morphologiques entre les chèvres métissées et la race locale Djallonké ?: 1=Oui, 0=Non, 3=Aucune idee"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_diff_morpho)) %>%
    mutate(!!col_name_new := as.character(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "^1=|oui") ~ "1",
      str_detect(str_to_lower(!!sym(col_name_new)), "aucune.*idée|aucune.*idee") ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "^0=|non") ~ "0",
      TRUE ~ !!sym(col_name_new)
    ))
  
  cat("✓ Colonne transformée\n")
  cat("Valeurs après transformation :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Avez-vous observé des différences morphologiques...' non trouvée\n")
}

#18.---- EXPORT DES DONNEES NETTOYEES ------------------------------------------------
cat("\n=== EXPORT DES DONNEES NETTOYEES ===\n")

# Exporter en Excel
output_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning/data5.xlsx"

# Essayer avec openxlsx (recommandé)
if (!require(openxlsx, quietly = TRUE)) {
  cat("Installation de openxlsx...\n")
  install.packages("openxlsx")
  library(openxlsx)
}

write.xlsx(raw, output_path, rowNames = FALSE, overwrite = TRUE)
cat("Fichier Excel sauvegardé :", output_path, "\n")
cat("Dimensions finales :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")

