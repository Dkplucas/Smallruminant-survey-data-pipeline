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
#1. REMPLISSAGE CELLULES VIDES "Avec quelles races de chèvre avez-vous commencé votre élevage de caprins ?"
# Chercher toutes les colonnes contenant "quelles races de chèvre avez-vous commencé"
col_races_depart <- names(raw)[grep("quelles.*races.*chèvre.*commencé|quelles.*races.*chevre.*commence", names(raw), ignore.case = TRUE)]

if (length(col_races_depart) > 0) {
  cat("\nColonnes trouvées:", length(col_races_depart), "\n")
  print(col_races_depart)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_races_depart) {
    cat("\nTraitement de la colonne:", col, "\n")
    cat("Valeurs avant remplissage :\n")
    print(table(raw[[col]], useNA = "ifany"))
    
    raw <- raw %>%
      mutate(!!col := as.character(!!sym(col))) %>%
      mutate(!!col := case_when(
        is.na(!!sym(col)) | !!sym(col) == "" | !!sym(col) == "NA" ~ "0",
        TRUE ~ !!sym(col)
      ))
    
    cat("✓ Cellules vides remplacées par '0'\n")
    cat("Valeurs après remplissage :\n")
    print(table(raw[[col]], useNA = "ifany"))
  }
} else {
  cat("\n⚠ Aucune colonne 'Avec quelles races de chèvre avez-vous commencé votre élevage de caprins ?' trouvée\n")
}
