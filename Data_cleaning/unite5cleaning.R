#Cleaning of 6.) Caractéristiques des Chèvres Métissées and 7.)####### (UE) 

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

#D. Data cleaning
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

#2. NETTOYAGE COLONNE "Si oui quelle différence: La Taille des metis..."
# Chercher la colonne
col_taille_metis <- names(raw)[grep("si.*oui.*quelle.*différence.*taille|si.*oui.*quelle.*difference.*taille", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_taille_metis)) {
  cat("\nColonne trouvée:", col_taille_metis, "\n")
  cat("Valeurs avant transformation :\n")
  print(table(raw[[col_taille_metis]], useNA = "ifany"))
  
  col_name_new <- "6.) Caractéristiques des Chèvres Métissées/Si oui quelle différence: La Taille des metis est ..... que celle des Djallonke: 1: Plus Grande, 2: Vide"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_taille_metis)) %>%
    mutate(!!col_name_new := as.character(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "plus.*grande") ~ "1",
      TRUE ~ !!sym(col_name_new)
    ))
  
  cat("✓ Colonne transformée\n")
  cat("Valeurs après transformation :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Si oui quelle différence: La Taille des metis...' non trouvée\n")
}

#3. NETTOYAGE COLONNE "Si oui quelle différence: Les metis sont...." 
# Chercher la colonne
col_poids_metis <- names(raw)[grep("si.*oui.*quelle.*différence.*les.*metis|si.*oui.*quelle.*difference.*les.*metis", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_poids_metis)) {
  cat("\nColonne trouvée:", col_poids_metis, "\n")
  cat("Valeurs avant transformation :\n")
  print(table(raw[[col_poids_metis]], useNA = "ifany"))
  
  col_name_new <- "6.) Caractéristiques des Chèvres Métissées/Si oui quelle différence: Les metis sont.... que ou aux Djallonké: 0=Vide, 1:Plus leger, 2:Plus lourd, 3:Similaire"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_poids_metis)) %>%
    mutate(!!col_name_new := as.character(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "plus.*léger|plus.*leger") ~ "1",
      str_detect(str_to_lower(!!sym(col_name_new)), "plus.*lourd") ~ "2",
      str_detect(str_to_lower(!!sym(col_name_new)), "similaire") ~ "3",
      TRUE ~ !!sym(col_name_new)
    ))
  
  cat("✓ Colonne transformée\n")
  cat("Valeurs après transformation :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Si oui quelle différence: Les metis sont...' non trouvée\n")
}

#4. REMPLISSAGE CELLULES VIDES "Autres traits de distinction entre metis et Djallonke /traits"
# Chercher toutes les colonnes contenant "autres traits de distinction"
col_traits_distinction <- names(raw)[grep("autres.*traits.*distinction|traits.*distinction.*metis", names(raw), ignore.case = TRUE)]

if (length(col_traits_distinction) > 0) {
  cat("\nColonnes trouvées:", length(col_traits_distinction), "\n")
  print(col_traits_distinction)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_traits_distinction) {
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
  cat("\n⚠ Aucune colonne 'Autres traits de distinction entre metis et Djallonke' trouvée\n")
}

#5. REMPLISSAGE CELLULES VIDES "Croissance /Meilleure que la race Djallonke"
# Chercher toutes les colonnes contenant "croissance" dans la section 7
col_croissance <- names(raw)[grep("7\\.).*croissance|Performances.*metis.*croissance", names(raw), ignore.case = TRUE)]

if (length(col_croissance) > 0) {
  cat("\nColonnes trouvées:", length(col_croissance), "\n")
  print(col_croissance)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_croissance) {
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
  cat("\n⚠ Aucune colonne 'Croissance' trouvée\n")
}

#6. REMPLISSAGE CELLULES VIDES "Croissance /Similaire que la race Djallonke"
# Chercher toutes les colonnes contenant "croissance" et "similaire" dans la section 7
col_croissance_similaire <- names(raw)[grep("7\\.).*croissance.*similaire|Performances.*metis.*croissance.*similaire", names(raw), ignore.case = TRUE)]

if (length(col_croissance_similaire) > 0) {
  cat("\nColonnes trouvées:", length(col_croissance_similaire), "\n")
  print(col_croissance_similaire)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_croissance_similaire) {
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
  cat("\n⚠ Aucune colonne 'Croissance /Similaire' trouvée\n")
}

#7. REMPLISSAGE CELLULES VIDES "Croissance /Moins bonne que la race Djallonke"
# Chercher toutes les colonnes contenant "croissance" et "moins" dans la section 7
col_croissance_moins <- names(raw)[grep("7\\.).*croissance.*moins|Performances.*metis.*croissance.*moins", names(raw), ignore.case = TRUE)]

if (length(col_croissance_moins) > 0) {
  cat("\nColonnes trouvées:", length(col_croissance_moins), "\n")
  print(col_croissance_moins)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_croissance_moins) {
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
  cat("\n⚠ Aucune colonne 'Croissance /Moins bonne' trouvée\n")
}

#8. REMPLISSAGE CELLULES VIDES "Résistance aux maladies /Meilleure que la race Djallonke"
# Chercher toutes les colonnes contenant "résistance aux maladies" dans la section 7
col_resistance_maladies <- names(raw)[grep("7\\.).*résistance.*maladies|Performances.*metis.*résistance|7\\.).*resistance.*maladies|Performances.*metis.*resistance", names(raw), ignore.case = TRUE)]

if (length(col_resistance_maladies) > 0) {
  cat("\nColonnes trouvées:", length(col_resistance_maladies), "\n")
  print(col_resistance_maladies)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_resistance_maladies) {
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
  cat("\n⚠ Aucune colonne 'Résistance aux maladies' trouvée\n")
}

#9. REMPLISSAGE CELLULES VIDES "Résistance aux maladies /Similaire que la race Djallonke"
# Chercher toutes les colonnes contenant "résistance aux maladies" et "similaire" dans la section 7
col_resistance_similaire <- names(raw)[grep("7\\.).*résistance.*maladies.*similaire|Performances.*metis.*résistance.*similaire|7\\.).*resistance.*maladies.*similaire|Performances.*metis.*resistance.*similaire", names(raw), ignore.case = TRUE)]

if (length(col_resistance_similaire) > 0) {
  cat("\nColonnes trouvées:", length(col_resistance_similaire), "\n")
  print(col_resistance_similaire)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_resistance_similaire) {
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
  cat("\n⚠ Aucune colonne 'Résistance aux maladies /Similaire' trouvée\n")
}

#10. REMPLISSAGE CELLULES VIDES "Résistance aux maladies /Moins bonne que la race Djallonke"
# Chercher toutes les colonnes contenant "résistance aux maladies" et "moins" dans la section 7
col_resistance_moins <- names(raw)[grep("7\\.).*résistance.*maladies.*moins|Performances.*metis.*résistance.*moins|7\\.).*resistance.*maladies.*moins|Performances.*metis.*resistance.*moins", names(raw), ignore.case = TRUE)]

if (length(col_resistance_moins) > 0) {
  cat("\nColonnes trouvées:", length(col_resistance_moins), "\n")
  print(col_resistance_moins)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_resistance_moins) {
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
  cat("\n⚠ Aucune colonne 'Résistance aux maladies /Moins bonne' trouvée\n")
}

#11. REMPLISSAGE CELLULES VIDES "Prolificité (nombre de chevreaux par portée)"
# Chercher toutes les colonnes contenant "prolificité" ou "nombre de chevreaux"
col_prolificite <- names(raw)[grep("prolificité|prolificite|nombre.*chevreaux.*portée|nombre.*chevreaux.*portee", names(raw), ignore.case = TRUE)]

if (length(col_prolificite) > 0) {
  cat("\nColonnes trouvées:", length(col_prolificite), "\n")
  print(col_prolificite)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_prolificite) {
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
  cat("\n⚠ Aucune colonne 'Prolificité' trouvée\n")
}


#12.---- EXPORT DES DONNEES NETTOYEES ------------------------------------------------
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

