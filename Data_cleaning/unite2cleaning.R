#Cleaning of II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) 

# library needed
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)

#A. Import the data1 exported from the unite1cleaning.R to continue the cleaning
xlsx_path <- "c:/Users/lucas/Downloads/data1.xlsx"
raw <- read_excel(xlsx_path, col_names = TRUE)
cat("Fichier charge :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
raw <- read_excel("data1.xlsx")

#B. Data cleaning
#1. SUPPRESSION COLONNE "Coordonnées géographiques :
# Chercher et supprimer la colonne
col_coord <- names(raw)[grep("coordonnées|coordonnees", names(raw), ignore.case = TRUE)][1]

cat("\nSuppression de la colonne 'Coordonnées géographiques :'...\n")

if (!is.na(col_coord)) {
  cat("Colonne trouvée:", col_coord, "\n")
  raw <- raw %>% select(-all_of(col_coord))
  cat("✓ Colonne supprimée avec succès\n")
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("⚠ Colonne 'Coordonnées géographiques :' non trouvée\n")
}

#2.  SUPPRESSION COLONNE "Latitude"
# Chercher et supprimer la colonne latitude
col_latitude <- names(raw)[grep("latitude", names(raw), ignore.case = TRUE)][1]

cat("\nSuppression de la colonne 'Latitude'...\n")

if (!is.na(col_latitude)) {
  cat("Colonne trouvée:", col_latitude, "\n")
  raw <- raw %>% select(-all_of(col_latitude))
  cat("✓ Colonne supprimée avec succès\n")
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("⚠ Colonne 'Latitude' non trouvée\n")
}

#3. SUPPRESSION COLONNE "Longitude"
# Chercher et supprimer la colonne longitude
col_longitude <- names(raw)[grep("longitude", names(raw), ignore.case = TRUE)][1]

cat("\nSuppression de la colonne 'Longitude'...\n")

if (!is.na(col_longitude)) {
  cat("Colonne trouvée:", col_longitude, "\n")
  raw <- raw %>% select(-all_of(col_longitude))
  cat("✓ Colonne supprimée avec succès\n")
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("⚠ Colonne 'Longitude' non trouvée\n")
}

#4. SUPPRESSION COLONNE "Altitude"
# Chercher et supprimer la colonne altitude
col_altitude <- names(raw)[grep("altitude", names(raw), ignore.case = TRUE)][1]

cat("\nSuppression de la colonne 'Altitude'...\n")

if (!is.na(col_altitude)) {
  cat("Colonne trouvée:", col_altitude, "\n")
  raw <- raw %>% select(-all_of(col_altitude))
  cat("✓ Colonne supprimée avec succès\n")
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("⚠ Colonne 'Altitude' non trouvée\n")
}

#5. SUPPRESSION COLONNE "Precision"
# Chercher et supprimer la colonne precision (coordonnées géographiques)
col_precision_list <- names(raw)[grep("precision", names(raw), ignore.case = TRUE)]

cat("\nSuppression de la colonne 'Precision'...\n")

if (length(col_precision_list) > 0) {
  cat("Colonnes trouvées:", length(col_precision_list), "\n")
  print(col_precision_list)
  raw <- raw %>% select(-all_of(col_precision_list))
  cat("✓ Colonnes supprimées avec succès\n")
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("⚠ Colonne 'Precision' non trouvée\n")
}

#6.  NETTOYAGE COLONNE "Appui d'une structure d'encadrement" 
# Chercher la colonne
col_appui <- names(raw)[grep("appui.*structure.*encadrement", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_appui)) {
  cat("\nColonne trouvée:", col_appui, "\n")
  cat("Valeurs avant transformation :\n")
  print(table(raw[[col_appui]], useNA = "ifany"))
  
  col_name_new <- "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Votre Unité d'élevage bénéficie-t-elle de l'appui d'une structure d'encadrement ?: 1=Oui, 0=Non"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_appui)) %>%
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
  cat("\n⚠ Colonne 'Appui d'une structure d'encadrement' non trouvée\n")
}

#7. SUPPRESSION COLONNE "Si Oui, nom de la structure"
# Chercher et supprimer la colonne
col_structure <- names(raw)[grep("si.*oui.*nom.*structure|nom.*structure", names(raw), ignore.case = TRUE)][1]

cat("\nSuppression de la colonne 'Si Oui, nom de la structure'...\n")

if (!is.na(col_structure)) {
  cat("Colonne trouvée:", col_structure, "\n")
  raw <- raw %>% select(-all_of(col_structure))
  cat("✓ Colonne supprimée avec succès\n")
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("⚠ Colonne 'Si Oui, nom de la structure' non trouvée\n")
}

#8. SUPPRESSION COLONNE "Types d'appuis offerts par la structure /1=Appui technique"
# Chercher et supprimer les colonnes contenant "types d'appuis" ou "appui technique"
col_appuis <- names(raw)[grep("types.*appuis|appui.*technique", names(raw), ignore.case = TRUE)]

cat("\nSuppression des colonnes 'Types d'appuis offerts par la structure'...\n")

if (length(col_appuis) > 0) {
  cat("Colonnes trouvées:", length(col_appuis), "\n")
  print(col_appuis)
  raw <- raw %>% select(-all_of(col_appuis))
  cat("✓ Colonnes supprimées avec succès\n")
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("⚠ Aucune colonne 'Types d'appuis offerts par la structure' non trouvée\n")
}

#9.  NETTOYAGE COLONNE "Etes-vous suivi par un technicien ?" 
# Chercher la colonne
col_technicien <- names(raw)[grep("etes-vous.*suivi|suivi.*technicien", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_technicien)) {
  cat("\nColonne trouvée:", col_technicien, "\n")
  cat("Valeurs avant transformation :\n")
  print(table(raw[[col_technicien]], useNA = "ifany"))
  
  col_name_new <- "II- CARACTERISTIQUES DE L'UNITE D'ELEVAGE (UE) /Etes-vous suivi par un technicien ?: 1=Oui, 0=Non"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_technicien)) %>%
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
  cat("\n⚠ Colonne 'Etes-vous suivi par un technicien ?' non trouvée\n")
}

#10. SUPPRESSION COLONNE "Quels sont les services offerts par le technicien ?"
# Chercher et supprimer la colonne
col_services <- names(raw)[grep("services.*offerts.*technicien", names(raw), ignore.case = TRUE)][1]

cat("\nSuppression de la colonne 'Services offerts par le technicien'...\n")

if (!is.na(col_services)) {
  cat("Colonne trouvée:", col_services, "\n")
  raw <- raw %>% select(-all_of(col_services))
  cat("✓ Colonne supprimée avec succès\n")
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("⚠ Colonne 'Services offerts par le technicien' non trouvée\n")
}

#11. SUPPRESSION COLONNE "Degré de satisfaction" 
# Chercher et supprimer la colonne
col_satisfaction <- names(raw)[grep("degré.*satisfaction|degree.*satisfaction", names(raw), ignore.case = TRUE)][1]

cat("\nSuppression de la colonne 'Degré de satisfaction'...\n")

if (!is.na(col_satisfaction)) {
  cat("Colonne trouvée:", col_satisfaction, "\n")
  raw <- raw %>% select(-all_of(col_satisfaction))
  cat("✓ Colonne supprimée avec succès\n")
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("⚠ Colonne 'Degré de satisfaction' non trouvée\n")
}

#12. REMPLISSAGE CELLULES VIDES "Type de main d'œuvre"
# Chercher toutes les colonnes contenant "type de main"
col_main_doeuvre <- names(raw)[grep("type.*main|main.*oeuvre|main.*œuvre", names(raw), ignore.case = TRUE)]

if (length(col_main_doeuvre) > 0) {
  cat("\nColonnes trouvées:", length(col_main_doeuvre), "\n")
  print(col_main_doeuvre)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_main_doeuvre) {
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
  cat("\n⚠ Aucune colonne 'Type de main d'œuvre' trouvée\n")
}

#14. REMPLISSAGE CELLULES VIDES "Type de système d'élevage"
# Chercher toutes les colonnes contenant "type de système" ou "système d'élevage"
col_systeme <- names(raw)[grep("type.*système|type.*systeme|système.*élevage|systeme.*elevage", names(raw), ignore.case = TRUE)]

if (length(col_systeme) > 0) {
  cat("\nColonnes trouvées:", length(col_systeme), "\n")
  print(col_systeme)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_systeme) {
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
  cat("\n⚠ Aucune colonne 'Type de système d'élevage' trouvée\n")
}

#15. REMPLISSAGE CELLULES VIDES "Espèces animales"
# Chercher toutes les colonnes contenant "espèces animales" ou "especes animales"
col_especes <- names(raw)[grep("espèces.*animales|especes.*animales", names(raw), ignore.case = TRUE)]

if (length(col_especes) > 0) {
  cat("\nColonnes trouvées:", length(col_especes), "\n")
  print(col_especes)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_especes) {
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
  cat("\n⚠ Aucune colonne 'Espèces animales' trouvée\n")
}

#16. REMPLISSAGE CELLULES VIDES "Races présentes dans le troupeau
# Chercher toutes les colonnes contenant "races présentes" ou "races presentes"
col_races <- names(raw)[grep("races.*présentes|races.*presentes|races.*troupeau", names(raw), ignore.case = TRUE)]

if (length(col_races) > 0) {
  cat("\nColonnes trouvées:", length(col_races), "\n")
  print(col_races)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_races) {
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
  cat("\n⚠ Aucune colonne 'Races présentes dans le troupeau' trouvée\n")
}

#18. Enreigistrement des donnees
cat("\n=== EXPORT DES DONNEES NETTOYEES ===\n")

# Exporter en Excel
output_path <- "c:/Users/lucas/Downloads/data2.xlsx"

# Essayer avec openxlsx (recommandé)
if (!require(openxlsx, quietly = TRUE)) {
  cat("Installation de openxlsx...\n")
  install.packages("openxlsx")
  library(openxlsx)
}

write.xlsx(raw, output_path, rowNames = FALSE, overwrite = TRUE)
cat("Fichier Excel sauvegardé :", output_path, "\n")
cat("Dimensions finales :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
