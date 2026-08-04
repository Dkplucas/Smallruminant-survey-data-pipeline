#Cleaning to get ready for all the qualitative variable (UE) 

# library needed
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)

#A. Import the data1 exported from the unite1cleaning.R to continue the cleaning
# Attention le dossier d'importation a changer veuiller a corriger en fontion de votre dossier de travail
getwd()
setwd("C:/Users/lucas/OneDrive/Bureau/Data/Test_CHI2")
xlsx_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Test_CHI2/data7.xlsx"
raw <- read_excel(xlsx_path, col_names = TRUE)
cat("Fichier charge :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
raw <- read_excel("data7.xlsx")

#E. Data cleaning to only get the qualitative variables

#Attention: "5.) Reproduction/Quelles sont les races sahéliennes présentes dans votre troupeau ?" 
#"5.) Reproduction/Pendant combien de temps gardez-vous les femelles reproductrices dans votre élevage avant de les renouveler ?"
# cette variable n'a pas ete traite car le contenu est vraiment desordoner et non arranger 

#1. SUPPRESSION COLONNES SUPPLÉMENTAIRES ------------------------------------------------
# Supprimer les colonnes supplémentaires demandées
cols_to_remove_extra <- c(
  "I.- IDENTIFICATION DU CHEF DE MENAGE /Age :",
  "I.- IDENTIFICATION DU CHEF DE MENAGE /Si Oui, depuis quand ?",
  "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelle est votre expérience en élevage?",
  "I.- IDENTIFICATION DU CHEF DE MENAGE /Si pratique de l'agriculture, quelle est la superficie de terre agricole mise en valeur pour les cultures dans votre champ ? kanti ou ha",
  "5.) Reproduction/Pendant combien de temps gardez-vous le(s) mâle(s) reproducteur(s) dans votre élevage avant de le(s) renouveler ?; 1=1ans, 2=2ans, 3=3ans, 4=4ans, 5=Autre a preciser",
  "7.) Performances des métis/Prolificité (nombre de chevreaux par portée)",
  "12.) Effectif et composition du cheptel/Effectif total du troupeau",
  "12.) Effectif et composition du cheptel/Nombre de mâle adultes Djallonke",
  "12.) Effectif et composition du cheptel/Nombre de mâle adultes Sahelien",
  "12.) Effectif et composition du cheptel/Nombre de mâle adultes Metis",
  "12.) Effectif et composition du cheptel/Nombre de mâle adultes Autre____",
  "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) mâles_Djallonke",
  "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) mâles_Sahelien",
  "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) mâles_Metis",
  "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) mâles_Autre",
  " 12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Djallonke",
  " 12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femellesSahelienne",
  " 12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Metis",
  "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Autre",
  " 12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) mâle_Djallonke",
  " 12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) mâle_Sahelien",
  " 12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) mâle_Metis",
  " 12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Djallonke",
  "12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Sahelien",
  " 12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Metis",
  " 12.) Effectif et composition du cheptel/Effectif il y a 12 mois_Djallonke",
  " 12.) Effectif et composition du cheptel/Effectif il y a 12 mois_Saheliens",
  " 12.) Effectif et composition du cheptel/Effectif il y a 12 mois_Metis"
)

cat("\nSuppression des colonnes supplémentaires...\n")
cat("Nombre de colonnes avant suppression:", ncol(raw), "\n")

raw <- raw %>% select(-any_of(cols_to_remove_extra))

cat("Nombre de colonnes après suppression:", ncol(raw), "\n")
cat("✓ Colonnes supplémentaires supprimées\n")

#3. SUPPRESSION COLONNES ADDITIONNELLES ------------------------------------------------
# Supprimer les colonnes additionnelles demandées
cols_to_remove_additional <- c(
  " 12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Djallonke",
  " 12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femellesSahelienne",
  " 12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femelles_Metis",
  " 12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) mâle_Djallonke",
  " 12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) mâle_Sahelien",
  " 12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) mâle_Metis",
  " 12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Djallonke",
  " 12.) Effectif et composition du cheptel/Nbre de petits (< 6 mois) femelle_Metis",
  "12.) Effectif et composition du cheptel/Effectif il y a 12 mois_Djallonke",
  "12.) Effectif et composition du cheptel/Effectif il y a 12 mois_Saheliens",
  "12.) Effectif et composition du cheptel/Effectif il y a 12 mois_Metis"
)

cat("\nSuppression des colonnes additionnelles...\n")
cat("Nombre de colonnes avant suppression:", ncol(raw), "\n")

raw <- raw %>% select(-any_of(cols_to_remove_additional))

cat("Nombre de colonnes après suppression:", ncol(raw), "\n")
cat("✓ Colonnes additionnelles supprimées\n")

#4. SUPPRESSION COLONNES MANQUANTES ------------------------------------------------
# Chercher et supprimer les colonnes manquantes avec grep
cols_to_find <- c(
  "femelles_Djallonke",
  "femellesSahelienne",
  "femelles_Metis",
  "mâle_Djallonke",
  "mâle_Sahelien",
  "mâle_Metis",
  "femelle_Djallonke",
  "femelle_Metis"
)

cat("\nSuppression des colonnes manquantes avec grep...\n")
cat("Nombre de colonnes avant suppression:", ncol(raw), "\n")

# Chercher les colonnes qui contiennent ces patterns
cols_found <- c()
for (pattern in cols_to_find) {
  matching_cols <- names(raw)[grep(pattern, names(raw), ignore.case = TRUE)]
  if (length(matching_cols) > 0) {
    cols_found <- c(cols_found, matching_cols)
    cat("Colonne trouvée:", matching_cols, "\n")
  }
}

if (length(cols_found) > 0) {
  raw <- raw %>% select(-all_of(cols_found))
  cat("✓", length(cols_found), "colonne(s) supprimée(s)\n")
}

cat("Nombre de colonnes après suppression:", ncol(raw), "\n")


#5.---- EXPORT DES DONNEES NETTOYEES -------------------------------------------------------------------
cat("\n=== EXPORT DES DONNEES NETTOYEES ===\n")

# Exporter en Excel
output_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Test_CHI2/data8.xlsx"

# Essayer avec openxlsx (recommandé)
if (!require(openxlsx, quietly = TRUE)) {
  cat("Installation de openxlsx...\n")
  install.packages("openxlsx")
  library(openxlsx)
}

write.xlsx(raw, output_path, rowNames = FALSE, overwrite = TRUE)
cat("Fichier Excel sauvegardé :", output_path, "\n")
cat("Dimensions finales :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")