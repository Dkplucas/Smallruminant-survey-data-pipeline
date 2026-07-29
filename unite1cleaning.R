#Cleaning of I.- IDENTIFICATION DU CHEF DE MENAGE

# library needed
library(dplyr)

#-------------------------------------------- A. Kobocollect extraction--------------------------------------------------------
# Premier pas  est de telecharger le formulaire sur kobocollect
# Prenez soin d'inserer ses parametres avant de telecharger le fichier pour etre sure que le code marche
# Pour l'option "Select export type" = "XSL" pour plus de compatibilite
# Pour l'option "Value and header format" = "Labels"
# cliquer sur "Advanced option"
# Pour l'option "Export Select Many questions as…" = "Seperate Colomns"
# Decochez l'option "Include fields from all 4 versions" qui pourraient inserrer des questions inexistante dans notre questionnaire
# Pour l'option " Include groups in headers" = "Group separator ( / )"
# Cochez "Include media URLs"
# Cochez "Date range"
# Cliquez exporter et copier le nom du fichier "Questionnaire_caracterisation_pratiques_de_croisements_-_latest_version_-_labels_-_2026-07-28-03-29-08.xlsx

#------------------------ B. Data Cleaning ---------------------------------------------------------------------------
# Set up the working directory 
setwd("c:/Users/lucas/Downloads/") # rensenyer le chemin du fichier et le dossier le contenant
getwd()# confirmer le dossier de travail

# importer le fichier excel en copiant l'emplacement du fichier xlsx
xlsx_path <- "c:/Users/lucas/Downloads/Questionnaire_caracterisation_pratiques_de_croisements_-_latest_version_-_labels_-_2026-07-28-03-29-08.xlsx"

# On lit sans se fier aux noms de colonnes (plusieurs questions a choix multiple
# produisent des en-tetes dupliques dans ce fichier) : on utilise la POSITION
# des colonnes, qui est stable, exactement comme lors du controle initial.

raw <- read_excel(xlsx_path, col_names = TRUE)
cat("Fichier charge :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")

# Exclure les colonnes "start", "end", "I.- IDENTIFICATION DU CHEF DE MENAGE /Code Enquêteur" et "I.- IDENTIFICATION DU CHEF DE MENAGE /Nom et prénoms de l'enquêté"
raw <- raw %>% select(-any_of(c("start", "end", "I.- IDENTIFICATION DU CHEF DE MENAGE /Code Enquêteur", "I.- IDENTIFICATION DU CHEF DE MENAGE /Nom et prénoms de l'enquêté")))
cat("Après exclusion (start, end, I.- IDENTIFICATION DU CHEF DE MENAGE /Code Enquêteur, I.- IDENTIFICATION DU CHEF DE MENAGE /Nom et prénoms de l'enquêté) :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")

# 1. NETTOYAGE COLONNE "I.- IDENTIFICATION DU CHEF DE MENAGE /Sexe"
# Renommer la colonne "Sexe" avec les codes
# Chercher le vrai nom de la colonne (peut contenir des préfixes)
col_sexe <- names(raw)[grep("Sexe", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_sexe)) {
  # Afficher les valeurs uniques avant nettoyage
  cat("Valeurs uniques dans la colonne '", col_sexe, "' avant nettoyage :\n", sep = "")
  print(table(raw[[col_sexe]]))
  
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Sexe: 1=Masculin, 2=Feminin"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_sexe)) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      str_detect(str_to_lower(!!sym(col_name_new)), "^1=|masculin") ~ "1",
      str_detect(str_to_lower(!!sym(col_name_new)), "^2=|feminin") ~ "2",
      TRUE ~ !!sym(col_name_new)
    ))
  cat("Colonne Sexe nettoyée et renommée\n")
  cat("Valeurs après nettoyage :\n")
  print(table(raw[[col_name_new]]))
}

# 2. NETTOYAGE COLONNE "Niveau d'instruction"
# Renommer la colonne et recoder
# Vérifier le vrai nom de la colonne (avec ou sans faute et préfixe)
col_instruction <- names(raw)[grep("Niveau.*instruction", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_instruction)) {
  # Afficher les valeurs uniques avant nettoyage
  cat("\nValeurs uniques dans la colonne '", col_instruction, "' avant nettoyage :\n", sep = "")
  print(table(raw[[col_instruction]], useNA = "ifany"))
  
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Niveau d'instruction: 1=Aucun, 2=Primaire, 3=Secondaire, 4=Superieure, 5=vide"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_instruction)) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) |
        !!sym(col_name_new) == "" ~ "5",
      str_detect(str_to_lower(!!sym(col_name_new)), "^1=|aucun") ~ "1",
      str_detect(str_to_lower(!!sym(col_name_new)), "^2=|primaire") ~ "2",
      str_detect(str_to_lower(!!sym(col_name_new)), "^3=|secondaire") ~ "3",
      str_detect(str_to_lower(!!sym(col_name_new)), "^4=|superieure") ~ "4",
      TRUE ~ !!sym(col_name_new)
    ))
  cat("Colonne Niveau d'instruction nettoyée et renommée\n")
  cat("Valeurs après nettoyage :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# 3. NETTOYAGE COLONNE "Catégorie d'âge"
# Renommer la colonne et recoder
# Vérifier le vrai nom de la colonne (avec accents et préfixe)
col_age <- names(raw)[grep("Catégorie.*âge", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_age)) {
  # Afficher les valeurs uniques avant nettoyage
  cat("\nValeurs uniques dans la colonne '", col_age, "' avant nettoyage :\n", sep = "")
  print(table(raw[[col_age]], useNA = "ifany"))
  
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Catégorie d'âge : 1= > 50ans, 2=20 a 30, 3=30 a 50"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_age)) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      str_detect(str_to_lower(!!sym(col_name_new)), "^1=|> 50|50ans") ~ "1",
      str_detect(str_to_lower(!!sym(col_name_new)), "^2=|20.*30") ~ "2",
      str_detect(str_to_lower(!!sym(col_name_new)), "^3=|30.*50") ~ "3",
      TRUE ~ !!sym(col_name_new)
    ))
  cat("Colonne Catégorie d'âge nettoyée et renommée\n")
  cat("Valeurs après nettoyage :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# 4. NETTOYAGE COLONNE "Situation matrimoniale"
# Renommer la colonne et recoder
# Vérifier le vrai nom de la colonne
col_matrimoniale <- names(raw)[grep("Situation.*matrimoniale", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_matrimoniale)) {
  # Afficher les valeurs uniques avant nettoyage
  cat("\nValeurs uniques dans la colonne '", col_matrimoniale, "' avant nettoyage :\n", sep = "")
  print(table(raw[[col_matrimoniale]], useNA = "ifany"))
  
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Situation matrimoniale : 1=Celibataire, 2=Divorce(e), 3=Marie, 4=Veuf/ve"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_matrimoniale)) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      str_detect(str_to_lower(!!sym(col_name_new)), "^1=|célibataire|celibataire") ~ "1",
      str_detect(str_to_lower(!!sym(col_name_new)), "^2=|divorcé|divorc") ~ "2",
      str_detect(str_to_lower(!!sym(col_name_new)), "^3=|marié|marie") ~ "3",
      str_detect(str_to_lower(!!sym(col_name_new)), "^4=|veuf|veuve") ~ "4",
      TRUE ~ !!sym(col_name_new)
    ))
  cat("Colonne Situation matrimoniale nettoyée et renommée\n")
  cat("Valeurs après nettoyage :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# 5. SUPPRESSION COLONNE "Si Autre, preciser"
# Chercher le nom exact de la colonne (variantes avec accents et préfixe)
col_to_remove <- names(raw)[grep("si.*autre.*precis|si.*autre.*précis|préciser|preciser", names(raw), ignore.case = TRUE)]
cat("\nColonnes à supprimer (contenant 'Si Autre' ou 'préciser') :\n")
print(col_to_remove)

if (length(col_to_remove) > 0) {
  raw <- raw %>% select(-all_of(col_to_remove))
  cat("Colonne(s) supprimée(s) :\n")
  print(col_to_remove)
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("Aucune colonne trouvée avec ce pattern\n")
  cat("Colonnes disponibles contenant 'autre' ou 'precis' :\n")
  autres_cols <- names(raw)[grep("autre|precis", names(raw), ignore.case = TRUE)]
  print(autres_cols)
}

#6. SUPPRESSION DES 2 PREMIERES LIGNES CORRESPONDANT AUX 2 FERMES DE TANKPE
cat("\nSuppression des 2 premières lignes (zone exclue)\n")
raw <- raw %>% slice(-c(1:2))
cat("Dimensions finales :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")

#7. NETTOYAGE COLONNE "S autre, preciser" pour la variable "Situation matrimoniale"
# Chercher la colonne contenant "Si Autre" et "preciser" (avec variantes comme "...10" à la fin)
col_to_remove <- names(raw)[grep("Si.*Autre.*precis", names(raw), ignore.case = TRUE)]

cat("\nColonnes trouvées contenant 'Si Autre' et 'preciser' :\n")
print(col_to_remove)

if (length(col_to_remove) > 0) {
  cat("\nSuppression de", length(col_to_remove), "colonne(s)...\n")
  raw <- raw %>% select(-all_of(col_to_remove))
  cat("✓ Colonne(s) supprimée(s) avec succès\n")
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("⚠ Aucune colonne trouvée. Voici toutes les colonnes disponibles :\n")
  print(names(raw))
}

#8. NETTOYAGE COLONNE I.- IDENTIFICATION DU CHEF DE MENAGE /Quelle est votre principale activité?
# Renommer la colonne et recoder
# Vérifier le vrai nom de la colonne
col_activite <- names(raw)[grep("principale.*activité|principale.*activite", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_activite)) {
  # Afficher les valeurs uniques avant nettoyage
  cat("\nValeurs uniques dans la colonne '", col_activite, "' avant nettoyage :\n", sep = "")
  print(table(raw[[col_activite]], useNA = "ifany"))
  
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelle est votre principale activité? : 1=Agriculture, 2=Artisanat, 3=Autre, 4=Commerce, 5=Elevage"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_activite)) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      str_detect(str_to_lower(!!sym(col_name_new)), "^1=|agriculture") ~ "1",
      str_detect(str_to_lower(!!sym(col_name_new)), "^2=|artisanat") ~ "2",
      str_detect(str_to_lower(!!sym(col_name_new)), "^3=|autre") ~ "3",
      str_detect(str_to_lower(!!sym(col_name_new)), "^4=|commerce") ~ "4",
      str_detect(str_to_lower(!!sym(col_name_new)), "^5=|elevage") ~ "5",
      TRUE ~ !!sym(col_name_new)
    ))
  cat("Colonne Quelle est votre principale activité? nettoyée et renommée\n")
  cat("Valeurs après nettoyage :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

#9. Les colonne des activites secondaire seront selectionner et analyser 
# ensemble car c'etait une question a choix multiple pour le test de CHI2
# Remplir les cellules vides des colonnes d'activités secondaires par "0"
cat("\nRemplissage des cellules vides des colonnes d'activités secondaires par '0'...\n")

col_elevage_temp <- names(raw)[grep("activites.*secondaires.*elevage", names(raw), ignore.case = TRUE)][1]
col_agriculture_temp <- names(raw)[grep("activites.*secondaires.*agriculture", names(raw), ignore.case = TRUE)][1]
col_commerce_temp <- names(raw)[grep("activites.*secondaires.*commerce", names(raw), ignore.case = TRUE)][1]
col_artisanat_temp <- names(raw)[grep("activites.*secondaires.*artisanat", names(raw), ignore.case = TRUE)][1]
col_autre_temp <- names(raw)[grep("activites.*secondaires.*autre", names(raw), ignore.case = TRUE)][1]

cols_activites_temp <- c(col_elevage_temp, col_agriculture_temp, col_commerce_temp, col_artisanat_temp, col_autre_temp)
cols_activites_temp <- cols_activites_temp[!is.na(cols_activites_temp)]

if (length(cols_activites_temp) > 0) {
  for (col in cols_activites_temp) {
    raw <- raw %>%
      mutate(!!col := case_when(
        is.na(!!sym(col)) | !!sym(col) == "" ~ "0",
        TRUE ~ as.character(!!sym(col))
      ))
  }
  cat("✓ Cellules vides remplacées par '0' pour", length(cols_activites_temp), "colonnes\n")
}

#10. Suppression des valeur de "Si autre, preciser" car ils seront pris en compte par la modalite "Autre"
col_precision <- names(raw)[grep("precision.*autre.*activité|precision.*autre.*secondaire", names(raw), ignore.case = TRUE)][1]

cat("\nSuppression de la colonne 'Precision si autre activité secondaire'...\n")

if (!is.na(col_precision)) {
  cat("Colonne trouvée:", col_precision, "\n")
  raw <- raw %>% select(-all_of(col_precision))
  cat("✓ Colonne supprimée avec succès\n")
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("⚠ Colonne non trouvée. Voici les colonnes contenant 'precision' :\n")
  precision_cols <- names(raw)[grep("precision", names(raw), ignore.case = TRUE)]
  print(precision_cols)
}

#11. NETTOYAGE COLONNE "Quelle est votre principale source de revenu?
# Renommer la colonne et recoder
# Vérifier le vrai nom de la colonne
col_revenu <- names(raw)[grep("principale.*source.*revenu|activité.*revenu", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_revenu)) {
  # Afficher les valeurs uniques avant nettoyage
  cat("\nValeurs uniques dans la colonne '", col_revenu, "' avant nettoyage :\n", sep = "")
  print(table(raw[[col_revenu]], useNA = "ifany"))
  
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelle est votre principale source de revenu? ou De quelle activité provient principalement vos revenus?: 1=Agriculture, 2=Artisanat, 3=Autre, 4=Commerce, 5=Elevage, 6=Vide"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_revenu)) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "6",
      str_detect(str_to_lower(!!sym(col_name_new)), "^1=|agriculture") ~ "1",
      str_detect(str_to_lower(!!sym(col_name_new)), "^2=|artisanat") ~ "2",
      str_detect(str_to_lower(!!sym(col_name_new)), "^3=|autre") ~ "3",
      str_detect(str_to_lower(!!sym(col_name_new)), "^4=|commerce") ~ "4",
      str_detect(str_to_lower(!!sym(col_name_new)), "^5=|elevage") ~ "5",
      str_detect(str_to_lower(!!sym(col_name_new)), "^6=|vide") ~ "6",
      TRUE ~ !!sym(col_name_new)
    ))
  cat("Colonne Quelle est votre principale source de revenu? nettoyée et renommée\n")
  cat("Valeurs après nettoyage :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

#12 . NETTOYAGE COLONNE "Avez vous reçu oui suivi une Formation en élevage ?" 
# Renommer la colonne et recoder
# Vérifier le vrai nom de la colonne
col_formation <- names(raw)[grep("formation.*élevage|formation.*elevage", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_formation)) {
  # Afficher les valeurs uniques avant nettoyage
  cat("\nValeurs uniques dans la colonne '", col_formation, "' avant nettoyage :\n", sep = "")
  print(table(raw[[col_formation]], useNA = "ifany"))
  
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Avez vous reçu oui suivi une Formation en élevage ?: 0=Non, 1=Oui"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_formation)) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      str_detect(str_to_lower(!!sym(col_name_new)), "^0=|non") ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "^1=|oui") ~ "1",
      TRUE ~ !!sym(col_name_new)
    ))
  cat("Colonne Avez vous reçu oui suivi une Formation en élevage ? nettoyée et renommée\n")
  cat("Valeurs après nettoyage :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

#13.REMPLISSAGE CELLULES VIDES "Si Oui, depuis quand ?
# Chercher la colonne
col_depuis <- names(raw)[grep("si.*oui.*depuis|depuis.*quand", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_depuis)) {
  cat("\nColonne trouvée:", col_depuis, "\n")
  cat("Valeurs avant remplissage :\n")
  print(table(raw[[col_depuis]], useNA = "ifany"))
  
  # Convertir en caractères, convertir les années en nombre d'années, et remplacer les cellules vides par "0"
  raw <- raw %>%
    mutate(!!col_depuis := as.character(!!sym(col_depuis))) %>%
    mutate(!!col_depuis := case_when(
      is.na(!!sym(col_depuis)) | !!sym(col_depuis) == "" | !!sym(col_depuis) == "NA" ~ "0",
      # Détecter si c'est une année (4 chiffres) et convertir en nombre d'années
      str_detect(!!sym(col_depuis), "^\\d{4}$") ~ as.character(2026 - as.numeric(!!sym(col_depuis))),
      TRUE ~ !!sym(col_depuis)
    ))
  
  cat("✓ Cellules vides remplacées par '0', années converties en nombre d'années\n")
  cat("Valeurs après remplissage :\n")
  print(table(raw[[col_depuis]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Si Oui, depuis quand ?' non trouvée\n")
}


# 14. REMPLISSAGE CELLULES VIDES "Quelle est votre expérience en élevage?"
# Chercher la colonne
col_experience <- names(raw)[grep("expérience.*élevage|experience.*elevage", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_experience)) {
  cat("\nColonne trouvée:", col_experience, "\n")
  cat("Valeurs avant remplissage :\n")
  print(table(raw[[col_experience]], useNA = "ifany"))
  
  # Convertir en caractères et remplacer les cellules vides par "0"
  raw <- raw %>%
    mutate(!!col_experience := as.character(!!sym(col_experience))) %>%
    mutate(!!col_experience := case_when(
      is.na(!!sym(col_experience)) | !!sym(col_experience) == "" | !!sym(col_experience) == "NA" ~ "0",
      TRUE ~ !!sym(col_experience)
    ))
  
  cat("✓ Cellules vides remplacées par '0'\n")
  cat("Valeurs après remplissage :\n")
  print(table(raw[[col_experience]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Quelle est votre expérience en élevage?' non trouvée\n")
}

#15. NETTOYAGE COLONNE "Appartenez vous à une Organisation Paysanne (OP) ou coopérative...?" 
# Chercher la colonne
col_op <- names(raw)[grep("organisation.*paysanne|coopérative|cooperative", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_op)) {
  cat("\nColonne trouvée:", col_op, "\n")
  cat("Valeurs avant transformation :\n")
  print(table(raw[[col_op]], useNA = "ifany"))
  
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Appartenez vous à une Organisation Paysanne (OP) ou coopérative...?: 0=Non, 1=Oui"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_op)) %>%
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
  cat("\n⚠ Colonne 'Appartenez vous à une Organisation Paysanne (OP) ou coopérative...?' non trouvée\n")
}

#16. SUPPRESSION COLONNE "Si Oui, nom de l'OP" 
# Supprimer la colonne spécifiquement
col_op_nom <- names(raw)[grep("si.*oui.*nom.*op|nom.*op", names(raw), ignore.case = TRUE)][1]

cat("\nSuppression de la colonne 'Si Oui, nom de l'OP'...\n")

if (!is.na(col_op_nom)) {
  cat("Colonne trouvée:", col_op_nom, "\n")
  raw <- raw %>% select(-all_of(col_op_nom))
  cat("✓ Colonne supprimée avec succès\n")
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("⚠ Colonne non trouvée. Voici les colonnes contenant 'nom' :\n")
  nom_cols <- names(raw)[grep("nom", names(raw), ignore.case = TRUE)]
  print(nom_cols)
}

#17. SUPPRESSION COLONNE "Année de création"
# Supprimer la colonne spécifiquement
col_annee <- names(raw)[grep("année.*création|annee.*creation", names(raw), ignore.case = TRUE)][1]

cat("\nSuppression de la colonne 'Année de création'...\n")

if (!is.na(col_annee)) {
  cat("Colonne trouvée:", col_annee, "\n")
  raw <- raw %>% select(-all_of(col_annee))
  cat("✓ Colonne supprimée avec succès\n")
  cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
} else {
  cat("⚠ Colonne non trouvée. Voici les colonnes contenant 'année' ou 'creation' :\n")
  annee_cols <- names(raw)[grep("année|annee|création|creation", names(raw), ignore.case = TRUE)]
  print(annee_cols)
}

#18 REMPLISSAGE CELLULES VIDES "Superficie de terre agricole"
# Chercher la colonne
col_superficie <- names(raw)[grep("superficie.*terre.*agricole|terre.*agricole.*valeur", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_superficie)) {
  cat("\nColonne trouvée:", col_superficie, "\n")
  cat("Valeurs avant remplissage :\n")
  print(table(raw[[col_superficie]], useNA = "ifany"))
  
  # Convertir en caractères et remplacer les cellules vides par "0"
  raw <- raw %>%
    mutate(!!col_superficie := as.character(!!sym(col_superficie))) %>%
    mutate(!!col_superficie := case_when(
      is.na(!!sym(col_superficie)) | !!sym(col_superficie) == "" | !!sym(col_superficie) == "NA" ~ "0",
      TRUE ~ !!sym(col_superficie)
    ))
  
  cat("✓ Cellules vides remplacées par '0'\n")
  cat("Valeurs après remplissage :\n")
  print(table(raw[[col_superficie]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Superficie de terre agricole' non trouvée\n")
}

#19.  REMPLISSAGE CELLULES VIDES "Principales cultures"

# Chercher toutes les colonnes contenant "principales cultures"
col_cultures <- names(raw)[grep("principales.*cultures", names(raw), ignore.case = TRUE)]

if (length(col_cultures) > 0) {
  cat("\nColonnes trouvées:", length(col_cultures), "\n")
  print(col_cultures)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_cultures) {
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
  cat("\n⚠ Aucune colonne 'Principales cultures' non trouvée\n")
}

#20. EXPORT DES DONNEES NETTOYEES
cat("\n=== EXPORT DES DONNEES NETTOYEES ===\n")

# Exporter en Excel
output_path <- "c:/Users/lucas/Downloads/data1.xlsx"

# Essayer avec openxlsx (recommandé)
if (!require(openxlsx, quietly = TRUE)) {
  cat("Installation de openxlsx...\n")
  install.packages("openxlsx")
  library(openxlsx)
}

write.xlsx(raw, output_path, rowNames = FALSE, overwrite = TRUE)
cat("Fichier Excel sauvegardé :", output_path, "\n")
cat("Dimensions finales :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")





