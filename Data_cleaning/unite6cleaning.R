#Cleaning of 8.) Caractéristiques des Chèvres Métissées and 9.)####### (UE) 

# library needed
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)

#A. Import the data1 exported from the unite1cleaning.R to continue the cleaning
# Attention le dossier d'importation a changer veuiller a corriger en fontion de votre dossier de travail
getwd()
setwd("C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning")
xlsx_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning/data5.xlsx"
raw <- read_excel(xlsx_path, col_names = TRUE)
cat("Fichier charge :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
raw <- read_excel("data5.xlsx")

#D. Data cleaning
#1. REMPLISSAGE CELLULES VIDES "Enregistrez-vous les données de reproduction et de croissance ?" 
# Chercher toutes les colonnes contenant "enregistrez-vous les données de reproduction et de croissance"
col_enregistrement_donnees <- names(raw)[grep("enregistrez.*données.*reproduction.*croissance|enregistrez.*donnees.*reproduction.*croissance", names(raw), ignore.case = TRUE)]

if (length(col_enregistrement_donnees) > 0) {
  cat("\nColonnes trouvées:", length(col_enregistrement_donnees), "\n")
  print(col_enregistrement_donnees)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_enregistrement_donnees) {
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
  cat("\n⚠ Aucune colonne 'Enregistrez-vous les données de reproduction et de croissance ?' trouvée\n")
}

#2. Suivi et enregistrement de données/Si, « oui », quelles sont les données prises en compte pour l’enregistrement ? /1= poids à la naissance
# Chercher toutes les colonnes contenant "quelles sont les données prises en compte"
col_donnees_enregistrement <- names(raw)[grep("quelles.*sont.*données.*prises.*compte.*enregistrement|quelles.*sont.*donnees.*prises.*compte.*enregistrement", names(raw), ignore.case = TRUE)]

if (length(col_donnees_enregistrement) > 0) {
  cat("\nColonnes trouvées:", length(col_donnees_enregistrement), "\n")
  print(col_donnees_enregistrement)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_donnees_enregistrement) {
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
  cat("\n⚠ Aucune colonne 'Si « oui », quelles sont les données prises en compte pour l'enregistrement ?' trouvée\n")
}

#3. REMPLISSAGE CELLULES VIDES "Si « non », pourquoi ? /raisons"
# Chercher toutes les colonnes contenant "si « non », pourquoi" ou "si non pourquoi"
col_raisons_non <- names(raw)[grep("si.*«.*non.*».*pourquoi|si.*non.*pourquoi", names(raw), ignore.case = TRUE)]

if (length(col_raisons_non) > 0) {
  cat("\nColonnes trouvées:", length(col_raisons_non), "\n")
  print(col_raisons_non)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_raisons_non) {
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
  cat("\n⚠ Aucune colonne 'Si « non », pourquoi ?' trouvée\n")
}

#4. REMPLISSAGE CELLULES VIDES "Quels sont les défis liés au croisement ? /défis"
# Chercher toutes les colonnes contenant "quels sont les défis liés au croisement"
col_defis_croisement <- names(raw)[grep("quels.*sont.*défis.*croisement|quels.*sont.*defis.*croisement", names(raw), ignore.case = TRUE)]

if (length(col_defis_croisement) > 0) {
  cat("\nColonnes trouvées:", length(col_defis_croisement), "\n")
  print(col_defis_croisement)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_defis_croisement) {
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
  cat("\n⚠ Aucune colonne 'Quels sont les défis liés au croisement ?' trouvée\n")
}

#------------------------ 9.) Perceptions et adaptations----------------------------------------------------------------------------------------------

#5.REMPLISSAGE CELLULES VIDES "Les croisements pratiqués ont-ils amélioré les performances de votre troupeau ?
# Chercher toutes les colonnes contenant "les croisements pratiqués ont-ils amélioré"
col_amelioration_performances <- names(raw)[grep("les.*croisements.*pratiqués.*ont.*ils.*amélioré|les.*croisements.*pratiques.*ont.*ils.*ameliore", names(raw), ignore.case = TRUE)]

if (length(col_amelioration_performances) > 0) {
  cat("\nColonnes trouvées:", length(col_amelioration_performances), "\n")
  print(col_amelioration_performances)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_amelioration_performances) {
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
  cat("\n⚠ Aucune colonne 'Les croisements pratiqués ont-ils amélioré les performances de votre troupeau ?' trouvée\n")
}

#6. REMPLISSAGE CELLULES VIDES "Quels avantages avez-vous observés à la suite des croisements ?
# Chercher toutes les colonnes contenant "quels avantages avez-vous observés à la suite des croisements"
col_avantages_croisements <- names(raw)[grep("quels.*avantages.*avez-vous.*observés|quels.*avantages.*avez.*vous.*observes", names(raw), ignore.case = TRUE)]

if (length(col_avantages_croisements) > 0) {
  cat("\nColonnes trouvées:", length(col_avantages_croisements), "\n")
  print(col_avantages_croisements)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_avantages_croisements) {
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
  cat("\n⚠ Aucune colonne 'Quels avantages avez-vous observés à la suite des croisements ?' trouvée\n")
}

#7.Perceptions et adaptations/Quels inconvénients avez-vous rencontrés suite aux croisements ? /1= Sensibilité accrue aux maladies
# Chercher toutes les colonnes contenant "quels inconvénients avez-vous rencontrés"
col_inconvenients_croisements <- names(raw)[grep("quels.*inconvénients.*avez-vous.*rencontrés|quels.*inconvenients.*avez.*vous.*rencontres", names(raw), ignore.case = TRUE)]

if (length(col_inconvenients_croisements) > 0) {
  cat("\nColonnes trouvées:", length(col_inconvenients_croisements), "\n")
  print(col_inconvenients_croisements)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_inconvenients_croisements) {
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
  cat("\n⚠ Aucune colonne 'Quels inconvénients avez-vous rencontrés suite aux croisements ?' trouvée\n")
}

#8. REMPLISSAGE CELLULES VIDES "Avez-vous reçu une formation ou des conseils sur les pratiques de croisement ?
# Chercher toutes les colonnes contenant "avez-vous reçu une formation ou des conseils"
col_formation_conseils <- names(raw)[grep("avez-vous.*reçu.*formation.*conseils|avez.*vous.*recu.*formation.*conseils", names(raw), ignore.case = TRUE)]

if (length(col_formation_conseils) > 0) {
  cat("\nColonnes trouvées:", length(col_formation_conseils), "\n")
  print(col_formation_conseils)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_formation_conseils) {
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
  cat("\n⚠ Aucune colonne 'Avez-vous reçu une formation ou des conseils sur les pratiques de croisement ?' trouvée\n")
}

#9. REMPLISSAGE CELLULES VIDES "Si oui, par qui ?
# Chercher toutes les colonnes contenant "si oui, par qui"
col_par_qui <- names(raw)[grep("si.*oui.*par.*qui", names(raw), ignore.case = TRUE)]

if (length(col_par_qui) > 0) {
  cat("\nColonnes trouvées:", length(col_par_qui), "\n")
  print(col_par_qui)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_par_qui) {
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
  cat("\n⚠ Aucune colonne 'Si oui, par qui ?' trouvée\n")
}

#10. REMPLISSAGE CELLULES VIDES "Comment évaluez-vous l'impact du croisement sur votre élevage ?
# Chercher toutes les colonnes contenant "comment évaluez-vous l'impact du croisement"
col_impact_croisement <- names(raw)[grep("comment.*évaluez.*vous.*impact.*croisement|comment.*evaluez.*vous.*impact.*croisement", names(raw), ignore.case = TRUE)]

if (length(col_impact_croisement) > 0) {
  cat("\nColonnes trouvées:", length(col_impact_croisement), "\n")
  print(col_impact_croisement)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_impact_croisement) {
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
  cat("\n⚠ Aucune colonne 'Comment évaluez-vous l'impact du croisement sur votre élevage ?' trouvée\n")
}

#11. REMPLISSAGE CELLULES VIDES "Souhaitez-vous adopter de nouvelles pratiques de croisement à l'avenir ?
# Chercher toutes les colonnes contenant "souhaitez-vous adopter de nouvelles pratiques de croisement"
col_nouvelles_pratiques <- names(raw)[grep("souhaitez.*vous.*adopter.*nouvelles.*pratiques.*croisement|souhaitez.*vous.*adopter.*nouvelles.*pratiques.*croisement", names(raw), ignore.case = TRUE)]

if (length(col_nouvelles_pratiques) > 0) {
  cat("\nColonnes trouvées:", length(col_nouvelles_pratiques), "\n")
  print(col_nouvelles_pratiques)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_nouvelles_pratiques) {
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
  cat("\n⚠ Aucune colonne 'Souhaitez-vous adopter de nouvelles pratiques de croisement à l'avenir ?' trouvée\n")
}

#12. REMPLISSAGE CELLULES VIDES "Seriez-vous prêt à collaborer pour des recherches et expérimentations..." 
# Chercher toutes les colonnes contenant "seriez-vous prêt à collaborer"
col_collaboration <- names(raw)[grep("seriez.*vous.*prêt.*collaborer|seriez.*vous.*pret.*collaborer", names(raw), ignore.case = TRUE)]

if (length(col_collaboration) > 0) {
  cat("\nColonnes trouvées:", length(col_collaboration), "\n")
  print(col_collaboration)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_collaboration) {
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
  cat("\n⚠ Aucune colonne 'Seriez-vous prêt à collaborer pour des recherches et expérimentations...' trouvée\n")
}

#13. REMPLISSAGE CELLULES VIDES "Si « non », pourquoi ? /raisons
# Chercher toutes les colonnes contenant "si « non », pourquoi" dans la section 9
col_raisons_non_section9 <- names(raw)[grep("9\\.).*si.*«.*non.*».*pourquoi|Perceptions.*adaptations.*si.*«.*non.*».*pourquoi", names(raw), ignore.case = TRUE)]

if (length(col_raisons_non_section9) > 0) {
  cat("\nColonnes trouvées:", length(col_raisons_non_section9), "\n")
  print(col_raisons_non_section9)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_raisons_non_section9) {
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
  cat("\n⚠ Aucune colonne 'Si « non », pourquoi ?' trouvée dans la section 9\n")
}

#14. REMPLISSAGE CELLULES VIDES "Quelles pourraient être votre niveau de collaboration ou participation...
# Chercher toutes les colonnes contenant "quelles pourraient être votre niveau de collaboration"
col_niveau_collaboration <- names(raw)[grep("quelles.*pourraient.*être.*niveau.*collaboration|quelles.*pourraient.*etre.*niveau.*collaboration", names(raw), ignore.case = TRUE)]

if (length(col_niveau_collaboration) > 0) {
  cat("\nColonnes trouvées:", length(col_niveau_collaboration), "\n")
  print(col_niveau_collaboration)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_niveau_collaboration) {
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
  cat("\n⚠ Aucune colonne 'Quelles pourraient être votre niveau de collaboration ou participation...' trouvée\n")
}

#------------------------------------10.) Contexte environnemental----------------------------------------------------

#15. NETTOYAGE COLONNE "Avez-vous observé des changements climatiques affectant votre élevage ?
# Chercher la colonne
col_changements_climatiques <- names(raw)[grep("avez-vous.*observé.*changements.*climatiques|avez.*vous.*observe.*changements.*climatiques", names(raw), ignore.case = TRUE)][1]

if (!is.na(col_changements_climatiques)) {
  cat("\nColonne trouvée:", col_changements_climatiques, "\n")
  cat("Valeurs avant transformation :\n")
  print(table(raw[[col_changements_climatiques]], useNA = "ifany"))
  
  col_name_new <- "10.) Contexte environnemental/Avez-vous observé des changements climatiques affectant votre élevage ?: 0=Ne sait pas, 1=Oui, 2=Non"
  
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_changements_climatiques)) %>%
    mutate(!!col_name_new := as.character(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := str_trim(!!sym(col_name_new))) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "ne.*sait.*pas|0=") ~ "0",
      str_detect(str_to_lower(!!sym(col_name_new)), "^1=|oui") ~ "1",
      str_detect(str_to_lower(!!sym(col_name_new)), "^2=|non") ~ "0",
      TRUE ~ !!sym(col_name_new)
    ))
  
  cat("✓ Colonne transformée\n")
  cat("Valeurs après transformation :\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Avez-vous observé des changements climatiques affectant votre élevage ?' non trouvée\n")
}

#16. REMPLISSAGE CELLULES VIDES "Comment (quelles sont vos observations..."
# Chercher toutes les colonnes contenant "comment" ou "quelles sont vos observations" dans la section 10
col_observations_cc <- names(raw)[grep("10\\.).*comment|10\\.).*quelles.*observations|Contexte.*environnemental.*comment|Contexte.*environnemental.*observations", names(raw), ignore.case = TRUE)]

if (length(col_observations_cc) > 0) {
  cat("\nColonnes trouvées:", length(col_observations_cc), "\n")
  print(col_observations_cc)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_observations_cc) {
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
  cat("\n⚠ Aucune colonne 'Comment (quelles sont vos observations...' trouvée\n")
}

#17. REMPLISSAGE CELLULES VIDES "Si oui, lesquels ? /changements climatiques"
# Chercher toutes les colonnes contenant "si oui, lesquels" dans la section 10
col_lesquels_cc <- names(raw)[grep("10\\.).*si.*oui.*lesquels|Contexte.*environnemental.*si.*oui.*lesquels", names(raw), ignore.case = TRUE)]

if (length(col_lesquels_cc) > 0) {
  cat("\nColonnes trouvées:", length(col_lesquels_cc), "\n")
  print(col_lesquels_cc)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_lesquels_cc) {
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
  cat("\n⚠ Aucune colonne 'Si oui, lesquels ?' trouvée dans la section 10\n")
}

#18. REMPLISSAGE CELLULES VIDES "Comment ces changements ont-ils influencé vos pratiques de croisement ?
# Chercher toutes les colonnes contenant "comment ces changements ont-ils influencé"
col_influence_pratiques <- names(raw)[grep("comment.*ces.*changements.*ont.*ils.*influencé|comment.*ces.*changements.*ont.*ils.*influence", names(raw), ignore.case = TRUE)]

if (length(col_influence_pratiques) > 0) {
  cat("\nColonnes trouvées:", length(col_influence_pratiques), "\n")
  print(col_influence_pratiques)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_influence_pratiques) {
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
  cat("\n⚠ Aucune colonne 'Comment ces changements ont-ils influencé vos pratiques de croisement ?' trouvée\n")
}

#19. REMPLISSAGE CELLULES VIDES "Quelles stratégies avez-vous mises en place pour adapter votre élevage...
# Chercher toutes les colonnes contenant "quelles stratégies avez-vous mises en place"
col_strategies_adaptation <- names(raw)[grep("quelles.*stratégies.*avez-vous.*mises.*place|quelles.*strategies.*avez.*vous.*mises.*place", names(raw), ignore.case = TRUE)]

if (length(col_strategies_adaptation) > 0) {
  cat("\nColonnes trouvées:", length(col_strategies_adaptation), "\n")
  print(col_strategies_adaptation)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_strategies_adaptation) {
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
  cat("\n⚠ Aucune colonne 'Quelles stratégies avez-vous mises en place pour adapter votre élevage...' trouvée\n")
}

#20. Rename and transform morphologiques differences column
old_col_name_morpho <- "6.) Caractéristiques des Chèvres Métissées/Avez-vous observé des différences morphologiques entre les chèvres métissées et la race locale Djallonké ?: 1=Oui, 0=Non, 3=Aucune idee"
new_col_name_morpho <- "6.) Caractéristiques des Chèvres Métissées/Avez-vous observé des différences morphologiques entre les chèvres métissées et la race locale Djallonké ?: 0=Non, 1=Oui"

if (old_col_name_morpho %in% names(raw)) {
  cat("\nTraitement de la colonne morphologiques...\n")
  cat("Valeurs avant :\n")
  print(table(raw[[old_col_name_morpho]], useNA = "ifany"))

  raw <- raw %>%
    rename(!!new_col_name_morpho := all_of(old_col_name_morpho)) %>%
    mutate(
      !!new_col_name_morpho := as.character(!!sym(new_col_name_morpho)),
      !!new_col_name_morpho := str_trim(!!sym(new_col_name_morpho)),
      !!new_col_name_morpho := case_when(
        is.na(!!sym(new_col_name_morpho)) | !!sym(new_col_name_morpho) == "" ~ "0",
        str_detect(str_to_lower(!!sym(new_col_name_morpho)), "^non$|^0$") ~ "0",
        str_detect(str_to_lower(!!sym(new_col_name_morpho)), "^oui$|^1$") ~ "1",
        str_detect(str_to_lower(!!sym(new_col_name_morpho)), "aucune.*idée|aucune.*idee") ~ "0",
        .default = "0"
      )
    )

  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après :\n")
  print(table(raw[[new_col_name_morpho]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne morphologiques non trouvée\n")
}

#21. Rename and transform métis size compared to Djallonke column
old_col_name_size <- "6.) Caractéristiques des Chèvres Métissées/Si oui quelle différence: La Taille des metis est ..... que celle des Djallonke: 1: Plus Grande, 2: Vide"
new_col_name_size <- "6.) Caractéristiques des Chèvres Métissées/Si oui quelle différence: La Taille des metis est ..... que celle des Djallonke:0=Non, 1=Plus Grande"

if (old_col_name_size %in% names(raw)) {
  cat("\nTraitement de la colonne taille metis...\n")
  cat("Valeurs avant :\n")
  print(table(raw[[old_col_name_size]], useNA = "ifany"))

  raw <- raw %>%
    rename(!!new_col_name_size := all_of(old_col_name_size)) %>%
    mutate(
      !!new_col_name_size := as.character(!!sym(new_col_name_size)),
      !!new_col_name_size := str_trim(!!sym(new_col_name_size)),
      !!new_col_name_size := case_when(
        is.na(!!sym(new_col_name_size)) | !!sym(new_col_name_size) == "" ~ "0",
        str_detect(str_to_lower(!!sym(new_col_name_size)), "plus.*grande") ~ "1",
        .default = "0"
      )
    )

  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après :\n")
  print(table(raw[[new_col_name_size]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne taille metis non trouvée\n")
}

#22. Rename and transform couleur distinction column
old_col_name_couleur <- "6.) Caractéristiques des Chèvres Métissées/Autres traits de distinction entre metis et Djallonke/couleur"
new_col_name_couleur <- "6.) Caractéristiques des Chèvres Métissées/Autres traits de distinction entre metis et Djallonke/couleur/0=Non, 1=Oui"

if (old_col_name_couleur %in% names(raw)) {
  cat("\nTraitement de la colonne couleur...\n")
  cat("Valeurs avant :\n")
  print(table(raw[[old_col_name_couleur]], useNA = "ifany"))

  raw <- raw %>%
    rename(!!new_col_name_couleur := all_of(old_col_name_couleur)) %>%
    mutate(
      !!new_col_name_couleur := as.character(!!sym(new_col_name_couleur)),
      !!new_col_name_couleur := str_trim(!!sym(new_col_name_couleur)),
      !!new_col_name_couleur := case_when(
        is.na(!!sym(new_col_name_couleur)) | !!sym(new_col_name_couleur) == "" ~ "0",
        .default = !!sym(new_col_name_couleur)
      )
    )

  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après :\n")
  print(table(raw[[new_col_name_couleur]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne couleur non trouvée\n")
}

#23. Rename and transform forme des cornes distinction column
old_col_name_cornes <- "6.) Caractéristiques des Chèvres Métissées/Autres traits de distinction entre metis et Djallonke/forme des cornes"
new_col_name_cornes <- "6.) Caractéristiques des Chèvres Métissées/Autres traits de distinction entre metis et Djallonke/forme des cornes/0=Non, 1=Oui"

if (old_col_name_cornes %in% names(raw)) {
  cat("\nTraitement de la colonne forme des cornes...\n")
  cat("Valeurs avant :\n")
  print(table(raw[[old_col_name_cornes]], useNA = "ifany"))

  raw <- raw %>%
    rename(!!new_col_name_cornes := all_of(old_col_name_cornes)) %>%
    mutate(
      !!new_col_name_cornes := as.character(!!sym(new_col_name_cornes)),
      !!new_col_name_cornes := str_trim(!!sym(new_col_name_cornes)),
      !!new_col_name_cornes := case_when(
        is.na(!!sym(new_col_name_cornes)) | !!sym(new_col_name_cornes) == "" ~ "0",
        .default = !!sym(new_col_name_cornes)
      )
    )

  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après :\n")
  print(table(raw[[new_col_name_cornes]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne forme des cornes non trouvée\n")
}

#24. Rename and transform forme des mamelles distinction column
old_col_name_mamelles <- "6.) Caractéristiques des Chèvres Métissées/Autres traits de distinction entre metis et Djallonke/forme des mamelles"
new_col_name_mamelles <- "6.) Caractéristiques des Chèvres Métissées/Autres traits de distinction entre metis et Djallonke/forme des mamelles/0=Non, 1=Oui"

if (old_col_name_mamelles %in% names(raw)) {
  cat("\nTraitement de la colonne forme des mamelles...\n")
  cat("Valeurs avant :\n")
  print(table(raw[[old_col_name_mamelles]], useNA = "ifany"))

  raw <- raw %>%
    rename(!!new_col_name_mamelles := all_of(old_col_name_mamelles)) %>%
    mutate(
      !!new_col_name_mamelles := as.character(!!sym(new_col_name_mamelles)),
      !!new_col_name_mamelles := str_trim(!!sym(new_col_name_mamelles)),
      !!new_col_name_mamelles := case_when(
        is.na(!!sym(new_col_name_mamelles)) | !!sym(new_col_name_mamelles) == "" ~ "0",
        .default = !!sym(new_col_name_mamelles)
      )
    )

  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après :\n")
  print(table(raw[[new_col_name_mamelles]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne forme des mamelles non trouvée\n")
}

#25. Rename and transform croissance performance column
old_col_name_croissance <- "7.) Performances des métis/Croissance /Meilleure que la race Djallonke"
new_col_name_croissance <- "7.) Performances des métis/Croissance /Meilleure que la race Djallonke/0=Non, 1=Oui"

if (old_col_name_croissance %in% names(raw)) {
  cat("\nTraitement de la colonne croissance...\n")
  cat("Valeurs avant :\n")
  print(table(raw[[old_col_name_croissance]], useNA = "ifany"))

  raw <- raw %>%
    rename(!!new_col_name_croissance := all_of(old_col_name_croissance)) %>%
    mutate(
      !!new_col_name_croissance := as.character(!!sym(new_col_name_croissance)),
      !!new_col_name_croissance := str_trim(!!sym(new_col_name_croissance)),
      !!new_col_name_croissance := case_when(
        is.na(!!sym(new_col_name_croissance)) | !!sym(new_col_name_croissance) == "" ~ "0",
        .default = !!sym(new_col_name_croissance)
      )
    )

  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après :\n")
  print(table(raw[[new_col_name_croissance]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne croissance non trouvée\n")
}

#26. Rename and transform croissance similaire performance column
old_col_name_croissance_sim <- "7.) Performances des métis/Croissance /Similaire que la race Djallonke"
new_col_name_croissance_sim <- "7.) Performances des métis/Croissance /Similaire que la race Djallonke/0=Non, 1=Oui"

if (old_col_name_croissance_sim %in% names(raw)) {
  cat("\nTraitement de la colonne croissance similaire...\n")
  cat("Valeurs avant :\n")
  print(table(raw[[old_col_name_croissance_sim]], useNA = "ifany"))

  raw <- raw %>%
    rename(!!new_col_name_croissance_sim := all_of(old_col_name_croissance_sim)) %>%
    mutate(
      !!new_col_name_croissance_sim := as.character(!!sym(new_col_name_croissance_sim)),
      !!new_col_name_croissance_sim := str_trim(!!sym(new_col_name_croissance_sim)),
      !!new_col_name_croissance_sim := case_when(
        is.na(!!sym(new_col_name_croissance_sim)) | !!sym(new_col_name_croissance_sim) == "" ~ "0",
        .default = !!sym(new_col_name_croissance_sim)
      )
    )

  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après :\n")
  print(table(raw[[new_col_name_croissance_sim]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne croissance similaire non trouvée\n")
}

#27. Rename and transform croissance moins bonne performance column
old_col_name_croissance_moins <- "7.) Performances des métis/Croissance /Moins bonne que la race Djallonke"
new_col_name_croissance_moins <- "7.) Performances des métis/Croissance /Moins bonne que la race Djallonke/0=Non, 1=Oui"

if (old_col_name_croissance_moins %in% names(raw)) {
  cat("\nTraitement de la colonne croissance moins bonne...\n")
  cat("Valeurs avant :\n")
  print(table(raw[[old_col_name_croissance_moins]], useNA = "ifany"))

  raw <- raw %>%
    rename(!!new_col_name_croissance_moins := all_of(old_col_name_croissance_moins)) %>%
    mutate(
      !!new_col_name_croissance_moins := as.character(!!sym(new_col_name_croissance_moins)),
      !!new_col_name_croissance_moins := str_trim(!!sym(new_col_name_croissance_moins)),
      !!new_col_name_croissance_moins := case_when(
        is.na(!!sym(new_col_name_croissance_moins)) | !!sym(new_col_name_croissance_moins) == "" ~ "0",
        .default = !!sym(new_col_name_croissance_moins)
      )
    )

  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après :\n")
  print(table(raw[[new_col_name_croissance_moins]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne croissance moins bonne non trouvée\n")
}

#28. Rename and transform résistance aux maladies meilleure performance column
old_col_name_resistance_mieux <- "7.) Performances des métis/Résistance aux maladies /Meilleure que la race Djallonke"
new_col_name_resistance_mieux <- "7.) Performances des métis/Résistance aux maladies /Meilleure que la race Djallonke/0=Non, 1=Oui"

if (old_col_name_resistance_mieux %in% names(raw)) {
  cat("\nTraitement de la colonne résistance aux maladies meilleure...\n")
  cat("Valeurs avant :\n")
  print(table(raw[[old_col_name_resistance_mieux]], useNA = "ifany"))

  raw <- raw %>%
    rename(!!new_col_name_resistance_mieux := all_of(old_col_name_resistance_mieux)) %>%
    mutate(
      !!new_col_name_resistance_mieux := as.character(!!sym(new_col_name_resistance_mieux)),
      !!new_col_name_resistance_mieux := str_trim(!!sym(new_col_name_resistance_mieux)),
      !!new_col_name_resistance_mieux := case_when(
        is.na(!!sym(new_col_name_resistance_mieux)) | !!sym(new_col_name_resistance_mieux) == "" ~ "0",
        .default = !!sym(new_col_name_resistance_mieux)
      )
    )

  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après :\n")
  print(table(raw[[new_col_name_resistance_mieux]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne résistance aux maladies meilleure non trouvée\n")
}

#29. Rename and transform résistance aux maladies similaire performance column
old_col_name_resistance_sim <- "7.) Performances des métis/Résistance aux maladies /Similaire que la race Djallonke"
new_col_name_resistance_sim <- "7.) Performances des métis/Résistance aux maladies /Similaire que la race Djallonke/0=Non, 1=Oui"

if (old_col_name_resistance_sim %in% names(raw)) {
  cat("\nTraitement de la colonne résistance aux maladies similaire...\n")
  cat("Valeurs avant :\n")
  print(table(raw[[old_col_name_resistance_sim]], useNA = "ifany"))

  raw <- raw %>%
    rename(!!new_col_name_resistance_sim := all_of(old_col_name_resistance_sim)) %>%
    mutate(
      !!new_col_name_resistance_sim := as.character(!!sym(new_col_name_resistance_sim)),
      !!new_col_name_resistance_sim := str_trim(!!sym(new_col_name_resistance_sim)),
      !!new_col_name_resistance_sim := case_when(
        is.na(!!sym(new_col_name_resistance_sim)) | !!sym(new_col_name_resistance_sim) == "" ~ "0",
        .default = !!sym(new_col_name_resistance_sim)
      )
    )

  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après :\n")
  print(table(raw[[new_col_name_resistance_sim]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne résistance aux maladies similaire non trouvée\n")
}

#30. Rename and transform résistance aux maladies moins bonne performance column
old_col_name_resistance_moins <- "7.) Performances des métis/Résistance aux maladies /Moins bonne que la race Djallonke"
new_col_name_resistance_moins <- "7.) Performances des métis/Résistance aux maladies /Moins bonne que la race Djallonke/0=Non, 1=Oui"

if (old_col_name_resistance_moins %in% names(raw)) {
  cat("\nTraitement de la colonne résistance aux maladies moins bonne...\n")
  cat("Valeurs avant :\n")
  print(table(raw[[old_col_name_resistance_moins]], useNA = "ifany"))

  raw <- raw %>%
    rename(!!new_col_name_resistance_moins := all_of(old_col_name_resistance_moins)) %>%
    mutate(
      !!new_col_name_resistance_moins := as.character(!!sym(new_col_name_resistance_moins)),
      !!new_col_name_resistance_moins := str_trim(!!sym(new_col_name_resistance_moins)),
      !!new_col_name_resistance_moins := case_when(
        is.na(!!sym(new_col_name_resistance_moins)) | !!sym(new_col_name_resistance_moins) == "" ~ "0",
        .default = !!sym(new_col_name_resistance_moins)
      )
    )

  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après :\n")
  print(table(raw[[new_col_name_resistance_moins]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne résistance aux maladies moins bonne non trouvée\n")
}

#31.---- EXPORT DES DONNEES NETTOYEES ------------------------------------------------
cat("\n=== EXPORT DES DONNEES NETTOYEES ===\n")

# Exporter en Excel
output_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning/data6.xlsx"

# Essayer avec openxlsx (recommandé)
if (!require(openxlsx, quietly = TRUE)) {
  cat("Installation de openxlsx...\n")
  install.packages("openxlsx")
  library(openxlsx)
}

write.xlsx(raw, output_path, rowNames = FALSE, overwrite = TRUE)
cat("Fichier Excel sauvegardé :", output_path, "\n")
cat("Dimensions finales :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")

