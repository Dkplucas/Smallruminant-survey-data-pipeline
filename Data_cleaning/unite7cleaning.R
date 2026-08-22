#Cleaning of 11.) and 12.)####### (UE) 

# library needed
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)

#A. Import the data1 exported from the unite1cleaning.R to continue the cleaning
# Attention le dossier d'importation a changer veuiller a corriger en fontion de votre dossier de travail
getwd()
setwd("C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning")
xlsx_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning/data6.xlsx"
raw <- read_excel(xlsx_path, col_names = TRUE)
cat("Fichier charge :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
raw <- read_excel("data6.xlsx")

#E. Data cleaning

#-----------------------------------11.) Suivi sanitaire-------------------------------------------------

#1. REMPLISSAGE CELLULES VIDES "Déparasitez-vous vos animaux ?
# Chercher toutes les colonnes contenant "déparasitez-vous vos animaux"
col_deparasitage <- names(raw)[grep("déparasitez.*vous.*animaux|deparasitez.*vous.*animaux", names(raw), ignore.case = TRUE)]

if (length(col_deparasitage) > 0) {
  cat("\nColonnes trouvées:", length(col_deparasitage), "\n")
  print(col_deparasitage)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_deparasitage) {
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
  cat("\n⚠ Aucune colonne 'Déparasitez-vous vos animaux ?' trouvée\n")
}

#2. REMPLISSAGE CELLULES VIDES "Quels sont les animaux déparasités ?
# Chercher toutes les colonnes contenant "quels sont les animaux déparasités"
col_animaux_deparasites <- names(raw)[grep("quels.*sont.*animaux.*déparasités|quels.*sont.*animaux.*deparasites", names(raw), ignore.case = TRUE)]

if (length(col_animaux_deparasites) > 0) {
  cat("\nColonnes trouvées:", length(col_animaux_deparasites), "\n")
  print(col_animaux_deparasites)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_animaux_deparasites) {
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
  cat("\n⚠ Aucune colonne 'Quels sont les animaux déparasités ?' trouvée\n")
}

#3. REMPLISSAGE CELLULES VIDES "Vaccinez-vous vos animaux ?
# Chercher toutes les colonnes contenant "vaccinez-vous vos animaux"
col_vaccination <- names(raw)[grep("vaccinez.*vous.*animaux", names(raw), ignore.case = TRUE)]

if (length(col_vaccination) > 0) {
  cat("\nColonnes trouvées:", length(col_vaccination), "\n")
  print(col_vaccination)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_vaccination) {
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
  cat("\n⚠ Aucune colonne 'Vaccinez-vous vos animaux ?' trouvée\n")
}

#4. REMPLISSAGE CELLULES VIDES "Si oui, à quel moment de l'année ?
# Chercher toutes les colonnes contenant "si oui, à quel moment de l'année" dans la section 11
col_moment_vaccination <- names(raw)[grep("11\\.).*si.*oui.*quel.*moment.*année|Suivi.*sanitaire.*si.*oui.*quel.*moment.*annee", names(raw), ignore.case = TRUE)]

if (length(col_moment_vaccination) > 0) {
  cat("\nColonnes trouvées:", length(col_moment_vaccination), "\n")
  print(col_moment_vaccination)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_moment_vaccination) {
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
  cat("\n⚠ Aucune colonne 'Si oui, à quel moment de l'année ?' trouvée dans la section 11\n")
}

#5. REMPLISSAGE CELLULES VIDES "Les animaux provenant d'un autre élevage ou marché sont-ils mis en quarantaine ?
# Chercher toutes les colonnes contenant "animaux provenant d'un autre élevage" et "quarantaine"
col_quarantaine <- names(raw)[grep("animaux.*provenant.*autre.*élevage.*quarantaine|animaux.*provenant.*autre.*elevage.*quarantaine", names(raw), ignore.case = TRUE)]

if (length(col_quarantaine) > 0) {
  cat("\nColonnes trouvées:", length(col_quarantaine), "\n")
  print(col_quarantaine)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_quarantaine) {
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
  cat("\n⚠ Aucune colonne 'Les animaux provenant d'un autre élevage ou marché sont-ils mis en quarantaine ?' trouvée\n")
}

#6. REMPLISSAGE CELLULES VIDES "Y-a-t-il des cas de mortalités liées aux maladies ?
# Chercher toutes les colonnes contenant "y-a-t-il des cas de mortalités liées aux maladies"
col_mortalites <- names(raw)[grep("y.*a.*t.*il.*cas.*mortalités.*maladies|y.*a.*t.*il.*cas.*mortalites.*maladies", names(raw), ignore.case = TRUE)]

if (length(col_mortalites) > 0) {
  cat("\nColonnes trouvées:", length(col_mortalites), "\n")
  print(col_mortalites)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_mortalites) {
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
  cat("\n⚠ Aucune colonne 'Y-a-t-il des cas de mortalités liées aux maladies ?' trouvée\n")
}

#7. REMPLISSAGE CELLULES VIDES "Si oui, quelles sont ces maladies ? /maladies
# Chercher toutes les colonnes contenant "si oui, quelles sont ces maladies" dans la section 11
col_quelles_maladies <- names(raw)[grep("11\\.).*si.*oui.*quelles.*maladies|Suivi.*sanitaire.*si.*oui.*quelles.*maladies", names(raw), ignore.case = TRUE)]

if (length(col_quelles_maladies) > 0) {
  cat("\nColonnes trouvées:", length(col_quelles_maladies), "\n")
  print(col_quelles_maladies)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_quelles_maladies) {
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
  cat("\n⚠ Aucune colonne 'Si oui, quelles sont ces maladies ?' trouvée dans la section 11\n")
}

#8. REMPLISSAGE CELLULES VIDES "Enregistrez-vous plus de cas de maladies chez les chèvres de races Djallonké..."
# Chercher toutes les colonnes contenant "enregistrez-vous plus de cas de maladies chez les chèvres"
col_plus_maladies_djallonke <- names(raw)[grep("enregistrez.*vous.*plus.*cas.*maladies.*chèvres|enregistrez.*vous.*plus.*cas.*maladies.*chevres", names(raw), ignore.case = TRUE)]

if (length(col_plus_maladies_djallonke) > 0) {
  cat("\nColonnes trouvées:", length(col_plus_maladies_djallonke), "\n")
  print(col_plus_maladies_djallonke)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_plus_maladies_djallonke) {
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
  cat("\n⚠ Aucune colonne 'Enregistrez-vous plus de cas de maladies chez les chèvres de races Djallonké...' trouvée\n")
}

#9. REMPLISSAGE CELLULES VIDES "Si oui, quelles sont les causes selon vous ?"
# Chercher toutes les colonnes contenant "si oui, quelles sont les causes selon vous" dans la section 11
col_causes_maladies <- names(raw)[grep("11\\.).*si.*oui.*quelles.*causes.*selon.*vous|Suivi.*sanitaire.*si.*oui.*quelles.*causes", names(raw), ignore.case = TRUE)]

if (length(col_causes_maladies) > 0) {
  cat("\nColonnes trouvées:", length(col_causes_maladies), "\n")
  print(col_causes_maladies)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_causes_maladies) {
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
  cat("\n⚠ Aucune colonne 'Si oui, quelles sont les causes selon vous ?' trouvée dans la section 11\n")
}

#10. REMPLISSAGE CELLULES VIDES "Enregistrez-vous plus cas de mortalité chez les chèvres de races Djallonké...
# Chercher toutes les colonnes contenant "enregistrez-vous plus.*cas.*mortalité.*chèvres"
col_plus_mortalite_djallonke <- names(raw)[grep("enregistrez.*vous.*plus.*cas.*mortalité.*chèvres|enregistrez.*vous.*plus.*cas.*mortalite.*chevres", names(raw), ignore.case = TRUE)]

if (length(col_plus_mortalite_djallonke) > 0) {
  cat("\nColonnes trouvées:", length(col_plus_mortalite_djallonke), "\n")
  print(col_plus_mortalite_djallonke)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_plus_mortalite_djallonke) {
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
  cat("\n⚠ Aucune colonne 'Enregistrez-vous plus cas de mortalité chez les chèvres de races Djallonké...' trouvée\n")
}

#11. REMPLISSAGE CELLULES VIDES "Si oui, quelles sont les causes selon vous ? /causes de mortalité"
# Chercher toutes les colonnes contenant "si oui, quelles sont les causes" qui n'ont pas déjà été traitées
# Cette section cible spécifiquement les causes de mortalité dans la section 11
col_causes_mortalite <- names(raw)[grep("si.*oui.*quelles.*causes.*selon.*vous", names(raw), ignore.case = TRUE)]

# Filtrer pour exclure les colonnes déjà traitées en 11CM
col_causes_mortalite <- setdiff(col_causes_mortalite, col_causes_maladies)

if (length(col_causes_mortalite) > 0) {
  cat("\nColonnes trouvées:", length(col_causes_mortalite), "\n")
  print(col_causes_mortalite)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_causes_mortalite) {
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
  cat("\n⚠ Aucune colonne supplémentaire 'Si oui, quelles sont les causes selon vous ?' trouvée\n")
}

#12. REMPLISSAGE CELLULES VIDES "Nbre de jeunes (>6 mois) mâles_Sahelien"
# Chercher toutes les colonnes contenant "nbre de jeunes" ou "effectif.*composition.*cheptel"
col_nbre_jeunes <- names(raw)[grep("nbre.*jeunes|effectif.*composition.*cheptel", names(raw), ignore.case = TRUE)]

if (length(col_nbre_jeunes) > 0) {
  cat("\nColonnes trouvées:", length(col_nbre_jeunes), "\n")
  print(col_nbre_jeunes)
  
  # Remplir les cellules vides par "0" pour chaque colonne
  for (col in col_nbre_jeunes) {
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
  cat("\n⚠ Aucune colonne 'Nbre de jeunes' trouvée\n")
}

#13. TRANSFORMATION "Si oui, lesquelles ?" - Perceptions et adaptations -------
# Renommer et transformer la colonne avec les différents types de croisement
old_col_name_perceptions <- "9.) Perceptions et adaptations/Si oui, lesquelles ?"
new_col_name_perceptions <- "9.) Perceptions et adaptations/Si oui, lesquelles ? 0=Vides, 1=Crossing, 2=Backcrossing, 3=Crossing & Backcrossing"

if (old_col_name_perceptions %in% names(raw)) {
  cat("\n=== TRANSFORMATION: Perceptions et adaptations/Si oui, lesquelles ? ===\n")
  cat("Valeurs avant transformation:\n")
  print(table(raw[[old_col_name_perceptions]], useNA = "ifany"))
  
  raw <- raw %>%
    rename(!!new_col_name_perceptions := all_of(old_col_name_perceptions)) %>%
    mutate(
      !!new_col_name_perceptions := as.character(!!sym(new_col_name_perceptions)),
      !!new_col_name_perceptions := str_trim(!!sym(new_col_name_perceptions)),
      !!new_col_name_perceptions := case_when(
        # Empty cells -> "0"
        is.na(!!sym(new_col_name_perceptions)) | !!sym(new_col_name_perceptions) == "" ~ "0",
        
        # Group 3 (Crossing & Backcrossing) -> "3" (multiple combinations with "ou x")
        str_detect(str_to_lower(!!sym(new_col_name_perceptions)), ".*ou\\s+x\\s+|.*ou\\s+x\\s*métis|.*ou\\s+x\\s*djallonk|.*ou\\s+x\\s*avec") ~ "3",
        
        # Group 2 (Backcrossing) -> "2"
        str_detect(str_to_lower(!!sym(new_col_name_perceptions)), "race\\s+pure\\s+sahélienne\\s+x\\s+métis|sahélienne\\s+x\\s+métis|bélier\\s+sahélien\\s+x\\s+brebis\\s+métis|métis\\s+x\\s+sahélien|métis\\s+de\\s+génération|sahélien\\s+x\\s+métis\\s+f1|métis\\s+performant\\s+x\\s+djallonk|croisement\\s+de\\s+métis\\s+ou\\s+djallonk|bélier.*métis|race\\s+sahélienne\\s+x\\s+métis") ~ "2",
        
        # Group 1 (Crossing) -> "1" - all the various crossing types
        str_detect(str_to_lower(!!sym(new_col_name_perceptions)), "bouc\\s+sahélien|croisement\\s+entre\\s+race\\s+locale|métis\\s+x\\s+métis|sahélien\\s+x\\s+djallonk|race\\s+exotique\\s+x|sahélienne\\s+x\\s+race\\s+exotique|sahélien\\s+x\\s+exotique|race\\s+sahélienne\\s+pure|races\\s+exotiques\\s+x\\s+djallonk|races\\s+nigériennes\\s+x|autres\\s+races\\s+performantes\\s+x\\s+métis|adoptez.*élevage|prête\\s+à\\s+essayer|insémination\\s+artificielle.*femelles|si.*race\\s+exotique|des\\s+races\\s+plus\\s+performantes|race\\s+exotique\\s+performante\\s+et\\s+rustique|d'autres\\s+races\\s+performances|si\\s+possibilité|proposer\\s+reproducteurs|race\\s+sahélienne\\s+ou\\s+exotique|desire\\s+avoir|pure\\s+sahélien\\s+x|sahélien\\s+pur\\s+x|lui\\s+proposer|l'éleveur\\s+souhaite|autres\\s+mâles\\s+de\\s+race\\s+exotique|autres\\s+races\\s+jugées\\s+performantes") ~ "1",
        
        .default = "0"
      )
    )
  
  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après transformation:\n")
  print(table(raw[[new_col_name_perceptions]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Si oui, lesquelles ?' (Perceptions et adaptations) non trouvée\n")
}

#14. TRANSFORMATION "Si « non », pourquoi ?" - Perceptions et adaptations ----
# Renommer et transformer la colonne avec les raisons du refus
old_col_name_pourquoi_non <- "9.) Perceptions et adaptations/Si « non », pourquoi ?"
new_col_name_pourquoi_non <- "9.) Perceptions et adaptations/Si « non », pourquoi ? 0=Vides, 1=Deception dans le system, 2=Pas de temp/consommation personnelle, 3=Financier"

if (old_col_name_pourquoi_non %in% names(raw)) {
  cat("\n=== TRANSFORMATION: Perceptions et adaptations/Si « non », pourquoi ? ===\n")
  cat("Valeurs avant transformation:\n")
  print(table(raw[[old_col_name_pourquoi_non]], useNA = "ifany"))
  
  raw <- raw %>%
    rename(!!new_col_name_pourquoi_non := all_of(old_col_name_pourquoi_non)) %>%
    mutate(
      !!new_col_name_pourquoi_non := as.character(!!sym(new_col_name_pourquoi_non)),
      !!new_col_name_pourquoi_non := str_trim(!!sym(new_col_name_pourquoi_non)),
      !!new_col_name_pourquoi_non := case_when(
        # Empty cells -> "0"
        is.na(!!sym(new_col_name_pourquoi_non)) | !!sym(new_col_name_pourquoi_non) == "" ~ "0",
        
        # Group 3 (Financier / Infrastructure) -> "3" - check first for longer patterns
        str_detect(str_to_lower(!!sym(new_col_name_pourquoi_non)), "ne.*disposant.*pas.*d'un.*bâtiment|voudrais.*d'abord.*avoir.*un.*minimum|n'est.*pas.*bien.*stable.*pour.*mieux.*suivre|souhaite.*s'installer.*sur.*sa.*ferme.*pour.*mieux.*s'organiser|lancer.*les.*activités.*d'élevage|n'est.*pas.*bien.*stable") ~ "3",
        
        # Group 1 (Deception dans le system) -> "1"
        str_detect(str_to_lower(!!sym(new_col_name_pourquoi_non)), "éleveur.*ayant.*été.*déçu.*par.*le.*passé|déçu.*par.*des.*projets.*et.*programmes.*non.*concluants|n'attend.*rien.*de.*l'etat.*et.*d'autres.*structures|cas.*de.*vols.*suite.*à.*ses.*précédentes.*collaboration|n'y.*crois.*pas.*à.*une.*initiative.*en.*faveur.*des.*éleveurs|marginalisation.*suite.*à.*une.*sélection|déçu|méfiant.*visà.*vis|n'attend.*rien.*de") ~ "1",
        
        # Group 2 (Pas de temps / consommation personnelle) -> "2"
        str_detect(str_to_lower(!!sym(new_col_name_pourquoi_non)), "élevage.*orientée.*vers.*la.*consommation|n'a.*pas.*assez.*de.*temps|n'a.*pas.*assez.*de.*temps") ~ "2",
        
        .default = "0"
      )
    )
  
  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après transformation:\n")
  print(table(raw[[new_col_name_pourquoi_non]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Si « non », pourquoi ?' (Perceptions et adaptations) non trouvée\n")
}

#15. TRANSFORMATION "Aucune observation faite établissant un lien entre CC et l'élevage" --------
# Renommer et transformer la colonne sur le lien entre changement climatique et élevage
old_col_name_cc_lien <- "Aucune observation faite établissant un lien entre CC et l'élevage"
new_col_name_cc_lien <- "Aucune observation faite établissant un lien entre CC et l'élevage/0=Vides, 1=Non lier au climat, 2=lier au climat"

if (old_col_name_cc_lien %in% names(raw)) {
  cat("\n=== TRANSFORMATION: Lien entre CC et élevage ===\n")
  cat("Valeurs avant transformation:\n")
  print(table(raw[[old_col_name_cc_lien]], useNA = "ifany"))
  
  raw <- raw %>%
    rename(!!new_col_name_cc_lien := all_of(old_col_name_cc_lien)) %>%
    mutate(
      !!new_col_name_cc_lien := as.character(!!sym(new_col_name_cc_lien)),
      !!new_col_name_cc_lien := str_trim(!!sym(new_col_name_cc_lien)),
      !!new_col_name_cc_lien := case_when(
        # Empty cells -> "0"
        is.na(!!sym(new_col_name_cc_lien)) | !!sym(new_col_name_cc_lien) == "" ~ "0",
        
        # Group 2 (Lier au climat) -> "2" - climate-related observations
        str_detect(str_to_lower(!!sym(new_col_name_cc_lien)), "disponible.*fourrager.*raréfie|rareté.*pluies|chaleur.*intense|période.*saisons.*décalées|irrégularité.*pluies|pluies.*ne.*sont.*plus.*régulières|saison.*sèche.*plus.*longue|humidité.*plus.*forte|affectent.*animaux|s'alimentent.*difficilement|deviennent.*moins.*épanouis|se.*recroquevillent|il.*fait.*plus.*chaud|il.*faut.*plus.*chaud|forte.*pluie.*saison.*pluvieuse|forte.*humidité|saisons.*plus.*en.*plus.*décalée|surabondance.*pluie|décalage.*pluies|longue.*saison.*sèche|difficulté.*d'alimentation") ~ "2",
        
        # Group 1 (Non lier au climat) -> "1"
        str_detect(str_to_lower(!!sym(new_col_name_cc_lien)), "aucune.*observation.*faite|pas.*de.*lien.*direct.*changement.*climatique|ne.*peut.*affirmer.*problèmes.*directement.*liés|perturbation.*pluies.*reconnue.*mais.*n'a.*pas.*pu.*faire.*lien|n'a.*pas.*pu.*faire.*lien|difficulté.*alimentation.*soulignée.*saison.*sèche") ~ "1",
        
        .default = "0"
      )
    )
  
  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après transformation:\n")
  print(table(raw[[new_col_name_cc_lien]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Aucune observation faite établissant un lien entre CC et l'élevage' non trouvée\n")
}

#16. TRANSFORMATION "Quelles sont les maladies récurrentes enregistrées dans votre élevage ?" --------
# Renommer et transformer la colonne sur les maladies récurrentes
old_col_name_maladies <- "11.) Suivi sanitaire/Quelles sont les maladies récurrentes enregistrées dans votre élevage ?."
new_col_name_maladies <- "11.) Suivi sanitaire/Quelles sont les maladies récurrentes enregistrées dans votre élevage ?. 0=Neant, 1=Diarhee, 2=Autres maladies"

if (old_col_name_maladies %in% names(raw)) {
  cat("\n=== TRANSFORMATION: Quelles sont les maladies récurrentes ===\n")
  cat("Valeurs avant transformation:\n")
  print(table(raw[[old_col_name_maladies]], useNA = "ifany"))
  
  raw <- raw %>%
    rename(!!new_col_name_maladies := all_of(old_col_name_maladies)) %>%
    mutate(
      !!new_col_name_maladies := as.character(!!sym(new_col_name_maladies)),
      !!new_col_name_maladies := str_trim(!!sym(new_col_name_maladies)),
      !!new_col_name_maladies := case_when(
        # Empty cells, "Rare," and "Néant" -> "0"
        is.na(!!sym(new_col_name_maladies)) | !!sym(new_col_name_maladies) == "" | 
        str_to_lower(!!sym(new_col_name_maladies)) == "rare," |
        str_to_lower(!!sym(new_col_name_maladies)) == "néant" ~ "0",
        
        # Group 2 (Autres maladies - specific disease descriptions) -> "2"
        !!sym(new_col_name_maladies) == "Peste avec les Sahéliens puis perte, quelques troubles digestifs, gale" |
        !!sym(new_col_name_maladies) == "Gale, adjoulou (abcès aux lèvres des animaux plus jeunes)" |
        !!sym(new_col_name_maladies) == "Diarrhée, gale" |
        !!sym(new_col_name_maladies) == "Diarrhée, constipation, problème respiratoire, sans réaction et l'animal tombe, intoxication après pâturage en début de pluie" |
        !!sym(new_col_name_maladies) == "Diarrhée , gale" |
        !!sym(new_col_name_maladies) == "Diarrhée , agalaxie" |
        !!sym(new_col_name_maladies) == "Pododermatite interdigitée (plaie entre onglons), diarrhée" |
        !!sym(new_col_name_maladies) == "Anorexie et isolement des animaux, diarrhée, gale" |
        !!sym(new_col_name_maladies) == "Gale" |
        !!sym(new_col_name_maladies) == "Signes: Isolement" |
        !!sym(new_col_name_maladies) == "Anorexie, dystocie, gale," |
        !!sym(new_col_name_maladies) == "Refus de s'alimenter" |
        !!sym(new_col_name_maladies) == "Attaque de tiques" |
        !!sym(new_col_name_maladies) == "Diarrhée en debut de saison pluvieuse" |
        !!sym(new_col_name_maladies) == "- Météorisation (ballonnement du rumen)" |
        !!sym(new_col_name_maladies) == "Diarrhée et cas de Météorisation signalé" |
        !!sym(new_col_name_maladies) == "Diarrhée quelques fois" |
        !!sym(new_col_name_maladies) == "Diarrhée chez les agneaux/agnelle, difficulté de mise bas" |
        !!sym(new_col_name_maladies) == "Piétin, les animaux qui bavent alors qu'ils mangent bien et quelques instants après meurent (fièvre aphteuse : soupçonnée)" |
        !!sym(new_col_name_maladies) == "Piétin (les animaux boitent)" |
        !!sym(new_col_name_maladies) == "Refus de s'alimenter " |
        !!sym(new_col_name_maladies) == "Refus de s'alimenter" |
        !!sym(new_col_name_maladies) == "Boitement (piétin)" |
        !!sym(new_col_name_maladies) == "Boitement (plaie entre les onglons)" |
        !!sym(new_col_name_maladies) == "Difficulté respiratoire" |
        !!sym(new_col_name_maladies) == "- Piétin ( plaie entre onglons entraînant le boitement)" |
        !!sym(new_col_name_maladies) == "Abcès aux tétines à la mise-bas" |
        !!sym(new_col_name_maladies) == "Piétin" |
        !!sym(new_col_name_maladies) == "La gale et la diarrhée" |
        !!sym(new_col_name_maladies) == "Perte d'appétit," |
        !!sym(new_col_name_maladies) == "Diarrhée, difficulté respiratoire, piétin" |
        !!sym(new_col_name_maladies) == "Mammite chez les chèvres métis" |
        !!sym(new_col_name_maladies) == "La mammite" |
        !!sym(new_col_name_maladies) == "Mammite" |
        !!sym(new_col_name_maladies) == "Diarrhée, abcès" ~ "2",
        
        # Group 1 (Diarrhée only) -> "1"
        !!sym(new_col_name_maladies) == "Diarrhée" |
        !!sym(new_col_name_maladies) == "Diarrhée, faiblesse générale des animaux" |
        !!sym(new_col_name_maladies) == "Diarrhée," |
        !!sym(new_col_name_maladies) == "Diarrhée chez les plus jeunes sujets" |
        !!sym(new_col_name_maladies) == "Diarrhée en début de saison pluvieuse" |
        !!sym(new_col_name_maladies) == "Diarrhée (en saison pluvieuse)" |
        !!sym(new_col_name_maladies) == "Diarrhée chez les plus jeunes" ~ "1",
        
        .default = "0"
      )
    )
  
  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après transformation:\n")
  print(table(raw[[new_col_name_maladies]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Quelles sont les maladies récurrentes enregistrées dans votre élevage ?' non trouvée\n")
}

#17. TRANSFORMATION "A quel moment apparaissent-elles ?" - Suivi sanitaire --------
# Renommer et transformer la colonne sur le moment d'apparition des maladies
old_col_name_moment <- "11.) Suivi sanitaire/A quel moment apparaissent-elles ?"
new_col_name_moment <- "11.) Suivi sanitaire/A quel moment apparaissent-elles ? 0=Vide, 1=Saison Seche, 2=Saison Pluvieuse, 3=Autres"

if (old_col_name_moment %in% names(raw)) {
  cat("\n=== TRANSFORMATION: A quel moment apparaissent-elles ? ===\n")
  cat("Valeurs avant transformation:\n")
  print(table(raw[[old_col_name_moment]], useNA = "ifany"))
  
  raw <- raw %>%
    rename(!!new_col_name_moment := all_of(old_col_name_moment)) %>%
    mutate(
      !!new_col_name_moment := as.character(!!sym(new_col_name_moment)),
      !!new_col_name_moment := str_trim(!!sym(new_col_name_moment)),
      !!new_col_name_moment := case_when(
        # Empty cells, "Non retenu", and various "Pas de moment" variations -> "0"
        is.na(!!sym(new_col_name_moment)) | !!sym(new_col_name_moment) == "" | 
        !!sym(new_col_name_moment) == "Non retenu" |
        !!sym(new_col_name_moment) == "Peut subvenir à tout moment" |
        !!sym(new_col_name_moment) == "Peu subvenir à n'importe quelle période de l'année" |
        !!sym(new_col_name_moment) == "Pas de moment précis, peut subvenir à tout moment" |
        !!sym(new_col_name_moment) == "Pas de période précise" |
        !!sym(new_col_name_moment) == "Pas de moment précis" |
        !!sym(new_col_name_moment) == "Non déterminé" |
        !!sym(new_col_name_moment) == "Pas moment précis" ~ "0",
        
        # Group 2 (Saison Pluvieuse) -> "2"
        !!sym(new_col_name_moment) == "Saison pluvieuse" |
        !!sym(new_col_name_moment) == "Surtout en début saison pluvieuse" |
        !!sym(new_col_name_moment) == "Début des pluies (Mars -Avril)" |
        !!sym(new_col_name_moment) == "Début saison pluvieuse, fin saison sèche, période de chaleur accrue" |
        !!sym(new_col_name_moment) == "Début saison pluvieuse (reprise des pluies)" |
        !!sym(new_col_name_moment) == "Diarrhée au début des saisons pluvieuses," |
        !!sym(new_col_name_moment) == "Diarrhée : début saison des pluies ;" |
        !!sym(new_col_name_moment) == "- Météorisation : première pousse après la reprise de pluie" |
        !!sym(new_col_name_moment) == "Diarrhée au début de la saison pluvieuse avec cueillette de nouveaux feuilles de fourrage" |
        !!sym(new_col_name_moment) == "Introduction de nouvel aliment et début saison des pluies pour la diarrhée." |
        !!sym(new_col_name_moment) == "Début saison des pluies" |
        !!sym(new_col_name_moment) == "Piétin : début saison pluvieuse" |
        !!sym(new_col_name_moment) == "Début et au cours des saisons pluvieuses" |
        !!sym(new_col_name_moment) == "Début saison pluvieuse ou en cas d'intoxication" |
        !!sym(new_col_name_moment) == " En saison pluvieuse (les mois de Mai, juin et juillet)" |
        !!sym(new_col_name_moment) == "Septembre - Octobre" |
        !!sym(new_col_name_moment) == "Début saison pluvieuse" |
        !!sym(new_col_name_moment) == "Début des pluies" |
        !!sym(new_col_name_moment) == "Début des saisons pluvieuses" |
        !!sym(new_col_name_moment) == "Saison pluvieuse (juin- août)" |
        !!sym(new_col_name_moment) == "Première pluie grand saison pluvieuse" |
        !!sym(new_col_name_moment) == "Début de saison pluvieuse" |
        !!sym(new_col_name_moment) == " En saison pluvieuse" |
        !!sym(new_col_name_moment) == "En août -septembre, période de pluie abondante" |
        !!sym(new_col_name_moment) == "Dans la saison pluvieuse" |
        !!sym(new_col_name_moment) == "En saison pluvieuse , nouvel aliment" |
        !!sym(new_col_name_moment) == "En début saison pluvieuse" |
        !!sym(new_col_name_moment) == "La diarrhée en saison pluvieuse ou en cas d'intoxication" |
        !!sym(new_col_name_moment) == "Saison pluvieuse ou introduction d'un nouvel aliment" |
        !!sym(new_col_name_moment) == "Début et fin Saison pluvieuse" ~ "2",
        
        # Group 3 (Autres - introduction d'aliment, changement d'aliment) -> "3"
        !!sym(new_col_name_moment) == "Introduction d'un nouvel aliment, mauvaise conservation des épluchures de manioc (non séchées) servies" |
        !!sym(new_col_name_moment) == "- A l'introduction d'un nouveau sujet sans mise en quarantaine" |
        !!sym(new_col_name_moment) == "Anorexie et isolement : pas de moment précis" |
        !!sym(new_col_name_moment) == "A l'introduction de nouveaux sujets" |
        !!sym(new_col_name_moment) == "Les blessures surviennent sans période fixe;" |
        !!sym(new_col_name_moment) == "Après mise bas, pas de période spécifique" |
        !!sym(new_col_name_moment) == "Introduction d'un nouvel aliment" |
        !!sym(new_col_name_moment) == "Début saison pluvieuse , et début saison sèche en septembre" |
        !!sym(new_col_name_moment) == "Changement d'aliment" |
        !!sym(new_col_name_moment) == "Pour diarrhée : début saison sèche surtout chez les agneaux et brebis allaitantes" |
        !!sym(new_col_name_moment) == "Après mise-bas" |
        !!sym(new_col_name_moment) == "Diarrhée pas de période précise" |
        !!sym(new_col_name_moment) == "Peut être observé à tout moment de l'année" |
        !!sym(new_col_name_moment) == "Diarrhée en cas de changement d'aliment" |
        !!sym(new_col_name_moment) == "Après 3 mises chez les chèvres métis," |
        !!sym(new_col_name_moment) == "Mammite chez les femelles après deux de carrière," |
        !!sym(new_col_name_moment) == "Après 2 ans ou 3 mises-bas pour les cas de mammite" |
        !!sym(new_col_name_moment) == "Pas de période précise, survient en cas de changement d'aliment" |
        !!sym(new_col_name_moment) == "Changement brusque d'aliment ou introduction brusque d'un nouvel aliment" |
        !!sym(new_col_name_moment) == "L'introduction ou consommation de nouvelles feuilles ou d'aliments, empoisonnement," |
        !!sym(new_col_name_moment) == " Liée à l'aliment sous drèches moisies" |
        !!sym(new_col_name_moment) == "Liée à l'aliment sous drèches moisies" ~ "3",
        
        # Group 1 (Saison Seche) -> "1"
        !!sym(new_col_name_moment) == "Souvent en saison sèche" |
        !!sym(new_col_name_moment) == "Pas de période précise mais fréquence élevée en saison sèche" |
        !!sym(new_col_name_moment) == "Novembre" |
        !!sym(new_col_name_moment) == "Souvent entre juillet août" |
        !!sym(new_col_name_moment) == "Gale: saison sèche " ~ "1",
        
        .default = "0"
      )
    )
  
  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après transformation:\n")
  print(table(raw[[new_col_name_moment]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'A quel moment apparaissent-elles ?' non trouvée\n")
}

#18. TRANSFORMATION "Selon vous, pourquoi ?" - Suivi sanitaire --------
# Renommer et transformer la colonne sur les raisons des maladies
old_col_name_pourquoi <- "11.) Suivi sanitaire/Selon vous, pourquoi ?"
new_col_name_pourquoi <- "11.) Suivi sanitaire/Selon vous, pourquoi ? 0=Vides, 1=Sanitaire, 2=Climat, 3=Forrage"

if (old_col_name_pourquoi %in% names(raw)) {
  cat("\n=== TRANSFORMATION: Selon vous, pourquoi ? ===\n")
  cat("Valeurs avant transformation:\n")
  print(table(raw[[old_col_name_pourquoi]], useNA = "ifany"))
  
  raw <- raw %>%
    rename(!!new_col_name_pourquoi := all_of(old_col_name_pourquoi)) %>%
    mutate(
      !!new_col_name_pourquoi := as.character(!!sym(new_col_name_pourquoi)),
      !!new_col_name_pourquoi := str_trim(!!sym(new_col_name_pourquoi)),
      !!new_col_name_pourquoi := case_when(
        # Empty cells and "don't know" variations -> "0"
        is.na(!!sym(new_col_name_pourquoi)) | !!sym(new_col_name_pourquoi) == "" | 
        !!sym(new_col_name_pourquoi) == "Ne sait pas" |
        !!sym(new_col_name_pourquoi) == "Ne sait pas." |
        !!sym(new_col_name_pourquoi) == "Aucune idée" |
        !!sym(new_col_name_pourquoi) == "Raison non identifiée" |
        !!sym(new_col_name_pourquoi) == "Pas connue" |
        !!sym(new_col_name_pourquoi) == "Nouvelles réponses" |
        !!sym(new_col_name_pourquoi) == "Pas de raison évoquée" |
        !!sym(new_col_name_pourquoi) == "Raison non connue" |
        !!sym(new_col_name_pourquoi) == "Absence de transition" |
        !!sym(new_col_name_pourquoi) == "N'étant pas habitué (absence de  transition alimentaire)" |
        !!sym(new_col_name_pourquoi) == " Aucune idée" |
        !!sym(new_col_name_pourquoi) == "Ne sait pas les raisons" ~ "0",
        
        # Group 1 (Sanitaire - health/food/hygiene/diet change) -> "1"
        !!sym(new_col_name_pourquoi) == "Fragilité de la santé des animaux avec le début de la saison," |
        !!sym(new_col_name_pourquoi) == "Santé fragile, l'intoxication," |
        !!sym(new_col_name_pourquoi) == "L'éleveur opte pour une préparation de décoction mélangée à la bouillie, très souvent servie à titre préventif. Habitat régulièrement balayé." |
        !!sym(new_col_name_pourquoi) == "- Propagation de maladie par le nouvel animal introduit dans l'élevage" |
        !!sym(new_col_name_pourquoi) == "- Taille des nouveaux nés pour la dystocie" |
        !!sym(new_col_name_pourquoi) == "Contamination, propagation de maladie" |
        !!sym(new_col_name_pourquoi) == "Diarrhée liée aux nouvelles pousses" |
        !!sym(new_col_name_pourquoi) == "La dystocie à cause de la taille des chevreaux" |
        !!sym(new_col_name_pourquoi) == "Divagation des animaux" |
        !!sym(new_col_name_pourquoi) == "L'éleveur suppose que c'est quand les animaux ne trouvent pas suffisamment à manger :" |
        !!sym(new_col_name_pourquoi) == "Parasites intestinaux" |
        !!sym(new_col_name_pourquoi) == "Changement de l'habitude alimentaire, nouvelles répousses" |
        !!sym(new_col_name_pourquoi) == "Changement d'aliment" |
        !!sym(new_col_name_pourquoi) == "Nouvel aliment" |
        !!sym(new_col_name_pourquoi) == "Diarrhée liée souvent au changement d'aliment ou aux nouvelles répousses" |
        !!sym(new_col_name_pourquoi) == "Empoisonnement," |
        !!sym(new_col_name_pourquoi) == "En cas de changement d'aliment pour la diarrhée" |
        !!sym(new_col_name_pourquoi) == "Mauvaise alimentation, changement d'aliment, intoxication, baisse de l'immunité de l'animal" |
        !!sym(new_col_name_pourquoi) == "Intoxication alimentaire ou empoisonnement, manque d'hygiène dans la bergerie" |
        !!sym(new_col_name_pourquoi) == "Changement d'aliment, nouvelles répousses" |
        !!sym(new_col_name_pourquoi) == "Manque de transition d'aliment," |
        !!sym(new_col_name_pourquoi) == "Produc abondante de lait" |
        !!sym(new_col_name_pourquoi) == "Nouvel aliment, jeunes pousses et drèche mal conservée" |
        !!sym(new_col_name_pourquoi) == "N'étant pas habitué (manque de transition alimentaire)" |
        !!sym(new_col_name_pourquoi) == "Manque de transition lié à la diarrhée" |
        !!sym(new_col_name_pourquoi) == "Moisissure, mauvaise conservation" |
        !!sym(new_col_name_pourquoi) == "Nouvelles répousses ou intoxication alimentaire" |
        !!sym(new_col_name_pourquoi) == "Manque de transition alimentaire, trouble digestif, ou moisissures" |
        !!sym(new_col_name_pourquoi) == "Hygiène dans les bâtiments d'élevage" |
        !!sym(new_col_name_pourquoi) == "Changement de régime alimentaire" |
        !!sym(new_col_name_pourquoi) == "Changement brusque d'aliment" |
        !!sym(new_col_name_pourquoi) == "Empoisonnement ou consommation d'aliments moisis" |
        !!sym(new_col_name_pourquoi) == "Diarrhée survient quand l'animal aurait consommé un aliment impropre , ou en cas d'intoxication" ~ "1",
        
        # Group 2 (Climat - humidity/weather/season) -> "2"
        !!sym(new_col_name_pourquoi) == "Pluie" |
        !!sym(new_col_name_pourquoi) == "Manque de soleil pour sécher les éplucand hures de manioc, période pluvieuse" |
        !!sym(new_col_name_pourquoi) == "Humidité causée par la pluie en début de saison pluvieuse, blessures lors du pâturage," |
        !!sym(new_col_name_pourquoi) == "Humidité" |
        !!sym(new_col_name_pourquoi) == "Soupçons portés sur les nouvelles réponses après les premières pluies entraînant une rupture entre habitudes alimentaires" |
        !!sym(new_col_name_pourquoi) == "Humidité, pluie" |
        !!sym(new_col_name_pourquoi) == "Juillet - août, raison liée à la saison" |
        !!sym(new_col_name_pourquoi) == "Quand les animaux commencent par manger les nouvelles pousses ou en saison sèche surtout en septembre dans le fourrage devient ligneux (dure)" |
        !!sym(new_col_name_pourquoi) == "La pluie, l'humilité" |
        !!sym(new_col_name_pourquoi) == "Changements de saison et d'aliment" |
        !!sym(new_col_name_pourquoi) == "Humidité, nouveaux fourrages" |
        !!sym(new_col_name_pourquoi) == "Changement d'aliment et claustration imposée," |
        !!sym(new_col_name_pourquoi) == "Humidité et nouvelles répousses de fourrage" |
        !!sym(new_col_name_pourquoi) == "Humidité et poussières sur les feuilles, fourrages broutés" |
        !!sym(new_col_name_pourquoi) == "Humidité et introduction d'un nouvel aliment" |
        !!sym(new_col_name_pourquoi) == "Diarrhée : infection," |
        !!sym(new_col_name_pourquoi) == "Humidité et la poussière dans la nature et surtout sur les feuilles servies ou consommées au pâturage" |
        !!sym(new_col_name_pourquoi) == "Humidité et poussière" |
        !!sym(new_col_name_pourquoi) == "L'humilité" ~ "2",
        
        # Group 3 (Forrage - forage/pasture) -> "3"
        !!sym(new_col_name_pourquoi) == "Moment du pâturage, bagarre" |
        !!sym(new_col_name_pourquoi) == "Nouveau pâturage, d'autres raisons possibles que l'éleveur dit ignorer" |
        !!sym(new_col_name_pourquoi) == "Jeunes pousses" |
        !!sym(new_col_name_pourquoi) == "Dû aux nouveaux fourrages" |
        !!sym(new_col_name_pourquoi) == "La pluie entraînant de jeunes feuilles" |
        !!sym(new_col_name_pourquoi) == "Heure d'aller au pâturage" |
        !!sym(new_col_name_pourquoi) == "Nouvelles pousses Fourragères" |
        !!sym(new_col_name_pourquoi) == "Nouvelles répousses" |
        !!sym(new_col_name_pourquoi) == "Peut-être à cause des herbes fraîches" |
        !!sym(new_col_name_pourquoi) == "Nouvelles répousses de fourrage" ~ "3",
        
        .default = "0"
      )
    )
  
  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après transformation:\n")
  print(table(raw[[new_col_name_pourquoi]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Selon vous, pourquoi ?' non trouvée\n")
}

#19. TRANSFORMATION "Si oui, avec quelle fréquence ?" - Suivi sanitaire --------
# Renommer et transformer la colonne sur la fréquence
old_col_name_freq <- "11.) Suivi sanitaire/Si oui, avec quelle fréquence ?"
new_col_name_freq <- "11.) Suivi sanitaire/Si oui, avec quelle fréquence par ans? 0=Vides, 1=[1:3], 2=[4:6], 3=Autres"

if (old_col_name_freq %in% names(raw)) {
  cat("\n=== TRANSFORMATION: Si oui, avec quelle fréquence ? ===\n")
  cat("Valeurs avant transformation:\n")
  print(table(raw[[old_col_name_freq]], useNA = "ifany"))
  
  raw <- raw %>%
    rename(!!new_col_name_freq := all_of(old_col_name_freq)) %>%
    mutate(
      !!new_col_name_freq := as.character(!!sym(new_col_name_freq)),
      !!new_col_name_freq := str_trim(!!sym(new_col_name_freq)),
      !!new_col_name_freq := case_when(
        # Empty cells -> "0"
        is.na(!!sym(new_col_name_freq)) | !!sym(new_col_name_freq) == "" ~ "0",
        
        # Group 2 (4-6 times per year) -> "2"
        !!sym(new_col_name_freq) == "Trimestrielle" |
        !!sym(new_col_name_freq) == "Chaque trois mois" |
        !!sym(new_col_name_freq) == "Bimestriel" |
        !!sym(new_col_name_freq) == "Chaque 3 mois" |
        !!sym(new_col_name_freq) == "3-4/an" |
        !!sym(new_col_name_freq) == "4 fois par an" |
        !!sym(new_col_name_freq) == "4/an" |
        !!sym(new_col_name_freq) == "6 fois par an" |
        !!sym(new_col_name_freq) == "3 à 4 fois par an" |
        !!sym(new_col_name_freq) == "Chaque trimestre" |
        !!sym(new_col_name_freq) == "Quelque fois" |
        !!sym(new_col_name_freq) == "Chaque deux mois" ~ "2",
        
        # Group 1 (1-3 times per year) -> "1"
        !!sym(new_col_name_freq) == "1 fois par semestre" |
        !!sym(new_col_name_freq) == "Chaque 6 mois" |
        !!sym(new_col_name_freq) == "2 fois/an" |
        !!sym(new_col_name_freq) == "2/an" |
        !!sym(new_col_name_freq) == "3 / an" |
        !!sym(new_col_name_freq) == "2 fois par an" |
        !!sym(new_col_name_freq) == "3/an" |
        !!sym(new_col_name_freq) == "3 fois par an" |
        !!sym(new_col_name_freq) == "1 fois par an (juillet - août)" |
        !!sym(new_col_name_freq) == "Une fois par an" |
        !!sym(new_col_name_freq) == "1 fois par an surtout quand ils tombent malades" |
        !!sym(new_col_name_freq) == "3 fois" |
        !!sym(new_col_name_freq) == "1 fois par an" |
        !!sym(new_col_name_freq) == "Deux fois par an" |
        !!sym(new_col_name_freq) == "2 fois par an (début juin et août)" |
        !!sym(new_col_name_freq) == " Début saison pluvieuse" |
        !!sym(new_col_name_freq) == "Deux fois par an ou en cas de maladies, refus de s'alimenter" |
        !!sym(new_col_name_freq) == "Une fois par an" |
        !!sym(new_col_name_freq) == "1 ou 2 fois par an surtout en cas de maladie" |
        !!sym(new_col_name_freq) == "Pas moment si l'animal est malade" |
        !!sym(new_col_name_freq) == "Par moment" |
        !!sym(new_col_name_freq) == "Quand ils sont malades" ~ "1",
        
        # Group 3 (Others - when animals are sick) -> "3"
        !!sym(new_col_name_freq) == "Quand les animaux sont malades ou présentent des signes de maladies" |
        !!sym(new_col_name_freq) == "Quand ils sont malades ou très souvent en début de saison pluvieuse" |
        !!sym(new_col_name_freq) == "En cas de maladies" |
        !!sym(new_col_name_freq) == "Pas de fréquence fixe. Mais en cas de mortalité" |
        !!sym(new_col_name_freq) == "Seulement en cas de maladies" ~ "3",
        
        .default = "0"
      )
    )
  
  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après transformation:\n")
  print(table(raw[[new_col_name_freq]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Si oui, avec quelle fréquence ?' non trouvée\n")
}

#20. TRANSFORMATION "Types génétiques" - Espèce de petits ruminants --------
old_col_name_species <- "12.) Effectif et composition du cheptel/Races caprines/Types génétiques"
new_col_name_species <- "12.) Espece de petit ruminants: 1=Caprins, 2=Ovins, 3=Ovins et Caprins"

if (old_col_name_species %in% names(raw)) {
  cat("\n=== TRANSFORMATION: Espèce de petits ruminants ===\n")
  cat("Valeurs avant transformation:\n")
  print(table(raw[[old_col_name_species]], useNA = "ifany"))

  normalize_species_text <- function(x) {
    x <- as.character(x)
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x <- str_to_lower(x)
    x <- str_replace_all(x, "\\s+", " ")
    x <- str_squish(x)
    x
  }

  group_1_values <- c(
    "Caprines",
    "Caprins/Djallonke et Métis",
    "Caprin",
    "Ovin",
    "Caprins / métis",
    "Caprin métis et djallonké",
    "Caprin djallonké et caprins métis",
    "Caprins djallonké/ caprins métis",
    "Caprin Metis et caprin djallonké",
    "Caprin Metis",
    "Ovin métis, Ovin djallonké",
    "Caprin métis",
    "Ovin Djallonké Ovin Métis",
    "Caprin métis\nCaprin djallonké",
    "Caprin Métis et djallonké",
    "Caprins djallonké et métis",
    "Caprin djallonké Caprin métis",
    "Caprin métis Caprin Djallonké",
    "Caprin métis Caprin djallonké",
    "Caprin Metis Caprin djallonké",
    "Caprins djallonké Caprins métis",
    "Caprins Djallonké Caprins métis",
    "Caprin djallonké Caprin métis",
    "Caprins Metis Caprins djallonké"
  )

  group_2_values <- c(
    "Ovin/métis",
    "Ovin (Djallonke et Métis)",
    "Ovin",
    "Ovins métis et djallonke",
    "Ovin djallonké et Ovin métis",
    "Ovin métis/ Ovin djallonké",
    "Ovin métis",
    "Ovin métis et Ovin djallonké",
    "Ovin métis Ovin sahélien",
    "Ovin métis Ovin djallonké",
    "Ovin djallonké Ovin métis"
  )

  group_3_values <- c(
    "Djallonke",
    "12/métis 1/brebis Djallonke",
    "métis ovins",
    "Caprin (Djallonke, Métis , Sahélien) , Ovin (métis, Sahélien)",
    "Effectif Ovin:  35 Effectif caprin : 29",
    "Ovin (métis et djallonke) et caprin (métis et djallonke)",
    "Ovins et caprins métis+ Djallonke",
    "Caprin (Djallonke et Métis) Ovin (métis)",
    "Djallonke et Ovins métis et djallonke",
    "8 caprins ( djallonké et métis) / 30 ovins (djallonké et métis)",
    "Ovin Métis et Djallonké",
    "Ovin/ métis et djallonké",
    "Ovin métis (30) caprins métis (15)",
    "Ovin métis (8)",
    "Ovin métis et djallonké",
    "Ovin métis/ djallonké",
    "Ovin métis et djallonké ( sahélien : 1; métis: 28 djallonké 6",
    "Ovin métis et Ovin djallonké",
    "Ovin djallonké/ Ovin métis",
    "Caprin (djallonké et métis) :",
    "Ovin métis ( 29) (6 beliers, 9 brebis, 14 jeunes dont 6 femelles et 8 mâles de moins de 6 mois)",
    "Ovin 6 métis (3 femelles reproductrices , 2 petits ‹ 6 mois et 1 mâle ‹ 6 mois), 11 Ovin djallonké (1 mâle reproducteur, dont 4 femelles reproductrices et 6 jeunes (3 femelles, 3 mâles < 6 mois)",
    "Ovin métis (30) et Ovin djallonké (15)",
    "Caprin djallonké Total : 4 Caprin métis Total : 5 Ovin métis Total : 92 Total Ovin + caprin : 101",
    "Ovin djallonké x'et métis Caprin djallonké et métis",
    "Ovin métis Ovin métis Caprin métis Caprin djallonké",
    "Ovin métis Caprin métis et djallonké",
    "Ovin métis Caprin djallonké Caprin métis",
    "Caprin  métis et djallonké Ovin métis",
    "Ovin métis Caprin metis Caprin djallonké",
    "Caprin djallonké Ovin métis",
    "Ovin métis Caprin métis",
    "Ovin métis Caprin djallonké  Caprin métis",
    "Ovin métis Caprin métis Caprin djallonké",
    "Ovin Métis Caprin djallonké",
    "Caprin métis : 9 , 4 femelles reproductrices, 2 jeune boucs Ovin Metis : 12 avec 3 et 2 béliers",
    "Caprin métis : 9 Caprin Sahélien : 5 Femelles reproductrices : 4 Mâles reproducteurs sahéliens : 2 (passés)",
    "Ovins métis Caprins métis",
    "Caprin djallonké Caprin métis",
    "Ovin métis\nCaprins métis Caprins djallonké",
    "Ovins djallonké Ovins métis Ovins Sahéliens Caprin djallonké Caprins métis",
    "Ovin métis Ovin djallonké",
    "Ovin métis (8) Caprin métis (7)",
    "Caprin (djallonké et métis) :\n1 bouc, 13 femelles reproductrices (2 djallonké + 11 métis), 3 mâles jeunes , 3 femelles jeunes, 4 mâles < 6 mois et 6 femelles < 6 mois) Total Caprin : 30 Ovin (métis):  Femelles reproductrices : 10 Mâle adulte (bélier) : 1 Mâle > 6 mois: 3 Femelles > 6 mois: 4Mâle < 6 mois: 5 Femelles< 6 mois: 5 Total Ovin: 28",
    "Ovin métis ( 29) (6 beliers, 9 brebis, 14 jeunes dont 6 femelles et 8 mâles de moins de 6 mois) Ovin djallonké ( 3 femelles) Caprin Metis 12 (4 chèvres, 2 boucs, 3 chevreaux, 3 chèvrelles)",
    "Ovin 6 métis (3 femelles reproductrices , 2 petits ‹ 6 mois et 1 mâle ‹ 6 mois), 11 Ovin djallonké (1 mâle reproducteur, dont 4 femelles reproductrices et 6 jeunes (3 femelles, 3 mâles < 6 mois) Caprin métis ( 2 femelles reproductrices)"
  )

  norm_group_1 <- normalize_species_text(group_1_values)
  norm_group_2 <- normalize_species_text(group_2_values)
  norm_group_3 <- normalize_species_text(group_3_values)

  raw <- raw %>%
    rename(!!new_col_name_species := all_of(old_col_name_species)) %>%
    mutate(
      !!new_col_name_species := as.character(!!sym(new_col_name_species)),
      .species_norm_tmp = normalize_species_text(!!sym(new_col_name_species)),
      !!new_col_name_species := case_when(
        is.na(.species_norm_tmp) | .species_norm_tmp == "" ~ "0",
        .species_norm_tmp %in% norm_group_1 ~ "1",
        .species_norm_tmp %in% norm_group_2 ~ "2",
        .species_norm_tmp %in% norm_group_3 ~ "3",
        TRUE ~ "0"
      )
    ) %>%
    select(-.species_norm_tmp)

  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après transformation:\n")
  print(table(raw[[new_col_name_species]], useNA = "ifany"))

  empty_count_species <- sum(is.na(raw[[new_col_name_species]]) | raw[[new_col_name_species]] == "")
  cat("Nombre de cellules vides après transformation:", empty_count_species, "\n")
  if (empty_count_species == 0) {
    cat("✓ SUCCESS: Aucune cellule vide dans la colonne transformée\n")
  } else {
    cat("⚠ WARNING:", empty_count_species, "cellule(s) vide(s) détectée(s)\n")
  }
} else {
  cat("\n⚠ Colonne '12.) Effectif et composition du cheptel/Races caprines/Types génétiques' non trouvée\n")
}

#21. TRANSFORMATION "Si oui, quelles sont ces maladies ?" - Suivi sanitaire --------
# Renommer et transformer la colonne sur les maladies
old_col_name_maladies_spec <- "11.) Suivi sanitaire/Si oui, quelles sont ces maladies ?"
new_col_name_maladies_spec <- "11.) Suivi sanitaire/Si oui, quelles sont ces maladies ? 0=Vides, 1=Anorexie, 2=Diarhee, 3=Autres"

if (old_col_name_maladies_spec %in% names(raw)) {
  cat("\n=== TRANSFORMATION: Si oui, quelles sont ces maladies ? ===\n")
  cat("Valeurs avant transformation:\n")
  print(table(raw[[old_col_name_maladies_spec]], useNA = "ifany"))
  
  raw <- raw %>%
    rename(!!new_col_name_maladies_spec := all_of(old_col_name_maladies_spec)) %>%
    mutate(
      !!new_col_name_maladies_spec := as.character(!!sym(new_col_name_maladies_spec)),
      !!new_col_name_maladies_spec := str_trim(!!sym(new_col_name_maladies_spec)),
      !!new_col_name_maladies_spec := case_when(
        # Empty cells -> "0"
        is.na(!!sym(new_col_name_maladies_spec)) | !!sym(new_col_name_maladies_spec) == "" ~ "0",
        
        # Group 1 (Anorexie) -> "1"
        !!sym(new_col_name_maladies_spec) == "Anorexie" |
        !!sym(new_col_name_maladies_spec) == "Refus de s'alimenter : anorexie " ~ "1",
        
        # Group 2 (Diarrhée variations) -> "2"
        !!sym(new_col_name_maladies_spec) == "Diarhee" |
        !!sym(new_col_name_maladies_spec) == "Diarrhée non traitée à temps" |
        !!sym(new_col_name_maladies_spec) == "Diarrhée non traitée efficacement," |
        !!sym(new_col_name_maladies_spec) == "Diarrhée" |
        !!sym(new_col_name_maladies_spec) == "Diarrhée surtout chez les plus jeunes" |
        !!sym(new_col_name_maladies_spec) == "Diarrhée chez les plus jeunes" |
        !!sym(new_col_name_maladies_spec) == "Diarrhée chez les petits (agneaux et agnelles)" |
        !!sym(new_col_name_maladies_spec) == "- Diarrhée entraînant mortalité souvent observée chez les sujets de moins de 6 mois" |
        !!sym(new_col_name_maladies_spec) == "Diarrhée, difficulté respiratoire" |
        !!sym(new_col_name_maladies_spec) == "Diarrhée," |
        !!sym(new_col_name_maladies_spec) == "Diarrhée , d'autres mortalités sont enregistrées sais aucun signe au préalable" |
        !!sym(new_col_name_maladies_spec) == "Cas de Diarrhée chez les chevreaux mais très rare" |
        !!sym(new_col_name_maladies_spec) == "Cas de diarrhée chez les moins de 6 mois" |
        !!sym(new_col_name_maladies_spec) == "Diarrhée chez les jeunes de moins de 6 mois" |
        !!sym(new_col_name_maladies_spec) == "Diarrhée chez les jeunes de moins de 6 mois. Mais pas fréquent." |
        !!sym(new_col_name_maladies_spec) == "Quelques fois causée par la diarrhée, souvent observée chez les sujets plus jeunes" |
        !!sym(new_col_name_maladies_spec) == "Diarrhée chez les plus jeunes," |
        !!sym(new_col_name_maladies_spec) == "Diarrhée, c'était notamment à l'acquisition des premiers reproducteurs sahéliens" ~ "2",
        
        # Group 3 (Autres - other diseases and conditions) -> "3"
        !!sym(new_col_name_maladies_spec) == "Autres" |
        !!sym(new_col_name_maladies_spec) == "Peste" |
        !!sym(new_col_name_maladies_spec) == "Les traitements sont faits régulièrement, vaccination régulière. Mortalité très rare (nulle en 2025)" |
        !!sym(new_col_name_maladies_spec) == "Pas de mortalité liée aux maladies ces dernières années" |
        !!sym(new_col_name_maladies_spec) == "Pododermatite interdigitée" |
        !!sym(new_col_name_maladies_spec) == "Anorexie et diarrhée" |
        !!sym(new_col_name_maladies_spec) == "La dernière vague de maladie remonte en 2023 où après une vaccination les animaux ont refusé de s'alimenter ce qui a engendré une perte énorme." |
        !!sym(new_col_name_maladies_spec) == "Attaque de tiques" |
        !!sym(new_col_name_maladies_spec) == "Peste des petits ruminants" |
        !!sym(new_col_name_maladies_spec) == "Refus de s'alimenter" |
        !!sym(new_col_name_maladies_spec) == "Météorisation" |
        !!sym(new_col_name_maladies_spec) == "Dystocie" |
        !!sym(new_col_name_maladies_spec) == "Mortalité liée au cas de diarrhée chez les jeunes animaux surtout au lait" |
        !!sym(new_col_name_maladies_spec) == "Piétin" |
        !!sym(new_col_name_maladies_spec) == "C'est surtout chez les mâles sahéliens que l'éleveur a souligné la fragilité de leur santé avec des mort subites sans signes de maladies au préalable" |
        !!sym(new_col_name_maladies_spec) == "Infections respiratoires" |
        !!sym(new_col_name_maladies_spec) == "Mortalité des mâles entiers après castration, l'animal devenant rigide" |
        !!sym(new_col_name_maladies_spec) == "Intoxication alimentaire" |
        !!sym(new_col_name_maladies_spec) == "Difficulté respiratoire" |
        !!sym(new_col_name_maladies_spec) == "Mammite, des cas de mortalité déclaré sans connaissance des causes réelles" |
        !!sym(new_col_name_maladies_spec) == "Mais très rare, souvent liée au diarrhée chez les moins de 6 moins" |
        !!sym(new_col_name_maladies_spec) == "Rare mais quelque fois à cause de la diarrhée chez les plus jeunes" |
        !!sym(new_col_name_maladies_spec) == "Les cas de mortalité sont rares compte tenu de la promptitude à traiter les animaux en cas de maladies" |
        !!sym(new_col_name_maladies_spec) == "Non de la maladie ignorée. Sans aucun signe l'animal bien portant le matin , meurt le soir" ~ "3",
        
        .default = "0"
      )
    )
  
  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après transformation:\n")
  print(table(raw[[new_col_name_maladies_spec]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Si oui, quelles sont ces maladies ?' non trouvée\n")
}

#22. TRANSFORMATION "Si oui, quelles sont les causes selon vous ?" - Croisement --------
# Renommer et transformer la colonne sur les causes (résistance/adaptation)
old_col_name_causes <- "11.) Suivi sanitaire/Si oui, quelles sont les causes selon vous ?...242"
new_col_name_causes <- "11.) Suivi sanitaire/Si oui, quelles sont les causes selon vous ?...242/ 0=Vides, 1=Effet genetique, 2=Resistance faible de metis"

if (old_col_name_causes %in% names(raw)) {
  cat("\n=== TRANSFORMATION: Si oui, quelles sont les causes selon vous ? ===\n")
  cat("Valeurs avant transformation:\n")
  print(table(raw[[old_col_name_causes]], useNA = "ifany"))
  
  raw <- raw %>%
    rename(!!new_col_name_causes := all_of(old_col_name_causes)) %>%
    mutate(
      !!new_col_name_causes := as.character(!!sym(new_col_name_causes)),
      !!new_col_name_causes := str_trim(!!sym(new_col_name_causes)),
      !!new_col_name_causes := case_when(
        # Empty cells and "0" -> "0"
        is.na(!!sym(new_col_name_causes)) | !!sym(new_col_name_causes) == "" | !!sym(new_col_name_causes) == "0" ~ "0",
        
        # Group 1 (Adaptation à l'environnement / Effet génétique) -> "1"
        !!sym(new_col_name_causes) == "Adaptation à l'environnement" |
        !!sym(new_col_name_causes) == "Dû au croisement, produits de croisement moins que les parents Djallonke" |
        !!sym(new_col_name_causes) == "Adaptation au climat" ~ "1",
        
        # Group 2 (Faible rusticité / Résistance faible des métis) -> "2"
        !!sym(new_col_name_causes) == "Faible rusticité" |
        !!sym(new_col_name_causes) == "Rusticité faible" |
        !!sym(new_col_name_causes) == "Performances génétiques" |
        !!sym(new_col_name_causes) == "Ils sont moins rustiques que les djallonké" |
        !!sym(new_col_name_causes) == "Fragilité de l'organisme des métis, moins résistants" |
        !!sym(new_col_name_causes) == "Djallonké plus rustique que les métis car mieux adapté au milieu" |
        !!sym(new_col_name_causes) == "Santé plus fragile, nécessite plus de suivi" |
        !!sym(new_col_name_causes) == "Problème de résistance, rusticité" |
        !!sym(new_col_name_causes) == "Sujets de croisement (métis) moins résistants" ~ "2",
        
        .default = "0"
      )
    )
  
  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après transformation:\n")
  print(table(raw[[new_col_name_causes]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Si oui, quelles sont les causes selon vous ?' non trouvée\n")
}

#23. TRANSFORMATION "Si oui, quelles sont les causes selon vous ?...246" - Suivi sanitaire --------
# Renommer et transformer la colonne sur les causes (climat/traitement/résistance métis)
old_col_name_causes_246 <- "11.) Suivi sanitaire/Si oui, quelles sont les causes selon vous ?...246"
new_col_name_causes_246 <- "11.) Suivi sanitaire/Si oui, quelles sont les causes selon vous ?...246/ 0=Vides, 1=Du au climat, 2=Faute au traitement, 3=Race metis fragile"

if (old_col_name_causes_246 %in% names(raw)) {
  cat("\n=== TRANSFORMATION: Si oui, quelles sont les causes selon vous ?...246 ===\n")
  cat("Valeurs avant transformation:\n")
  print(table(raw[[old_col_name_causes_246]], useNA = "ifany"))
  
  raw <- raw %>%
    rename(!!new_col_name_causes_246 := all_of(old_col_name_causes_246)) %>%
    mutate(
      !!new_col_name_causes_246 := as.character(!!sym(new_col_name_causes_246)),
      !!new_col_name_causes_246 := str_trim(!!sym(new_col_name_causes_246)),
      !!new_col_name_causes_246 := case_when(
        # Empty cells and "Non connues par l'éleveur" -> "0"
        is.na(!!sym(new_col_name_causes_246)) | !!sym(new_col_name_causes_246) == "" | 
        !!sym(new_col_name_causes_246) == "Non connues par l'éleveur" ~ "0",
        
        # Group 1 (Climate factors) -> "1"
        !!sym(new_col_name_causes_246) == "L'humilité en saison sèche" |
        !!sym(new_col_name_causes_246) == "Adaptation au climat et conditions d'élevage réussi chez les métis" |
        !!sym(new_col_name_causes_246) == "Produits de croisement déjà acclimaté au milieu et aux conditions d'élevage" |
        !!sym(new_col_name_causes_246) == "Humidité en saison pluvieuse" ~ "1",
        
        # Group 2 (Treatment factors) -> "2"
        !!sym(new_col_name_causes_246) == "Efficacité des traitements" |
        !!sym(new_col_name_causes_246) == "Traitement régulier au même titre, bien-être animal" ~ "2",
        
        # Group 3 (Métis fragility/resistance) -> "3"
        !!sym(new_col_name_causes_246) == "Les cas de maladie sont plutôt fréquent chez les races exotiques et métis que chez les Djallonke" |
        !!sym(new_col_name_causes_246) == "Résistance" |
        !!sym(new_col_name_causes_246) == "Les métis arrive à s'adapter au milieu d'élevage" |
        !!sym(new_col_name_causes_246) == "Si différence: Rusticité" |
        !!sym(new_col_name_causes_246) == "Métis moins résistant" |
        !!sym(new_col_name_causes_246) == "Les métis résistent moins aux maladies que les djallonkés, les sahéliens étant plus plus sensibles" |
        !!sym(new_col_name_causes_246) == "Plus affectés par les maladies" |
        !!sym(new_col_name_causes_246) == "Les métis ne sont pas si résistants que les métis. Pour des cas graves de maladies, la durée de la convalescence est plus longue chez les métis qui reprennent la croissance avec assez de retard contrairement aux djallonké." |
        !!sym(new_col_name_causes_246) == "Santé plus fragile" |
        !!sym(new_col_name_causes_246) == "Métis moins résistants que djallonké" |
        !!sym(new_col_name_causes_246) == "Les métis sont plus fragiles" |
        !!sym(new_col_name_causes_246) == "Les métis sont fragiles" |
        !!sym(new_col_name_causes_246) == "Les métis sont moins résistants" |
        !!sym(new_col_name_causes_246) == "Les métis plus exigeants , moins résistants que les djallonkés" |
        !!sym(new_col_name_causes_246) == "Les métis sont moins résistants , mortalité enregistrée après castration des mâles" |
        !!sym(new_col_name_causes_246) == "Métis moins résistants" |
        !!sym(new_col_name_causes_246) == "Les métis sont moins résistants" |
        !!sym(new_col_name_causes_246) == "Les métis ne sont pas aussi résistants que les djallonkés" |
        !!sym(new_col_name_causes_246) == "Métis résistent aux maladies" |
        !!sym(new_col_name_causes_246) == "Métis bien résistants également, adaptés à nos conditions d'élevage" |
        !!sym(new_col_name_causes_246) == "Les métis et surtout les animaux sont moins rustiques que les djallonkés" |
        !!sym(new_col_name_causes_246) == "Les sujets de croisement sont moins résistants que les djallonkés" |
        !!sym(new_col_name_causes_246) == "Les métis résistent aux maladies autant que les djallonkés. Ce qui n'est pas le cas chez les sahéliens." ~ "3",
        
        .default = "0"
      )
    )
  
  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après transformation:\n")
  print(table(raw[[new_col_name_causes_246]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Si oui, quelles sont les causes selon vous ?...246' non trouvée\n")
}

#24. TRANSFORMATION "Comment (quelles sont vos observations?...)" - Contexte environnemental --------
# Renommer et transformer la colonne sur les observations liées au changement climatique
old_col_name_comment_obs <- "10.) Contexte environnemental/Comment (quelles sont vos observations? si ne sait pas si les CC affectent son élevage???"
new_col_name_comment_obs <- "10.) Contexte environnemental/Comment (quelles sont vos observations? si ne sait pas si les CC affectent son élevage??? 0=Vides, 1=Probleme du climat, 2=Autres"

if (old_col_name_comment_obs %in% names(raw)) {
  cat("\n=== TRANSFORMATION: Comment (quelles sont vos observations?...) ===\n")
  cat("Valeurs avant transformation:\n")
  print(table(raw[[old_col_name_comment_obs]], useNA = "ifany"))
  
  raw <- raw %>%
    rename(!!new_col_name_comment_obs := all_of(old_col_name_comment_obs)) %>%
    mutate(
      !!new_col_name_comment_obs := as.character(!!sym(new_col_name_comment_obs)),
      !!new_col_name_comment_obs := str_trim(!!sym(new_col_name_comment_obs)),
      !!new_col_name_comment_obs := case_when(
        # Empty cells and "no observed link" variants -> "0"
        is.na(!!sym(new_col_name_comment_obs)) | !!sym(new_col_name_comment_obs) == "" |
        !!sym(new_col_name_comment_obs) == "Aucune observation faite établissant un lien entre CC et l'élevage" |
        !!sym(new_col_name_comment_obs) == "Ne peut affirmer des problèmes directement liés au cc" |
        !!sym(new_col_name_comment_obs) == "L'éleveur n'a pas pu faire un lien" ~ "0",
        
        # Group 1 (Climate-related problems) -> "1"
        !!sym(new_col_name_comment_obs) == "Le disponible fourrager du parcours naturels de raréfie plus tôt, sécheresse longue" |
        !!sym(new_col_name_comment_obs) == "Rareté des pluies," |
        !!sym(new_col_name_comment_obs) == "Chaleur intense" |
        !!sym(new_col_name_comment_obs) == "Période des saisons décalées" |
        !!sym(new_col_name_comment_obs) == "Irrégularité des pluies" |
        !!sym(new_col_name_comment_obs) == "La chaleur est devenue intense" |
        !!sym(new_col_name_comment_obs) == "Les pluies ne sont plus régulières. La saison sèche est plus longue." |
        !!sym(new_col_name_comment_obs) == "Humidité plus forte en saison pluvieuse, ce qui affectent les animaux. Ils s'alimentent difficilement, devient moins épanouis, se recroquevillent et finissent par mourir." |
        !!sym(new_col_name_comment_obs) == "Il fait plus chaud" |
        !!sym(new_col_name_comment_obs) == "Il faut plus chaud," |
        !!sym(new_col_name_comment_obs) == "De forte pluie en saison pluvieuse, forte humidité" |
        !!sym(new_col_name_comment_obs) == "Saisons de plus en plus décalée. Surabondance de pluie et forte chaleur" |
        !!sym(new_col_name_comment_obs) == "Décalage des pluies, longue saison sèche, difficulté d'alimentation des animaux" ~ "1",
        
        # Group 2 (Other/indirect observations) -> "2"
        !!sym(new_col_name_comment_obs) == "Pas de lien direct avec le changement climatique, mais difficulté d'alimentation soulignée en saison sèche" |
        !!sym(new_col_name_comment_obs) == "Perturbation des pluies reconnue par l'éleveur mais n'a pas pu faire un lien avec les activités d'élevage." ~ "2",
        
        .default = "0"
      )
    )
  
  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après transformation:\n")
  print(table(raw[[new_col_name_comment_obs]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Comment (quelles sont vos observations?...)' non trouvée\n")
}

#25. TRANSFORMATION "Si oui, à quel moment de l'année ?" (variante apostrophe typographique) - Suivi sanitaire --------
# Renommer et transformer la colonne sur le moment de l'année (vaccination)
old_col_name_moment_an_v2 <- "11.) Suivi sanitaire/Si oui, à quel moment de l’année ?"
new_col_name_moment_an_v2 <- "11.) Suivi sanitaire/Si oui, à quel moment de l’année ? 0=Vides, 1={1,2 par ans}, 2=Autres"

if (old_col_name_moment_an_v2 %in% names(raw)) {
  cat("\n=== TRANSFORMATION: Si oui, à quel moment de l’année ? ===\n")
  cat("Valeurs avant transformation:\n")
  print(table(raw[[old_col_name_moment_an_v2]], useNA = "ifany"))
  
  raw <- raw %>%
    rename(!!new_col_name_moment_an_v2 := all_of(old_col_name_moment_an_v2)) %>%
    mutate(
      !!new_col_name_moment_an_v2 := as.character(!!sym(new_col_name_moment_an_v2)),
      !!new_col_name_moment_an_v2 := str_trim(!!sym(new_col_name_moment_an_v2)),
      !!new_col_name_moment_an_v2 := case_when(
        # Empty cells and "no specific moment" -> "0"
        is.na(!!sym(new_col_name_moment_an_v2)) | !!sym(new_col_name_moment_an_v2) == "" |
        !!sym(new_col_name_moment_an_v2) == "Pas de moment spécifique" ~ "0",
        
        # Group 1 (Specific months / 1-2 times per year) -> "1"
        !!sym(new_col_name_moment_an_v2) == "Pas précis, mais 1/an" |
        !!sym(new_col_name_moment_an_v2) == "Mars (début saison pluvieuse)" |
        !!sym(new_col_name_moment_an_v2) == "Avant début grande saison pluvieuse (janvier - mars)" |
        !!sym(new_col_name_moment_an_v2) == "Début d'année" |
        !!sym(new_col_name_moment_an_v2) == "Mars-Avril" |
        !!sym(new_col_name_moment_an_v2) == "Mars - Avril" |
        !!sym(new_col_name_moment_an_v2) == "2 fois par an (janvier et juin)" |
        !!sym(new_col_name_moment_an_v2) == "Mars" |
        !!sym(new_col_name_moment_an_v2) == "2/an" |
        !!sym(new_col_name_moment_an_v2) == "Février-mars" |
        !!sym(new_col_name_moment_an_v2) == "Mars et juillet" |
        !!sym(new_col_name_moment_an_v2) == "Mois de Mars" |
        !!sym(new_col_name_moment_an_v2) == "Octobre - Novembre" |
        !!sym(new_col_name_moment_an_v2) == "Décembre ou février" |
        !!sym(new_col_name_moment_an_v2) == "Mars-avril" |
        !!sym(new_col_name_moment_an_v2) == "Début saison (mois de mai)" |
        !!sym(new_col_name_moment_an_v2) == "Début saison pluvieuse" |
        !!sym(new_col_name_moment_an_v2) == "Février - Mars" |
        !!sym(new_col_name_moment_an_v2) == "Mois de Février et Octobre" |
        !!sym(new_col_name_moment_an_v2) == "Chaque six mois ( février, août)" |
        !!sym(new_col_name_moment_an_v2) == "Février et août" |
        !!sym(new_col_name_moment_an_v2) == "Juillet" |
        !!sym(new_col_name_moment_an_v2) == "Février - mars" |
        !!sym(new_col_name_moment_an_v2) == "En mars début saison pluvieuse" |
        !!sym(new_col_name_moment_an_v2) == "Début saison pluvieuse, février ou mars" |
        !!sym(new_col_name_moment_an_v2) == "Février et juillet" |
        !!sym(new_col_name_moment_an_v2) == "Mars et août" |
        !!sym(new_col_name_moment_an_v2) == "Février" |
        !!sym(new_col_name_moment_an_v2) == "Mars et décembre" |
        !!sym(new_col_name_moment_an_v2) == "En août" |
        !!sym(new_col_name_moment_an_v2) == "Août" ~ "1",
        
        # Group 2 (Other/unrelated) -> "2"
        !!sym(new_col_name_moment_an_v2) == "Moment non précis, en fonction du calendrier de l'agent traitant de l'ONG Élevage sans frontières" |
        !!sym(new_col_name_moment_an_v2) == "Diarrhée en cas d'introduction de nouvel aliment" ~ "2",
        
        .default = "0"
      )
    )
  
  cat("✓ Colonne renommée et transformée\n")
  cat("Valeurs après transformation:\n")
  print(table(raw[[new_col_name_moment_an_v2]], useNA = "ifany"))
} else {
  cat("\n⚠ Colonne 'Si oui, à quel moment de l’année ?' (variante) non trouvée\n")
}

#26. SUPPRESSION COLONNES INUTILES ------------------------------------------------
# Supprimer les colonnes non nécessaires
cols_to_remove <- c(
  "Merci pour votre participation ! Ce questionnaire contribue à une meilleure compréhension des pratiques de croisement caprin au Bénin pour une amélioration génétique durable.",
  "9.) Perceptions et adaptations/Si oui, par qui ?",
  "9.) Perceptions et adaptations/Autre à préciser",
  "10.) Contexte environnemental/Si oui, lesquels ?",
  "_id",
  "_uuid",
  "_submission_time",
  "_validation_status",
  "_notes",
  "_status",
  "_submitted_by",
  "__version__",
  "_tags",
  "meta/rootUuid",
  "_index"
)

cat("\nSuppression des colonnes inutiles...\n")
cat("Nombre de colonnes avant suppression:", ncol(raw), "\n")

raw <- raw %>% select(-any_of(cols_to_remove))

cat("Nombre de colonnes après suppression:", ncol(raw), "\n")
cat("✓ Colonnes inutiles supprimées\n")


#27.---- EXPORT DES DONNEES NETTOYEES -------------------------------------------------------------------
cat("\n=== EXPORT DES DONNEES NETTOYEES ===\n")

# Exporter en Excel
output_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning/data7.xlsx"

# Essayer avec openxlsx (recommandé)
if (!require(openxlsx, quietly = TRUE)) {
  cat("Installation de openxlsx...\n")
  install.packages("openxlsx")
  library(openxlsx)
}

write.xlsx(raw, output_path, rowNames = FALSE, overwrite = TRUE)
cat("Fichier Excel sauvegardé :", output_path, "\n")
cat("Dimensions finales :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")

#28.---- VERIFICATION: Confirmer que la colonne supprimée est bien absente ----------------------------
cat("\n=== VERIFICATION: Colonne supprimée ===\n")
col_to_check <- "9.) Perceptions et adaptations/Si oui, par qui ?"
if (col_to_check %in% names(raw)) {
  cat("⚠ WARNING: La colonne '", col_to_check, "' est encore présente dans les données!\n", sep = "")
} else {
  cat("✓ SUCCESS: La colonne '", col_to_check, "' a été supprimée avec succès!\n", sep = "")
}

#29.---- VERIFICATION: Contrôler qu'il n'y a pas de cellules vides dans la colonne transformée ---
cat("\n=== VERIFICATION: Colonne Si oui, lesquelles ? (Perceptions et adaptations) ===\n")
col_to_verify <- "9.) Perceptions et adaptations/Si oui, lesquelles ? 0=Vides, 1=Crossing, 2=Backcrossing, 3=Crossing & Backcrossing"
if (col_to_verify %in% names(raw)) {
  empty_count <- sum(is.na(raw[[col_to_verify]]) | raw[[col_to_verify]] == "")
  cat("Colonne:", col_to_verify, "\n")
  cat("Nombre de cellules vides:", empty_count, "\n")
  if (empty_count > 0) {
    cat("⚠ WARNING: Trouvé", empty_count, "cellule(s) vide(s) dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify]], useNA = "ifany"))
  } else {
    cat("✓ SUCCESS: Aucune cellule vide trouvée dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify]], useNA = "ifany"))
  }
} else {
  cat("⚠ Colonne 'Si oui, lesquelles ?' non trouvée pour vérification\n")
}

#30.---- VERIFICATION: Colonne Si « non », pourquoi ? (Perceptions et adaptations) ===
cat("\n=== VERIFICATION: Colonne Si « non », pourquoi ? (Perceptions et adaptations) ===\n")
col_to_verify_pourquoi <- "9.) Perceptions et adaptations/Si « non », pourquoi ? 0=Vides, 1=Deception dans le system, 2=Pas de temp/consommation personnelle, 3=Financier"
if (col_to_verify_pourquoi %in% names(raw)) {
  empty_count <- sum(is.na(raw[[col_to_verify_pourquoi]]) | raw[[col_to_verify_pourquoi]] == "")
  cat("Colonne:", col_to_verify_pourquoi, "\n")
  cat("Nombre de cellules vides:", empty_count, "\n")
  if (empty_count > 0) {
    cat("⚠ WARNING: Trouvé", empty_count, "cellule(s) vide(s) dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_pourquoi]], useNA = "ifany"))
  } else {
    cat("✓ SUCCESS: Aucune cellule vide trouvée dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_pourquoi]], useNA = "ifany"))
  }
} else {
  cat("⚠ Colonne 'Si « non », pourquoi ?' non trouvée pour vérification\n")
}

#31.---- VERIFICATION: Confirmer que la colonne supprimée est bien absente ---
cat("\n=== VERIFICATION: Colonne Autre à préciser (Perceptions et adaptations) supprimée ===\n")
col_to_check_autre <- "9.) Perceptions et adaptations/Autre à préciser"
if (col_to_check_autre %in% names(raw)) {
  cat("⚠ WARNING: La colonne '", col_to_check_autre, "' est encore présente dans les données!\n", sep = "")
} else {
  cat("✓ SUCCESS: La colonne '", col_to_check_autre, "' a été supprimée avec succès!\n", sep = "")
}

#32.---- VERIFICATION: Colonne Lien entre CC et élevage - Pas de cellules vides -----
cat("\n=== VERIFICATION: Colonne Lien entre CC et élevage (Aucune observation faite...) ===\n")
col_to_verify_cc <- "Aucune observation faite établissant un lien entre CC et l'élevage/0=Vides, 1=Non lier au climat, 2=lier au climat"
if (col_to_verify_cc %in% names(raw)) {
  empty_count <- sum(is.na(raw[[col_to_verify_cc]]) | raw[[col_to_verify_cc]] == "")
  cat("Colonne:", col_to_verify_cc, "\n")
  cat("Nombre de cellules vides:", empty_count, "\n")
  if (empty_count > 0) {
    cat("⚠ WARNING: Trouvé", empty_count, "cellule(s) vide(s) dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_cc]], useNA = "ifany"))
  } else {
    cat("✓ SUCCESS: Aucune cellule vide trouvée dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_cc]], useNA = "ifany"))
  }
} else {
  cat("⚠ Colonne 'Aucune observation faite établissant un lien entre CC et l'élevage' non trouvée pour vérification\n")
}

#33.---- VERIFICATION: Confirmer que la colonne "Si oui, lesquels ?" (Contexte environnemental) est supprimée ---
cat("\n=== VERIFICATION: Colonne Si oui, lesquels ? (Contexte environnemental) supprimée ===\n")
col_to_check_contexte <- "10.) Contexte environnemental/Si oui, lesquels ?"
if (col_to_check_contexte %in% names(raw)) {
  cat("⚠ WARNING: La colonne '", col_to_check_contexte, "' est encore présente dans les données!\n", sep = "")
} else {
  cat("✓ SUCCESS: La colonne '", col_to_check_contexte, "' a été supprimée avec succès!\n", sep = "")
}

#34.---- VERIFICATION: Colonne Quelles sont les maladies récurrentes - Pas de cellules vides -----
cat("\n=== VERIFICATION: Colonne Quelles sont les maladies récurrentes enregistrées ===\n")
col_to_verify_maladies <- "11.) Suivi sanitaire/Quelles sont les maladies récurrentes enregistrées dans votre élevage ?. 0=Neant, 1=Diarhee, 2=Autres maladies"
if (col_to_verify_maladies %in% names(raw)) {
  empty_count <- sum(is.na(raw[[col_to_verify_maladies]]) | raw[[col_to_verify_maladies]] == "")
  cat("Colonne:", col_to_verify_maladies, "\n")
  cat("Nombre de cellules vides:", empty_count, "\n")
  if (empty_count > 0) {
    cat("⚠ WARNING: Trouvé", empty_count, "cellule(s) vide(s) dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_maladies]], useNA = "ifany"))
  } else {
    cat("✓ SUCCESS: Aucune cellule vide trouvée dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_maladies]], useNA = "ifany"))
  }
} else {
  cat("⚠ Colonne 'Quelles sont les maladies récurrentes enregistrées dans votre élevage ?' non trouvée pour vérification\n")
}

#35.---- VERIFICATION: Colonne A quel moment apparaissent-elles ? - Pas de cellules vides -----
cat("\n=== VERIFICATION: Colonne A quel moment apparaissent-elles ? ===\n")
col_to_verify_moment <- "11.) Suivi sanitaire/A quel moment apparaissent-elles ? 0=Vide, 1=Saison Seche, 2=Saison Pluvieuse, 3=Autres"
if (col_to_verify_moment %in% names(raw)) {
  empty_count <- sum(is.na(raw[[col_to_verify_moment]]) | raw[[col_to_verify_moment]] == "")
  cat("Colonne:", col_to_verify_moment, "\n")
  cat("Nombre de cellules vides:", empty_count, "\n")
  if (empty_count > 0) {
    cat("⚠ WARNING: Trouvé", empty_count, "cellule(s) vide(s) dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_moment]], useNA = "ifany"))
  } else {
    cat("✓ SUCCESS: Aucune cellule vide trouvée dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_moment]], useNA = "ifany"))
  }
} else {
  cat("⚠ Colonne 'A quel moment apparaissent-elles ?' non trouvée pour vérification\n")
}

#36.---- VERIFICATION: Colonne Selon vous, pourquoi ? - Pas de cellules vides -----
cat("\n=== VERIFICATION: Colonne Selon vous, pourquoi ? ===\n")
col_to_verify_pourquoi <- "11.) Suivi sanitaire/Selon vous, pourquoi ? 0=Vides, 1=Sanitaire, 2=Climat, 3=Forrage"
if (col_to_verify_pourquoi %in% names(raw)) {
  empty_count <- sum(is.na(raw[[col_to_verify_pourquoi]]) | raw[[col_to_verify_pourquoi]] == "")
  cat("Colonne:", col_to_verify_pourquoi, "\n")
  cat("Nombre de cellules vides:", empty_count, "\n")
  if (empty_count > 0) {
    cat("⚠ WARNING: Trouvé", empty_count, "cellule(s) vide(s) dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_pourquoi]], useNA = "ifany"))
  } else {
    cat("✓ SUCCESS: Aucune cellule vide trouvée dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_pourquoi]], useNA = "ifany"))
  }
} else {
  cat("⚠ Colonne 'Selon vous, pourquoi ?' non trouvée pour vérification\n")
}

#37.---- VERIFICATION: Colonne Si oui, avec quelle fréquence ? - Pas de cellules vides -----
cat("\n=== VERIFICATION: Colonne Si oui, avec quelle fréquence ? ===\n")
col_to_verify_freq <- "11.) Suivi sanitaire/Si oui, avec quelle fréquence par ans? 0=Vides, 1=[1:3], 2=[4:6], 3=Autres"
if (col_to_verify_freq %in% names(raw)) {
  empty_count <- sum(is.na(raw[[col_to_verify_freq]]) | raw[[col_to_verify_freq]] == "")
  cat("Colonne:", col_to_verify_freq, "\n")
  cat("Nombre de cellules vides:", empty_count, "\n")
  if (empty_count > 0) {
    cat("⚠ WARNING: Trouvé", empty_count, "cellule(s) vide(s) dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_freq]], useNA = "ifany"))
  } else {
    cat("✓ SUCCESS: Aucune cellule vide trouvée dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_freq]], useNA = "ifany"))
  }
} else {
  cat("⚠ Colonne 'Si oui, avec quelle fréquence ?' non trouvée pour vérification\n")
}

#38.---- VERIFICATION: Colonne Si oui, quelles sont ces maladies ? - Pas de cellules vides -----
cat("\n=== VERIFICATION: Colonne Si oui, quelles sont ces maladies ? ===\n")
col_to_verify_maladies_spec <- "11.) Suivi sanitaire/Si oui, quelles sont ces maladies ? 0=Vides, 1=Anorexie, 2=Diarhee, 3=Autres"
if (col_to_verify_maladies_spec %in% names(raw)) {
  empty_count <- sum(is.na(raw[[col_to_verify_maladies_spec]]) | raw[[col_to_verify_maladies_spec]] == "")
  cat("Colonne:", col_to_verify_maladies_spec, "\n")
  cat("Nombre de cellules vides:", empty_count, "\n")
  if (empty_count > 0) {
    cat("⚠ WARNING: Trouvé", empty_count, "cellule(s) vide(s) dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_maladies_spec]], useNA = "ifany"))
  } else {
    cat("✓ SUCCESS: Aucune cellule vide trouvée dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_maladies_spec]], useNA = "ifany"))
  }
} else {
  cat("⚠ Colonne 'Si oui, quelles sont ces maladies ?' non trouvée pour vérification\n")
}

#39.---- VERIFICATION: Colonne Si oui, quelles sont les causes selon vous ?...242 - Pas de cellules vides -----
cat("\n=== VERIFICATION: Colonne Si oui, quelles sont les causes selon vous ?...242 ===\n")
col_to_verify_causes <- "11.) Suivi sanitaire/Si oui, quelles sont les causes selon vous ?...242/ 0=Vides, 1=Effet genetique, 2=Resistance faible de metis"
if (col_to_verify_causes %in% names(raw)) {
  empty_count <- sum(is.na(raw[[col_to_verify_causes]]) | raw[[col_to_verify_causes]] == "")
  cat("Colonne:", col_to_verify_causes, "\n")
  cat("Nombre de cellules vides:", empty_count, "\n")
  if (empty_count > 0) {
    cat("⚠ WARNING: Trouvé", empty_count, "cellule(s) vide(s) dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_causes]], useNA = "ifany"))
  } else {
    cat("✓ SUCCESS: Aucune cellule vide trouvée dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_causes]], useNA = "ifany"))
  }
} else {
  cat("⚠ Colonne 'Si oui, quelles sont les causes selon vous ?' non trouvée pour vérification\n")
}

#40.---- VERIFICATION: Colonne Si oui, quelles sont les causes selon vous ?...246 - Pas de cellules vides -----
cat("\n=== VERIFICATION: Colonne Si oui, quelles sont les causes selon vous ?...246 ===\n")
col_to_verify_causes_246 <- "11.) Suivi sanitaire/Si oui, quelles sont les causes selon vous ?...246/ 0=Vides, 1=Du au climat, 2=Faute au traitement, 3=Race metis fragile"
if (col_to_verify_causes_246 %in% names(raw)) {
  empty_count <- sum(is.na(raw[[col_to_verify_causes_246]]) | raw[[col_to_verify_causes_246]] == "")
  cat("Colonne:", col_to_verify_causes_246, "\n")
  cat("Nombre de cellules vides:", empty_count, "\n")
  if (empty_count > 0) {
    cat("⚠ WARNING: Trouvé", empty_count, "cellule(s) vide(s) dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_causes_246]], useNA = "ifany"))
  } else {
    cat("✓ SUCCESS: Aucune cellule vide trouvée dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_causes_246]], useNA = "ifany"))
  }
} else {
  cat("⚠ Colonne 'Si oui, quelles sont les causes selon vous ?...246' non trouvée pour vérification\n")
}

#41.---- VERIFICATION: Colonne Comment (quelles sont vos observations?...) - Pas de cellules vides -----
cat("\n=== VERIFICATION: Colonne Comment (quelles sont vos observations?...) ===\n")
col_to_verify_comment_obs <- "10.) Contexte environnemental/Comment (quelles sont vos observations? si ne sait pas si les CC affectent son élevage??? 0=Vides, 1=Probleme du climat, 2=Autres"
if (col_to_verify_comment_obs %in% names(raw)) {
  empty_count <- sum(is.na(raw[[col_to_verify_comment_obs]]) | raw[[col_to_verify_comment_obs]] == "")
  cat("Colonne:", col_to_verify_comment_obs, "\n")
  cat("Nombre de cellules vides:", empty_count, "\n")
  if (empty_count > 0) {
    cat("⚠ WARNING: Trouvé", empty_count, "cellule(s) vide(s) dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_comment_obs]], useNA = "ifany"))
  } else {
    cat("✓ SUCCESS: Aucune cellule vide trouvée dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_comment_obs]], useNA = "ifany"))
  }
} else {
  cat("⚠ Colonne 'Comment (quelles sont vos observations?...)' non trouvée pour vérification\n")
}

#42.---- VERIFICATION: Colonne Si oui, à quel moment de l'année ? (variante apostrophe) - Pas de cellules vides -----
cat("\n=== VERIFICATION: Colonne Si oui, à quel moment de l’année ? (variante) ===\n")
col_to_verify_moment_an_v2 <- "11.) Suivi sanitaire/Si oui, à quel moment de l’année ? 0=Vides, 1={1,2 par ans}, 2=Autres"
if (col_to_verify_moment_an_v2 %in% names(raw)) {
  empty_count <- sum(is.na(raw[[col_to_verify_moment_an_v2]]) | raw[[col_to_verify_moment_an_v2]] == "")
  cat("Colonne:", col_to_verify_moment_an_v2, "\n")
  cat("Nombre de cellules vides:", empty_count, "\n")
  if (empty_count > 0) {
    cat("⚠ WARNING: Trouvé", empty_count, "cellule(s) vide(s) dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_moment_an_v2]], useNA = "ifany"))
  } else {
    cat("✓ SUCCESS: Aucune cellule vide trouvée dans cette colonne!\n")
    cat("Distribution des valeurs:\n")
    print(table(raw[[col_to_verify_moment_an_v2]], useNA = "ifany"))
  }
} else {
  cat("⚠ Colonne 'Si oui, à quel moment de l’année ?' (variante) non trouvée pour vérification\n")
}
