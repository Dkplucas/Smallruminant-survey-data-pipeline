# Cleaning of 4.) Reproduction (UE)

library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)

# ============================================================================
# CONFIGURATION
# ============================================================================

COLS_TO_DELETE <- list(
  list(pattern = "si.*oui.*comment.*faites", label = "Si « Oui », comment faites-vous ?"),
  list(pattern = "6=autre.*préciser|6=autre.*preciser", label = "6=autre (préciser)"),
  list(pattern = "difficultés.*alimentation|difficultes.*alimentation", label = "Quelles sont les difficultés liées à l'alimentation des animaux ?")
)

COLS_TO_RENAME_AND_TRANSFORM <- list(
  list(pattern = "disposez.*système.*marquage.*systématique|disposez.*systeme.*marquage.*systematique",
       new_name = "4.) Conduite des animaux/Est-ce que vous disposez d'un système de marquage systématique pour l'identification et le suivi des animaux de votre élevage ?/0=Non, 1=Oui"),
  list(pattern = "préférerez.*boucles.*collier|preferez.*boucles.*collier",
       new_name = "4.) Conduite des animaux/Que préférerez-vous entre les boucles et un collier attaché au cou  pour le marquage de vos animaux ? 0=Vides/Pas de preference, 1=Boucles, 2=Collier",
       mappings = list("0" = c("pas de préférence", "pas de preference"),
                       "1" = c("boucles"),
                       "2" = c("collier"))),
  list(pattern = "animaux.*conduits.*pâturage|animaux.*conduits.*paturage",
       new_name = "4.) Conduite des animaux/Vos animaux sont-ils conduits au pâturage ? 0=Non, 1=Oui"),
  list(pattern = "pâturage.*compléments.*alimentaires|paturage.*complements.*alimentaires",
       new_name = "4.) Conduite des animaux/Après le pâturage servez-vous de compléments alimentaires aux animaux ? 0=Non, 1=Oui"),
  list(pattern = "faîtes.*complément.*minéral|faites.*complement.*mineral",
       new_name = "4.) Conduite des animaux/Faîtes-vous le complément minéral ? 0=Non, 1=Oui")
)

COLS_TO_RENAME_AND_FILL <- list(
  list(pattern = "prêt.*adopter.*marquage.*\\s/Oui$|pret.*adopter.*marquage.*\\s/Oui$",
       new_name = "4.) Conduite des animaux/Si « Non », est-ce que vous serez prêt à adopter et utiliser un système de marquage pour une identification et un meilleur suivi de vos animaux ? /Oui/0=Non, 1=Oui"),
  list(pattern = "prêt.*adopter.*marquage.*\\s/Non$|pret.*adopter.*marquage.*\\s/Non$",
       new_name = "4.) Conduite des animaux/Si « Non », est-ce que vous serez prêt à adopter et utiliser un système de marquage pour une identification et un meilleur suivi de vos animaux ? /Non/0=Non, 1=Oui"),
  list(pattern = "assure.*conduite.*pâturage.*1.*éleveur|assure.*conduite.*paturage.*1.*eleveur",
       new_name = "4.) Conduite des animaux/Si oui, qui assure la conduite au pâturage ?/1=éleveur lui-même/0=Non, 1=Oui"),
  list(pattern = "assure.*conduite.*pâturage.*2.*animalier|assure.*conduite.*paturage.*2.*animalier",
       new_name = "4.) Conduite des animaux/Si oui, qui assure la conduite au pâturage ?/2=animalier/0=Non, 1=Oui"),
  list(pattern = "assure.*conduite.*pâturage.*3.*famille|assure.*conduite.*paturage.*3.*famille",
       new_name = "4.) Conduite des animaux/Si oui, qui assure la conduite au pâturage ?/3=membre de la famille/0=Non, 1=Oui"),
  list(pattern = "pratiques.*conduite.*claustration.*saison.*cultures|pratiques.*conduite.*claustration.*saison.*cultures",
       new_name = "4.) Conduite des animaux/Si non, quels sont les pratiques de conduite adoptées par les éleveurs ? /1= Claustration stricte uniquement pendant la saison des cultures/0=Non, 1=Oui"),
  list(pattern = "pratiques.*conduite.*claustration.*toutes.*saisons|pratiques.*conduite.*claustration.*toutes.*saisons",
       new_name = "4.) Conduite des animaux/Si non, quels sont les pratiques de conduite adoptées par les éleveurs ? /2= Claustration stricte en toutes saisons/0=Non, 1=Oui"),
  list(pattern = "pratiques.*conduite.*divagation.*totale|pratiques.*conduite.*divagation.*totale",
       new_name = "4.) Conduite des animaux/Si non, quels sont les pratiques de conduite adoptées par les éleveurs ? /3= Divagation totale en toutes saisons/0=Non, 1=Oui"),
  list(pattern = "pratiques.*conduite.*divagation.*partielle|pratiques.*conduite.*divagation.*partielle",
       new_name = "4.) Conduite des animaux/Si non, quels sont les pratiques de conduite adoptées par les éleveurs ? /4= Divagation partielle en saison sèche ou après récolte/0=Non, 1=Oui"),
  list(pattern = "pratiques.*conduite.*attaché.*piquets|pratiques.*conduite.*attache.*piquets",
       new_name = "4.) Conduite des animaux/Si non, quels sont les pratiques de conduite adoptées par les éleveurs ? /5= Attaché aux piquets au pâturage/0=Non, 1=Oui"),
  list(pattern = "pratiques.*conduite.*autres$|pratiques.*conduite.*\\s6=.*autres",
       new_name = "4.) Conduite des animaux/Si non, quels sont les pratiques de conduite adoptées par les éleveurs ? /6= Autres/0=Non, 1=Oui"),
  list(pattern = "compléments.*alimentaires.*sous-produits|complements.*alimentaires.*sous-produits|compléments.*alimentaires.*maraîchers|complements.*alimentaires.*maraîchers",
       new_name = "4.) Conduite des animaux/Si oui, quels sont ces compléments alimentaires ? /1= sous-produits maraîchers/0=Non, 1=Oui"),
  list(pattern = "compléments.*alimentaires.*résidus|complements.*alimentaires.*residus|compléments.*alimentaires.*récoltes|complements.*alimentaires.*recoltes",
       new_name = "4.) Conduite des animaux/Si oui, quels sont ces compléments alimentaires ? /2= résidus de récoltes/0=Non, 1=Oui"),
  list(pattern = "compléments.*alimentaires.*agro-industriels|complements.*alimentaires.*agro-industriels|compléments.*alimentaires.*spai|complements.*alimentaires.*spai",
       new_name = "4.) Conduite des animaux/Si oui, quels sont ces compléments alimentaires ? /3= sous-produits agro-industriels (SPAI)/0=Non, 1=Oui"),
  list(pattern = "compléments.*alimentaires.*fourrage.*arbres|complements.*alimentaires.*fourrage.*arbres|compléments.*alimentaires.*fourrage.*arbustes|complements.*alimentaires.*fourrage.*arbustes",
       new_name = "4.) Conduite des animaux/Si oui, quels sont ces compléments alimentaires ? /4= fourrage des arbustes/arbres/0=Non, 1=Oui"),
  list(pattern = "compléments.*alimentaires.*herbes.*nature|complements.*alimentaires.*herbes.*nature|compléments.*alimentaires.*herbes.*collectées|complements.*alimentaires.*herbes.*collectees",
       new_name = "4.) Conduite des animaux/Si oui, quels sont ces compléments alimentaires ? /5= Herbes collectées dans la nature/0=Non, 1=Oui"),
  list(pattern = "compléments.*alimentaires.*autre$|complements.*alimentaires.*autre$|compléments.*alimentaires.*\\s6=.*autre|complements.*alimentaires.*\\s6=.*autre",
       new_name = "4.) Conduite des animaux/Si oui, quels sont ces compléments alimentaires ? /6= autre/0=Non, 1=Oui"),
  list(pattern = "sources.*minéraux.*sel.*cuisine|sources.*mineraux.*sel.*cuisine",
       new_name = "4.) Conduite des animaux/Si oui, quelles sources de minéraux utilisez-vous ? /1= sel de cuisine/0=Non, 1=Oui"),
  list(pattern = "sources.*minéraux.*pierres.*lécher|sources.*mineraux.*pierres.*lecher",
       new_name = "4.) Conduite des animaux/Si oui, quelles sources de minéraux utilisez-vous ? /2= pierres à lécher/0=Non, 1=Oui"),
  list(pattern = "sources.*minéraux.*blocs.*multi|sources.*mineraux.*blocs.*multi",
       new_name = "4.) Conduite des animaux/Si oui, quelles sources de minéraux utilisez-vous ? /3= blocs multi-nutritionnels/0=Non, 1=Oui")
)

COLS_TO_FILL <- list(
  list(pattern = "quelles.*races.*chèvre.*commencé|quelles.*races.*chevre.*commence", label = "Races de chèvre initiales")
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

rename_and_transform_yesno <- function(raw, pattern, new_name, mappings = NULL) {
  col <- find_col(raw, pattern)
  if (is.na(col)) return(raw)

  cat("\nColonne trouvée:", col, "\n")
  cat("Valeurs avant transformation :\n")
  print(table(raw[[col]], useNA = "ifany"))

  if (!is.null(mappings)) {
    # Custom mappings provided
    raw <- raw %>%
      rename(!!new_name := all_of(col)) %>%
      mutate(!!new_name := as.character(!!sym(new_name))) %>%
      mutate(!!new_name := str_trim(!!sym(new_name))) %>%
      mutate(!!new_name := case_when(
        is.na(!!sym(new_name)) | !!sym(new_name) == "" ~ "0",
        str_detect(str_to_lower(!!sym(new_name)), "pas de préférence|pas de preference") ~ "0",
        str_detect(str_to_lower(!!sym(new_name)), "^boucles") ~ "1",
        str_detect(str_to_lower(!!sym(new_name)), "^collier") ~ "2",
        TRUE ~ !!sym(new_name)
      )) %>%
      {
        cat("✓ Colonne renommée et transformée\n")
        cat("Valeurs après transformation :\n")
        print(table(.[[new_name]], useNA = "ifany"))
        .
      }
  } else {
    # Default yes/no transformation
    raw <- raw %>%
      rename(!!new_name := all_of(col)) %>%
      mutate(!!new_name := as.character(!!sym(new_name))) %>%
      mutate(!!new_name := str_trim(!!sym(new_name))) %>%
      mutate(!!new_name := case_when(
        is.na(!!sym(new_name)) | !!sym(new_name) == "" ~ "0",
        str_to_lower(!!sym(new_name)) == "non" ~ "0",
        str_to_lower(!!sym(new_name)) == "oui" ~ "1",
        TRUE ~ !!sym(new_name)
      )) %>%
      {
        cat("✓ Colonne renommée et transformée\n")
        cat("Valeurs après transformation :\n")
        print(table(.[[new_name]], useNA = "ifany"))
        .
      }
  }

  raw
}

rename_and_fill_col <- function(raw, pattern, new_name) {
  col <- find_col(raw, pattern)
  if (is.na(col)) return(raw)

  cat("\nColonne trouvée:", col, "\n")
  cat("Valeurs avant remplissage :\n")
  print(table(raw[[col]], useNA = "ifany"))

  raw %>%
    rename(!!new_name := all_of(col)) %>%
    mutate(!!new_name := as.character(!!sym(new_name))) %>%
    mutate(!!new_name := case_when(
      is.na(!!sym(new_name)) | !!sym(new_name) == "" | !!sym(new_name) == "NA" ~ "0",
      TRUE ~ !!sym(new_name)
    )) %>%
    {
      cat("✓ Colonne renommée et cellules vides remplies par '0'\n")
      cat("Valeurs après remplissage :\n")
      print(table(.[[new_name]], useNA = "ifany"))
      .
    }
}

fill_empty_cols <- function(raw, pattern, label) {
  cols <- find_cols(raw, pattern)
  if (length(cols) == 0) {
    cat("⚠", label, "- aucune colonne trouvée\n")
    return(raw)
  }

  cat("\nColonnes trouvées:", length(cols), "\n")

  for (col in cols) {
    cat("\nTraitement de:", col, "\n")
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

  raw
}

# ============================================================================
# WORKFLOW
# ============================================================================

cat("=== CLEANING IV: REPRODUCTION ===\n\n")

# Load data
xlsx_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning/data3.xlsx"
raw <- read_excel(xlsx_path, col_names = TRUE)
cat("Fichier chargé :", nrow(raw), "lignes x", ncol(raw), "colonnes\n\n")

# Delete columns
cat("--- Suppression de colonnes ---\n")
for (spec in COLS_TO_DELETE) {
  raw <- delete_column(raw, spec$pattern, spec$label)
}
cat("Dimensions après suppression :", nrow(raw), "lignes x", ncol(raw), "colonnes\n\n")

# Rename and transform Yes/No columns
cat("--- Renommage et transformation des colonnes Non/Oui ---\n")
for (spec in COLS_TO_RENAME_AND_TRANSFORM) {
  raw <- rename_and_transform_yesno(raw, spec$pattern, spec$new_name, mappings = spec$mappings)
}

# Rename and fill columns
cat("\n--- Renommage et remplissage de colonnes ---\n")
for (spec in COLS_TO_RENAME_AND_FILL) {
  raw <- rename_and_fill_col(raw, spec$pattern, spec$new_name)
}

# Fill empty cells
cat("\n--- Remplissage des cellules vides ---\n")
for (spec in COLS_TO_FILL) {
  raw <- fill_empty_cols(raw, spec$pattern, spec$label)
}

# Export
cat("\n=== EXPORT DES DONNEES NETTOYEES ===\n")
output_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning/data4.xlsx"
if (!require(openxlsx, quietly = TRUE)) {
  install.packages("openxlsx")
  library(openxlsx)
}
write.xlsx(raw, output_path, rowNames = FALSE, overwrite = TRUE)
cat("✓ Fichier sauvegardé :", output_path, "\n")
cat("Dimensions finales :", nrow(raw), "lignes x", ncol(raw), "colonnes\n")
