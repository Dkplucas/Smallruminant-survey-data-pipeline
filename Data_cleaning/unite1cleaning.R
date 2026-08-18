# Cleaning of I.- IDENTIFICATION DU CHEF DE MENAGE (OPTIMIZED)
# This version reduces ~450 lines to ~150 by eliminating repetition

library(dplyr)
library(stringr)
library(readxl)
library(openxlsx)

# ============================================================================
# CONFIGURATION: Central specs for all column cleaning
# ============================================================================
COLUMN_SPECS <- list(
  sexe = list(
    pattern = "Sexe",
    new_name = "I.- IDENTIFICATION DU CHEF DE MENAGE /Sexe: 1=Masculin, 2=Feminin",
    codes = c("1" = "masculin", "2" = "feminin"),
    allow_missing = FALSE
  ),
  instruction = list(
    pattern = "Niveau.*instruction",
    new_name = "I.- IDENTIFICATION DU CHEF DE MENAGE /Niveau d'instruction: 1=Aucun, 2=Primaire, 3=Secondaire, 4=Superieure",
    codes = c("1" = "aucun", "2" = "primaire", "3" = "secondaire", "4" = "supérieur|superieure"),
    allow_missing = TRUE,
    missing_code = "1",
    transform_missing = TRUE
  ),
  age = list(
    pattern = "Catégorie.*âge",
    new_name = "I.- IDENTIFICATION DU CHEF DE MENAGE /Catégorie d'âge : 1= > 50ans,  2=30 a 50, 3=20 a 30",
    codes = c("1" = ">50ans|> 50", "2" = "30.*50|30 a 50", "3" = "20.*30|20 a 30"),
    allow_missing = FALSE
  ),
  matrimoniale = list(
    pattern = "Situation.*matrimoniale",
    new_name = "I.- IDENTIFICATION DU CHEF DE MENAGE /Situation matrimoniale : 1=Celibataire, 2=Divorce(e), 3=Marie, 4=Veuf/ve",
    codes = c("1" = "célibataire|celibataire", "2" = "divorcé|divorc", "3" = "marié|marie", "4" = "veuf|veuve"),
    allow_missing = FALSE
  ),
  activite = list(
    pattern = "principale.*activité|principale.*activite",
    new_name = "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelle est votre principale activité? : 1=Agriculture, 2=Artisanat, 3=Autre, 4=Commerce, 5=Elevage",
    codes = c("1" = "agriculture", "2" = "artisanat", "3" = "autre", "4" = "commerce", "5" = "elevage"),
    allow_missing = FALSE
  ),
  revenu = list(
    pattern = "principale.*source.*revenu|activité.*revenu",
    new_name = "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelle est votre principale source de revenu? ou De quelle activité provient principalement vos revenus?: 1=Agriculture, 2=Artisanat, 3=Autre, 4=Commerce, 5=Elevage, 6=Vide",
    codes = c("1" = "agriculture", "2" = "artisanat", "3" = "autre", "4" = "commerce", "5" = "elevage"),
    allow_missing = TRUE,
    missing_code = "6"
  ),
  formation = list(
    pattern = "formation.*élevage|formation.*elevage",
    new_name = "I.- IDENTIFICATION DU CHEF DE MENAGE /Avez vous reçu oui suivi une Formation en élevage ?: 0=Non, 1=Oui",
    codes = c("0" = "non", "1" = "oui"),
    allow_missing = FALSE
  ),
  op = list(
    pattern = "organisation.*paysanne|coopérative|cooperative",
    new_name = "I.- IDENTIFICATION DU CHEF DE MENAGE /Appartenez vous à une Organisation Paysanne (OP) ou coopérative...?: 0=Non, 1=Oui",
    codes = c("0" = "non", "1" = "oui"),
    allow_missing = TRUE,
    missing_code = "0"
  )
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Find column by pattern (safely returns NA if not found)
find_col <- function(df, pattern) {
  matches <- names(df)[grep(pattern, names(df), ignore.case = TRUE)]
  if (length(matches) == 0) NA else matches[1]
}

# Clean and recode a single categorical column
clean_categorical_col <- function(raw, spec, verbose = TRUE) {
  col <- find_col(raw, spec$pattern)

  if (is.na(col)) {
    if (verbose) cat("⚠ Column matching '", spec$pattern, "' not found\n", sep = "")
    return(raw)
  }

  if (verbose) {
    cat("\n--- Cleaning:", spec$new_name, "---\n")
    cat("Before:\n")
    print(table(raw[[col]], useNA = "ifany"))
  }

  raw <- raw %>%
    rename(!!spec$new_name := all_of(col)) %>%
    mutate(!!spec$new_name := str_trim(!!sym(spec$new_name)))

  # Build case_when conditions dynamically
  cond_expr <- paste0(
    "raw %>% mutate(!!spec$new_name := case_when(\n",
    if (spec$allow_missing) paste0("is.na(!!sym(spec$new_name)) | !!sym(spec$new_name) == '' ~ '", spec$missing_code, "',\n"),
    paste(
      sprintf("str_detect(str_to_lower(!!sym(spec$new_name)), '%s') ~ '%s'",
              spec$codes, names(spec$codes)),
      collapse = ",\n"
    ),
    ",\nTRUE ~ !!sym(spec$new_name)\n))"
  )

  raw <- eval(parse(text = cond_expr))

  # Transform old missing code to new missing code if specified
  if (!is.null(spec$transform_missing) && spec$transform_missing && !is.null(spec$missing_code)) {
    # Find what the old missing code was (typically "5" for vide)
    old_missing_codes <- c("5", "NA")  # Common missing code representations
    raw <- raw %>%
      mutate(!!spec$new_name := case_when(
        !!sym(spec$new_name) %in% old_missing_codes ~ spec$missing_code,
        TRUE ~ !!sym(spec$new_name)
      ))
  }

  if (verbose) {
    cat("After:\n")
    print(table(raw[[spec$new_name]], useNA = "ifany"))
  }

  raw
}

# Fill NA/empty cells with a default value across multiple columns
fill_empty_cells <- function(raw, cols, fill_value = "0", verbose = TRUE) {
  if (length(cols) == 0) return(raw)

  if (verbose) {
    cat("\nFilling empty cells in", length(cols), "columns with '", fill_value, "'...\n", sep = "")
  }

  raw %>%
    mutate(across(all_of(cols),
      ~case_when(
        is.na(.) | . == "" | . == "NA" ~ fill_value,
        TRUE ~ as.character(.)
      )
    ))
}

# Convert year to years of experience
years_since <- function(year_str, current_year = 2026) {
  if (is.na(year_str) || year_str == "" || year_str == "NA") {
    return("0")
  }
  if (str_detect(year_str, "^\\d{4}$")) {
    return(as.character(current_year - as.numeric(year_str)))
  }
  return(as.character(year_str))
}

# Find and remove columns matching patterns
remove_cols_by_pattern <- function(raw, patterns, verbose = TRUE) {
  cols_to_remove <- c()
  for (pattern in patterns) {
    cols_to_remove <- c(cols_to_remove, find_col(raw, pattern))
  }
  cols_to_remove <- cols_to_remove[!is.na(cols_to_remove)]

  if (length(cols_to_remove) > 0) {
    if (verbose) {
      cat("\nRemoving", length(cols_to_remove), "column(s):\n")
      print(cols_to_remove)
    }
    raw <- raw %>% select(-all_of(cols_to_remove))
  } else if (verbose) {
    cat("\nNo columns found matching patterns\n")
  }

  raw
}

# ============================================================================
# MAIN WORKFLOW
# ============================================================================

# Setup
cat("=== DATA CLEANING: HOUSEHOLD IDENTIFICATION ===\n\n")
setwd("c:/Users/lucas/Downloads/")

xlsx_path <- "c:/Users/lucas/Downloads/Questionnaire_caracterisation_pratiques_de_croisements_-_latest_version_-_labels_-_2026-07-28-03-29-08.xlsx"

# Load data
raw <- read_excel(xlsx_path, col_names = TRUE)
cat("Initial load:", nrow(raw), "rows ×", ncol(raw), "columns\n\n")

# Remove sensitive/timing columns
raw <- raw %>%
  select(-any_of(c("start", "end",
                   "I.- IDENTIFICATION DU CHEF DE MENAGE /Code Enquêteur",
                   "I.- IDENTIFICATION DU CHEF DE MENAGE /Nom et prénoms de l'enquêté")))
cat("After removing sensitive columns:", nrow(raw), "rows ×", ncol(raw), "columns\n\n")

# Clean categorical columns using specs
raw <- clean_categorical_col(raw, COLUMN_SPECS$sexe)
raw <- clean_categorical_col(raw, COLUMN_SPECS$instruction)
raw <- clean_categorical_col(raw, COLUMN_SPECS$age)
raw <- clean_categorical_col(raw, COLUMN_SPECS$matrimoniale)
raw <- clean_categorical_col(raw, COLUMN_SPECS$activite)
raw <- clean_categorical_col(raw, COLUMN_SPECS$revenu)
raw <- clean_categorical_col(raw, COLUMN_SPECS$formation)
raw <- clean_categorical_col(raw, COLUMN_SPECS$op)

# Remove rows for excluded zones
cat("\n--- Removing first 2 rows (excluded zones) ---\n")
raw <- raw %>% slice(-c(1:2))
cat("Rows after removal:", nrow(raw), "\n")

# Remove "Si Autre" specification columns
raw <- remove_cols_by_pattern(raw,
  c("Si.*Autre.*precis", "precision.*autre.*activité", "precision.*autre.*secondaire"))

# Remove specific column "Si Autre, preciser...12"
cat("\n--- Removing 'Si Autre, preciser...12' column ---\n")
raw <- raw %>%
  select(-any_of(c("I.- IDENTIFICATION DU CHEF DE MENAGE /Si Autre, preciser...12")))
if (!"I.- IDENTIFICATION DU CHEF DE MENAGE /Si Autre, preciser...12" %in% names(raw)) {
  cat("✓ Column removed successfully\n")
} else {
  cat("⚠ Column still present\n")
}

# Remove specific column "SI Autre source principale de revenu préciser"
cat("\n--- Removing 'SI Autre source principale de revenu préciser' column ---\n")
raw <- raw %>%
  select(-any_of(c("I.- IDENTIFICATION DU CHEF DE MENAGE /SI Autre source principale de revenu préciser")))
if (!"I.- IDENTIFICATION DU CHEF DE MENAGE /SI Autre source principale de revenu préciser" %in% names(raw)) {
  cat("✓ Column removed successfully\n")
} else {
  cat("⚠ Column still present\n")
}

# Handle secondary activities (multi-select)
cat("\n--- Processing secondary activities ---\n")
secondary_activity_patterns <- c(
  "activites.*secondaires.*elevage",
  "activites.*secondaires.*agriculture",
  "activites.*secondaires.*commerce",
  "activites.*secondaires.*artisanat",
  "activites.*secondaires.*autre"
)
secondary_cols <- sapply(secondary_activity_patterns, function(p) find_col(raw, p))
secondary_cols <- secondary_cols[!is.na(secondary_cols)]

raw <- fill_empty_cells(raw, secondary_cols, "0")

# Rename secondary activity column: Elevage
cat("\n--- Renaming secondary activity column: Elevage ---\n")
col_elevage <- find_col(raw, "activites.*secondaires.*elevage")
if (!is.na(col_elevage)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont vos activites secondaires ?/Elevage: 0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_elevage)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename secondary activity column: Agriculture
cat("\n--- Renaming secondary activity column: Agriculture ---\n")
col_agriculture <- find_col(raw, "activites.*secondaires.*agriculture")
if (!is.na(col_agriculture)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont vos activites secondaires ?/Agriculture: 0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_agriculture)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename secondary activity column: Commerce
cat("\n--- Renaming secondary activity column: Commerce ---\n")
col_commerce <- find_col(raw, "activites.*secondaires.*commerce")
if (!is.na(col_commerce)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont vos activites secondaires ?/Commerce: 0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_commerce)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename secondary activity column: Artisanat
cat("\n--- Renaming secondary activity column: Artisanat ---\n")
col_artisanat <- find_col(raw, "activites.*secondaires.*artisanat")
if (!is.na(col_artisanat)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont vos activites secondaires ?/Artisanat: 0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_artisanat)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename secondary activity column: Autre
cat("\n--- Renaming secondary activity column: Autre ---\n")
col_autre <- find_col(raw, "activites.*secondaires.*autre")
if (!is.na(col_autre)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont vos activites secondaires ?/Autre: 0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_autre)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename main cultures column: maïs
cat("\n--- Renaming main cultures column: maïs ---\n")
col_cultures_mais <- find_col(raw, "principales.*cultures.*maïs|principales.*cultures.*mais")
if (!is.na(col_cultures_mais)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont les principales cultures pratiquées par votre ménage ? /1=maïs/0=Vides,1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_cultures_mais))
  cat("✓ Column renamed successfully\n")
  cat("Unique values after renaming:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename main cultures column: manioc
cat("\n--- Renaming main cultures column: manioc ---\n")
col_cultures_manioc <- find_col(raw, "principales.*cultures.*manioc")
if (!is.na(col_cultures_manioc)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont les principales cultures pratiquées par votre ménage ? /2=manioc/0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_cultures_manioc)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename main cultures column: igname
cat("\n--- Renaming main cultures column: igname ---\n")
col_cultures_igname <- find_col(raw, "principales.*cultures.*igname")
if (!is.na(col_cultures_igname)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont les principales cultures pratiquées par votre ménage ? /3=igname/0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_cultures_igname)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename main cultures column: arachide
cat("\n--- Renaming main cultures column: arachide ---\n")
col_cultures_arachide <- find_col(raw, "principales.*cultures.*arachide")
if (!is.na(col_cultures_arachide)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont les principales cultures pratiquées par votre ménage ? /4=arachide/0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_cultures_arachide)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename main cultures column: haricot (niébé)
cat("\n--- Renaming main cultures column: haricot (niébé) ---\n")
col_cultures_haricot <- find_col(raw, "principales.*cultures.*haricot|principales.*cultures.*nié")
if (!is.na(col_cultures_haricot)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont les principales cultures pratiquées par votre ménage ? /5=haricot (niébé)/0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_cultures_haricot)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename main cultures column: soja
cat("\n--- Renaming main cultures column: soja ---\n")
col_cultures_soja <- find_col(raw, "principales.*cultures.*soja")
if (!is.na(col_cultures_soja)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont les principales cultures pratiquées par votre ménage ? /6=soja/0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_cultures_soja)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename main cultures column: mil et sorgho
cat("\n--- Renaming main cultures column: mil et sorgho ---\n")
col_cultures_mil <- find_col(raw, "principales.*cultures.*mil|principales.*cultures.*sorgho")
if (!is.na(col_cultures_mil)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont les principales cultures pratiquées par votre ménage ? /7=mil et sorgho/0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_cultures_mil)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename main cultures column: patate douce
cat("\n--- Renaming main cultures column: patate douce ---\n")
col_cultures_patate <- find_col(raw, "principales.*cultures.*patate|principales.*cultures.*douce")
if (!is.na(col_cultures_patate)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont les principales cultures pratiquées par votre ménage ? /8=patate douce/0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_cultures_patate)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename main cultures column: pomme de terre
cat("\n--- Renaming main cultures column: pomme de terre ---\n")
col_cultures_pomme <- find_col(raw, "principales.*cultures.*pomme|principales.*cultures.*terre")
if (!is.na(col_cultures_pomme)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont les principales cultures pratiquées par votre ménage ? /9=pomme de terre/0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_cultures_pomme)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename main cultures column: légumes
cat("\n--- Renaming main cultures column: légumes ---\n")
col_cultures_legumes <- find_col(raw, "principales.*cultures.*légumes|principales.*cultures.*legumes")
if (!is.na(col_cultures_legumes)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont les principales cultures pratiquées par votre ménage ? /10=légumes/0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_cultures_legumes)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Rename main cultures column: autre (à préciser)
cat("\n--- Renaming main cultures column: autre (à préciser) ---\n")
col_cultures_autre <- find_col(raw, "principales.*cultures.*autre|principales.*cultures.*précis")
if (!is.na(col_cultures_autre)) {
  col_name_new <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont les principales cultures pratiquées par votre ménage ? /11=autre (à préciser)/0=Non, 1=Oui"
  raw <- raw %>%
    rename(!!col_name_new := all_of(col_cultures_autre)) %>%
    mutate(!!col_name_new := case_when(
      is.na(!!sym(col_name_new)) | !!sym(col_name_new) == "" ~ "0",
      TRUE ~ as.character(!!sym(col_name_new))
    ))
  cat("✓ Column renamed and empty cells filled with '0'\n")
  cat("Unique values after processing:\n")
  print(table(raw[[col_name_new]], useNA = "ifany"))
}

# Remove specific column "Si 11=autre (à préciser)"
cat("\n--- Removing 'Si 11=autre (à préciser)' column ---\n")
raw <- raw %>%
  select(-any_of(c("I.- IDENTIFICATION DU CHEF DE MENAGE /Si 11=autre (à préciser)")))
if (!"I.- IDENTIFICATION DU CHEF DE MENAGE /Si 11=autre (à préciser)" %in% names(raw)) {
  cat("✓ Column removed successfully\n")
} else {
  cat("⚠ Column still present\n")
}

# Handle numeric fields with year conversion
cat("\n--- Processing experience and land fields ---\n")

# Years since formation training
col_depuis <- find_col(raw, "si.*oui.*depuis|depuis.*quand")
if (!is.na(col_depuis)) {
  raw <- raw %>%
    mutate(!!col_depuis := as.character(!!sym(col_depuis))) %>%
    mutate(!!col_depuis := sapply(!!sym(col_depuis), years_since))
}

# Experience, land, and main crops: fill zeros for missing
numeric_fields <- c(
  find_col(raw, "expérience.*élevage|experience.*elevage"),
  find_col(raw, "superficie.*terre.*agricole|terre.*agricole.*valeur"),
  find_col(raw, "principales.*cultures")
)
numeric_fields <- numeric_fields[!is.na(numeric_fields)]
raw <- fill_empty_cells(raw, numeric_fields, "0")

# Remove unused year/OP name columns
raw <- remove_cols_by_pattern(raw,
  c("année.*création|annee.*creation", "si.*oui.*nom.*op|nom.*op"))

# Summary
cat("\n=== SUMMARY ===\n")
cat("Final dimensions:", nrow(raw), "rows ×", ncol(raw), "columns\n")
cat("Columns cleaned: ", length(COLUMN_SPECS), "\n", sep = "")

# Export
cat("\n=== EXPORTING ===\n")
output_path <- "C:/Users/lucas/OneDrive/Bureau/Data/Data_cleaning/data1.xlsx"
write.xlsx(raw, output_path, rowNames = FALSE, overwrite = TRUE)
cat("✓ Saved to:", output_path, "\n")

