#' Cleaning of 6.) Caractéristiques des Chèvres Métissées and 7.) Performances (UE)
#' Optimized version: reduced code duplication, faster execution, better maintainability

library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)

# === CONFIG ===
INPUT_FILE <- "data4.xlsx"
OUTPUT_FILE <- "data5.xlsx"

# === HELPER FUNCTION: Find and transform a single column ===
#' Apply standard cleaning transformation to a column
#' @param data DataFrame
#' @param pattern Regex pattern to find column
#' @param new_name New column name after renaming
#' @param transform_rules List of rules: list(pattern = "regex", value = "replacement")
#' @param verbose Print before/after tables
apply_column_transform <- function(data, pattern, new_name, transform_rules, verbose = TRUE) {
  col_old <- names(data)[grep(pattern, names(data), ignore.case = TRUE)][1]

  if (is.na(col_old)) {
    if (verbose) cat("\n⚠ Column matching '", pattern, "' not found\n", sep = "")
    return(data)
  }

  if (verbose) {
    cat("\nFound column:", col_old, "\n")
    cat("Values before:\n")
    print(table(data[[col_old]], useNA = "ifany"))
  }

  # Single mutate with all transformations combined
  data <- data %>%
    rename(!!new_name := all_of(col_old)) %>%
    mutate(
      !!new_name := as.character(!!sym(new_name)),
      !!new_name := str_trim(!!sym(new_name)),
      !!new_name := case_when(
        is.na(!!sym(new_name)) | !!sym(new_name) == "" ~ "0",
        .default = !!sym(new_name)
      )
    )

  # Apply pattern-based transformations
  for (rule in transform_rules) {
    data <- data %>%
      mutate(
        !!new_name := if_else(
          str_detect(str_to_lower(!!sym(new_name)), rule$pattern),
          rule$value,
          !!sym(new_name)
        )
      )
  }

  if (verbose) {
    cat("✓ Column transformed\n")
    cat("Values after:\n")
    print(table(data[[new_name]], useNA = "ifany"))
  }

  data
}

#' Apply fill-empty transformation to one or more columns
#' @param data DataFrame
#' @param pattern Regex pattern to find columns
#' @param verbose Print before/after tables
apply_fill_empty <- function(data, pattern, verbose = TRUE) {
  cols <- names(data)[grep(pattern, names(data), ignore.case = TRUE)]

  if (length(cols) == 0) {
    if (verbose) cat("\n⚠ No columns matching '", pattern, "' found\n", sep = "")
    return(data)
  }

  if (verbose) {
    cat("\nFound", length(cols), "column(s):\n")
    print(cols)
  }

  # Batch process all matching columns at once
  data <- data %>%
    mutate(across(
      all_of(cols),
      ~case_when(
        is.na(.) | . == "" | . == "NA" ~ "0",
        .default = as.character(.)
      )
    ))

  if (verbose) {
    for (col in cols) {
      cat("\n✓", col, "\n")
      print(table(data[[col]], useNA = "ifany"))
    }
  }

  data
}

# === LOAD DATA ===
cat("Loading data...\n")
raw <- read_excel(INPUT_FILE, col_names = TRUE)
cat("Loaded:", nrow(raw), "rows ×", ncol(raw), "columns\n")

# === EARLY PROCESSING: Introduction of exotic race and males reproducers ===
# Use grep to find columns and process them
cat("\n=== EARLY: Processing Exotic Race and Males Reproducers Columns ===\n")

# Find exotic race column
exotic_cols <- names(raw)[grep("quand.*même.*introduit.*race.*exotique|passé.*race.*exotique", names(raw), ignore.case = TRUE)]
if (length(exotic_cols) > 0) {
  old_col_exotic <- exotic_cols[1]
  new_col_exotic <- "5.) Reproduction/Avez-vous quand même introduit une fois par le passé une race exotique ou des métis issu d'animaux de race exotiques dans votre élevage ? 0=Non, 1=Oui"

  cat("\n✓ Found exotic race column\n")
  cat("Values before:\n")
  print(table(raw[[old_col_exotic]], useNA = "ifany"))

  raw <- raw %>%
    rename(!!new_col_exotic := all_of(old_col_exotic)) %>%
    mutate(
      !!new_col_exotic := as.character(!!sym(new_col_exotic)),
      !!new_col_exotic := str_trim(!!sym(new_col_exotic)),
      !!new_col_exotic := case_when(
        is.na(!!sym(new_col_exotic)) | !!sym(new_col_exotic) == "" ~ "0",
        str_detect(str_to_lower(!!sym(new_col_exotic)), "^non$") ~ "0",
        str_detect(str_to_lower(!!sym(new_col_exotic)), "^oui$") ~ "1",
        .default = "0"
      )
    )

  cat("✓ Exotic race column transformed\n")
  cat("Values after:\n")
  print(table(raw[[new_col_exotic]], useNA = "ifany"))
} else {
  cat("⚠ Exotic race column not found\n")
}

# Find males reproducers column
males_cols <- names(raw)[grep("y-a-t-il.*mâles.*reproducteurs|males.*reproducteurs.*actuellement", names(raw), ignore.case = TRUE)]
if (length(males_cols) > 0) {
  old_col_males <- males_cols[1]
  new_col_males <- "5.) Reproduction/Y-a-t-il de mâles reproducteurs actuellement dans l'élevage ? 0=Non, 1=Oui"

  cat("\n✓ Found males reproducers column\n")
  cat("Values before:\n")
  print(table(raw[[old_col_males]], useNA = "ifany"))

  raw <- raw %>%
    rename(!!new_col_males := all_of(old_col_males)) %>%
    mutate(
      !!new_col_males := as.character(!!sym(new_col_males)),
      !!new_col_males := str_trim(!!sym(new_col_males)),
      !!new_col_males := case_when(
        is.na(!!sym(new_col_males)) | !!sym(new_col_males) == "" ~ "0",
        str_detect(str_to_lower(!!sym(new_col_males)), "^non$") ~ "0",
        str_detect(str_to_lower(!!sym(new_col_males)), "^oui$") ~ "1",
        .default = "0"
      )
    )

  cat("✓ Males reproducers column transformed\n")
  cat("Values after:\n")
  print(table(raw[[new_col_males]], useNA = "ifany"))
} else {
  cat("⚠ Males reproducers column not found\n")
}

# === Fill empty cells in males reproducers sources columns ===
cat("\n=== Filling empty cells in males reproducers sources ===\n")

source_patterns <- list(
  "d.où.*achat",
  "d.où.*échange.*voisin|d.où.*echange.*voisin",
  "d.où.*héritage|d.où.*heritage",
  "d.où.*don",
  "d.où.*l.élevage|d.où.*l.elevage",
  "d.où.*centre.*recherche"
)

for (pattern in source_patterns) {
  source_cols <- names(raw)[grep(pattern, names(raw), ignore.case = TRUE)]

  if (length(source_cols) > 0) {
    col <- source_cols[1]
    cat("\nProcessing:", col, "\n")
    cat("Values before:\n")
    print(table(raw[[col]], useNA = "ifany"))

    raw <- raw %>%
      mutate(
        !!col := as.character(!!sym(col)),
        !!col := case_when(
          is.na(!!sym(col)) | !!sym(col) == "" ~ "0",
          .default = !!sym(col)
        )
      )

    cat("Values after:\n")
    print(table(raw[[col]], useNA = "ifany"))
  }
}

# === Usually have male reproducers when not currently available ===
cat("\n=== Processing: Usually have male reproducers column ===\n")

usually_have_cols <- names(raw)[grep("si.*non.*reproducteur.*mâle|disposez-vous.*habituellement|disposez.*vous.*habituellement", names(raw), ignore.case = TRUE)]

if (length(usually_have_cols) > 0) {
  old_col_usually <- usually_have_cols[1]
  new_col_usually <- "5.) Reproduction/Si \"non\", soit pas actuellement de reproducteur mâle », en disposez-vous habituellement dans l'élevage ? 0=Non, 1=Oui"

  cat("\n✓ Found usually have male reproducers column\n")
  cat("Values before:\n")
  print(table(raw[[old_col_usually]], useNA = "ifany"))

  raw <- raw %>%
    rename(!!new_col_usually := all_of(old_col_usually)) %>%
    mutate(
      !!new_col_usually := as.character(!!sym(new_col_usually)),
      !!new_col_usually := str_trim(!!sym(new_col_usually)),
      !!new_col_usually := case_when(
        is.na(!!sym(new_col_usually)) | !!sym(new_col_usually) == "" ~ "0",
        str_detect(str_to_lower(!!sym(new_col_usually)), "^oui$") ~ "1",
        str_detect(str_to_lower(!!sym(new_col_usually)), "^non$") ~ "0",
        .default = "0"
      )
    )

  cat("✓ Usually have male reproducers column transformed\n")
  cat("Values after:\n")
  print(table(raw[[new_col_usually]], useNA = "ifany"))
} else {
  cat("⚠ Usually have male reproducers column not found\n")
}

# === SECTION 1: Morphological differences ===
cat("\n=== 1. Morphological Differences ===\n")
raw <- apply_column_transform(
  raw,
  pattern = "avez-vous.*observé.*différences.*morphologiques|avez.*vous.*observe.*differences.*morphologiques",
  new_name = "6.) Caractéristiques des Chèvres Métissées/Avez-vous observé des différences morphologiques entre les chèvres métissées et la race locale Djallonké ?: 1=Oui, 0=Non, 3=Aucune idee",
  transform_rules = list(
    list(pattern = "^1=|oui", value = "1"),
    list(pattern = "aucune.*idée|aucune.*idee", value = "0"),
    list(pattern = "^0=|non", value = "0")
  )
)

# === SECTION 2: Size difference ===
cat("\n=== 2. Size Difference ===\n")
raw <- apply_column_transform(
  raw,
  pattern = "si.*oui.*quelle.*différence.*taille|si.*oui.*quelle.*difference.*taille",
  new_name = "6.) Caractéristiques des Chèvres Métissées/Si oui quelle différence: La Taille des metis est ..... que celle des Djallonke: 1: Plus Grande, 2: Vide",
  transform_rules = list(
    list(pattern = "plus.*grande", value = "1")
  )
)

# === SECTION 3: Weight difference ===
cat("\n=== 3. Weight Difference ===\n")
raw <- apply_column_transform(
  raw,
  pattern = "si.*oui.*quelle.*différence.*les.*metis|si.*oui.*quelle.*difference.*les.*metis",
  new_name = "6.) Caractéristiques des Chèvres Métissées/Si oui quelle différence: Les metis sont.... que ou aux Djallonké: 0=Vide, 1:Plus leger, 2:Plus lourd, 3:Similaire",
  transform_rules = list(
    list(pattern = "plus.*léger|plus.*leger", value = "1"),
    list(pattern = "plus.*lourd", value = "2"),
    list(pattern = "similaire", value = "3")
  )
)

# === SECTIONS 4-11: Fill empty cells in trait/performance columns ===
cat("\n=== 4-11. Trait & Performance Characteristics ===\n")

raw <- apply_fill_empty(raw, "autres.*traits.*distinction|traits.*distinction.*metis")
raw <- apply_fill_empty(raw, "7\\.).*croissance|Performances.*metis.*croissance")
raw <- apply_fill_empty(raw, "7\\.).*croissance.*similaire|Performances.*metis.*croissance.*similaire")
raw <- apply_fill_empty(raw, "7\\.).*croissance.*moins|Performances.*metis.*croissance.*moins")
raw <- apply_fill_empty(raw, "7\\.).*résistance.*maladies|Performances.*metis.*résistance|7\\.).*resistance.*maladies|Performances.*metis.*resistance")
raw <- apply_fill_empty(raw, "7\\.).*résistance.*maladies.*similaire|Performances.*metis.*résistance.*similaire|7\\.).*resistance.*maladies.*similaire|Performances.*metis.*resistance.*similaire")
raw <- apply_fill_empty(raw, "7\\.).*résistance.*maladies.*moins|Performances.*metis.*résistance.*moins|7\\.).*resistance.*maladies.*moins|Performances.*metis.*resistance.*moins")
raw <- apply_fill_empty(raw, "prolificité|prolificite|nombre.*chevreaux.*portée|nombre.*chevreaux.*portee")

# === RENAME COLUMNS ===
cat("\n=== RENAME COLUMNS ===\n")

# Rename Djallonke column
old_col_name_1 <- "5.) Reproduction/Avec quelles races de chèvre avez-vous commencé votre élevage de caprins ? /Locale Djallonke"
new_col_name_1 <- "5.) Reproduction/Avec quelles races de chèvre avez-vous commencé votre élevage de caprins ? /Locale Djallonke/0=Non, 1=Oui"

if (old_col_name_1 %in% names(raw)) {
  raw <- raw %>% rename(!!new_col_name_1 := all_of(old_col_name_1))
  cat("✓ Column renamed:", old_col_name_1, "→", new_col_name_1, "\n")
} else {
  cat("⚠ Djallonke column not found\n")
}

# Rename Sahelien column
old_col_name_2 <- "5.) Reproduction/Avec quelles races de chèvre avez-vous commencé votre élevage de caprins ? /Sahelien"
new_col_name_2 <- "5.) Reproduction/Avec quelles races de chèvre avez-vous commencé votre élevage de caprins ? /Sahelien/0=Non, 1=Oui"

if (old_col_name_2 %in% names(raw)) {
  raw <- raw %>% rename(!!new_col_name_2 := all_of(old_col_name_2))
  cat("✓ Column renamed:", old_col_name_2, "→", new_col_name_2, "\n")
} else {
  cat("⚠ Sahelien column not found\n")
}

# Rename Métis column
old_col_name_3 <- "5.) Reproduction/Avec quelles races de chèvre avez-vous commencé votre élevage de caprins ? /Métis"
new_col_name_3 <- "5.) Reproduction/Avec quelles races de chèvre avez-vous commencé votre élevage de caprins ? /Métis/0=Non, 1=Oui"

if (old_col_name_3 %in% names(raw)) {
  raw <- raw %>% rename(!!new_col_name_3 := all_of(old_col_name_3))
  cat("✓ Column renamed:", old_col_name_3, "→", new_col_name_3, "\n")
} else {
  cat("⚠ Métis column not found\n")
}

# Rename Autre à préciser column
old_col_name_4 <- "5.) Reproduction/Avec quelles races de chèvre avez-vous commencé votre élevage de caprins ? /Autre à préciser"
new_col_name_4 <- "5.) Reproduction/Avec quelles races de chèvre avez-vous commencé votre élevage de caprins ? /Autre à préciser/0=Non, 1=Oui"

if (old_col_name_4 %in% names(raw)) {
  raw <- raw %>% rename(!!new_col_name_4 := all_of(old_col_name_4))
  cat("✓ Column renamed:", old_col_name_4, "→", new_col_name_4, "\n")
} else {
  cat("⚠ Autre à préciser column not found\n")
}

# Rename saillie naturelle column
old_col_name_5 <- "5.) Reproduction/Quel mode de reproduction adoptez-vous dans le troupeau ?  /1= saillie naturelle"
new_col_name_5 <- "5.) Reproduction/Quel mode de reproduction adoptez-vous dans le troupeau ?  /1= saillie naturelle/0=Non, 1=Oui"

if (old_col_name_5 %in% names(raw)) {
  raw <- raw %>% rename(!!new_col_name_5 := all_of(old_col_name_5))
  cat("✓ Column renamed:", old_col_name_5, "→", new_col_name_5, "\n")
} else {
  cat("⚠ saillie naturelle column not found\n")
}

# Rename insémination artificielle column
old_col_name_6 <- "5.) Reproduction/Quel mode de reproduction adoptez-vous dans le troupeau ?  /2= insémination artificielle"
new_col_name_6 <- "5.) Reproduction/Quel mode de reproduction adoptez-vous dans le troupeau ?  /2= insémination artificielle/0=Non, 1=Oui"

if (old_col_name_6 %in% names(raw)) {
  raw <- raw %>% rename(!!new_col_name_6 := all_of(old_col_name_6))
  cat("✓ Column renamed:", old_col_name_6, "→", new_col_name_6, "\n")
} else {
  cat("⚠ insémination artificielle column not found\n")
}

# Rename autre column
old_col_name_7 <- "5.) Reproduction/Quel mode de reproduction adoptez-vous dans le troupeau ?  /3= autre"
new_col_name_7 <- "5.) Reproduction/Quel mode de reproduction adoptez-vous dans le troupeau ?  /3= autre/0=Non, 1=Oui"

if (old_col_name_7 %in% names(raw)) {
  raw <- raw %>% rename(!!new_col_name_7 := all_of(old_col_name_7))
  cat("✓ Column renamed:", old_col_name_7, "→", new_col_name_7, "\n")
} else {
  cat("⚠ autre column not found\n")
}

# Rename Suivi des chaleurs column
old_col_name_8 <- "5.) Reproduction/Quelles méthodes de contrôle de la reproduction utilisez-vous ? /1= Suivi des chaleurs"
new_col_name_8 <- "5.) Reproduction/Quelles méthodes de contrôle de la reproduction utilisez-vous ? /1= Suivi des chaleurs/0=Non, 1=Oui"

if (old_col_name_8 %in% names(raw)) {
  raw <- raw %>% rename(!!new_col_name_8 := all_of(old_col_name_8))
  cat("✓ Column renamed:", old_col_name_8, "→", new_col_name_8, "\n")
} else {
  cat("⚠ Suivi des chaleurs column not found\n")
}

# Rename Utilisation de calendriers de saillie column
old_col_name_9 <- "5.) Reproduction/Quelles méthodes de contrôle de la reproduction utilisez-vous ? /2= Utilisation de calendriers de saillie"
new_col_name_9 <- "5.) Reproduction/Quelles méthodes de contrôle de la reproduction utilisez-vous ? /2= Utilisation de calendriers de saillie/0=Non, 1=Oui"

if (old_col_name_9 %in% names(raw)) {
  raw <- raw %>% rename(!!new_col_name_9 := all_of(old_col_name_9))
  cat("✓ Column renamed:", old_col_name_9, "→", new_col_name_9, "\n")
} else {
  cat("⚠ Utilisation de calendriers de saillie column not found\n")
}

# Rename Aucun contrôle column
old_col_name_10 <- "5.) Reproduction/Quelles méthodes de contrôle de la reproduction utilisez-vous ? /3= Aucun contrôle"
new_col_name_10 <- "5.) Reproduction/Quelles méthodes de contrôle de la reproduction utilisez-vous ? /3= Aucun contrôle/0=Non, 1=Oui"

if (old_col_name_10 %in% names(raw)) {
  raw <- raw %>% rename(!!new_col_name_10 := all_of(old_col_name_10))
  cat("✓ Column renamed:", old_col_name_10, "→", new_col_name_10, "\n")
} else {
  cat("⚠ Aucun contrôle column not found\n")
}

# Rename and transform race exotique column
old_col_name_11 <- "5.) Reproduction/Avez-vous actuellement une race exotique dans votre troupeau ?"
new_col_name_11 <- "5.) Reproduction/Avez-vous actuellement une race exotique dans votre troupeau ? 0=Non, 1=Oui"

if (old_col_name_11 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_11 := all_of(old_col_name_11)) %>%
    mutate(
      !!new_col_name_11 := as.character(!!sym(new_col_name_11)),
      !!new_col_name_11 := str_trim(!!sym(new_col_name_11)),
      !!new_col_name_11 := case_when(
        is.na(!!sym(new_col_name_11)) | !!sym(new_col_name_11) == "" ~ "0",
        str_detect(str_to_lower(!!sym(new_col_name_11)), "^non$") ~ "0",
        str_detect(str_to_lower(!!sym(new_col_name_11)), "^oui$") ~ "1",
        .default = !!sym(new_col_name_11)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_11, "→", new_col_name_11, "\n")
} else {
  cat("⚠ race exotique column not found\n")
}

# Rename and transform Alpine column
old_col_name_12 <- "5.) Reproduction/Si oui, lesquelles ? /1= Alpine"
new_col_name_12 <- "5.) Reproduction/Si oui, lesquelles ? /1= Alpine/0=Non, 1=Oui"

if (old_col_name_12 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_12 := all_of(old_col_name_12)) %>%
    mutate(
      !!new_col_name_12 := as.character(!!sym(new_col_name_12)),
      !!new_col_name_12 := str_trim(!!sym(new_col_name_12)),
      !!new_col_name_12 := case_when(
        is.na(!!sym(new_col_name_12)) | !!sym(new_col_name_12) == "" ~ "0",
        .default = !!sym(new_col_name_12)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_12, "→", new_col_name_12, "\n")
} else {
  cat("⚠ Alpine column not found\n")
}

# Rename and transform Saanen column
old_col_name_13 <- "5.) Reproduction/Si oui, lesquelles ? /2= Saanen"
new_col_name_13 <- "5.) Reproduction/Si oui, lesquelles ? /2= Saanen/0=Non, 1=Oui"

if (old_col_name_13 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_13 := all_of(old_col_name_13)) %>%
    mutate(
      !!new_col_name_13 := as.character(!!sym(new_col_name_13)),
      !!new_col_name_13 := str_trim(!!sym(new_col_name_13)),
      !!new_col_name_13 := case_when(
        is.na(!!sym(new_col_name_13)) | !!sym(new_col_name_13) == "" ~ "0",
        .default = !!sym(new_col_name_13)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_13, "→", new_col_name_13, "\n")
} else {
  cat("⚠ Saanen column not found\n")
}

# Rename and transform Maradi column
old_col_name_14 <- "5.) Reproduction/Si oui, lesquelles ? /3= Maradi"
new_col_name_14 <- "5.) Reproduction/Si oui, lesquelles ? /3= Maradi/0=Non, 1=Oui"

if (old_col_name_14 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_14 := all_of(old_col_name_14)) %>%
    mutate(
      !!new_col_name_14 := as.character(!!sym(new_col_name_14)),
      !!new_col_name_14 := str_trim(!!sym(new_col_name_14)),
      !!new_col_name_14 := case_when(
        is.na(!!sym(new_col_name_14)) | !!sym(new_col_name_14) == "" ~ "0",
        .default = !!sym(new_col_name_14)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_14, "→", new_col_name_14, "\n")
} else {
  cat("⚠ Maradi column not found\n")
}

# Rename and transform achat column
old_col_name_17 <- "5.) Reproduction/Si oui, d'où proviennent-ils ? /1= achat"
new_col_name_17 <- "5.) Reproduction/Si oui, d'où proviennent-ils ? /1= achat/0=Non, 1=Oui"

if (old_col_name_17 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_17 := all_of(old_col_name_17)) %>%
    mutate(
      !!new_col_name_17 := as.character(!!sym(new_col_name_17)),
      !!new_col_name_17 := str_trim(!!sym(new_col_name_17)),
      !!new_col_name_17 := case_when(
        is.na(!!sym(new_col_name_17)) | !!sym(new_col_name_17) == "" ~ "0",
        .default = !!sym(new_col_name_17)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_17, "→", new_col_name_17, "\n")
} else {
  cat("⚠ achat column not found\n")
}

# Rename and transform échange avec un élevage voisin column
old_col_name_18 <- "5.) Reproduction/Si oui, d'où proviennent-ils ? /2= échange avec un élevage voisin"
new_col_name_18 <- "5.) Reproduction/Si oui, d'où proviennent-ils ? /2= échange avec un élevage voisin/0=Non, 1=Oui"

if (old_col_name_18 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_18 := all_of(old_col_name_18)) %>%
    mutate(
      !!new_col_name_18 := as.character(!!sym(new_col_name_18)),
      !!new_col_name_18 := str_trim(!!sym(new_col_name_18)),
      !!new_col_name_18 := case_when(
        is.na(!!sym(new_col_name_18)) | !!sym(new_col_name_18) == "" ~ "0",
        .default = !!sym(new_col_name_18)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_18, "→", new_col_name_18, "\n")
} else {
  cat("⚠ échange avec un élevage voisin column not found\n")
}

# Rename and transform héritage column
old_col_name_19 <- "5.) Reproduction/Si oui, d'où proviennent-ils ? /3= héritage"
new_col_name_19 <- "5.) Reproduction/Si oui, d'où proviennent-ils ? /3= héritage/0=Non, 1=Oui"

if (old_col_name_19 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_19 := all_of(old_col_name_19)) %>%
    mutate(
      !!new_col_name_19 := as.character(!!sym(new_col_name_19)),
      !!new_col_name_19 := str_trim(!!sym(new_col_name_19)),
      !!new_col_name_19 := case_when(
        is.na(!!sym(new_col_name_19)) | !!sym(new_col_name_19) == "" ~ "0",
        .default = !!sym(new_col_name_19)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_19, "→", new_col_name_19, "\n")
} else {
  cat("⚠ héritage column not found\n")
}

# Rename and transform don column
old_col_name_20 <- "5.) Reproduction/Si oui, d'où proviennent-ils ? /4= don"
new_col_name_20 <- "5.) Reproduction/Si oui, d'où proviennent-ils ? /4= don/0=Non, 1=Oui"

if (old_col_name_20 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_20 := all_of(old_col_name_20)) %>%
    mutate(
      !!new_col_name_20 := as.character(!!sym(new_col_name_20)),
      !!new_col_name_20 := str_trim(!!sym(new_col_name_20)),
      !!new_col_name_20 := case_when(
        is.na(!!sym(new_col_name_20)) | !!sym(new_col_name_20) == "" ~ "0",
        .default = !!sym(new_col_name_20)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_20, "→", new_col_name_20, "\n")
} else {
  cat("⚠ don column not found\n")
}

# Rename and transform de l'élevage column
old_col_name_21 <- "5.) Reproduction/Si oui, d'où proviennent-ils ? /5= de l'élevage"
new_col_name_21 <- "5.) Reproduction/Si oui, d'où proviennent-ils ? /5= de l'élevage/0=Non, 1=Oui"

if (old_col_name_21 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_21 := all_of(old_col_name_21)) %>%
    mutate(
      !!new_col_name_21 := as.character(!!sym(new_col_name_21)),
      !!new_col_name_21 := str_trim(!!sym(new_col_name_21)),
      !!new_col_name_21 := case_when(
        is.na(!!sym(new_col_name_21)) | !!sym(new_col_name_21) == "" ~ "0",
        .default = !!sym(new_col_name_21)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_21, "→", new_col_name_21, "\n")
} else {
  cat("⚠ de l'élevage column not found\n")
}

# Rename and transform d'un centre de recherche column
old_col_name_22 <- "5.) Reproduction/Si oui, d'où proviennent-ils ? /6= d'un centre de recherche"
new_col_name_22 <- "5.) Reproduction/Si oui, d'où proviennent-ils ? /6= d'un centre de recherche/0=Non, 1=Oui"

if (old_col_name_22 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_22 := all_of(old_col_name_22)) %>%
    mutate(
      !!new_col_name_22 := as.character(!!sym(new_col_name_22)),
      !!new_col_name_22 := str_trim(!!sym(new_col_name_22)),
      !!new_col_name_22 := case_when(
        is.na(!!sym(new_col_name_22)) | !!sym(new_col_name_22) == "" ~ "0",
        .default = !!sym(new_col_name_22)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_22, "→", new_col_name_22, "\n")
} else {
  cat("⚠ d'un centre de recherche column not found\n")
}

# Rename and transform Djallonke males reproducers column
old_col_name_23 <- "5.) Reproduction/(Si oui), préciser la race ou le type génétique du ou reproducteurs mêles actuellelrnt présents dans le troupeau ? /Djallonke"
new_col_name_23 <- "5.) Reproduction/(Si oui), préciser la race ou le type génétique du ou reproducteurs mêles actuellelrnt présents dans le troupeau ? /Djallonke/0=Non, 1=oui"

if (old_col_name_23 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_23 := all_of(old_col_name_23)) %>%
    mutate(
      !!new_col_name_23 := as.character(!!sym(new_col_name_23)),
      !!new_col_name_23 := str_trim(!!sym(new_col_name_23)),
      !!new_col_name_23 := case_when(
        is.na(!!sym(new_col_name_23)) | !!sym(new_col_name_23) == "" ~ "0",
        .default = !!sym(new_col_name_23)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_23, "→", new_col_name_23, "\n")
} else {
  cat("⚠ Djallonke males reproducers column not found\n")
}

# Rename and transform Sahelien males reproducers column
old_col_name_24 <- "5.) Reproduction/(Si oui), préciser la race ou le type génétique du ou reproducteurs mêles actuellelrnt présents dans le troupeau ? /Sahelien"
new_col_name_24 <- "5.) Reproduction/(Si oui), préciser la race ou le type génétique du ou reproducteurs mêles actuellelrnt présents dans le troupeau ? /Sahelien/0=Non, 1=Oui"

if (old_col_name_24 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_24 := all_of(old_col_name_24)) %>%
    mutate(
      !!new_col_name_24 := as.character(!!sym(new_col_name_24)),
      !!new_col_name_24 := str_trim(!!sym(new_col_name_24)),
      !!new_col_name_24 := case_when(
        is.na(!!sym(new_col_name_24)) | !!sym(new_col_name_24) == "" ~ "0",
        .default = !!sym(new_col_name_24)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_24, "→", new_col_name_24, "\n")
} else {
  cat("⚠ Sahelien males reproducers column not found\n")
}

# Rename and transform Metis males reproducers column
old_col_name_25 <- "5.) Reproduction/(Si oui), préciser la race ou le type génétique du ou reproducteurs mêles actuellelrnt présents dans le troupeau ? /Metis"
new_col_name_25 <- "5.) Reproduction/(Si oui), préciser la race ou le type génétique du ou reproducteurs mêles actuellelrnt présents dans le troupeau ? /Metis/0=Non, 1=Oui"

if (old_col_name_25 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_25 := all_of(old_col_name_25)) %>%
    mutate(
      !!new_col_name_25 := as.character(!!sym(new_col_name_25)),
      !!new_col_name_25 := str_trim(!!sym(new_col_name_25)),
      !!new_col_name_25 := case_when(
        is.na(!!sym(new_col_name_25)) | !!sym(new_col_name_25) == "" ~ "0",
        .default = !!sym(new_col_name_25)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_25, "→", new_col_name_25, "\n")
} else {
  cat("⚠ Metis males reproducers column not found\n")
}

# Rename and transform Maradi males reproducers column
old_col_name_26 <- "5.) Reproduction/(Si oui), préciser la race ou le type génétique du ou reproducteurs mêles actuellelrnt présents dans le troupeau ? /Maradi"
new_col_name_26 <- "5.) Reproduction/(Si oui), préciser la race ou le type génétique du ou reproducteurs mêles actuellelrnt présents dans le troupeau ? /Maradi/0=Non, 1=Oui"

if (old_col_name_26 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_26 := all_of(old_col_name_26)) %>%
    mutate(
      !!new_col_name_26 := as.character(!!sym(new_col_name_26)),
      !!new_col_name_26 := str_trim(!!sym(new_col_name_26)),
      !!new_col_name_26 := case_when(
        is.na(!!sym(new_col_name_26)) | !!sym(new_col_name_26) == "" ~ "0",
        .default = !!sym(new_col_name_26)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_26, "→", new_col_name_26, "\n")
} else {
  cat("⚠ Maradi males reproducers column not found\n")
}

# Rename and transform Saanen males reproducers column
old_col_name_27 <- "5.) Reproduction/(Si oui), préciser la race ou le type génétique du ou reproducteurs mêles actuellelrnt présents dans le troupeau ? /Saanen"
new_col_name_27 <- "5.) Reproduction/(Si oui), préciser la race ou le type génétique du ou reproducteurs mêles actuellelrnt présents dans le troupeau ? /Saanen/0=Non, 1=Oui"

if (old_col_name_27 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_27 := all_of(old_col_name_27)) %>%
    mutate(
      !!new_col_name_27 := as.character(!!sym(new_col_name_27)),
      !!new_col_name_27 := str_trim(!!sym(new_col_name_27)),
      !!new_col_name_27 := case_when(
        is.na(!!sym(new_col_name_27)) | !!sym(new_col_name_27) == "" ~ "0",
        .default = !!sym(new_col_name_27)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_27, "→", new_col_name_27, "\n")
} else {
  cat("⚠ Saanen males reproducers column not found\n")
}

# Rename and transform Alpine males reproducers column
old_col_name_28 <- "5.) Reproduction/(Si oui), préciser la race ou le type génétique du ou reproducteurs mêles actuellelrnt présents dans le troupeau ? /Alpine"
new_col_name_28 <- "5.) Reproduction/(Si oui), préciser la race ou le type génétique du ou reproducteurs mêles actuellelrnt présents dans le troupeau ? /Alpine/0=Non, 1=Oui"

if (old_col_name_28 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_28 := all_of(old_col_name_28)) %>%
    mutate(
      !!new_col_name_28 := as.character(!!sym(new_col_name_28)),
      !!new_col_name_28 := str_trim(!!sym(new_col_name_28)),
      !!new_col_name_28 := case_when(
        is.na(!!sym(new_col_name_28)) | !!sym(new_col_name_28) == "" ~ "0",
        .default = !!sym(new_col_name_28)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_28, "→", new_col_name_28, "\n")
} else {
  cat("⚠ Alpine males reproducers column not found\n")
}

# Rename and transform male reproducer availability column
old_col_name_29 <- "5.) Reproduction/Si \"non\", soit pas actuellement de reproducteur mâle », en disposez-vous habituellement dans l'élevage ?"
new_col_name_29 <- "5.) Reproduction/Si \"non\", soit pas actuellement de reproducteur mâle », en disposez-vous habituellement dans l'élevage ? 0=Non, 1=Oui"

if (old_col_name_29 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_29 := all_of(old_col_name_29)) %>%
    mutate(
      !!new_col_name_29 := as.character(!!sym(new_col_name_29)),
      !!new_col_name_29 := str_trim(!!sym(new_col_name_29)),
      !!new_col_name_29 := case_when(
        is.na(!!sym(new_col_name_29)) | !!sym(new_col_name_29) == "" ~ "0",
        str_detect(str_to_lower(!!sym(new_col_name_29)), "^non$") ~ "0",
        str_detect(str_to_lower(!!sym(new_col_name_29)), "^oui$") ~ "1",
        .default = !!sym(new_col_name_29)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_29, "→", new_col_name_29, "\n")
} else {
  cat("⚠ male reproducer availability column not found\n")
}

# Rename and transform male reproducer retention column
old_col_name_30 <- "5.) Reproduction/Pendant combien de temps gardez-vous le(s) mâle(s) reproducteur(s) dans votre élevage avant de le(s) renouveler ?"
new_col_name_30 <- "5.) Reproduction/Pendant combien de temps gardez-vous le(s) mâle(s) reproducteur(s) dans votre élevage avant de le(s) renouveler ? 1=1ans, 2=2ans, 3=3ans, 4=4ans, 5=Autre à préciser"

if (old_col_name_30 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_30 := all_of(old_col_name_30)) %>%
    mutate(
      !!new_col_name_30 := as.character(!!sym(new_col_name_30)),
      !!new_col_name_30 := str_trim(!!sym(new_col_name_30)),
      !!new_col_name_30 := case_when(
        is.na(!!sym(new_col_name_30)) | !!sym(new_col_name_30) == "" ~ NA_character_,
        str_detect(str_to_lower(!!sym(new_col_name_30)), "^1|1.*ans") ~ "1",
        str_detect(str_to_lower(!!sym(new_col_name_30)), "^2|2.*ans") ~ "2",
        str_detect(str_to_lower(!!sym(new_col_name_30)), "^3|3.*ans") ~ "3",
        str_detect(str_to_lower(!!sym(new_col_name_30)), "^4|4.*ans") ~ "4",
        str_detect(str_to_lower(!!sym(new_col_name_30)), "autre.*préciser|autre") ~ "5",
        .default = !!sym(new_col_name_30)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_30, "→", new_col_name_30, "\n")
} else {
  cat("⚠ male reproducer retention column not found\n")
}

# Rename and transform male renewal achat column
old_col_name_31 <- "5.) Reproduction/D'où provient souvent le mâle de renouvellement ? /1= achat"
new_col_name_31 <- "5.) Reproduction/D'où provient souvent le mâle de renouvellement ? /1= achat/0=Non, 1=Oui"

if (old_col_name_31 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_31 := all_of(old_col_name_31)) %>%
    mutate(
      !!new_col_name_31 := as.character(!!sym(new_col_name_31)),
      !!new_col_name_31 := str_trim(!!sym(new_col_name_31)),
      !!new_col_name_31 := case_when(
        is.na(!!sym(new_col_name_31)) | !!sym(new_col_name_31) == "" ~ "0",
        .default = !!sym(new_col_name_31)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_31, "→", new_col_name_31, "\n")
} else {
  cat("⚠ male renewal achat column not found\n")
}

# Rename and transform male renewal échange column
old_col_name_32 <- "5.) Reproduction/D'où provient souvent le mâle de renouvellement ? /2= échange avec un élevage voisin"
new_col_name_32 <- "5.) Reproduction/D'où provient souvent le mâle de renouvellement ? /2= échange avec un élevage voisin/0=Non, 1=Oui"

if (old_col_name_32 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_32 := all_of(old_col_name_32)) %>%
    mutate(
      !!new_col_name_32 := as.character(!!sym(new_col_name_32)),
      !!new_col_name_32 := str_trim(!!sym(new_col_name_32)),
      !!new_col_name_32 := case_when(
        is.na(!!sym(new_col_name_32)) | !!sym(new_col_name_32) == "" ~ "0",
        .default = !!sym(new_col_name_32)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_32, "→", new_col_name_32, "\n")
} else {
  cat("⚠ male renewal échange column not found\n")
}

# Rename and transform male renewal don column
old_col_name_33 <- "5.) Reproduction/D'où provient souvent le mâle de renouvellement ? /3= don"
new_col_name_33 <- "5.) Reproduction/D'où provient souvent le mâle de renouvellement ? /3= don/0=Non, 1=Oui"

if (old_col_name_33 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_33 := all_of(old_col_name_33)) %>%
    mutate(
      !!new_col_name_33 := as.character(!!sym(new_col_name_33)),
      !!new_col_name_33 := str_trim(!!sym(new_col_name_33)),
      !!new_col_name_33 := case_when(
        is.na(!!sym(new_col_name_33)) | !!sym(new_col_name_33) == "" ~ "0",
        .default = !!sym(new_col_name_33)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_33, "→", new_col_name_33, "\n")
} else {
  cat("⚠ male renewal don column not found\n")
}

# Rename and transform male renewal de l'élevage column
old_col_name_34 <- "5.) Reproduction/D'où provient souvent le mâle de renouvellement ? /5= de l'élevage"
new_col_name_34 <- "5.) Reproduction/D'où provient souvent le mâle de renouvellement ? /5= de l'élevage/0=Non, 1=Oui"

if (old_col_name_34 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_34 := all_of(old_col_name_34)) %>%
    mutate(
      !!new_col_name_34 := as.character(!!sym(new_col_name_34)),
      !!new_col_name_34 := str_trim(!!sym(new_col_name_34)),
      !!new_col_name_34 := case_when(
        is.na(!!sym(new_col_name_34)) | !!sym(new_col_name_34) == "" ~ "0",
        .default = !!sym(new_col_name_34)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_34, "→", new_col_name_34, "\n")
} else {
  cat("⚠ male renewal de l'élevage column not found\n")
}

# Rename and transform male renewal centre de recherche column
old_col_name_35 <- "5.) Reproduction/D'où provient souvent le mâle de renouvellement ? /6= d'un centre de recherche"
new_col_name_35 <- "5.) Reproduction/D'où provient souvent le mâle de renouvellement ? /6= d'un centre de recherche/0=Non, 1=Oui"

if (old_col_name_35 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_35 := all_of(old_col_name_35)) %>%
    mutate(
      !!new_col_name_35 := as.character(!!sym(new_col_name_35)),
      !!new_col_name_35 := str_trim(!!sym(new_col_name_35)),
      !!new_col_name_35 := case_when(
        is.na(!!sym(new_col_name_35)) | !!sym(new_col_name_35) == "" ~ "0",
        .default = !!sym(new_col_name_35)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_35, "→", new_col_name_35, "\n")
} else {
  cat("⚠ male renewal centre de recherche column not found\n")
}

# Rename and transform centre de recherche préciser column
old_col_name_36 <- "5.) Reproduction/6= d'un centre de recherche (préciser)"
new_col_name_36 <- "5.) Reproduction/6= d'un centre de recherche (préciser)/0=Non, 1=Ferme d'Etat de Betekoukou"

if (old_col_name_36 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_36 := all_of(old_col_name_36)) %>%
    mutate(
      !!new_col_name_36 := as.character(!!sym(new_col_name_36)),
      !!new_col_name_36 := str_trim(!!sym(new_col_name_36)),
      !!new_col_name_36 := case_when(
        is.na(!!sym(new_col_name_36)) | !!sym(new_col_name_36) == "" ~ "0",
        str_detect(!!sym(new_col_name_36), "Ferme d'Etat de Betekoukou") ~ "1",
        .default = !!sym(new_col_name_36)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_36, "→", new_col_name_36, "\n")
} else {
  cat("⚠ centre de recherche préciser column not found\n")
}

# Rename and transform sahélienne races column
old_col_name_37 <- "5.) Reproduction/Quelles sont les races sahéliennes présentes dans votre troupeau ?"
new_col_name_37 <- "5.) Reproduction/Quelles sont les races sahéliennes présentes dans votre troupeau ? 0=Non, 1=Djabadjaba(Ovin), 2=Tominnougbo(Caprin), 3=Ayogbo(Caprin), 4=HALAHALA"

if (old_col_name_37 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_37 := all_of(old_col_name_37)) %>%
    mutate(
      !!new_col_name_37 := as.character(!!sym(new_col_name_37)),
      !!new_col_name_37 := str_trim(!!sym(new_col_name_37)),
      !!new_col_name_37 := case_when(
        is.na(!!sym(new_col_name_37)) | !!sym(new_col_name_37) == "" ~ "0",
        str_detect(str_to_lower(!!sym(new_col_name_37)), "djabadjaba|mâle.*sahélien.*djabadjaba|race.*sahélienne.*djabadjaba") ~ "1",
        str_detect(str_to_lower(!!sym(new_col_name_37)), "tomin.*nou.*gbo|tomin.*nougbo|mâle.*sahélien.*tomin") ~ "2",
        str_detect(str_to_lower(!!sym(new_col_name_37)), "ayôgbô|ayogbo") ~ "3",
        str_detect(str_to_lower(!!sym(new_col_name_37)), "halahala|nom.*halahala") ~ "4",
        .default = "0"
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_37, "→", new_col_name_37, "\n")
} else {
  cat("⚠ sahélienne races column not found\n")
}

# Rename and transform female reproduction retention column
old_col_name_38 <- "5.) Reproduction/Pendant combien de temps gardez-vous les femelles reproductrices dans votre élevage avant de les renouveler ?"
new_col_name_38 <- "5.) Reproduction/Pendant combien de temps gardez-vous les femelles reproductrices dans votre élevage avant de les renouveler ? 1=[3;4], 2=[4;5], 3=[5;10]"

if (old_col_name_38 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_38 := all_of(old_col_name_38)) %>%
    mutate(
      !!new_col_name_38 := as.character(!!sym(new_col_name_38)),
      !!new_col_name_38 := str_trim(!!sym(new_col_name_38)),
      !!new_col_name_38 := case_when(
        is.na(!!sym(new_col_name_38)) | !!sym(new_col_name_38) == "" ~ "0",
        str_detect(str_to_lower(!!sym(new_col_name_38)), "^3$") ~ "1",
        str_detect(str_to_lower(!!sym(new_col_name_38)), "^4$|4-5|4;5") ~ "2",
        # Specific pattern: › 5 ans, Aussi longtemps que les performances sont bonnes.
        str_detect(str_to_lower(!!sym(new_col_name_38)), "›\\s*5\\s*ans.*performances.*bonnes|aussi.*longtemps.*performances.*bonnes") ~ "3",
        # All variations for 5+ years and performance-based duration
        str_detect(str_to_lower(!!sym(new_col_name_38)), "^5|5[\\s-]|>\\s*5|›\\s*5|5\\s*ans|6\\s*ans|7\\s*ans|8\\s*ans|performance|tant que|pas de|pas d|non.prédéfini|non.défini|sans limite|dépend|fonction|aucune limite|au moins|autant|moyenne|en moyenne|possible") ~ "3",
        .default = "0"
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_38, "→", new_col_name_38, "\n")
} else {
  cat("⚠ female reproduction retention column not found\n")
}

# Rename and transform crossbreeding column
old_col_name_39 <- "5.) Reproduction/Pratiquez-vous des croisements entre différentes races caprines ?"
new_col_name_39 <- "5.) Reproduction/Pratiquez-vous des croisements entre différentes races caprines ? 0=Non, 1=Oui"

if (old_col_name_39 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_39 := all_of(old_col_name_39)) %>%
    mutate(
      !!new_col_name_39 := as.character(!!sym(new_col_name_39)),
      !!new_col_name_39 := str_trim(!!sym(new_col_name_39)),
      !!new_col_name_39 := case_when(
        is.na(!!sym(new_col_name_39)) | !!sym(new_col_name_39) == "" ~ "0",
        str_detect(str_to_lower(!!sym(new_col_name_39)), "^non$") ~ "0",
        str_detect(str_to_lower(!!sym(new_col_name_39)), "^oui$") ~ "1",
        .default = !!sym(new_col_name_39)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_39, "→", new_col_name_39, "\n")
} else {
  cat("⚠ crossbreeding column not found\n")
}

# Rename and transform Djallonke crossbreeding column
old_col_name_40 <- "5.) Reproduction/Si oui, quelles races sont impliquées ?/Race locale Djallonke"
new_col_name_40 <- "5.) Reproduction/Si oui, quelles races sont impliquées ?/Race locale Djallonke/ 0=Non, 1=Oui"

if (old_col_name_40 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_40 := all_of(old_col_name_40)) %>%
    mutate(
      !!new_col_name_40 := as.character(!!sym(new_col_name_40)),
      !!new_col_name_40 := str_trim(!!sym(new_col_name_40)),
      !!new_col_name_40 := case_when(
        is.na(!!sym(new_col_name_40)) | !!sym(new_col_name_40) == "" ~ "0",
        .default = !!sym(new_col_name_40)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_40, "→", new_col_name_40, "\n")
} else {
  cat("⚠ Djallonke crossbreeding column not found\n")
}

# Rename and transform Sahelienne crossbreeding column
old_col_name_41 <- "5.) Reproduction/Si oui, quelles races sont impliquées ?/Race Sahelienne"
new_col_name_41 <- "5.) Reproduction/Si oui, quelles races sont impliquées ?/Race Sahelienne/0=Non, 1=Oui"

if (old_col_name_41 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_41 := all_of(old_col_name_41)) %>%
    mutate(
      !!new_col_name_41 := as.character(!!sym(new_col_name_41)),
      !!new_col_name_41 := str_trim(!!sym(new_col_name_41)),
      !!new_col_name_41 := case_when(
        is.na(!!sym(new_col_name_41)) | !!sym(new_col_name_41) == "" ~ "0",
        .default = !!sym(new_col_name_41)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_41, "→", new_col_name_41, "\n")
} else {
  cat("⚠ Sahelienne crossbreeding column not found\n")
}

# Rename and transform exotic race crossbreeding column
old_col_name_42 <- "5.) Reproduction/Si oui, quelles races sont impliquées ?/Race exotique"
new_col_name_42 <- "5.) Reproduction/Si oui, quelles races sont impliquées ?/Race exotique/0=Non, 1=Oui"

if (old_col_name_42 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_42 := all_of(old_col_name_42)) %>%
    mutate(
      !!new_col_name_42 := as.character(!!sym(new_col_name_42)),
      !!new_col_name_42 := str_trim(!!sym(new_col_name_42)),
      !!new_col_name_42 := case_when(
        is.na(!!sym(new_col_name_42)) | !!sym(new_col_name_42) == "" ~ "0",
        .default = !!sym(new_col_name_42)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_42, "→", new_col_name_42, "\n")
} else {
  cat("⚠ exotic race crossbreeding column not found\n")
}

# Rename and transform other race crossbreeding column
old_col_name_43 <- "5.) Reproduction/Si oui, quelles races sont impliquées ?/Autre _______"
new_col_name_43 <- "5.) Reproduction/Si oui, quelles races sont impliquées ?/Autre _______/0=Non, 1=Oui"

if (old_col_name_43 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_43 := all_of(old_col_name_43)) %>%
    mutate(
      !!new_col_name_43 := as.character(!!sym(new_col_name_43)),
      !!new_col_name_43 := str_trim(!!sym(new_col_name_43)),
      !!new_col_name_43 := case_when(
        is.na(!!sym(new_col_name_43)) | !!sym(new_col_name_43) == "" ~ "0",
        .default = !!sym(new_col_name_43)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_43, "→", new_col_name_43, "\n")
} else {
  cat("⚠ other race crossbreeding column not found\n")
}

# Rename and transform crossbreeding objective growth column
old_col_name_44 <- "5.) Reproduction/Objectifs principaux du croisement /1= Améliorer la croissance"
new_col_name_44 <- "5.) Reproduction/Objectifs principaux du croisement /1= Améliorer la croissance/0=Non, 1=Oui"

if (old_col_name_44 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_44 := all_of(old_col_name_44)) %>%
    mutate(
      !!new_col_name_44 := as.character(!!sym(new_col_name_44)),
      !!new_col_name_44 := str_trim(!!sym(new_col_name_44)),
      !!new_col_name_44 := case_when(
        is.na(!!sym(new_col_name_44)) | !!sym(new_col_name_44) == "" ~ "0",
        .default = !!sym(new_col_name_44)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_44, "→", new_col_name_44, "\n")
} else {
  cat("⚠ crossbreeding objective growth column not found\n")
}

# Rename and transform crossbreeding objective prolificité column
old_col_name_45 <- "5.) Reproduction/Objectifs principaux du croisement /2= la Prolificité"
new_col_name_45 <- "5.) Reproduction/Objectifs principaux du croisement /2= la Prolificité/0=Non, 1=Oui"

if (old_col_name_45 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_45 := all_of(old_col_name_45)) %>%
    mutate(
      !!new_col_name_45 := as.character(!!sym(new_col_name_45)),
      !!new_col_name_45 := str_trim(!!sym(new_col_name_45)),
      !!new_col_name_45 := case_when(
        is.na(!!sym(new_col_name_45)) | !!sym(new_col_name_45) == "" ~ "0",
        .default = !!sym(new_col_name_45)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_45, "→", new_col_name_45, "\n")
} else {
  cat("⚠ crossbreeding objective prolificité column not found\n")
}

# Rename and transform crossbreeding objective disease resistance column
old_col_name_46 <- "5.) Reproduction/Objectifs principaux du croisement /3= Améliorer la résistance aux maladies"
new_col_name_46 <- "5.) Reproduction/Objectifs principaux du croisement /3= Améliorer la résistance aux maladies/0=Non, 1=Oui"

if (old_col_name_46 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_46 := all_of(old_col_name_46)) %>%
    mutate(
      !!new_col_name_46 := as.character(!!sym(new_col_name_46)),
      !!new_col_name_46 := str_trim(!!sym(new_col_name_46)),
      !!new_col_name_46 := case_when(
        is.na(!!sym(new_col_name_46)) | !!sym(new_col_name_46) == "" ~ "0",
        .default = !!sym(new_col_name_46)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_46, "→", new_col_name_46, "\n")
} else {
  cat("⚠ crossbreeding objective disease resistance column not found\n")
}

# Rename and transform crossbreeding objective local adaptation column
old_col_name_47 <- "5.) Reproduction/Objectifs principaux du croisement /4= Adapter aux conditions locales"
new_col_name_47 <- "5.) Reproduction/Objectifs principaux du croisement /4= Adapter aux conditions locales/0=Non, 1=Oui"

if (old_col_name_47 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_47 := all_of(old_col_name_47)) %>%
    mutate(
      !!new_col_name_47 := as.character(!!sym(new_col_name_47)),
      !!new_col_name_47 := str_trim(!!sym(new_col_name_47)),
      !!new_col_name_47 := case_when(
        is.na(!!sym(new_col_name_47)) | !!sym(new_col_name_47) == "" ~ "0",
        .default = !!sym(new_col_name_47)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_47, "→", new_col_name_47, "\n")
} else {
  cat("⚠ crossbreeding objective local adaptation column not found\n")
}

# Rename and transform crossbreeding objective consumer preference column
old_col_name_48 <- "5.) Reproduction/Objectifs principaux du croisement /5= Produire des animaux répondant aux préférences des consommateurs"
new_col_name_48 <- "5.) Reproduction/Objectifs principaux du croisement /5= Produire des animaux répondant aux préférences des consommateurs/0=Non, 1=Oui"

if (old_col_name_48 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_48 := all_of(old_col_name_48)) %>%
    mutate(
      !!new_col_name_48 := as.character(!!sym(new_col_name_48)),
      !!new_col_name_48 := str_trim(!!sym(new_col_name_48)),
      !!new_col_name_48 := case_when(
        is.na(!!sym(new_col_name_48)) | !!sym(new_col_name_48) == "" ~ "0",
        .default = !!sym(new_col_name_48)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_48, "→", new_col_name_48, "\n")
} else {
  cat("⚠ crossbreeding objective consumer preference column not found\n")
}

# Rename and transform crossbreeding objective other column
old_col_name_49 <- "5.) Reproduction/Objectifs principaux du croisement /6= Autre"
new_col_name_49 <- "5.) Reproduction/Objectifs principaux du croisement /6= Autre/0=Non, 1=Oui"

if (old_col_name_49 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_49 := all_of(old_col_name_49)) %>%
    mutate(
      !!new_col_name_49 := as.character(!!sym(new_col_name_49)),
      !!new_col_name_49 := str_trim(!!sym(new_col_name_49)),
      !!new_col_name_49 := case_when(
        is.na(!!sym(new_col_name_49)) | !!sym(new_col_name_49) == "" ~ "0",
        .default = !!sym(new_col_name_49)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_49, "→", new_col_name_49, "\n")
} else {
  cat("⚠ crossbreeding objective other column not found\n")
}

# Rename and transform crossbreeding type Djallonké-Sahélienne column
old_col_name_50 <- "5.) Reproduction/Quels sont les principaux types de croisement pratiqués ? /1= Croisement entre races locales Djallonké et Sahéliennes"
new_col_name_50 <- "5.) Reproduction/Quels sont les principaux types de croisement pratiqués ? /1= Croisement entre races locales Djallonké et Sahéliennes/0=Non, 1=Oui"

if (old_col_name_50 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_50 := all_of(old_col_name_50)) %>%
    mutate(
      !!new_col_name_50 := as.character(!!sym(new_col_name_50)),
      !!new_col_name_50 := str_trim(!!sym(new_col_name_50)),
      !!new_col_name_50 := case_when(
        is.na(!!sym(new_col_name_50)) | !!sym(new_col_name_50) == "" ~ "0",
        .default = !!sym(new_col_name_50)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_50, "→", new_col_name_50, "\n")
} else {
  cat("⚠ crossbreeding type Djallonké-Sahélienne column not found\n")
}

# Rename and transform crossbreeding type local-exotic column
old_col_name_51 <- "5.) Reproduction/Quels sont les principaux types de croisement pratiqués ? /2= Croisement entre races locales et exotiques"
new_col_name_51 <- "5.) Reproduction/Quels sont les principaux types de croisement pratiqués ? /2= Croisement entre races locales et exotiques/0=Non, 1=Oui"

if (old_col_name_51 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_51 := all_of(old_col_name_51)) %>%
    mutate(
      !!new_col_name_51 := as.character(!!sym(new_col_name_51)),
      !!new_col_name_51 := str_trim(!!sym(new_col_name_51)),
      !!new_col_name_51 := case_when(
        is.na(!!sym(new_col_name_51)) | !!sym(new_col_name_51) == "" ~ "0",
        .default = !!sym(new_col_name_51)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_51, "→", new_col_name_51, "\n")
} else {
  cat("⚠ crossbreeding type local-exotic column not found\n")
}

# Rename and transform crossbreeding type backcrossing column
old_col_name_52 <- "5.) Reproduction/Quels sont les principaux types de croisement pratiqués ? /3= Croisement en retour (backcrossing)"
new_col_name_52 <- "5.) Reproduction/Quels sont les principaux types de croisement pratiqués ? /3= Croisement en retour (backcrossing)/0=Non, 1=Oui"

if (old_col_name_52 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_52 := all_of(old_col_name_52)) %>%
    mutate(
      !!new_col_name_52 := as.character(!!sym(new_col_name_52)),
      !!new_col_name_52 := str_trim(!!sym(new_col_name_52)),
      !!new_col_name_52 := case_when(
        is.na(!!sym(new_col_name_52)) | !!sym(new_col_name_52) == "" ~ "0",
        .default = !!sym(new_col_name_52)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_52, "→", new_col_name_52, "\n")
} else {
  cat("⚠ crossbreeding type backcrossing column not found\n")
}

# Rename and transform crossbreeding practice Sahélian bucks column
old_col_name_53 <- "5.) Reproduction/Si croisement entre races locales Djallonké et Sahéliennes,  quelle est la pratique courante ?/1= Utilisation de boucs sahéliens sur des chèvres Djallonké"
new_col_name_53 <- "5.) Reproduction/Si croisement entre races locales Djallonké et Sahéliennes,  quelle est la pratique courante ?/1= Utilisation de boucs sahéliens sur des chèvres Djallonké/0=Non, 1=Oui"

if (old_col_name_53 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_53 := all_of(old_col_name_53)) %>%
    mutate(
      !!new_col_name_53 := as.character(!!sym(new_col_name_53)),
      !!new_col_name_53 := str_trim(!!sym(new_col_name_53)),
      !!new_col_name_53 := case_when(
        is.na(!!sym(new_col_name_53)) | !!sym(new_col_name_53) == "" ~ "0",
        .default = !!sym(new_col_name_53)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_53, "→", new_col_name_53, "\n")
} else {
  cat("⚠ crossbreeding practice Sahélian bucks column not found\n")
}

# Rename and transform crossbreeding practice Djallonké bucks column
old_col_name_54 <- "5.) Reproduction/Si croisement entre races locales Djallonké et Sahéliennes,  quelle est la pratique courante ?/2= Utilisation de boucs Djallonké sur des chèvres Sahéliennes"
new_col_name_54 <- "5.) Reproduction/Si croisement entre races locales Djallonké et Sahéliennes,  quelle est la pratique courante ?/2= Utilisation de boucs Djallonké sur des chèvres Sahéliennes/0=Non, 1=Oui"

if (old_col_name_54 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_54 := all_of(old_col_name_54)) %>%
    mutate(
      !!new_col_name_54 := as.character(!!sym(new_col_name_54)),
      !!new_col_name_54 := str_trim(!!sym(new_col_name_54)),
      !!new_col_name_54 := case_when(
        is.na(!!sym(new_col_name_54)) | !!sym(new_col_name_54) == "" ~ "0",
        .default = !!sym(new_col_name_54)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_54, "→", new_col_name_54, "\n")
} else {
  cat("⚠ crossbreeding practice Djallonké bucks column not found\n")
}

# Rename and transform crossbreeding practice Métis bucks column
old_col_name_55 <- "5.) Reproduction/Si croisement entre races locales Djallonké et Sahéliennes,  quelle est la pratique courante ?/3= Utilisation de boucs Métis sur des chèvres Sahéliennes"
new_col_name_55 <- "5.) Reproduction/Si croisement entre races locales Djallonké et Sahéliennes,  quelle est la pratique courante ?/3= Utilisation de boucs Métis sur des chèvres Sahéliennes/0=Non, 1=Oui"

if (old_col_name_55 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_55 := all_of(old_col_name_55)) %>%
    mutate(
      !!new_col_name_55 := as.character(!!sym(new_col_name_55)),
      !!new_col_name_55 := str_trim(!!sym(new_col_name_55)),
      !!new_col_name_55 := case_when(
        is.na(!!sym(new_col_name_55)) | !!sym(new_col_name_55) == "" ~ "0",
        .default = !!sym(new_col_name_55)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_55, "→", new_col_name_55, "\n")
} else {
  cat("⚠ crossbreeding practice Métis bucks column not found\n")
}

# Rename and transform crossbreeding practice Djallonké bucks on Métis females column
old_col_name_56 <- "5.) Reproduction/Si croisement entre races locales Djallonké et Sahéliennes,  quelle est la pratique courante ?/4= Utilisation de boucs Djallonké sur des chèvres Métisses"
new_col_name_56 <- "5.) Reproduction/Si croisement entre races locales Djallonké et Sahéliennes,  quelle est la pratique courante ?/4= Utilisation de boucs Djallonké sur des chèvres Métisses/0=Non, 1=Oui"

if (old_col_name_56 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_56 := all_of(old_col_name_56)) %>%
    mutate(
      !!new_col_name_56 := as.character(!!sym(new_col_name_56)),
      !!new_col_name_56 := str_trim(!!sym(new_col_name_56)),
      !!new_col_name_56 := case_when(
        is.na(!!sym(new_col_name_56)) | !!sym(new_col_name_56) == "" ~ "0",
        .default = !!sym(new_col_name_56)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_56, "→", new_col_name_56, "\n")
} else {
  cat("⚠ crossbreeding practice Djallonké bucks on Métis females column not found\n")
}

# Rename and transform crossbreeding between métis column - Oui
old_col_name_57 <- "5.) Reproduction/Pratiquez-vous le croisement entre les métis ?  /Oui"
new_col_name_57 <- "5.) Reproduction/Pratiquez-vous le croisement entre les métis ?  /Oui/0=Non, 1=Oui"

if (old_col_name_57 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_57 := all_of(old_col_name_57)) %>%
    mutate(
      !!new_col_name_57 := as.character(!!sym(new_col_name_57)),
      !!new_col_name_57 := str_trim(!!sym(new_col_name_57)),
      !!new_col_name_57 := case_when(
        is.na(!!sym(new_col_name_57)) | !!sym(new_col_name_57) == "" ~ "0",
        str_detect(str_to_lower(!!sym(new_col_name_57)), "^oui$") ~ "1",
        str_detect(str_to_lower(!!sym(new_col_name_57)), "^non$") ~ "0",
        .default = !!sym(new_col_name_57)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_57, "→", new_col_name_57, "\n")
} else {
  cat("⚠ crossbreeding between métis (Oui) column not found\n")
}

# Rename and transform crossbreeding between métis column - Non
old_col_name_58 <- "5.) Reproduction/Pratiquez-vous le croisement entre les métis ?  /Non"
new_col_name_58 <- "5.) Reproduction/Pratiquez-vous le croisement entre les métis ?  /Non/0=Non, 1=Oui"

if (old_col_name_58 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_58 := all_of(old_col_name_58)) %>%
    mutate(
      !!new_col_name_58 := as.character(!!sym(new_col_name_58)),
      !!new_col_name_58 := str_trim(!!sym(new_col_name_58)),
      !!new_col_name_58 := case_when(
        is.na(!!sym(new_col_name_58)) | !!sym(new_col_name_58) == "" ~ "0",
        .default = !!sym(new_col_name_58)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_58, "→", new_col_name_58, "\n")
} else {
  cat("⚠ crossbreeding between métis (Non) column not found\n")
}

# Rename and transform métis crossing method column
old_col_name_59 <- "5.) Reproduction/Si oui, Comment le faîte vous ?"
new_col_name_59 <- "5.) Reproduction/Si oui, Comment le faîte vous ?/0=Vides, 1=Crossing, 2=Backcrossing, 3=Crossing & Backcrossing"

if (old_col_name_59 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_59 := all_of(old_col_name_59)) %>%
    mutate(
      !!new_col_name_59 := as.character(!!sym(new_col_name_59)),
      !!new_col_name_59 := str_trim(!!sym(new_col_name_59)),
      !!new_col_name_59 := case_when(
        is.na(!!sym(new_col_name_59)) | !!sym(new_col_name_59) == "" ~ "0",
        # Crossing (1): métis x métis patterns
        str_detect(str_to_lower(!!sym(new_col_name_59)), "métis.*f1.*ou.*non.*x.*métis.*f1.*ou.*non|métis.*x.*métis.*ovin.*et.*caprin|^métis x métis$|métis.*sahélien.*x.*métis.*sahélienne|métis.*f1.*x.*métis.*f1|métis.*f1.*x.*métis.*f1|metis.*x.*métis.*parents|sahélien.*x.*djallonké|balami.*x.*djallonké") ~ "1",
        # Backcrossing (2): métis x djallonke patterns
        str_detect(str_to_lower(!!sym(new_col_name_59)), "mâle.*métis.*f1.*x.*brebis.*djallonke|métis.*produit.*sahélien.*x.*djallonke|métis.*croisement.*sahélien.*x.*djallonke|bélier.*métis.*x.*djallonke|mâle.*metis.*x.*djallonke|métis.*x.*djallonke|sahélien.*x.*métis|^backcrossing$|djallonké.*x.*métis|métis.*x.*djallonké") ~ "2",
        # Crossing & Backcrossing (3): combined patterns
        str_detect(str_to_lower(!!sym(new_col_name_59)), "djallonke.*x.*métis,.*métis.*x.*métis|métis.*x.*djallonke,.*métis.*x.*métis") ~ "3",
        .default = "0"
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_59, "→", new_col_name_59, "\n")
} else {
  cat("⚠ métis crossing method column not found\n")
}

# Rename and transform taille corporelle criteria column
old_col_name_60 <- "5.) Reproduction/Critères de choix des reproducteurs /1= Taille corporelle"
new_col_name_60 <- "5.) Reproduction/Critères de choix des reproducteurs /1= Taille corporelle/0=Non, 1=Oui"

if (old_col_name_60 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_60 := all_of(old_col_name_60)) %>%
    mutate(
      !!new_col_name_60 := as.character(!!sym(new_col_name_60)),
      !!new_col_name_60 := str_trim(!!sym(new_col_name_60)),
      !!new_col_name_60 := case_when(
        is.na(!!sym(new_col_name_60)) | !!sym(new_col_name_60) == "" ~ "0",
        .default = !!sym(new_col_name_60)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_60, "→", new_col_name_60, "\n")
} else {
  cat("⚠ taille corporelle criteria column not found\n")
}

# Rename and transform résistance aux maladies criteria column
old_col_name_61 <- "5.) Reproduction/Critères de choix des reproducteurs /2= Résistance aux maladies"
new_col_name_61 <- "5.) Reproduction/Critères de choix des reproducteurs /2= Résistance aux maladies/0=Non, 1=Oui"

if (old_col_name_61 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_61 := all_of(old_col_name_61)) %>%
    mutate(
      !!new_col_name_61 := as.character(!!sym(new_col_name_61)),
      !!new_col_name_61 := str_trim(!!sym(new_col_name_61)),
      !!new_col_name_61 := case_when(
        is.na(!!sym(new_col_name_61)) | !!sym(new_col_name_61) == "" ~ "0",
        .default = !!sym(new_col_name_61)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_61, "→", new_col_name_61, "\n")
} else {
  cat("⚠ résistance aux maladies criteria column not found\n")
}

# Rename and transform performances passées criteria column
old_col_name_62 <- "5.) Reproduction/Critères de choix des reproducteurs /3= Performances passées"
new_col_name_62 <- "5.) Reproduction/Critères de choix des reproducteurs /3= Performances passées/0=Non, 1=Oui"

if (old_col_name_62 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_62 := all_of(old_col_name_62)) %>%
    mutate(
      !!new_col_name_62 := as.character(!!sym(new_col_name_62)),
      !!new_col_name_62 := str_trim(!!sym(new_col_name_62)),
      !!new_col_name_62 := case_when(
        is.na(!!sym(new_col_name_62)) | !!sym(new_col_name_62) == "" ~ "0",
        .default = !!sym(new_col_name_62)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_62, "→", new_col_name_62, "\n")
} else {
  cat("⚠ performances passées criteria column not found\n")
}

# Rename and transform origine génétique criteria column
old_col_name_63 <- "5.) Reproduction/Critères de choix des reproducteurs /4= Origine génétique (race)"
new_col_name_63 <- "5.) Reproduction/Critères de choix des reproducteurs /4= Origine génétique (race)/0=Non, 1=Oui"

if (old_col_name_63 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_63 := all_of(old_col_name_63)) %>%
    mutate(
      !!new_col_name_63 := as.character(!!sym(new_col_name_63)),
      !!new_col_name_63 := str_trim(!!sym(new_col_name_63)),
      !!new_col_name_63 := case_when(
        is.na(!!sym(new_col_name_63)) | !!sym(new_col_name_63) == "" ~ "0",
        .default = !!sym(new_col_name_63)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_63, "→", new_col_name_63, "\n")
} else {
  cat("⚠ origine génétique criteria column not found\n")
}

# Rename and transform autre criteria column
old_col_name_64 <- "5.) Reproduction/Critères de choix des reproducteurs /5= Autre"
new_col_name_64 <- "5.) Reproduction/Critères de choix des reproducteurs /5= Autre/0=Non, 1=Oui"

if (old_col_name_64 %in% names(raw)) {
  raw <- raw %>%
    rename(!!new_col_name_64 := all_of(old_col_name_64)) %>%
    mutate(
      !!new_col_name_64 := as.character(!!sym(new_col_name_64)),
      !!new_col_name_64 := str_trim(!!sym(new_col_name_64)),
      !!new_col_name_64 := case_when(
        is.na(!!sym(new_col_name_64)) | !!sym(new_col_name_64) == "" ~ "0",
        .default = !!sym(new_col_name_64)
      )
    )
  cat("✓ Column renamed and transformed:", old_col_name_64, "→", new_col_name_64, "\n")
} else {
  cat("⚠ autre criteria column not found\n")
}

# === DELETE COLUMNS ===
cat("\n=== DELETE COLUMNS ===\n")

cols_to_delete_exact <- c(
  "5.) Reproduction/Si autre à préciser",
  "5.) Reproduction/Si 3=autre (préciser)",
  "5.) Reproduction/Si autre à préciser (periode de renouvellement mâles)",
  "5.) Reproduction/6= Autre (préciser)"
)

# Delete exact match columns
for (col in cols_to_delete_exact) {
  if (col %in% names(raw)) {
    raw <- raw %>% select(-all_of(col))
    cat("✓ Column deleted:", col, "\n")
  } else {
    cat("⚠ Column not found for deletion:", col, "\n")
  }
}

# Delete columns by pattern
cols_to_delete_pattern <- c(
  "centre.*recherche.*préciser|centre.*recherche.*preciser"
)

for (pattern in cols_to_delete_pattern) {
  cols_matching <- names(raw)[grep(pattern, names(raw), ignore.case = TRUE)]
  if (length(cols_matching) > 0) {
    for (col in cols_matching) {
      raw <- raw %>% select(-all_of(col))
      cat("✓ Column deleted:", col, "\n")
    }
  } else {
    cat("⚠ No columns found matching pattern:", pattern, "\n")
  }
}

# === EXPORT ===
cat("\n=== EXPORT CLEANED DATA ===\n")
write.xlsx(raw, OUTPUT_FILE, rowNames = FALSE, overwrite = TRUE)
cat("✓ File saved:", OUTPUT_FILE, "\n")
cat("  Final dimensions:", nrow(raw), "rows ×", ncol(raw), "columns\n")

# === VERIFICATION: Check for empty cells in female reproduction retention column ===
cat("\n=== VERIFICATION: Female Reproduction Retention Column ===\n")
if (new_col_name_38 %in% names(raw)) {
  empty_count <- sum(is.na(raw[[new_col_name_38]]) | raw[[new_col_name_38]] == "")
  cat("Column:", new_col_name_38, "\n")
  cat("Empty cells count:", empty_count, "\n")
  if (empty_count > 0) {
    cat("⚠ WARNING: Found", empty_count, "empty cell(s) in this column!\n")
    cat("Unique values in column:\n")
    print(table(raw[[new_col_name_38]], useNA = "ifany"))
  } else {
    cat("✓ SUCCESS: No empty cells found in this column!\n")
    cat("Value distribution:\n")
    print(table(raw[[new_col_name_38]], useNA = "ifany"))
  }
} else {
  cat("⚠ Column not found for verification\n")
}
