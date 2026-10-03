# =============================================================================
# CATPCA (categorical principal component analysis) summary table
# -- equivalent of Table 5 in the reference paper, adapted to this dataset --
# =============================================================================
# What this script does:
#   1. Reads the raw survey export (labels version)
#   2. Excludes the training population (Abomey-Calavi + any blank-commune rows)
#   3. Builds the 5 available candidate variables (out of the paper's 6 -
#      "manure collection and use as fertilizer" was never asked in this
#      survey, so it is NOT included):
#        - Breed composition of the herd (Local only / Mixed / Metis-Autre
#          only) - computed from the breed x sex x age columns, for ALL
#          farmers regardless of species (this variable doesn't require
#          species-pure herds, unlike the earlier goat/sheep-specific tables)
#        - Feed supplementation (Yes/No)
#        - Use of crop residues for feeding (Yes/No)
#        - Cultivated land size (ha) - only available for agriculture
#          practitioners with a positive reported area (~131 of 211 farmers)
#        - Total herd size (small ruminants)
#   4. Runs CATPCA via Gifi::princals() (R's closest equivalent to SPSS's
#      CATPCA - categorical/nonlinear PCA via optimal scaling)
#   5. Computes Cronbach's alpha per dimension using the standard CATPCA
#      formula: alpha = (J/(J-1)) * (1 - 1/eigenvalue), J = number of variables
#   6. Builds the final summary table (eigenvalues, variance explained,
#      component loadings) and exports it to Excel
#
# Required packages: readxl, dplyr, tidyr, Gifi, openxlsx
# Install Gifi if needed: install.packages("Gifi")
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(Gifi)
library(openxlsx)

# ---- File paths (edit these to match your local paths) --------------------
LABELS_PATH <- "C:/Users/lucas/OneDrive/Bureau/Data/Tables/Table 4/Questionnaire_caracterisation_pratiques_de_croisements_-_latest_version_-_labels_-_2026-08-25-07-38-57.xlsx"
OUTPUT_PATH <- "catpca_table5_equivalent.xlsx"

# ---- Robust column lookup ---------------------------------------------------
# Column headers in this export sometimes carry trailing/leading whitespace
# that isn't visible when reading the text, and that whitespace can silently
# get stripped or altered when code is copied between editors/consoles. To
# avoid depending on an exact character-for-character match (including
# invisible spaces), columns are found by a distinctive substring instead,
# with whitespace trimmed on both sides before comparing.
find_col <- function(df, pattern, fixed = TRUE) {
  hits <- grep(pattern, str_trim(names(df)), fixed = fixed, value = FALSE)
  if (length(hits) != 1) {
    stop(sprintf("Expected exactly 1 column matching '%s', found %d.\nMatches: %s",
                  pattern, length(hits),
                  paste(names(df)[hits], collapse = " | ")))
  }
  names(df)[hits]
}

# Breed columns, identified by a short distinctive substring rather than the
# full header text, then resolved into real column names once the file is
# loaded (see find_col() above and the resolution block below).
BREED_COL_PATTERNS <- list(
  Djallonke = c("m\u00e2le adultes Djallonke", "m\u00e2les_Djallonke", "femelles_Djallonke",
                "m\u00e2le_Djallonke", "femelle_Djallonke"),
  Sahelien  = c("m\u00e2le adultes Sahelien", "m\u00e2les_Sahelien", "femellesSahelienne",
                "m\u00e2le_Sahelien", "femelle_Sahelien"),
  Metis     = c("m\u00e2le adultes Metis", "m\u00e2les_Metis", "femelles_Metis",
                "m\u00e2le_Metis", "femelle_Metis"),
  Autre     = c("m\u00e2le adultes Autre", "m\u00e2les_Autre", "femelles_Autre")
)

# =============================================================================
# 1. Read data
# =============================================================================
raw <- read_excel(LABELS_PATH, col_types = "text")

# ---- Resolve breed columns from the actual loaded data ----------------------
BREED_COLS <- lapply(BREED_COL_PATTERNS, function(patterns) {
  sapply(patterns, function(p) find_col(raw, p))
})
cat("Resolved breed columns (", sum(lengths(BREED_COLS)), "total ):\n")
for (b in names(BREED_COLS)) cat("  ", b, ":", length(BREED_COLS[[b]]), "columns\n")
cat("\n")

# ---- Resolve the exact column names present in THIS file -------------------
COMMUNE_COL  <- find_col(raw, "/Commune")
TOTAL_COL    <- find_col(raw, "Effectif total du troupeau")
AREA_COL     <- find_col(raw, "superficie de terre agricole")
MAIN_ACT_COL <- find_col(raw, "principale activit")
SEC_AGRI_COL <- find_col(raw, "activites secondaires ?/Agriculture")
SUPPL_COL    <- find_col(raw, "compl\u00e9ments alimentaires aux animaux")
RESIDUE_COL  <- find_col(raw, "r\u00e9sidus de r\u00e9coltes")

cat("Resolved column names:\n")
cat("  COMMUNE_COL :", COMMUNE_COL, "\n")
cat("  TOTAL_COL   :", TOTAL_COL, "\n")
cat("  AREA_COL    :", AREA_COL, "\n")
cat("  MAIN_ACT_COL:", MAIN_ACT_COL, "\n")
cat("  SEC_AGRI_COL:", SEC_AGRI_COL, "\n")
cat("  SUPPL_COL   :", SUPPL_COL, "\n")
cat("  RESIDUE_COL :", RESIDUE_COL, "\n\n")

# =============================================================================
# 2. Exclude the training population (Abomey-Calavi + blank commune)
# =============================================================================
d <- raw %>%
  filter(!is.na(.data[[COMMUNE_COL]]), .data[[COMMUNE_COL]] != "Abomey-Calavi")
cat("N after excluding training population:", nrow(d), "\n")

# =============================================================================
# 3. Build the 5 candidate variables
# =============================================================================

## 3a. Breed composition (for ALL farmers - no species restriction needed)
for (breed in names(BREED_COLS)) {
  cols <- BREED_COLS[[breed]]
  d[[paste0("has_", breed)]] <- rowSums(
    sapply(cols, function(c) replace_na(as.numeric(d[[c]]), 0))
  ) > 0
}
d <- d %>%
  mutate(
    has_local  = has_Djallonke | has_Sahelien,
    has_exotic = has_Metis | has_Autre,
    breed_composition = case_when(
      has_local  & !has_exotic ~ "Local only",
      has_exotic & !has_local  ~ "Metis/Autre only",
      has_local  & has_exotic  ~ "Mixed",
      TRUE ~ NA_character_
    )
  )

## 3b. Feed supplementation (Yes/No)
d$feed_supplementation <- d[[SUPPL_COL]]

## 3c. Use of crop residues for feeding (Yes/No; No-supplementation farmers = No)
# NOTE: RESIDUE_COL is coded 0/1 for respondents who said "Oui" to
# supplementation (0 = did not use crop residues specifically, 1 = did), and
# is only NA for respondents who said "Non" to supplementation overall (or
# did not answer that question at all). A naive is.na() check would wrongly
# label the 0-coded "supplemented, but not with residues" farmers as missing
# or as users, so the value itself must be checked, not just its presence.
d$residue_raw <- as.numeric(d[[RESIDUE_COL]])
d$crop_residue_use <- case_when(
  d$feed_supplementation == "Non" ~ "Non",
  is.na(d$feed_supplementation)   ~ NA_character_,
  d$residue_raw == 1              ~ "Oui",
  d$residue_raw == 0              ~ "Non",
  TRUE ~ NA_character_
)

## 3d. Cultivated land size (ha) - restricted to declared agriculture
## practitioners with a positive reported area (the same 131-farmer
## population established earlier in this project for this variable)
# NOTE: this question is conditional ("Si pratique de l'agriculture..."), so
# it should only have a value for farmers who practice agriculture. Two
# groups must be excluded, not just treated as missing by chance:
#   (a) non-practitioners whose raw value is exactly 0 - a data-collection
#       skip artifact (the tool recorded 0 instead of leaving it blank), and
#   (b) non-practitioners who DO have a positive reported area (22 farmers) -
#       an inconsistent record (flagged earlier in this project) where
#       someone who doesn't identify agriculture as a main or secondary
#       activity nonetheless has an area value. Keeping these in would
#       silently pull the analytic sample back up from 131 to 153.
d$is_practitioner <- (d[[MAIN_ACT_COL]] == "Agriculture") | (as.numeric(d[[SEC_AGRI_COL]]) == 1)
d$land_size <- as.numeric(d[[AREA_COL]])
d$land_size[!d$is_practitioner] <- NA_real_
d$land_size[d$land_size == 0] <- NA_real_

cat("Land size restricted to agriculture practitioners with positive area:",
    sum(!is.na(d$land_size)), "farmers (should match the established n=131)\n")

## 3e. Total herd size
d$herd_size <- as.numeric(d[[TOTAL_COL]])

cat("\nMissingness per variable:\n")
cat("  Breed composition:", sum(is.na(d$breed_composition)), "\n")
cat("  Feed supplementation:", sum(is.na(d$feed_supplementation)), "\n")
cat("  Crop residue use:", sum(is.na(d$crop_residue_use)), "\n")
cat("  Land size:", sum(is.na(d$land_size)), "\n")
cat("  Herd size:", sum(is.na(d$herd_size)), "\n")

# =============================================================================
# 4. Build the analytic (complete-case) dataset and run CATPCA
# =============================================================================
catpca_vars <- c("breed_composition", "feed_supplementation", "crop_residue_use",
                  "land_size", "herd_size")

cat("\nMissingness per variable, out of", nrow(d), "farmers:\n")
for (v in catpca_vars) {
  cat(sprintf("  %-25s %d missing\n", v, sum(is.na(d[[v]]))))
}

analytic <- d %>%
  select(all_of(catpca_vars)) %>%
  mutate(across(c(breed_composition, feed_supplementation, crop_residue_use), as.factor)) %>%
  drop_na()

cat(sprintf("\nEXACT final analytic sample for CATPCA (complete cases on all 5 variables): n = %d\n",
            nrow(analytic)))
cat("(Expected: n = 130 - land size is the binding constraint, since it is only\n")
cat(" valid for the 131 declared agriculture practitioners, minus 1 farmer who is\n")
cat(" also missing on feed supplementation/crop residue use.)\n")

# Gifi::princals() internally extracts columns with x[, i], which returns a
# one-column tibble (not a vector) when the input is a tibble (as produced by
# dplyr), and then fails with "arguments imply differing number of rows: 0, N".
# Converting to a plain base data.frame avoids that.
analytic <- as.data.frame(analytic)

# levels: Gifi accepts "nominal", "ordinal" or "metric" (not "numerical").
#   - "nominal" for the unordered categorical variables
#   - "metric"  for the continuous variables (linear/numeric scaling, the
#               equivalent of SPSS CATPCA's "numeric" scaling level)
fit <- princals(analytic,
                ndim = 2,
                levels = c("nominal", "nominal", "nominal", "metric", "metric"))

print(summary(fit))

# =============================================================================
# 5. Eigenvalues, variance explained
# =============================================================================
eigenvalues <- fit$evals[1:2]
n_vars <- ncol(analytic)
variance_explained <- eigenvalues / n_vars * 100

# Cronbach's alpha per dimension, standard CATPCA formula
cronbach_alpha <- (n_vars / (n_vars - 1)) * (1 - 1 / eigenvalues)

# =============================================================================
# 6. Component loadings
# =============================================================================
loadings <- as.data.frame(fit$loadings)
colnames(loadings) <- c("Dimension_1", "Dimension_2")
loadings$Variable <- rownames(loadings)
loadings <- loadings %>% select(Variable, Dimension_1, Dimension_2)

cat("\n===== Component loadings =====\n")
print(loadings)

summary_table <- data.frame(
  Parameter = c("Cronbach's alpha", "Total eigenvalues", "Total variance explained (%)"),
  Dimension_1 = c(cronbach_alpha[1], eigenvalues[1], variance_explained[1]),
  Dimension_2 = c(cronbach_alpha[2], eigenvalues[2], variance_explained[2])
)

cat("\n===== Summary table (Table 5 equivalent) =====\n")
print(summary_table)
cat(sprintf("\nn = %d farms used in the final CATPCA model\n", nrow(analytic)))
cat("NOTE: 'Manure collection and use as fertilizer' is not in this dataset and is excluded.\n")

# =============================================================================
# 7. Export to Excel
# =============================================================================
wb <- createWorkbook()
addWorksheet(wb, "CATPCA_summary")
writeData(wb, "CATPCA_summary", summary_table)
writeData(wb, "CATPCA_summary", data.frame(Note = sprintf("n = %d complete-case farms used (of %d total, after excluding training population)", nrow(analytic), nrow(d))),
          startRow = nrow(summary_table) + 3)
writeData(wb, "CATPCA_summary", data.frame(Note = "Manure collection and use as fertilizer was not asked in this survey and is excluded from this model."),
          startRow = nrow(summary_table) + 4)

addWorksheet(wb, "Component_loadings")
writeData(wb, "Component_loadings", loadings)

saveWorkbook(wb, OUTPUT_PATH, overwrite = TRUE)
cat("\nSaved:", OUTPUT_PATH, "\n")
