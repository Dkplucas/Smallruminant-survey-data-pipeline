# =============================================================================
# Farm typology -- equivalent of Table 6 in the reference paper
# "Comparative profile of the different types of farms differentiated by
#  clustering", adapted to this small-ruminant dataset
# =============================================================================
# What this script does:
#   1. Reads the raw survey export and excludes the training population
#   2. Rebuilds the same 5 variables used in the CATPCA (breed composition,
#      feed supplementation, crop-residue use, cultivated land size, herd size)
#      and keeps the 130 complete cases
#   3. Clusters the farms. The paper used SPSS TwoStep clustering, which has no
#      R equivalent; the standard substitute for mixed categorical/continuous
#      data is Gower distance + PAM (k-medoids). Land size and herd size are
#      log-transformed before computing distances because both are strongly
#      right-skewed (a few very large values would otherwise dominate).
#   4. Chooses the number of clusters transparently: for k = 2..6 it reports
#      the average silhouette width and a bootstrap stability score (adjusted
#      Rand index), and picks the SMALLEST k whose clusters are stable
#      (mean ARI >= 0.80). Silhouette alone is NOT used, because with mostly
#      categorical variables it keeps rising with k (clusters just become the
#      cells of the cross-classification). Override with K_OVERRIDE if wanted.
#   5. Builds the Table 6 profile: % of farms per category (with chi-square and
#      a/b/c letters from Bonferroni-adjusted pairwise proportion tests; plain
#      Pearson chi-square with the asymptotic p-value, no Monte Carlo) and
#      mean +/- SD for continuous variables (Kruskal-Wallis p-value, letters
#      from Bonferroni-adjusted pairwise Wilcoxon tests)
#   6. Exports the table, the k-selection diagnostics, the farm-level cluster
#      assignments, and the cluster distribution by phytogeographic zone and
#      by species to Excel
#
# IMPORTANT interpretation notes
#   - The clustering variables are the same variables then "tested" across
#     clusters (as in the paper's Table 6), so those tests are descriptive:
#     they show how the clusters differ, not independent evidence of it.
#   - Crop-residue use is nested inside feed supplementation (nobody who does
#     not supplement uses residues), so feeding practice carries extra weight
#     in the distance. This is a property of the data, not of the method.
#   - "Manure collection and use" (a key variable in the paper) was never asked
#     in this survey and is not included.
#
# Required packages: readxl, dplyr, tidyr, stringr, stringi, cluster, openxlsx
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(cluster)
library(openxlsx)

# ---- File paths & settings (edit to match your local paths) ----------------
LABELS_PATH <- "Questionnaire_caracterisation_pratiques_de_croisements_-_latest_version_-_labels_-_2026-08-25-07-38-57.xlsx"
ZONES_PATH  <- "zones.xlsx"
OUTPUT_PATH <- "farm_typology_table5.xlsx"

K_RANGE        <- 2:6      # candidate numbers of clusters
STABILITY_MIN  <- 0.80     # bootstrap ARI threshold for "stable"
N_BOOT         <- 200      # bootstrap resamples per k
K_OVERRIDE     <- NULL     # e.g. K_OVERRIDE <- 4 to force a specific k
MIN_CATEGORY_N <- 5        # categories with fewer farms than this are left out of the chi-square TEST only
                           # (set to 0 to keep every category in the test)
CLUSTER_LABELS <- NULL     # e.g. c("Type A","Type B",...) once you have named them
SEED           <- 1

# ---- Robust column lookup (see CATPCA script: avoids invisible-whitespace bugs)
find_col <- function(df, pattern, fixed = TRUE) {
  hits <- grep(pattern, str_trim(names(df)), fixed = fixed, value = FALSE)
  if (length(hits) != 1) {
    stop(sprintf("Expected exactly 1 column matching '%s', found %d.\nMatches: %s",
                 pattern, length(hits), paste(names(df)[hits], collapse = " | ")))
  }
  names(df)[hits]
}

normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(str_squish(x))
  x <- str_replace_all(x, "[\u2019'`-]", "")
  x <- str_replace_all(x, "[^a-z0-9]", "")
  recode(x, "toribossito" = "tori", "dassazoume" = "dassa", "dassazounme" = "dassa", .default = x)
}

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
# 1. Read data and resolve columns
# =============================================================================
raw   <- read_excel(LABELS_PATH, col_types = "text")
zones <- read_excel(ZONES_PATH,  col_types = "text")

BREED_COLS <- lapply(BREED_COL_PATTERNS, function(ps) sapply(ps, function(p) find_col(raw, p)))
COMMUNE_COL  <- find_col(raw, "/Commune")
TOTAL_COL    <- find_col(raw, "Effectif total du troupeau")
AREA_COL     <- find_col(raw, "superficie de terre agricole")
MAIN_ACT_COL <- find_col(raw, "principale activit")
SEC_AGRI_COL <- find_col(raw, "activites secondaires ?/Agriculture")
SUPPL_COL    <- find_col(raw, "compl\u00e9ments alimentaires aux animaux")
RESIDUE_COL  <- find_col(raw, "r\u00e9sidus de r\u00e9coltes")
OVINS_COL    <- find_col(raw, "animales que vous \u00e9levez?/1=Ovins")
CAPRINS_COL  <- find_col(raw, "animales que vous \u00e9levez?/2=Caprins")

d <- raw %>%
  filter(!is.na(.data[[COMMUNE_COL]]), .data[[COMMUNE_COL]] != "Abomey-Calavi")
cat("N after excluding training population:", nrow(d), "\n")
d$row_id <- seq_len(nrow(d))

# =============================================================================
# 2. Build the 5 variables (identical to the CATPCA script)
# =============================================================================
for (breed in names(BREED_COLS)) {
  d[[paste0("has_", breed)]] <- rowSums(
    sapply(BREED_COLS[[breed]], function(c) replace_na(as.numeric(d[[c]]), 0))) > 0
}
d <- d %>%
  mutate(
    has_local  = has_Djallonke | has_Sahelien,
    has_exotic = has_Metis | has_Autre,
    breed_composition = case_when(
      has_local  & !has_exotic ~ "Local breed(s) only",
      has_exotic & !has_local  ~ "Metis/Autre only",
      has_local  & has_exotic  ~ "Mixed (local + Metis/Autre)",
      TRUE ~ NA_character_))

d$feed_supplementation <- recode(d[[SUPPL_COL]], "Oui" = "Yes", "Non" = "No")
d$residue_raw <- as.numeric(d[[RESIDUE_COL]])
d$crop_residue_use <- case_when(
  d$feed_supplementation == "No" ~ "No",
  is.na(d$feed_supplementation)  ~ NA_character_,
  d$residue_raw == 1             ~ "Yes",
  d$residue_raw == 0             ~ "No",
  TRUE ~ NA_character_)

d$is_practitioner <- (d[[MAIN_ACT_COL]] == "Agriculture") | (as.numeric(d[[SEC_AGRI_COL]]) == 1)
d$land_size <- as.numeric(d[[AREA_COL]])
d$land_size[!d$is_practitioner] <- NA_real_   # non-practitioner values are skip artifacts / inconsistent
d$land_size[d$land_size == 0]   <- NA_real_
d$herd_size <- as.numeric(d[[TOTAL_COL]])

d$species <- case_when(
  as.numeric(d[[OVINS_COL]]) == 1 & as.numeric(d[[CAPRINS_COL]]) == 0 ~ "Sheep only",
  as.numeric(d[[OVINS_COL]]) == 0 & as.numeric(d[[CAPRINS_COL]]) == 1 ~ "Goats only",
  as.numeric(d[[OVINS_COL]]) == 1 & as.numeric(d[[CAPRINS_COL]]) == 1 ~ "Goats + sheep",
  TRUE ~ "Unknown")

zones_valid <- zones %>%
  fill(`Vegetation zones`, `Phytogeographic zones`) %>%
  filter(!is.na(District), District != "") %>%
  mutate(key = normalize_key(District))
d$key <- normalize_key(d[[COMMUNE_COL]])
d <- d %>% left_join(zones_valid %>% select(key, `Phytogeographic zones`), by = "key")

cluster_vars <- c("breed_composition", "feed_supplementation", "crop_residue_use",
                  "land_size", "herd_size")
a <- d %>% filter(if_all(all_of(cluster_vars), ~ !is.na(.x)))
cat(sprintf("Complete cases used for clustering: n = %d (expected 130)\n", nrow(a)))

# =============================================================================
# 3. Gower distance (land size and herd size log-transformed)
# =============================================================================
X <- data.frame(
  breed = factor(a$breed_composition),
  suppl = factor(a$feed_supplementation),
  resid = factor(a$crop_residue_use),
  land  = log(a$land_size),
  herd  = log(a$herd_size))
gd <- daisy(X, metric = "gower")
D  <- as.matrix(gd)
n  <- nrow(D)

# =============================================================================
# 4. Choose k: silhouette + bootstrap stability
# =============================================================================
ari <- function(x, y) {                         # adjusted Rand index
  tab <- table(x, y); N <- sum(tab)
  c2 <- function(v) v * (v - 1) / 2
  sij <- sum(c2(tab)); ra <- sum(c2(rowSums(tab))); rb <- sum(c2(colSums(tab)))
  expct <- ra * rb / c2(N); mx <- (ra + rb) / 2
  if (mx == expct) return(1)
  (sij - expct) / (mx - expct)
}

set.seed(SEED)
sel <- do.call(rbind, lapply(K_RANGE, function(k) {
  base <- pam(gd, k = k, diss = TRUE)
  boot_ari <- mean(sapply(seq_len(N_BOOT), function(b) {
    idx <- sample(n, n, replace = TRUE)
    pb  <- pam(D[idx, idx], k = k, diss = TRUE)
    asg <- apply(D[, idx[pb$id.med], drop = FALSE], 1, which.min)
    ari(base$clustering, asg)
  }))
  data.frame(k = k,
             avg_silhouette = round(base$silinfo$avg.width, 3),
             bootstrap_ARI  = round(boot_ari, 3),
             smallest_cluster_n = min(base$clusinfo[, "size"]),
             sizes = paste(sort(base$clusinfo[, "size"], decreasing = TRUE), collapse = "/"))
}))
stable <- sel$k[sel$bootstrap_ARI >= STABILITY_MIN]
K <- if (!is.null(K_OVERRIDE)) K_OVERRIDE else if (length(stable)) min(stable) else sel$k[which.max(sel$bootstrap_ARI)]
sel$chosen <- ifelse(sel$k == K, "<== chosen", "")
cat("\n===== Cluster-number selection =====\n"); print(sel, row.names = FALSE)
cat(sprintf("\nChosen k = %d (smallest k with bootstrap ARI >= %.2f)\n", K, STABILITY_MIN))

fit <- pam(gd, k = K, diss = TRUE)
# relabel clusters 1..K by decreasing size
ord <- order(table(fit$clustering), decreasing = TRUE)
a$cluster <- match(fit$clustering, ord)
labs <- if (is.null(CLUSTER_LABELS)) paste("Cluster", seq_len(K)) else CLUSTER_LABELS
a$cluster_f <- factor(a$cluster, levels = seq_len(K), labels = labs)

# =============================================================================
# 5. Helpers: compact letters, tests
# =============================================================================
# Compact letter display from a symmetric logical matrix (TRUE = significantly
# different), using the insert-and-absorb algorithm.
cld_letters <- function(diff, k) {
  sets <- list(seq_len(k))
  for (i in seq_len(k - 1)) for (j in (i + 1):k) if (isTRUE(diff[i, j])) {
    new <- list()
    for (s in sets) {
      if (i %in% s && j %in% s) { new[[length(new) + 1]] <- setdiff(s, i)
                                  new[[length(new) + 1]] <- setdiff(s, j) }
      else new[[length(new) + 1]] <- s
    }
    new <- new[lengths(new) > 0]
    keep <- rep(TRUE, length(new))
    for (u in seq_along(new)) for (v in seq_along(new)) {
      if (u != v && keep[u] && keep[v] && all(new[[u]] %in% new[[v]]) &&
          (length(new[[u]]) < length(new[[v]]) || u > v)) keep[u] <- FALSE
    }
    sets <- new[keep]
  }
  sets <- sets[order(sapply(sets, min))]
  vapply(seq_len(k), function(g) paste(letters[which(sapply(sets, function(s) g %in% s))], collapse = ""), "")
}

prop_letters <- function(x, nn, k) {
  pairs <- combn(k, 2); np <- ncol(pairs)
  diff <- matrix(FALSE, k, k)
  for (p in seq_len(np)) {
    i <- pairs[1, p]; j <- pairs[2, p]
    if (x[i] == x[j] && nn[i] == nn[j]) next
    if ((x[i] == 0 && x[j] == 0) || (x[i] == nn[i] && x[j] == nn[j])) next
    pv <- suppressWarnings(prop.test(c(x[i], x[j]), c(nn[i], nn[j]), correct = FALSE)$p.value)
    if (!is.na(pv) && pv * np < 0.05) diff[i, j] <- diff[j, i] <- TRUE
  }
  cld_letters(diff, k)
}

num_letters <- function(v, g, k) {
  pairs <- combn(k, 2); np <- ncol(pairs)
  diff <- matrix(FALSE, k, k)
  for (p in seq_len(np)) {
    i <- pairs[1, p]; j <- pairs[2, p]
    pv <- suppressWarnings(wilcox.test(v[g == i], v[g == j], exact = FALSE)$p.value)
    if (!is.na(pv) && pv * np < 0.05) diff[i, j] <- diff[j, i] <- TRUE
  }
  cld_letters(diff, k)
}

fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

# =============================================================================
# 6. Build the Table 6 profile
# =============================================================================
excl_notes <- character(0)
cat_vars <- list(
  list(label = "Breed composition", var = "breed_composition",
       levels = c("Local breed(s) only", "Mixed (local + Metis/Autre)", "Metis/Autre only")),
  list(label = "Feed supplementation", var = "feed_supplementation", levels = c("Yes", "No")),
  list(label = "Use of crop residues for feeding", var = "crop_residue_use", levels = c("Yes", "No")))

col_names <- c("Variable", paste0("Overall (n = ", nrow(a), ")"),
               paste0(labs, " (n = ", as.integer(table(a$cluster)), ")"), "Chi-square", "p-value", "Test")
blank <- function() setNames(as.list(rep("", length(col_names))), col_names)
rows <- list()
add <- function(r) rows[[length(rows) + 1]] <<- r

h <- blank(); h[["Variable"]] <- "Frequency (% of farms)"; add(h)
for (cv in cat_vars) {
  ct  <- table(a$cluster, a[[cv$var]])[, cv$levels, drop = FALSE]
  # Plain Pearson chi-square (asymptotic p-value). Categories with fewer than
  # MIN_CATEGORY_N farms overall are left out of the TEST ONLY (they still
  # appear in the table), because a near-empty category makes the expected
  # counts too small for the chi-square approximation to be valid.
  keep   <- colSums(ct) >= MIN_CATEGORY_N
  ct_t   <- ct[, keep, drop = FALSE]
  chi    <- suppressWarnings(chisq.test(ct_t, correct = FALSE))
  pval   <- chi$p.value
  pct_lo <- 100 * mean(chi$expected < 5)
  hr <- blank(); hr[["Variable"]] <- cv$label
  hr[["Chi-square"]] <- sprintf("%.3f", unname(chi$statistic)); hr[["p-value"]] <- fmt_p(pval)
  hr[["Test"]] <- sprintf("Pearson chi-square, df = %d, n = %d; %.0f%% of cells expected < 5",
                          unname(chi$parameter), sum(ct_t), pct_lo)
  if (any(!keep)) {
    excl_notes <- c(excl_notes, sprintf("%s: category '%s' (%d farm%s) excluded from the chi-square test only.",
                    cv$label, paste(colnames(ct)[!keep], collapse = "', '"), sum(ct[, !keep]),
                    ifelse(sum(ct[, !keep]) == 1, "", "s")))
  }
  add(hr)
  for (lv in cv$levels) {
    x  <- as.integer(ct[, lv]); nn <- as.integer(rowSums(ct))
    lt <- if (pval < 0.05) prop_letters(x, nn, K) else rep("", K)
    r <- blank(); r[["Variable"]] <- paste0("   ", lv)
    r[[2]] <- sprintf("%.1f", 100 * sum(a[[cv$var]] == lv) / nrow(a))
    for (g in seq_len(K)) r[[2 + g]] <- trimws(sprintf("%.1f %s", 100 * x[g] / nn[g], lt[g]))
    add(r)
  }
}
h <- blank(); h[["Variable"]] <- "Means \u00b1 SD (p-value: Kruskal-Wallis)"; add(h)
for (cv in list(list(label = "Herd size, small ruminants (heads)", var = "herd_size"),
                list(label = "Cultivated land size (ha)", var = "land_size"))) {
  v <- a[[cv$var]]
  kw <- kruskal.test(v ~ a$cluster)
  lt <- if (kw$p.value < 0.05) num_letters(v, a$cluster, K) else rep("", K)
  r <- blank(); r[["Variable"]] <- cv$label
  r[[2]] <- sprintf("%.1f \u00b1 %.1f", mean(v), sd(v))
  for (g in seq_len(K)) r[[2 + g]] <- trimws(sprintf("%.1f \u00b1 %.1f %s", mean(v[a$cluster == g]), sd(v[a$cluster == g]), lt[g]))
  r[["Chi-square"]] <- sprintf("H = %.3f", unname(kw$statistic)); r[["p-value"]] <- fmt_p(kw$p.value)
  r[["Test"]] <- "Kruskal-Wallis"
  add(r)
}
table6 <- do.call(rbind, lapply(rows, as.data.frame, check.names = FALSE, stringsAsFactors = FALSE))
cat("\n===== Table 6 equivalent =====\n"); print(table6, row.names = FALSE, right = FALSE)

# =============================================================================
# 7. Cluster context: distribution by zone and by species
# =============================================================================
zone_order <- c("Vallee de l\u2019Oueme (VOZ)", "Plateau", "Borgou-Sud", "Zou")
a$zone <- factor(a$`Phytogeographic zones`, levels = zone_order)
zone_pct <- as.data.frame.matrix(round(100 * prop.table(table(a$zone, a$cluster_f), 1), 1))
zone_pct <- cbind(Zone = rownames(zone_pct), n_farms = as.integer(table(a$zone)), zone_pct)
sp_pct <- as.data.frame.matrix(round(100 * prop.table(table(a$species, a$cluster_f), 1), 1))
sp_pct <- cbind(Species = rownames(sp_pct), n_farms = as.integer(table(a$species)), sp_pct)

assign_tbl <- a %>%
  transmute(row_id, Cluster = as.character(cluster_f), Commune = .data[[COMMUNE_COL]],
            Zone = as.character(zone), Species = species, breed_composition,
            feed_supplementation, crop_residue_use, land_size_ha = land_size, herd_size)

# =============================================================================
# 8. Export to Excel
# =============================================================================
wb <- createWorkbook()
hs <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2", halign = "center", wrapText = TRUE,
                  fontName = "Arial", fontSize = 10)
bs <- createStyle(fontName = "Arial", fontSize = 10)
sec <- createStyle(textDecoration = "bold", fontName = "Arial", fontSize = 10, fgFill = "#F2F2F2")

addWorksheet(wb, "Table6_profile")
writeData(wb, "Table6_profile", table6, headerStyle = hs)
addStyle(wb, "Table6_profile", bs, rows = 2:(nrow(table6) + 1), cols = seq_len(ncol(table6)), gridExpand = TRUE)
sec_rows <- which(table6$Variable %in% c("Frequency (% of farms)", "Means \u00b1 SD (p-value: Kruskal-Wallis)")) + 1
addStyle(wb, "Table6_profile", sec, rows = sec_rows, cols = seq_len(ncol(table6)), gridExpand = TRUE)
setColWidths(wb, "Table6_profile", cols = 1, widths = 44)
setColWidths(wb, "Table6_profile", cols = 2:(ncol(table6) - 1), widths = 18)
setColWidths(wb, "Table6_profile", cols = ncol(table6), widths = 40)
notes <- c(
  "Notes",
  "Clusters: Gower distance + PAM (k-medoids); land size and herd size log-transformed for the distance. Cluster labels are ordered by size.",
  sprintf("k = %d chosen as the smallest number of clusters with bootstrap stability (adjusted Rand index) >= %.2f; see 'Cluster_selection'.", K, STABILITY_MIN),
  "Letters (a, b, c): within a row, clusters sharing a letter do not differ significantly (Bonferroni-adjusted pairwise tests, alpha = 0.05). Shown only when the overall test is significant.",
  "Chi-square: Pearson test with the standard (asymptotic) p-value. 'Test' column shows df, n and the % of cells with expected count < 5.",
  excl_notes,
  "These variables were used to form the clusters, so the tests describe cluster differences; they are not independent evidence of them.",
  "Manure collection and use as fertilizer (a clustering variable in the paper) was not asked in this survey and is not included.")
writeData(wb, "Table6_profile", data.frame(x = notes), startRow = nrow(table6) + 4, colNames = FALSE)

for (nm in c("Cluster_selection")) {
  addWorksheet(wb, nm); writeData(wb, nm, sel, headerStyle = hs)
  addStyle(wb, nm, bs, rows = 2:(nrow(sel) + 1), cols = seq_len(ncol(sel)), gridExpand = TRUE)
  setColWidths(wb, nm, cols = seq_len(ncol(sel)), widths = "auto")
}
addWorksheet(wb, "Cluster_context")
writeData(wb, "Cluster_context", data.frame(x = "% of each phytogeographic zone's farms belonging to each cluster (rows sum to 100)"), colNames = FALSE)
writeData(wb, "Cluster_context", zone_pct, startRow = 2, headerStyle = hs)
writeData(wb, "Cluster_context", data.frame(x = "% of each species group's farms belonging to each cluster (rows sum to 100)"),
          startRow = nrow(zone_pct) + 5, colNames = FALSE)
writeData(wb, "Cluster_context", sp_pct, startRow = nrow(zone_pct) + 6, headerStyle = hs)
setColWidths(wb, "Cluster_context", cols = 1:ncol(zone_pct), widths = 24)

addWorksheet(wb, "Farm_assignments")
writeData(wb, "Farm_assignments", assign_tbl, headerStyle = hs)
setColWidths(wb, "Farm_assignments", cols = seq_len(ncol(assign_tbl)), widths = "auto")
freezePane(wb, "Farm_assignments", firstRow = TRUE)

saveWorkbook(wb, OUTPUT_PATH, overwrite = TRUE)
cat("\nSaved:", OUTPUT_PATH, "\n")
