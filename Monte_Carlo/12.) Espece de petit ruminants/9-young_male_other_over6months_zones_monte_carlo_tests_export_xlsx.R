# Young male Other-category animals (>6 months) vs zones: Monte Carlo permutation tests
# Inputs: data7.xlsx and zones.xlsx
# Output: young_male_other_over6months_zones_monte_carlo_results.xlsx

packages <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "writexl")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install packages first: install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))")
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(stringr)
  library(stringi); library(writexl)
})

DATA_FILE <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "young_male_other_over6months_zones_monte_carlo_results.xlsx"
COUNT_COL <- "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) mâles_Autre"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
B <- 10000L
SEED <- 20260916L
ALPHA <- 0.05

normalize_header <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |> str_to_lower() |> str_squish()
  str_replace_all(x, "[^a-z0-9]", "")
}
normalize_key <- function(x) {
  x <- normalize_header(x)
  dplyr::recode(x, "toribossito" = "tori", "dassazoume" = "dassa", "dassazounme" = "dassa", .default = x)
}
resolve_column <- function(expected, headers, terms, label) {
  if (expected %in% headers) return(expected)
  nh <- normalize_header(headers)
  hit <- which(nh == normalize_header(expected))
  if (length(hit) == 1L) return(headers[hit])
  tt <- normalize_header(terms)
  hit <- which(vapply(nh, function(h) all(vapply(tt, function(t) grepl(t, h, fixed = TRUE), logical(1))), logical(1)))
  if (length(hit) == 1L) { message(label, " matched flexibly to: ", headers[hit]); return(headers[hit]) }
  stop(label, " column could not be identified uniquely. Candidates: ", paste(headers[grepl("jeunes|male|autre", nh)], collapse = " | "))
}

if (!file.exists(DATA_FILE)) stop("data7.xlsx not found in: ", getwd())
if (!file.exists(ZONES_FILE)) stop("zones.xlsx not found in: ", getwd())
raw <- read_excel(DATA_FILE, 1, col_types = "text", .name_repair = "minimal")
zraw <- read_excel(ZONES_FILE, 1, col_types = "text", .name_repair = "unique")
COUNT_COL <- resolve_column(COUNT_COL, names(raw), c("Effectif et composition", "jeunes", "6 mois", "males", "Autre"), "Count")
COMMUNE_COL <- resolve_column(COMMUNE_COL, names(raw), c("CARACTERISTIQUES", "UNITE", "ELEVAGE", "Commune"), "Commune")

zones <- zraw |>
  transmute(
    vegetation_zone = str_squish(str_replace_all(`Vegetation zones`, "\\u00A0", " ")),
    phytogeo_zone = str_squish(str_replace_all(`Phytogeographic zones`, "\\u00A0", " ")),
    district = str_squish(str_replace_all(District, "\\u00A0", " "))
  ) |>
  fill(vegetation_zone, phytogeo_zone) |>
  filter(!is.na(district), district != "", !str_detect(str_to_lower(vegetation_zone), "^total")) |>
  mutate(commune_key = normalize_key(district)) |>
  select(commune_key, vegetation_zone, phytogeo_zone) |>
  distinct(commune_key, .keep_all = TRUE)

clean_num <- function(x) suppressWarnings(as.numeric(str_replace_all(str_squish(x), ",", ".")))
data <- raw |>
  transmute(row_id = row_number(), raw_count = str_squish(.data[[COUNT_COL]]), count = clean_num(.data[[COUNT_COL]]), commune_key = normalize_key(.data[[COMMUNE_COL]])) |>
  left_join(zones, by = "commune_key") |>
  mutate(blank = is.na(raw_count) | raw_count == "", nonnumeric = !blank & is.na(count), negative = !is.na(count) & count < 0, noninteger = !is.na(count) & count >= 0 & abs(count - round(count)) > 1e-8, valid = !blank & !nonnumeric & !negative & !noninteger)

kw_h <- function(ranks, groups, tie_correction) {
  n <- length(ranks); idx <- split(seq_along(ranks), groups)
  h <- 12 / (n * (n + 1)) * sum(vapply(idx, function(i) sum(ranks[i])^2 / length(i), numeric(1))) - 3 * (n + 1)
  h / tie_correction
}

run_mc <- function(zone_var, label, seed_offset) {
  d <- data |> filter(valid, !is.na(.data[[zone_var]]), .data[[zone_var]] != "")
  g <- droplevels(factor(d[[zone_var]])); x <- d$count; n <- length(x); k <- nlevels(g)
  desc <- d |> mutate(zone = g) |> group_by(zone) |> summarise(N=n(), Mean=mean(count), SD=sd(count), Median=median(count), Q1=quantile(count,.25), Q3=quantile(count,.75), Minimum=min(count), Maximum=max(count), Zero_count=sum(count==0), Zero_percent=100*mean(count==0), .groups="drop")
  if (n < 2L || k < 2L || length(unique(x)) < 2L) {
    sm <- data.frame(Comparison=label, N=n, Groups=k, Observed_H=NA_real_, Degrees_of_freedom=ifelse(k>1,k-1,NA), Permutations=0L, Monte_Carlo_p=NA_real_, Monte_Carlo_SE=NA_real_, CI95_low=NA_real_, CI95_high=NA_real_, Epsilon_squared=NA_real_, Alpha=ALPHA, Decision="No statistical test performed", Recommendation="Not testable: insufficient observations, groups, or count variation.")
    return(list(summary=sm, descriptive=desc))
  }
  ranks <- rank(x, ties.method="average")
  ties <- table(x); tie_correction <- 1 - sum(ties^3 - ties)/(n^3 - n)
  observed <- kw_h(ranks, g, tie_correction)
  set.seed(SEED + seed_offset); extreme <- 0L; step <- max(1L, B %/% 10L)
  for (b in seq_len(B)) {
    hp <- kw_h(sample(ranks, n, replace=FALSE), g, tie_correction)
    if (hp >= observed - 1e-12) extreme <- extreme + 1L
    if (b %% step == 0L || b == B) message(label, ": ", b, "/", B, " permutations completed")
  }
  p <- (extreme + 1)/(B + 1); se <- sqrt(p*(1-p)/(B+1)); lo <- max(0,p-1.96*se); hi <- min(1,p+1.96*se)
  eps <- max(0, (observed-k+1)/(n-k))
  sm <- data.frame(Comparison=label,N=n,Groups=k,Observed_H=observed,Degrees_of_freedom=k-1,Permutations=B,Monte_Carlo_p=p,Monte_Carlo_SE=se,CI95_low=lo,CI95_high=hi,Epsilon_squared=eps,Alpha=ALPHA,Decision=ifelse(p<ALPHA,"Reject H0: distributions differ","Do not reject H0: no evidence of a difference"),Recommendation="Report Monte Carlo permutation p-value based on the tie-corrected Kruskal-Wallis H statistic.")
  list(summary=sm, descriptive=desc)
}

veg <- run_mc("vegetation_zone", "Young male Other count (>6 months) x vegetation zone", 0L)
phyto <- run_mc("phytogeo_zone", "Young male Other count (>6 months) x phytogeographic zone", 100L)
quality <- data.frame(Metric=c("Total records","Valid nonnegative integer counts","Zero counts","Blank responses","Nonnumeric responses","Negative counts","Noninteger counts","Unmatched communes"), Value=c(nrow(data),sum(data$valid),sum(data$valid & data$count==0),sum(data$blank),sum(data$nonnumeric),sum(data$negative),sum(data$noninteger),sum(is.na(data$vegetation_zone))))
notes <- data.frame(Parameter=c("Variable type","Primary test","Permutations","Zero handling","Invalid values"), Assessment=c("Quantitative discrete count.","Monte Carlo permutation test using tie-corrected Kruskal-Wallis H.",as.character(B),"Zero is a valid count.","Blank, nonnumeric, negative, and noninteger values are excluded and reported."))

writexl::write_xlsx(list(Summary=bind_rows(veg$summary,phyto$summary), Veg_descriptive=veg$descriptive, Phyto_descriptive=phyto$descriptive, Data_quality=quality, Method_notes=notes), OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, winslash="/", mustWork=FALSE))
