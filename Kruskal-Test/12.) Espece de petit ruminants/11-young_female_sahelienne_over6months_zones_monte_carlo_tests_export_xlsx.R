# Young female Sahelian count (>6 months) vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: young_female_sahelienne_over6months_zones_monte_carlo_results.xlsx

packages <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "writexl")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install packages first: install.packages(c(",
       paste(sprintf('"%s"', missing), collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(stringi); library(writexl)
})

DATA_FILE <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "young_female_sahelienne_over6months_zones_monte_carlo_results.xlsx"
B <- 10000L
SEED <- 20260916L
ALPHA <- 0.05
COUNT_COL <- "12.) Effectif et composition du cheptel/Nbre de jeunes (>6 mois) femellesSahelienne"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

normalize_header <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |> str_to_lower() |> str_squish()
  str_replace_all(x, "[^a-z0-9]", "")
}
resolve_column <- function(expected, headers, terms, label) {
  if (expected %in% headers) return(expected)
  nh <- normalize_header(headers)
  hit <- which(nh == normalize_header(expected))
  if (length(hit) == 1L) {
    message(label, " matched after normalization to: ", headers[hit])
    return(headers[hit])
  }
  tt <- normalize_header(terms)
  hit <- which(vapply(nh, function(h) all(vapply(tt, function(t) grepl(t, h, fixed = TRUE), logical(1))), logical(1)))
  if (length(hit) == 1L) {
    message(label, " matched flexibly to: ", headers[hit])
    return(headers[hit])
  }
  candidates <- headers[grepl("jeunes|femelles|sahel", nh)]
  stop(label, " column could not be identified uniquely. Candidate headers: ", paste(candidates, collapse = " | "))
}
normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |> str_to_lower() |> str_squish()
  x <- str_replace_all(x, "[’'`-]", "") |> str_replace_all("[^a-z0-9]", "")
  recode(x, "toribossito" = "tori", "dassazoume" = "dassa", "dassazounme" = "dassa", .default = x)
}

if (!file.exists(DATA_FILE)) stop("data7.xlsx was not found in: ", getwd())
if (!file.exists(ZONES_FILE)) stop("zones.xlsx was not found in: ", getwd())
raw <- read_excel(DATA_FILE, 1, col_types = "text", .name_repair = "minimal")
zraw <- read_excel(ZONES_FILE, 1, col_types = "text", .name_repair = "unique")
COUNT_COL <- resolve_column(COUNT_COL, names(raw), c("Effectif et composition", "jeunes", "6 mois", "femelles", "Sahelienne"), "Young-female Sahelian count")
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

parse_number <- function(x) suppressWarnings(as.numeric(str_replace_all(str_squish(x), ",", ".")))
data <- raw |>
  transmute(
    commune_key = normalize_key(.data[[COMMUNE_COL]]),
    raw_count = str_squish(.data[[COUNT_COL]]),
    count = parse_number(.data[[COUNT_COL]])
  ) |>
  left_join(zones, by = "commune_key") |>
  mutate(
    blank = is.na(raw_count) | raw_count == "",
    nonnumeric = !blank & is.na(count),
    negative = !is.na(count) & count < 0,
    noninteger = !is.na(count) & count >= 0 & abs(count - round(count)) > 1e-9,
    valid = !blank & !nonnumeric & !negative & !noninteger
  )

kw_h <- function(ranks, group_index, group_sizes, n, tie_correction) {
  sums <- numeric(length(group_sizes))
  for (j in seq_along(group_sizes)) sums[j] <- sum(ranks[group_index == j])
  h <- (12 / (n * (n + 1))) * sum((sums^2) / group_sizes) - 3 * (n + 1)
  h / tie_correction
}
run_mc <- function(zone_var, label) {
  d <- data |> filter(valid, !is.na(.data[[zone_var]]), .data[[zone_var]] != "")
  zone <- droplevels(factor(d[[zone_var]])); y <- d$count
  n <- length(y); k <- nlevels(zone)
  if (n < 2L || k < 2L || length(unique(y)) < 2L) {
    reason <- if (n < 2L) "Fewer than two complete valid observations." else if (k < 2L) "Only one zone category represented." else "All valid counts are identical."
    return(list(summary = data.frame(Comparison=label,N=n,Groups=k,H_statistic=NA_real_,DF=NA_integer_,Permutations=0L,Monte_Carlo_p=NA_real_,Monte_Carlo_SE=NA_real_,CI95_low=NA_real_,CI95_high=NA_real_,Epsilon_squared=NA_real_,Alpha=ALPHA,Decision="No statistical test performed",Recommendation=reason), descriptive=data.frame(Note=reason)))
  }
  ranks <- rank(y, ties.method = "average")
  tab <- table(y); tie_correction <- 1 - sum(tab^3 - tab) / (n^3 - n)
  gi <- as.integer(zone); gs <- tabulate(gi, nbins = k)
  h_obs <- kw_h(ranks, gi, gs, n, tie_correction)
  set.seed(SEED + ifelse(zone_var == "phytogeo_zone", 100L, 0L))
  extreme <- 0L; progress_points <- unique(pmax(1L, round(seq(0.1, 1, 0.1) * B)))
  for (b in seq_len(B)) {
    h_perm <- kw_h(sample(ranks, n, replace = FALSE), gi, gs, n, tie_correction)
    if (h_perm >= h_obs - 1e-12) extreme <- extreme + 1L
    if (b %in% progress_points) message(label, ": ", b, "/", B, " permutations completed")
  }
  p <- (extreme + 1) / (B + 1); se <- sqrt(p * (1 - p) / (B + 1))
  eps <- max(0, (h_obs - k + 1) / (n - k))
  desc <- data.frame(value=y, zone=zone) |> group_by(zone) |> summarise(N=n(),Mean=mean(value),SD=sd(value),Median=median(value),Q1=quantile(value,.25),Q3=quantile(value,.75),Minimum=min(value),Maximum=max(value),Zero_count=sum(value==0),Zero_percent=100*mean(value==0),.groups="drop")
  sm <- data.frame(Comparison=label,N=n,Groups=k,H_statistic=h_obs,DF=k-1L,Permutations=B,Monte_Carlo_p=p,Monte_Carlo_SE=se,CI95_low=max(0,p-1.96*se),CI95_high=min(1,p+1.96*se),Epsilon_squared=eps,Alpha=ALPHA,Decision=ifelse(p<ALPHA,"Reject H0: distributions differ by zone","Do not reject H0: no evidence of a difference"),Recommendation="Report the Monte Carlo permutation p-value based on the Kruskal-Wallis H statistic.")
  list(summary=sm, descriptive=desc)
}

veg <- run_mc("vegetation_zone", "Young female Sahelian count (>6 months) x vegetation zone")
phyto <- run_mc("phytogeo_zone", "Young female Sahelian count (>6 months) x phytogeographic zone")
quality <- data.frame(Metric=c("Total records","Valid nonnegative integer counts","Zero counts","Blank responses","Nonnumeric responses","Negative values","Noninteger values","Unmatched communes"),Value=c(nrow(data),sum(data$valid),sum(data$valid & data$count==0),sum(data$blank),sum(data$nonnumeric),sum(data$negative),sum(data$noninteger),sum(is.na(data$vegetation_zone))))
notes <- data.frame(Parameter=c("Variable type","Primary test","Permutations","Zero treatment","Invalid values"),Assessment=c("Quantitative discrete count variable.","Monte Carlo permutation test using the tie-corrected Kruskal-Wallis H statistic.",paste0(B," finite permutations; progress printed every 10%."),"Zero is retained as a valid count.","Blank, nonnumeric, negative, and noninteger values are excluded and reported."))
writexl::write_xlsx(list(Summary=bind_rows(veg$summary,phyto$summary),Veg_descriptive=veg$descriptive,Phyto_descriptive=phyto$descriptive,Data_quality=quality,Method_notes=notes), OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, winslash="/", mustWork=FALSE))
