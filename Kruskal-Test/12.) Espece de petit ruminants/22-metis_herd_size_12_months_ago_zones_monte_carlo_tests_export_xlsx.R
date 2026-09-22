# Metis herd size 12 months ago vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: metis_herd_size_12_months_ago_zones_monte_carlo_results.xlsx

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
OUTPUT_FILE <- "metis_herd_size_12_months_ago_zones_monte_carlo_results.xlsx"
COUNT_COL <- "12.) Effectif et composition du cheptel/Effectif il y a 12 mois_Metis"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
ALPHA <- 0.05
B <- 10000L
SEED <- 20260916L

normalize_header <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |> str_to_lower() |> str_squish()
  str_replace_all(x, "[^a-z0-9]", "")
}
normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |> str_to_lower() |> str_squish()
  x <- str_replace_all(x, "[’'`-]", "") |> str_replace_all("[^a-z0-9]", "")
  recode(x, "toribossito" = "tori", "dassazoume" = "dassa",
         "dassazounme" = "dassa", .default = x)
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
  hit <- which(vapply(nh, function(h) all(vapply(tt, function(t) str_detect(h, fixed(t)), logical(1))), logical(1)))
  if (length(hit) == 1L) {
    message(label, " matched flexibly to: ", headers[hit])
    return(headers[hit])
  }
  candidates <- headers[str_detect(nh, "effectifilya12mois|metis|commune")]
  stop(label, " could not be identified uniquely. Candidate headers: ", paste(candidates, collapse = " | "))
}

if (!file.exists(DATA_FILE)) stop("data7.xlsx not found in: ", getwd())
if (!file.exists(ZONES_FILE)) stop("zones.xlsx not found in: ", getwd())
raw <- read_excel(DATA_FILE, 1, col_types = "text", .name_repair = "minimal")
zraw <- read_excel(ZONES_FILE, 1, col_types = "text", .name_repair = "unique")
COUNT_COL <- resolve_column(COUNT_COL, names(raw), c("Effectif il y a 12 mois", "Metis"), "Metis count")
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

parse_number <- function(x) {
  x <- str_squish(str_replace_all(as.character(x), "\\u00A0", " "))
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

data <- raw |>
  transmute(row_id = row_number(), commune_raw = .data[[COMMUNE_COL]],
            commune_key = normalize_key(.data[[COMMUNE_COL]]), raw_count = .data[[COUNT_COL]]) |>
  mutate(value = parse_number(raw_count), blank = is.na(raw_count) | str_squish(raw_count) == "",
         nonnumeric = !blank & is.na(value), negative = !is.na(value) & value < 0,
         noninteger = !is.na(value) & value >= 0 & abs(value - round(value)) > 1e-8,
         valid = !blank & !nonnumeric & !negative & !noninteger) |>
  left_join(zones, by = "commune_key")

kw_h <- function(ranks, groups) {
  n <- length(ranks)
  sizes <- as.numeric(table(groups))
  sums <- as.numeric(rowsum(ranks, groups, reorder = FALSE))
  h <- 12 / (n * (n + 1)) * sum((sums^2) / sizes) - 3 * (n + 1)
  ties <- table(ranks)
  correction <- 1 - sum(ties^3 - ties) / (n^3 - n)
  if (!is.finite(correction) || correction <= 0) return(NA_real_)
  h / correction
}

run_mc <- function(zone_var, label, seed_offset = 0L) {
  d <- data |> filter(valid, !is.na(.data[[zone_var]]), .data[[zone_var]] != "")
  d$zone <- droplevels(factor(d[[zone_var]]))
  n <- nrow(d); k <- nlevels(d$zone)
  if (n < 2L || k < 2L || length(unique(d$value)) < 2L) {
    reason <- if (n < 2L) "Fewer than two valid observations." else if (k < 2L) "Only one zone represented." else "All valid counts are identical."
    sm <- data.frame(Comparison=label, N=n, Groups=k, Observed_KW_H=NA_real_, Monte_Carlo_B=NA_integer_, Monte_Carlo_p=NA_real_, Monte_Carlo_SE=NA_real_, Monte_Carlo_95CI_low=NA_real_, Monte_Carlo_95CI_high=NA_real_, Epsilon_squared=NA_real_, Alpha=ALPHA, Decision="No statistical test performed", Recommendation=reason)
    return(list(summary=sm, descriptive=data.frame(Note=reason)))
  }
  ranks <- rank(d$value, ties.method = "average")
  obs_h <- kw_h(ranks, d$zone)
  if (!is.finite(obs_h)) {
    reason <- "Kruskal-Wallis statistic is undefined because all ranks are tied."
    sm <- data.frame(Comparison=label, N=n, Groups=k, Observed_KW_H=NA_real_, Monte_Carlo_B=NA_integer_, Monte_Carlo_p=NA_real_, Monte_Carlo_SE=NA_real_, Monte_Carlo_95CI_low=NA_real_, Monte_Carlo_95CI_high=NA_real_, Epsilon_squared=NA_real_, Alpha=ALPHA, Decision="No statistical test performed", Recommendation=reason)
    return(list(summary=sm, descriptive=data.frame(Note=reason)))
  }
  set.seed(SEED + seed_offset)
  extreme <- 0L
  checkpoints <- unique(pmax(1L, round(seq(0.1, 1, by=0.1) * B)))
  for (b in seq_len(B)) {
    hp <- kw_h(sample(ranks, length(ranks), replace=FALSE), d$zone)
    if (is.finite(hp) && hp >= obs_h - 1e-12) extreme <- extreme + 1L
    if (b %in% checkpoints) message(label, ": ", b, "/", B, " permutations completed")
  }
  p <- (extreme + 1) / (B + 1)
  se <- sqrt(p * (1 - p) / (B + 1))
  eps <- max(0, (obs_h - k + 1) / (n - k))
  sm <- data.frame(Comparison=label, N=n, Groups=k, Observed_KW_H=obs_h, Monte_Carlo_B=B, Monte_Carlo_p=p, Monte_Carlo_SE=se, Monte_Carlo_95CI_low=max(0,p-1.96*se), Monte_Carlo_95CI_high=min(1,p+1.96*se), Epsilon_squared=eps, Alpha=ALPHA, Decision=ifelse(p<ALPHA,"Statistically significant difference","No statistically significant difference"), Recommendation="Report the Monte Carlo permutation p-value based on the tie-corrected Kruskal-Wallis H statistic.")
  desc <- d |> group_by(zone) |> summarise(N=n(), Mean=mean(value), SD=ifelse(n()>1,sd(value),NA_real_), Median=median(value), Q1=quantile(value,.25), Q3=quantile(value,.75), Minimum=min(value), Maximum=max(value), Zero_count=sum(value==0), Zero_percent=100*mean(value==0), .groups="drop")
  list(summary=sm, descriptive=desc)
}

veg <- run_mc("vegetation_zone", "Metis herd size 12 months ago x vegetation zone", 0L)
phyto <- run_mc("phytogeo_zone", "Metis herd size 12 months ago x phytogeographic zone", 100L)
summary <- bind_rows(veg$summary, phyto$summary)
quality <- data.frame(Metric=c("Total records", "Valid nonnegative integer counts", "Zero counts", "Blank responses", "Nonnumeric responses", "Negative counts", "Noninteger counts", "Unmatched communes"), Value=c(nrow(data), sum(data$valid), sum(data$valid & data$value==0), sum(data$blank), sum(data$nonnumeric), sum(data$negative,na.rm=TRUE), sum(data$noninteger,na.rm=TRUE), sum(is.na(data$vegetation_zone))))
notes <- data.frame(Parameter=c("Variable type","Primary test","Permutations","Zero values","Null hypothesis","Effect size"), Assessment=c("Quantitative discrete count variable.","Monte Carlo permutation test using the tie-corrected Kruskal-Wallis H statistic.",paste0(B," finite permutations; progress displayed every 10%."),"Zero is retained as a valid count.","The count distribution is identical across zones.","Epsilon-squared."))
write_xlsx(list(Summary=summary, Veg_descriptive=veg$descriptive, Phyto_descriptive=phyto$descriptive, Data_quality=quality, Method_notes=notes), OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, winslash="/", mustWork=FALSE))
