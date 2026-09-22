# Total herd size vs zones: standard asymptotic Kruskal-Wallis analysis
# Inputs: data7_dimension_fixed.xlsx and zones.xlsx
# Output: herd_size_zones_standard_kruskal_wallis_results.xlsx

packages <- c("readxl", "dplyr", "stringr", "stringi", "writexl")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) stop("Install: ", paste(missing_packages, collapse = ", "))
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(stringi); library(writexl)
})

DATA_FILE <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "herd_size_zones_standard_kruskal_wallis_results.xlsx"
HERD_COL <- "12.) Effectif et composition du cheptel/Effectif total du troupeau"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
ALPHA <- 0.05

normalize_place <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, fixed("\u00A0"), " ")
  x <- str_replace_all(x, "[’‘`´]", "'")
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(str_squish(x))
  x <- str_replace_all(x, "['-]", "")
  x <- str_replace_all(x, "[^a-z0-9]", "")
  recode(x, "dassazoume" = "dassa", "dassazounme" = "dassa",
         "toribossito" = "tori", .default = x)
}
parse_number <- function(x) suppressWarnings(as.numeric(str_replace_all(str_squish(as.character(x)), ",", ".")))

if (!file.exists(DATA_FILE)) stop("File not found: ", DATA_FILE)
if (!file.exists(ZONES_FILE)) stop("File not found: ", ZONES_FILE)
survey <- read_excel(DATA_FILE, sheet = 1, col_types = "text", .name_repair = "minimal")
if (!all(c(HERD_COL, COMMUNE_COL) %in% names(survey))) stop("Required columns missing.")
if (nrow(survey) != 211L) stop("Expected 211 records; imported ", nrow(survey), ".")

zone_lookup <- data.frame(
  matched_district = c("Toffo","Zogbodomey","Dogbo","Athiémé","Kpomasse","Tori",
    "Kétou","Bohicon","Agbangninzoun","Djidja","Dassa","Glazoué","N'dali","Tchaourou","Savè"),
  vegetation_zone = c(rep("Guineo-Congolaise", 9), rep("Guineo-Soudanienne", 6)),
  phytogeographic_zone = c(rep("Vallée de l'Ouémé (VOZ)", 4), rep("Plateau", 5),
    rep("Zou", 2), rep("Borgou-Sud", 4)), stringsAsFactors = FALSE
) |>
  mutate(commune_key = normalize_place(matched_district)) |>
  select(commune_key, matched_district, vegetation_zone, phytogeographic_zone)

joined <- survey |>
  transmute(survey_row = row_number() + 1L,
    commune_original = .data[[COMMUNE_COL]],
    commune_key = normalize_place(.data[[COMMUNE_COL]]),
    herd_raw = .data[[HERD_COL]], herd_size = parse_number(.data[[HERD_COL]])) |>
  left_join(zone_lookup, by = "commune_key")
if (any(is.na(joined$matched_district))) stop("Unmatched district records: ", sum(is.na(joined$matched_district)))
if (sum(!is.na(joined$herd_size)) != 211L) stop("Expected 211 valid herd-size values.")
analysis <- joined |> filter(!is.na(herd_size))

vegetation_counts <- analysis |> count(vegetation_zone, name = "N")
phytogeo_counts <- analysis |> count(phytogeographic_zone, name = "N")
expected_veg <- c("Guineo-Congolaise" = 93, "Guineo-Soudanienne" = 118)
expected_phy <- c("Borgou-Sud" = 78, "Plateau" = 35, "Vallée de l'Ouémé (VOZ)" = 58, "Zou" = 40)
obs_veg <- setNames(as.numeric(vegetation_counts$N), vegetation_counts$vegetation_zone)
obs_phy <- setNames(as.numeric(phytogeo_counts$N), phytogeo_counts$phytogeographic_zone)
if (!isTRUE(all.equal(unname(obs_veg[names(expected_veg)]), unname(expected_veg)))) stop("Vegetation sizes changed.")
if (!isTRUE(all.equal(unname(obs_phy[names(expected_phy)]), unname(expected_phy)))) stop("Phytogeographic sizes changed.")

describe_herd <- function(zone_var) analysis |>
  group_by(Group = .data[[zone_var]]) |>
  summarise(N = n(), Mean = mean(herd_size), Median = median(herd_size), SD = sd(herd_size),
    Q1 = quantile(herd_size, .25, names = FALSE), Q3 = quantile(herd_size, .75, names = FALSE),
    IQR = IQR(herd_size), Minimum = min(herd_size), Maximum = max(herd_size), .groups = "drop")

run_kw <- function(zone_var, label) {
  d <- analysis |> filter(!is.na(.data[[zone_var]]), .data[[zone_var]] != "")
  groups <- droplevels(factor(d[[zone_var]]))
  test <- kruskal.test(d$herd_size ~ groups)
  n <- nrow(d); k <- nlevels(groups); h <- unname(test$statistic)
  epsilon_squared <- max(0, (h - k + 1) / (n - k))
  data.frame(Comparison = label, N = n, Groups = k, H_statistic = h,
    Degrees_of_freedom = unname(test$parameter), P_value = test$p.value,
    Epsilon_squared = epsilon_squared, Alpha = ALPHA,
    Decision = ifelse(test$p.value < ALPHA, "Reject H0", "Do not reject H0"),
    stringsAsFactors = FALSE)
}

summary_results <- bind_rows(
  run_kw("vegetation_zone", "Total herd size vs vegetation zone"),
  run_kw("phytogeographic_zone", "Total herd size vs phytogeographic zone"))
population_audit <- data.frame(Metric = c("All survey records", "Valid herd-size records",
  "Excluded records", "Unmatched districts"), Value = c(nrow(joined), nrow(analysis),
  nrow(joined) - nrow(analysis), sum(is.na(joined$matched_district))))
method_notes <- data.frame(Item = c("Primary test", "P-value", "Effect size", "Alpha"),
  Detail = c("Standard Kruskal-Wallis test only for both comparisons.",
    "Asymptotic chi-square p-value returned by R kruskal.test.",
    "Epsilon-squared = max(0, (H-k+1)/(n-k)).", as.character(ALPHA)))
write_xlsx(list(Summary = summary_results,
  Vegetation_descriptive = describe_herd("vegetation_zone"),
  Phytogeo_descriptive = describe_herd("phytogeographic_zone"),
  Vegetation_counts = vegetation_counts, Phytogeo_counts = phytogeo_counts,
  Population_audit = population_audit, Method_notes = method_notes), OUTPUT_FILE)
message("Results exported to: ", normalizePath(OUTPUT_FILE, winslash = "/", mustWork = FALSE))
