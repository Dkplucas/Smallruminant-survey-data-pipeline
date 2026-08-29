# Education level vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: chi_square_results_education_zones.xlsx

packages <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "openxlsx")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install packages first: install.packages(c(", paste(sprintf('"%s"', missing), collapse=", "), "))")
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(stringr)
  library(stringi); library(openxlsx)
})

DATA_FILE <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "chi_square_results_education_zones.xlsx"
ALPHA <- 0.05
EDUCATION_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Niveau d'instruction: 1=Aucun, 2=Primaire, 3=Secondaire, 4=Superieure"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
set.seed(20260827)

normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |> str_to_lower() |> str_squish()
  x <- str_replace_all(x, "[’'`-]", "") |> str_replace_all("[^a-z0-9]", "")
  recode(x, "toribossito"="tori", "dassazoume"="dassa",
         "dassazounme"="dassa", .default=x)
}

data <- read_excel(DATA_FILE, sheet=1, col_types="text", .name_repair="minimal")
zones_raw <- read_excel(ZONES_FILE, sheet=1, col_types="text", .name_repair="unique")
stopifnot(EDUCATION_COL %in% names(data), COMMUNE_COL %in% names(data))

zones <- zones_raw |>
  transmute(
    vegetation_zone=str_squish(str_replace_all(`Vegetation zones`, "\\u00A0", " ")),
    phytogeo_zone=str_squish(str_replace_all(`Phytogeographic zones`, "\\u00A0", " ")),
    district=str_squish(str_replace_all(District, "\\u00A0", " ")),
    reported_n=suppressWarnings(as.numeric(str_replace_all(`Number of farmers`, "\\u00A0", "")))
  ) |>
  fill(vegetation_zone, phytogeo_zone) |>
  filter(!is.na(district), district!="", !str_detect(str_to_lower(vegetation_zone), "^total")) |>
  mutate(commune_key=normalize_key(district)) |>
  select(commune_key, district, vegetation_zone, phytogeo_zone, reported_n)

analysis_data <- data |>
  transmute(
    education_code=str_squish(.data[[EDUCATION_COL]]),
    commune_original=str_squish(.data[[COMMUNE_COL]]),
    commune_key=normalize_key(.data[[COMMUNE_COL]])
  ) |>
  mutate(education=factor(education_code, levels=c("1","2","3","4"),
                          labels=c("None","Primary","Secondary","Higher")),
         invalid_education=is.na(education) & !is.na(education_code) & education_code!="") |>
  left_join(zones, by="commune_key")

run_test <- function(df, zone_var, label) {
  d <- df |> filter(!is.na(education), !is.na(.data[[zone_var]])) |> droplevels()
  observed <- table(d$education, d[[zone_var]])
  pearson <- suppressWarnings(chisq.test(observed, correct=FALSE))
  expected <- pearson$expected
  n <- sum(observed); cells <- length(expected)
  df_chi <- (nrow(observed)-1)*(ncol(observed)-1)
  below5 <- sum(expected < 5); below1 <- sum(expected < 1)
  pct_below5 <- 100*below5/cells
  expected_ok <- below1==0 && pct_below5 <= 20
  margins_ok <- !any(rowSums(observed)==0) && !any(colSums(observed)==0)
  chi_ok <- expected_ok && margins_ok
  if (chi_ok) {
    selected <- pearson; method <- "Pearson chi-square test"
  } else if (nrow(observed)==2 && ncol(observed)==2) {
    selected <- fisher.test(observed); method <- "Fisher's exact test"
  } else {
    selected <- fisher.test(observed, simulate.p.value=TRUE, B=100000)
    method <- "Fisher-Freeman-Halton exact test, Monte Carlo B=100000"
  }
  v <- sqrt(unname(pearson$statistic)/(n*min(nrow(observed)-1,ncol(observed)-1)))
  list(label=label, observed=observed, expected=expected, n=n, cells=cells,
       df=df_chi, pearson=pearson, min_expected=min(expected), below5=below5,
       pct_below5=pct_below5, below1=below1, expected_ok=expected_ok,
       margins_ok=margins_ok, method=method, selected=selected, cramer_v=v,
       decision=ifelse(selected$p.value<ALPHA,"Statistically significant association",
                       "No statistically significant association"))
}

veg <- run_test(analysis_data, "vegetation_zone", "Education level vs vegetation zone")
phyto <- run_test(analysis_data, "phytogeo_zone", "Education level vs phytogeographic zone")

summary_row <- function(x) data.frame(
  Comparison=x$label, Sample_size_N=x$n, Rows=nrow(x$observed), Columns=ncol(x$observed),
  Cell_count=x$cells, Degrees_of_freedom=x$df,
  Pearson_chi_square=unname(x$pearson$statistic), Pearson_p_value=x$pearson$p.value,
  Minimum_expected_frequency=x$min_expected, Cells_expected_below_5=x$below5,
  Percent_cells_expected_below_5=x$pct_below5, Cells_expected_below_1=x$below1,
  Expected_frequency_condition=ifelse(x$expected_ok,"PASS","FAIL"),
  Nonzero_margins=ifelse(x$margins_ok,"PASS","FAIL"), Selected_test=x$method,
  Selected_test_p_value=x$selected$p.value, Alpha=ALPHA, Decision=x$decision,
  Cramers_V=x$cramer_v, stringsAsFactors=FALSE)
summary <- bind_rows(summary_row(veg), summary_row(phyto))

matrix_df <- function(x) {
  z <- as.data.frame.matrix(x)
  z <- data.frame(
    `Education level` = rownames(z),
    z,
    check.names = FALSE,
    row.names = NULL
  )
  z
}
quality <- data.frame(
  Item=c("Valid education records","Missing education records","Invalid education codes","Unmatched zone records"),
  Value=c(sum(!is.na(analysis_data$education)),
          sum(is.na(analysis_data$education_code)|analysis_data$education_code==""),
          sum(analysis_data$invalid_education), sum(is.na(analysis_data$vegetation_zone))))
notes <- data.frame(
  Prerequisite=c("Categorical variables","Independent observations","Mutually exclusive cells",
                 "Expected frequency rule","Representative sampling","Complex survey design","Interpretation"),
  Assessment=c("PASS","Must be justified from study design","PASS if each farmer occurs once and belongs to one category",
               "PASS when no expected count is below 1 and no more than 20% are below 5",
               "Must be justified for population inference",
               "Use survey::svychisq() if weights, strata, or clusters apply",
               "Association only, not causation"))

wb <- createWorkbook()
hdr <- createStyle(fontColour="white", fgFill="#1F4E78", textDecoration="bold", halign="center", wrapText=TRUE)
for (nm in c("Summary","Veg_observed","Veg_expected","Phyto_observed","Phyto_expected","Data_quality","Validity_notes")) addWorksheet(wb,nm,gridLines=FALSE)
writeData(wb,"Summary",summary,headerStyle=hdr,withFilter=TRUE)
writeData(wb,"Veg_observed",matrix_df(veg$observed),headerStyle=hdr)
writeData(wb,"Veg_expected",matrix_df(round(veg$expected,4)),headerStyle=hdr)
writeData(wb,"Phyto_observed",matrix_df(phyto$observed),headerStyle=hdr)
writeData(wb,"Phyto_expected",matrix_df(round(phyto$expected,4)),headerStyle=hdr)
writeData(wb,"Data_quality",quality,headerStyle=hdr)
writeData(wb,"Validity_notes",notes,headerStyle=hdr)
for (nm in names(wb)) {
  freezePane(wb,nm,firstRow=TRUE); setColWidths(wb,nm,1:50,"auto")
}
setColWidths(wb,"Summary",1:ncol(summary),18)
setColWidths(wb,"Summary",c(1,15,18),c(38,48,42))
setColWidths(wb,"Validity_notes",1:2,c(32,80))
addStyle(wb,"Summary",createStyle(numFmt="0.0000"),rows=2:3,
         cols=which(names(summary)%in%c("Pearson_chi_square","Pearson_p_value","Selected_test_p_value","Cramers_V")),gridExpand=TRUE)
saveWorkbook(wb,OUTPUT_FILE,overwrite=TRUE)

cat("Results exported to", OUTPUT_FILE, "\n")
print(summary)
