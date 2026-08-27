# Sweet potato cultivation vs vegetation and phytogeographic zones
# Important: if all responses are identical, chi-square cannot be calculated.

packages <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "openxlsx")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install packages first: install.packages(c(", paste(sprintf('"%s"', missing), collapse=", "), "))")
suppressPackageStartupMessages({library(readxl); library(dplyr); library(tidyr); library(stringr); library(stringi); library(openxlsx)})

DATA_FILE <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "chi_square_results_sweet_potato_cultivation_zones.xlsx"
ALPHA <- 0.05
B <- 100000L
SEED <- 20260828L
SWEET_POTATO_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont les principales cultures pratiquées par votre ménage ? /8=patate douce/0=Non, 1=Oui"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |> str_to_lower() |> str_squish()
  x <- str_replace_all(x, "[’'`-]", "") |> str_replace_all("[^a-z0-9]", "")
  recode(x, "toribossito"="tori", "dassazoume"="dassa", "dassazounme"="dassa", .default=x)
}

raw <- read_excel(DATA_FILE, 1, col_types="text", .name_repair="minimal")
zraw <- read_excel(ZONES_FILE, 1, col_types="text", .name_repair="unique")
if (!SWEET_POTATO_COL %in% names(raw)) stop("Sweet-potato column not found exactly in data7.xlsx")
if (!COMMUNE_COL %in% names(raw)) stop("Commune column not found exactly in data7.xlsx")

zones <- zraw |>
  transmute(vegetation_zone=str_squish(str_replace_all(`Vegetation zones`, "\\u00A0", " ")),
            phytogeo_zone=str_squish(str_replace_all(`Phytogeographic zones`, "\\u00A0", " ")),
            district=str_squish(str_replace_all(District, "\\u00A0", " "))) |>
  fill(vegetation_zone, phytogeo_zone) |>
  filter(!is.na(district), district!="", !str_detect(str_to_lower(vegetation_zone), "^total")) |>
  mutate(commune_key=normalize_key(district)) |>
  select(commune_key, district, vegetation_zone, phytogeo_zone)

data <- raw |>
  transmute(sweet_potato_code=str_squish(.data[[SWEET_POTATO_COL]]),
            commune_original=str_squish(.data[[COMMUNE_COL]]),
            commune_key=normalize_key(.data[[COMMUNE_COL]])) |>
  mutate(sweet_potato=factor(sweet_potato_code, levels=c("0", "1"), labels=c("No", "Yes")),
         invalid_code=is.na(sweet_potato) & !is.na(sweet_potato_code) & sweet_potato_code!="") |>
  left_join(zones, by="commune_key")

run_test <- function(df, zone_var, label) {
  d <- df |> filter(!is.na(sweet_potato), !is.na(.data[[zone_var]]))
  observed_full <- table(d$sweet_potato, factor(d[[zone_var]]))
  active_rows <- rowSums(observed_full) > 0
  active_cols <- colSums(observed_full) > 0
  observed <- observed_full[active_rows, active_cols, drop=FALSE]

  testable <- nrow(observed) >= 2 && ncol(observed) >= 2
  if (!testable) {
    summary <- data.frame(
      Comparison=label, N=sum(observed_full), Rows_with_observations=sum(active_rows),
      Columns_with_observations=sum(active_cols), Cell_count=length(observed_full),
      Degrees_of_freedom=0, Pearson_chi_square=NA_real_, Asymptotic_p_value=NA_real_,
      Minimum_expected=NA_real_, Cells_expected_below_5=NA_integer_,
      Percent_expected_below_5=NA_real_, Cells_expected_below_1=NA_integer_,
      Asymptotic_chi_square_valid="NO", Selected_test="No test possible",
      Selected_p_value=NA_real_, Monte_Carlo_B=NA_integer_, Monte_Carlo_p_value=NA_real_,
      Alpha=ALPHA, Decision="Not testable",
      Cramers_V=NA_real_,
      Recommendation="All respondents have the same sweet-potato response. There is no response variation, so neither Pearson chi-square, Fisher's exact test, nor Monte Carlo chi-square can test association.",
      stringsAsFactors=FALSE)
    expected <- matrix(NA_real_, nrow=nrow(observed_full), ncol=ncol(observed_full), dimnames=dimnames(observed_full))
    stdres <- expected
    percent <- prop.table(observed_full, margin=2)*100
    return(list(summary=summary, observed=observed_full, expected=expected, stdres=stdres, percent=percent))
  }

  pearson <- suppressWarnings(chisq.test(observed, correct=FALSE))
  expected <- pearson$expected; n <- sum(observed); cells <- length(expected)
  dfchi <- (nrow(observed)-1)*(ncol(observed)-1); n5 <- sum(expected<5); n1 <- sum(expected<1); pct5 <- 100*n5/cells
  valid <- n1==0 & pct5<=20
  set.seed(SEED + ifelse(zone_var=="phytogeo_zone", 100, 0))
  mc <- chisq.test(observed, simulate.p.value=TRUE, B=B)
  if (valid) {selected <- pearson; method <- "Pearson chi-square (asymptotic)"} else {selected <- mc; method <- paste0("Pearson chi-square with Monte Carlo p-value (B=", B, ")")}
  v <- sqrt(unname(pearson$statistic)/(n*min(nrow(observed)-1,ncol(observed)-1)))
  summary <- data.frame(Comparison=label, N=n, Rows_with_observations=nrow(observed), Columns_with_observations=ncol(observed),
    Cell_count=cells, Degrees_of_freedom=dfchi, Pearson_chi_square=unname(pearson$statistic), Asymptotic_p_value=pearson$p.value,
    Minimum_expected=min(expected), Cells_expected_below_5=n5, Percent_expected_below_5=pct5, Cells_expected_below_1=n1,
    Asymptotic_chi_square_valid=ifelse(valid,"YES","NO"), Selected_test=method, Selected_p_value=selected$p.value,
    Monte_Carlo_B=B, Monte_Carlo_p_value=mc$p.value, Alpha=ALPHA,
    Decision=ifelse(selected$p.value<ALPHA,"Statistically significant association","No statistically significant association"),
    Cramers_V=v, Recommendation=ifelse(valid,"Report ordinary Pearson chi-square.","Report Monte Carlo p-value."), stringsAsFactors=FALSE)
  list(summary=summary, observed=observed, expected=expected, stdres=pearson$stdres, percent=prop.table(observed,margin=2)*100)
}

veg <- run_test(data, "vegetation_zone", "Sweet potato cultivation vs vegetation zone")
phyto <- run_test(data, "phytogeo_zone", "Sweet potato cultivation vs phytogeographic zone")
summary <- bind_rows(veg$summary, phyto$summary)
matrix_df <- function(x) {z <- as.data.frame.matrix(x); data.frame(`Sweet potato`=rownames(z), z, check.names=FALSE, row.names=NULL)}
quality <- data.frame(Parameter=c("Rows read","Valid sweet-potato records","No","Yes","Missing records","Invalid codes","Unmatched zone records"),
 Value=c(nrow(data),sum(!is.na(data$sweet_potato)),sum(data$sweet_potato=="No",na.rm=TRUE),sum(data$sweet_potato=="Yes",na.rm=TRUE),sum(is.na(data$sweet_potato_code)|data$sweet_potato_code==""),sum(data$invalid_code),sum(is.na(data$vegetation_zone))))
notes <- data.frame(Parameter=c("Variable type","Observed variation","Why chi-square is unavailable","Monte Carlo","Interpretation","Future data"),
 Assessment=c("Sweet-potato cultivation is a binary qualitative variable.","All 211 records are coded No and zero records are coded Yes.","A chi-square association test requires at least two observed response categories. Here the Yes row has a zero total, expected counts of zero, and degrees of freedom equal to zero.","Monte Carlo and Fisher tests cannot solve the absence of variation. They are alternatives for sparse tables, not for a variable with only one observed category.","The correct result is not testable, not non-significant.","The test can be performed if future data include at least one Yes response, subject to expected-count checks."))

wb <- createWorkbook(); hdr <- createStyle(fontColour="white",fgFill="#1F4E78",textDecoration="bold",halign="center",wrapText=TRUE)
write_sheet <- function(n,x){addWorksheet(wb,n,gridLines=FALSE);writeData(wb,n,x,headerStyle=hdr,withFilter=TRUE);freezePane(wb,n,firstRow=TRUE);setColWidths(wb,n,1:ncol(x),"auto")}
write_sheet("Summary",summary);write_sheet("Veg_observed",matrix_df(veg$observed));write_sheet("Veg_expected",matrix_df(veg$expected));write_sheet("Veg_percent",matrix_df(round(veg$percent,2)));write_sheet("Phyto_observed",matrix_df(phyto$observed));write_sheet("Phyto_expected",matrix_df(phyto$expected));write_sheet("Phyto_percent",matrix_df(round(phyto$percent,2)));write_sheet("Data_quality",quality);write_sheet("Validity_notes",notes)
setColWidths(wb,"Summary",1:ncol(summary),18);setColWidths(wb,"Summary",c(1,14,19,21),c(48,30,24,110));setColWidths(wb,"Validity_notes",1:2,c(36,115))
saveWorkbook(wb,OUTPUT_FILE,overwrite=TRUE)
cat("Results exported to",OUTPUT_FILE,"\n");print(summary)
