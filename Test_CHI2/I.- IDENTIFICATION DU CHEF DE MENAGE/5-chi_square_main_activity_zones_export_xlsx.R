# Main activity vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: chi_square_results_main_activity_zones.xlsx

packages <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "openxlsx")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install packages first: install.packages(c(", paste(sprintf('"%s"', missing), collapse=", "), "))")
suppressPackageStartupMessages({
 library(readxl); library(dplyr); library(tidyr); library(stringr); library(stringi); library(openxlsx)
})
DATA_FILE <- "data7.xlsx"; ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "chi_square_results_main_activity_zones.xlsx"
ALPHA <- 0.05; B <- 100000L; SEED <- 20260827L
MAIN_ACTIVITY_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelle est votre principale activité? : 1=Agriculture, 2=Artisanat, 3=Autre, 4=Commerce, 5=Elevage"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
set.seed(SEED)

normalize_key <- function(x) {
 x <- str_replace_all(as.character(x), "\\u00A0", " ")
 x <- stri_trans_general(x, "Latin-ASCII") |> str_to_lower() |> str_squish()
 x <- str_replace_all(x, "[’'`-]", "") |> str_replace_all("[^a-z0-9]", "")
 recode(x, "toribossito"="tori", "dassazoume"="dassa", "dassazounme"="dassa", .default=x)
}

data_raw <- read_excel(DATA_FILE, 1, col_types="text", .name_repair="minimal")
zones_raw <- read_excel(ZONES_FILE, 1, col_types="text", .name_repair="unique")
if (!MAIN_ACTIVITY_COL %in% names(data_raw)) stop("Main-activity column not found exactly in data7.xlsx")
if (!COMMUNE_COL %in% names(data_raw)) stop("Commune column not found exactly in data7.xlsx")

zones <- zones_raw |>
 transmute(vegetation_zone=str_squish(str_replace_all(`Vegetation zones`,"\\u00A0"," ")),
           phytogeo_zone=str_squish(str_replace_all(`Phytogeographic zones`,"\\u00A0"," ")),
           district=str_squish(str_replace_all(District,"\\u00A0"," ")),
           reported_n=suppressWarnings(as.numeric(str_replace_all(`Number of farmers`,"\\u00A0","")))) |>
 fill(vegetation_zone,phytogeo_zone) |>
 filter(!is.na(district),district!="",!str_detect(str_to_lower(vegetation_zone),"^total")) |>
 mutate(commune_key=normalize_key(district)) |>
 select(commune_key,district,vegetation_zone,phytogeo_zone,reported_n)

data <- data_raw |>
 transmute(main_activity_code=str_squish(.data[[MAIN_ACTIVITY_COL]]),
           commune_original=str_squish(.data[[COMMUNE_COL]]),
           commune_key=normalize_key(.data[[COMMUNE_COL]])) |>
 mutate(main_activity=factor(main_activity_code,levels=c("1","2","3","4","5"),
          labels=c("Agriculture","Crafts","Other","Commerce","Livestock")),
        invalid_main_activity=is.na(main_activity)&!is.na(main_activity_code)&main_activity_code!="") |>
 left_join(zones,by="commune_key")

run_test <- function(df,zone_var,label) {
 d <- df |> filter(!is.na(main_activity),!is.na(.data[[zone_var]])) |> droplevels()
 observed <- table(d$main_activity,d[[zone_var]])
 pearson <- suppressWarnings(chisq.test(observed,correct=FALSE)); expected <- pearson$expected
 n <- sum(observed); cells <- length(expected); df_chi <- (nrow(observed)-1)*(ncol(observed)-1)
 below5 <- sum(expected<5); below1 <- sum(expected<1); pct_below5 <- 100*below5/cells
 expected_ok <- below1==0 && pct_below5<=20
 margins_ok <- !any(rowSums(observed)==0) && !any(colSums(observed)==0)
 asymptotic_ok <- expected_ok && margins_ok
 if (asymptotic_ok) {
   selected <- pearson; method <- "Pearson chi-square test (asymptotic p-value)"
   mc_B <- NA_integer_; mc_se <- mc_low <- mc_high <- NA_real_
 } else {
   set.seed(SEED); selected <- chisq.test(observed,simulate.p.value=TRUE,B=B)
   method <- paste0("Pearson chi-square test with Monte Carlo p-value (B=",B,")")
   mc_B <- B; mc_se <- sqrt(selected$p.value*(1-selected$p.value)/(B+1))
   mc_low <- max(0,selected$p.value-1.96*mc_se); mc_high <- min(1,selected$p.value+1.96*mc_se)
 }
 v <- sqrt(unname(pearson$statistic)/(n*min(nrow(observed)-1,ncol(observed)-1)))
 summary <- data.frame(Comparison=label,Sample_size_N=n,Rows=nrow(observed),Columns=ncol(observed),
   Cell_count=cells,Degrees_of_freedom=df_chi,Pearson_chi_square=unname(pearson$statistic),
   Asymptotic_p_value=pearson$p.value,Minimum_expected_frequency=min(expected),
   Cells_expected_below_5=below5,Percent_cells_expected_below_5=pct_below5,
   Cells_expected_below_1=below1,Expected_frequency_condition=ifelse(expected_ok,"PASS","FAIL"),
   Nonzero_margins=ifelse(margins_ok,"PASS","FAIL"),Asymptotic_chi_square_valid=ifelse(asymptotic_ok,"YES","NO"),
   Selected_test=method,Selected_p_value=selected$p.value,Monte_Carlo_B=mc_B,Monte_Carlo_SE=mc_se,
   Monte_Carlo_95CI_low=mc_low,Monte_Carlo_95CI_high=mc_high,Alpha=ALPHA,
   Decision=ifelse(selected$p.value<ALPHA,"Statistically significant association","No statistically significant association"),
   Cramers_V=v,Recommendation=ifelse(asymptotic_ok,"Use and report the ordinary Pearson chi-square result.",
   "Report the Monte Carlo chi-square p-value because expected-count conditions fail."),stringsAsFactors=FALSE)
 list(summary=summary,observed=observed,expected=expected,std_resid=pearson$stdres)
}
veg <- run_test(data,"vegetation_zone","Main activity vs vegetation zone")
phyto <- run_test(data,"phytogeo_zone","Main activity vs phytogeographic zone")
summary <- bind_rows(veg$summary,phyto$summary)
matrix_df <- function(x) { z <- as.data.frame.matrix(x); data.frame(`Main activity`=rownames(z),z,check.names=FALSE,row.names=NULL) }
quality <- data.frame(Parameter=c("Rows read","Valid main-activity records","Missing main-activity records","Invalid main-activity codes","Unmatched zone records"),
 Value=c(nrow(data),sum(!is.na(data$main_activity)),sum(is.na(data$main_activity_code)|data$main_activity_code==""),sum(data$invalid_main_activity),sum(is.na(data$vegetation_zone))))
notes <- data.frame(Parameter=c("Variable type","Null hypothesis","Expected-count rule","Monte Carlo rule","Independence","Sampling design","Interpretation"),
 Assessment=c("Main activity and zone are qualitative variables; chi-square is appropriate.","Main activity and zone are independent.",
 "No expected count below 1 and no more than 20% below 5.","Monte Carlo is selected automatically if the expected-count rule fails.",
 "Each farmer must contribute to one cell only.","Use a Rao-Scott test if survey weights, strata, or clusters apply.","Association does not establish causation."))

wb <- createWorkbook(); hdr <- createStyle(fontColour="white",fgFill="#1F4E78",textDecoration="bold",halign="center",wrapText=TRUE)
write_sheet <- function(name,x) { addWorksheet(wb,name,gridLines=FALSE); writeData(wb,name,x,headerStyle=hdr,withFilter=TRUE); freezePane(wb,name,firstRow=TRUE); setColWidths(wb,name,1:ncol(x),"auto") }
write_sheet("Summary",summary); write_sheet("Veg_observed",matrix_df(veg$observed)); write_sheet("Veg_expected",matrix_df(round(veg$expected,4)))
write_sheet("Veg_std_residuals",matrix_df(round(veg$std_resid,4))); write_sheet("Phyto_observed",matrix_df(phyto$observed))
write_sheet("Phyto_expected",matrix_df(round(phyto$expected,4))); write_sheet("Phyto_std_residuals",matrix_df(round(phyto$std_resid,4)))
write_sheet("Data_quality",quality); write_sheet("Validity_notes",notes)
setColWidths(wb,"Summary",1:ncol(summary),18); setColWidths(wb,"Summary",c(1,16,23,25),c(38,55,42,85)); setColWidths(wb,"Validity_notes",1:2,c(35,95))
num_cols <- which(vapply(summary,is.numeric,logical(1))); addStyle(wb,"Summary",createStyle(numFmt="0.0000"),rows=2:3,cols=num_cols,gridExpand=TRUE)
saveWorkbook(wb,OUTPUT_FILE,overwrite=TRUE); cat("Results exported to",OUTPUT_FILE,"\n"); print(summary)
