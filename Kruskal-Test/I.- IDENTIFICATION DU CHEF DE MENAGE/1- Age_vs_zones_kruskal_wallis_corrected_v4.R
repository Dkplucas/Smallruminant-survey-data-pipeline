# Corrected age-vs-zone analysis, version 4
# REQUIRED inputs: data7_dimension_fixed.xlsx and zones.xlsx
# Output: age_vs_zones_kruskal_wallis_corrected_results.xlsx

packages <- c("readxl", "dplyr", "stringr", "stringi", "writexl")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) stop("Install: ", paste(missing_packages, collapse = ", "))
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(stringi); library(writexl)
})

DATA_FILE <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "age_vs_zones_kruskal_wallis_corrected_results.xlsx"
AGE_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Age :"
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
  recode(x, "dassazoume"="dassa", "dassazounme"="dassa",
         "toribossito"="tori", .default=x)
}

if (!file.exists(DATA_FILE)) stop("File not found: ", DATA_FILE,
  ". Download the repaired workbook and place it beside this script.")
if (!file.exists(ZONES_FILE)) stop("File not found: ", ZONES_FILE)

survey <- read_excel(DATA_FILE, sheet=1, col_types="text", .name_repair="minimal")
if (!all(c(AGE_COL, COMMUNE_COL) %in% names(survey))) stop("Required columns are missing.")
if (nrow(survey) != 211L) stop("Imported ", nrow(survey), " rows instead of 211.")
message("Survey rows imported: ", nrow(survey))

zone_lookup <- data.frame(
  matched_district=c("Toffo","Zogbodomey","Dogbo","Athiémé","Kpomasse","Tori",
    "Kétou","Bohicon","Agbangninzoun","Djidja","Dassa","Glazoué","N'dali","Tchaourou","Savè"),
  vegetation_zone=c(rep("Guineo-Congolaise",9),rep("Guineo-Soudanienne",6)),
  phytogeographic_zone=c(rep("Vallee de l'Oueme (VOZ)",4),rep("Plateau",5),
    rep("Zou",2),rep("Borgou-Sud",4)), stringsAsFactors=FALSE
) |>
  mutate(commune_key=normalize_place(matched_district)) |>
  select(commune_key,matched_district,vegetation_zone,phytogeographic_zone)

joined <- survey |>
  transmute(survey_row=row_number()+1L,
    commune_original=.data[[COMMUNE_COL]],
    commune_key=normalize_place(.data[[COMMUNE_COL]]),
    age_raw=.data[[AGE_COL]],
    age=suppressWarnings(as.numeric(str_replace_all(str_squish(.data[[AGE_COL]]),",",".")))) |>
  left_join(zone_lookup,by="commune_key")

# Show the real commune distribution before testing.
message("Nonmissing commune values: ",sum(!is.na(joined$commune_original)))
unmatched <- joined |> filter(is.na(matched_district))
if (nrow(unmatched)>0L) stop("Unmatched records: ",nrow(unmatched),". Keys: ",
  paste(unique(unmatched$commune_key),collapse=", "))

vegetation_counts <- joined |> count(vegetation_zone,name="N")
phytogeo_counts <- joined |> count(phytogeographic_zone,name="N")
expected_vegetation <- c("Guineo-Congolaise"=93,"Guineo-Soudanienne"=118)
expected_phytogeo <- c("Vallee de l'Oueme (VOZ)"=58,"Plateau"=35,"Zou"=40,"Borgou-Sud"=78)
observed_vegetation <- setNames(as.numeric(vegetation_counts$N),vegetation_counts$vegetation_zone)
observed_phytogeo <- setNames(as.numeric(phytogeo_counts$N),phytogeo_counts$phytogeographic_zone)
if (!isTRUE(all.equal(unname(observed_vegetation[names(expected_vegetation)]),unname(expected_vegetation))))
  stop("Vegetation totals incorrect: ",paste(vegetation_counts$vegetation_zone,vegetation_counts$N,sep="=",collapse=", "))
if (!isTRUE(all.equal(unname(observed_phytogeo[names(expected_phytogeo)]),unname(expected_phytogeo))))
  stop("Phytogeographic totals incorrect: ",paste(phytogeo_counts$phytogeographic_zone,phytogeo_counts$N,sep="=",collapse=", "))

analysis <- joined |> filter(!is.na(age))
describe_age <- function(zone_var) analysis |>
  group_by(Group=.data[[zone_var]]) |>
  summarise(N=n(),Mean=mean(age),Median=median(age),SD=sd(age),
    Q1=quantile(age,.25,names=FALSE),Q3=quantile(age,.75,names=FALSE),
    IQR=IQR(age),Minimum=min(age),Maximum=max(age),.groups="drop")
run_kw <- function(zone_var,label) {
  d <- analysis |> filter(!is.na(.data[[zone_var]]),.data[[zone_var]]!="")
  groups <- droplevels(factor(d[[zone_var]])); test <- kruskal.test(d$age~groups)
  data.frame(Comparison=label,N=nrow(d),Groups=nlevels(groups),
    H_statistic=unname(test$statistic),Degrees_of_freedom=unname(test$parameter),
    P_value=test$p.value,Alpha=ALPHA,
    Decision=ifelse(test$p.value<ALPHA,"Reject H0","Do not reject H0"))
}
summary_results <- bind_rows(
  run_kw("vegetation_zone","Age vs vegetation zone"),
  run_kw("phytogeographic_zone","Age vs phytogeographic zone"))
matching_audit <- joined |> count(commune_original,commune_key,matched_district,
  vegetation_zone,phytogeographic_zone,name="Farmer_count") |> arrange(commune_key)
age_quality <- data.frame(Metric=c("Survey records","Matched records","Unmatched records",
  "Valid numeric ages","Missing/nonnumeric ages","Minimum age","Maximum age"),
  Value=c(nrow(joined),sum(!is.na(joined$matched_district)),nrow(unmatched),
  sum(!is.na(joined$age)),sum(is.na(joined$age)),min(joined$age,na.rm=TRUE),max(joined$age,na.rm=TRUE)))
write_xlsx(list(Summary=summary_results,
  Vegetation_descriptive=describe_age("vegetation_zone"),
  Phytogeo_descriptive=describe_age("phytogeographic_zone"),
  Vegetation_counts=vegetation_counts,Phytogeo_counts=phytogeo_counts,
  Matching_audit=matching_audit,Age_quality=age_quality),OUTPUT_FILE)
message("Results exported to: ",normalizePath(OUTPUT_FILE,winslash="/",mustWork=FALSE))
