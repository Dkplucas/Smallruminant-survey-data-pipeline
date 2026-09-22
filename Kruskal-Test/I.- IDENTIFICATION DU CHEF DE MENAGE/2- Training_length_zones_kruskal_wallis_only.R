# Training length vs zones: Kruskal-Wallis only
# Population logic unchanged: training response = Yes (1) and length > 0.

packages <- c("readxl", "dplyr", "stringr", "stringi", "writexl")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing_packages)>0L) stop("Install: ",paste(missing_packages,collapse=", "))
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(stringi); library(writexl)
})
DATA_FILE <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "training_length_zones_kruskal_wallis_results.xlsx"
ALPHA <- 0.05
TRAINING_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Avez vous reçu oui suivi une Formation en élevage ?: 0=Non, 1=Oui"
LENGTH_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Si Oui, depuis quand ?"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

normalize_place <- function(x) {
  x <- as.character(x); x <- str_replace_all(x,fixed("\u00A0")," ")
  x <- str_replace_all(x,"[’‘`´]","'"); x <- stri_trans_general(x,"Latin-ASCII")
  x <- str_to_lower(str_squish(x)); x <- str_replace_all(x,"['-]","")
  x <- str_replace_all(x,"[^a-z0-9]","")
  recode(x,"dassazoume"="dassa","dassazounme"="dassa","toribossito"="tori",.default=x)
}
parse_number <- function(x) suppressWarnings(as.numeric(str_replace_all(str_squish(as.character(x)),",",".")))
if (!file.exists(DATA_FILE)) stop("File not found: ",DATA_FILE)
if (!file.exists(ZONES_FILE)) stop("File not found: ",ZONES_FILE)
survey <- read_excel(DATA_FILE,sheet=1,col_types="text",.name_repair="minimal")
required <- c(TRAINING_COL,LENGTH_COL,COMMUNE_COL)
if (!all(required %in% names(survey))) stop("Missing required column(s): ",paste(setdiff(required,names(survey)),collapse=" | "))
if (nrow(survey)!=211L) stop("Expected 211 records; imported ",nrow(survey),".")

zone_lookup <- data.frame(
  matched_district=c("Toffo","Zogbodomey","Dogbo","Athiémé","Kpomasse","Tori","Kétou","Bohicon","Agbangninzoun","Djidja","Dassa","Glazoué","N'dali","Tchaourou","Savè"),
  vegetation_zone=c(rep("Guineo-Congolaise",9),rep("Guineo-Soudanienne",6)),
  phytogeographic_zone=c(rep("Vallée de l'Ouémé (VOZ)",4),rep("Plateau",5),rep("Zou",2),rep("Borgou-Sud",4)),
  stringsAsFactors=FALSE) |>
  mutate(commune_key=normalize_place(matched_district)) |>
  select(commune_key,matched_district,vegetation_zone,phytogeographic_zone)

joined <- survey |>
  transmute(survey_row=row_number()+1L,commune_original=.data[[COMMUNE_COL]],
    commune_key=normalize_place(.data[[COMMUNE_COL]]),
    training_response=parse_number(.data[[TRAINING_COL]]),
    training_length_raw=.data[[LENGTH_COL]],training_length=parse_number(.data[[LENGTH_COL]])) |>
  left_join(zone_lookup,by="commune_key") |>
  mutate(included=training_response==1 & !is.na(training_length) & training_length>0,
    no_but_positive_length=training_response==0 & !is.na(training_length) & training_length>0)
if (any(is.na(joined$matched_district))) stop("Unmatched district records: ",sum(is.na(joined$matched_district)))
analysis <- joined |> filter(included)
flagged_no <- joined |> filter(no_but_positive_length) |>
  select(survey_row,commune_original,training_response,training_length,
         vegetation_zone,phytogeographic_zone)
if (nrow(analysis)!=43L) stop("Population changed: expected 43; obtained ",nrow(analysis),".")
if (nrow(flagged_no)!=2L) stop("Expected 2 No-with-positive-length records; found ",nrow(flagged_no),".")

vegetation_counts <- analysis |> count(vegetation_zone,name="N")
phytogeo_counts <- analysis |> count(phytogeographic_zone,name="N")
expected_veg <- c("Guineo-Congolaise"=28,"Guineo-Soudanienne"=15)
expected_phy <- c("Plateau"=17,"Vallée de l'Ouémé (VOZ)"=11,"Borgou-Sud"=9,"Zou"=6)
obs_veg <- setNames(as.numeric(vegetation_counts$N),vegetation_counts$vegetation_zone)
obs_phy <- setNames(as.numeric(phytogeo_counts$N),phytogeo_counts$phytogeographic_zone)
if (!isTRUE(all.equal(unname(obs_veg[names(expected_veg)]),unname(expected_veg)))) stop("Vegetation sizes changed.")
if (!isTRUE(all.equal(unname(obs_phy[names(expected_phy)]),unname(expected_phy)))) stop("Phytogeographic sizes changed.")

describe_length <- function(zone_var) analysis |>
  group_by(Group=.data[[zone_var]]) |>
  summarise(N=n(),Mean=mean(training_length),Median=median(training_length),SD=sd(training_length),
    Q1=quantile(training_length,.25,names=FALSE),Q3=quantile(training_length,.75,names=FALSE),
    IQR=IQR(training_length),Minimum=min(training_length),Maximum=max(training_length),.groups="drop")
run_kw <- function(zone_var,label) {
  d <- analysis |> filter(!is.na(.data[[zone_var]]),.data[[zone_var]]!="")
  groups <- droplevels(factor(d[[zone_var]])); test <- kruskal.test(d$training_length~groups)
  data.frame(Comparison=label,N=nrow(d),Groups=nlevels(groups),H_statistic=unname(test$statistic),
    Degrees_of_freedom=unname(test$parameter),P_value=test$p.value,Alpha=ALPHA,
    Decision=ifelse(test$p.value<ALPHA,"Reject H0","Do not reject H0"),stringsAsFactors=FALSE)
}
summary_results <- bind_rows(
  run_kw("vegetation_zone","Training length vs vegetation zone"),
  run_kw("phytogeographic_zone","Training length vs phytogeographic zone"))
population_audit <- data.frame(Metric=c("All survey records","Included: Yes and positive length",
  "Excluded records","No response with positive length, excluded and flagged","Unmatched districts"),
  Value=c(nrow(joined),nrow(analysis),nrow(joined)-nrow(analysis),nrow(flagged_no),sum(is.na(joined$matched_district))))
method_notes <- data.frame(Item=c("Population definition","Primary test","Alpha"),
  Detail=c("Training response Yes (1) with positive training length.",
    "Kruskal-Wallis only for both geographical comparisons.",as.character(ALPHA)))
write_xlsx(list(Summary=summary_results,
  Vegetation_descriptive=describe_length("vegetation_zone"),
  Phytogeo_descriptive=describe_length("phytogeographic_zone"),
  Vegetation_counts=vegetation_counts,Phytogeo_counts=phytogeo_counts,
  Excluded_No_with_length=flagged_no,Population_audit=population_audit,
  Method_notes=method_notes),OUTPUT_FILE)
message("Results exported to: ",normalizePath(OUTPUT_FILE,winslash="/",mustWork=FALSE))
