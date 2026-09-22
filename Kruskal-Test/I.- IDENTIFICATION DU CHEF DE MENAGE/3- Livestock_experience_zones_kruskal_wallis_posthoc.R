# Livestock experience vs zones: Kruskal-Wallis + conditional post-hoc tests

packages <- c("readxl", "dplyr", "stringr", "stringi", "writexl")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing_packages)>0L) stop("Install: ",paste(missing_packages,collapse=", "))
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(stringi); library(writexl)
})
DATA_FILE <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "livestock_experience_zones_kruskal_wallis_posthoc_results.xlsx"
ALPHA <- 0.05
EXPERIENCE_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelle est votre expérience en élevage?"
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
if (!all(c(EXPERIENCE_COL,COMMUNE_COL)%in%names(survey))) stop("Required columns missing.")
if (nrow(survey)!=211L) stop("Expected 211 records; imported ",nrow(survey),".")
zone_lookup <- data.frame(
  matched_district=c("Toffo","Zogbodomey","Dogbo","Athiémé","Kpomasse","Tori","Kétou","Bohicon","Agbangninzoun","Djidja","Dassa","Glazoué","N'dali","Tchaourou","Savè"),
  vegetation_zone=c(rep("Guineo-Congolaise",9),rep("Guineo-Soudanienne",6)),
  phytogeographic_zone=c(rep("Vallée de l'Ouémé (VOZ)",4),rep("Plateau",5),rep("Zou",2),rep("Borgou-Sud",4)),stringsAsFactors=FALSE) |>
  mutate(commune_key=normalize_place(matched_district)) |>
  select(commune_key,matched_district,vegetation_zone,phytogeographic_zone)
joined <- survey |>
  transmute(survey_row=row_number()+1L,commune_original=.data[[COMMUNE_COL]],
    commune_key=normalize_place(.data[[COMMUNE_COL]]),
    experience_raw=.data[[EXPERIENCE_COL]],experience=parse_number(.data[[EXPERIENCE_COL]])) |>
  left_join(zone_lookup,by="commune_key")
if (any(is.na(joined$matched_district))) stop("Unmatched district records: ",sum(is.na(joined$matched_district)))
if (sum(!is.na(joined$experience))!=211L) stop("Expected 211 valid experience values.")
analysis <- joined |> filter(!is.na(experience))
vegetation_counts <- analysis |> count(vegetation_zone,name="N")
phytogeo_counts <- analysis |> count(phytogeographic_zone,name="N")
expected_veg <- c("Guineo-Congolaise"=93,"Guineo-Soudanienne"=118)
expected_phy <- c("Borgou-Sud"=78,"Plateau"=35,"Vallée de l'Ouémé (VOZ)"=58,"Zou"=40)
obs_veg <- setNames(as.numeric(vegetation_counts$N),vegetation_counts$vegetation_zone)
obs_phy <- setNames(as.numeric(phytogeo_counts$N),phytogeo_counts$phytogeographic_zone)
if (!isTRUE(all.equal(unname(obs_veg[names(expected_veg)]),unname(expected_veg)))) stop("Vegetation sizes changed.")
if (!isTRUE(all.equal(unname(obs_phy[names(expected_phy)]),unname(expected_phy)))) stop("Phytogeographic sizes changed.")
describe_experience <- function(zone_var) analysis |>
  group_by(Group=.data[[zone_var]]) |>
  summarise(N=n(),Mean=mean(experience),Median=median(experience),SD=sd(experience),
    Q1=quantile(experience,.25,names=FALSE),Q3=quantile(experience,.75,names=FALSE),
    IQR=IQR(experience),Minimum=min(experience),Maximum=max(experience),.groups="drop")
run_kw <- function(zone_var,label) {
  d <- analysis |> filter(!is.na(.data[[zone_var]]),.data[[zone_var]]!="")
  groups <- droplevels(factor(d[[zone_var]])); test <- kruskal.test(d$experience~groups)
  data.frame(Comparison=label,N=nrow(d),Groups=nlevels(groups),H_statistic=unname(test$statistic),
    Degrees_of_freedom=unname(test$parameter),P_value=test$p.value,Alpha=ALPHA,
    Decision=ifelse(test$p.value<ALPHA,"Reject H0","Do not reject H0"),stringsAsFactors=FALSE)
}
summary_results <- bind_rows(
  run_kw("vegetation_zone","Livestock experience vs vegetation zone"),
  run_kw("phytogeographic_zone","Livestock experience vs phytogeographic zone"))
phy_kw_p <- summary_results$P_value[summary_results$Comparison=="Livestock experience vs phytogeographic zone"]
if (length(phy_kw_p)==1L && phy_kw_p<ALPHA) {
  lev <- c("Borgou-Sud","Plateau","Vallée de l'Ouémé (VOZ)","Zou")
  pairs <- combn(lev,2,simplify=FALSE)
  posthoc <- bind_rows(lapply(pairs,function(pair) {
    x <- analysis$experience[analysis$phytogeographic_zone==pair[1]]
    y <- analysis$experience[analysis$phytogeographic_zone==pair[2]]
    test <- wilcox.test(x,y,alternative="two.sided",exact=FALSE,correct=TRUE)
    data.frame(Group_1=pair[1],Group_2=pair[2],N_1=length(x),N_2=length(y),
      W_statistic=unname(test$statistic),Raw_p_value=test$p.value,stringsAsFactors=FALSE)
  })) |>
    mutate(BH_adjusted_p=p.adjust(Raw_p_value,method="BH"),
      Bonferroni_adjusted_p=p.adjust(Raw_p_value,method="bonferroni"),
      Significant_BH=BH_adjusted_p<ALPHA,
      Significant_Bonferroni=Bonferroni_adjusted_p<ALPHA)
} else {
  posthoc <- data.frame(Note="Post-hoc tests skipped because the overall phytogeographic Kruskal-Wallis test was not significant.")
}
population_audit <- data.frame(Metric=c("All survey records","Valid experience records","Excluded records","Unmatched districts"),Value=c(nrow(joined),nrow(analysis),nrow(joined)-nrow(analysis),sum(is.na(joined$matched_district))))
method_notes <- data.frame(Item=c("Overall test","Post-hoc rule","Adjustments","Alpha"),Detail=c(
  "Kruskal-Wallis only for both zone comparisons.",
  "Pairwise Wilcoxon rank-sum tests only when phytogeographic Kruskal-Wallis p<0.05; no vegetation post-hoc because there are two groups.",
  "Benjamini-Hochberg and Bonferroni adjusted p-values.",as.character(ALPHA)))
write_xlsx(list(Summary=summary_results,
  Vegetation_descriptive=describe_experience("vegetation_zone"),
  Phytogeo_descriptive=describe_experience("phytogeographic_zone"),
  Phytogeo_pairwise_posthoc=posthoc,Vegetation_counts=vegetation_counts,
  Phytogeo_counts=phytogeo_counts,Population_audit=population_audit,
  Method_notes=method_notes),OUTPUT_FILE)
message("Results exported to: ",normalizePath(OUTPUT_FILE,winslash="/",mustWork=FALSE))
