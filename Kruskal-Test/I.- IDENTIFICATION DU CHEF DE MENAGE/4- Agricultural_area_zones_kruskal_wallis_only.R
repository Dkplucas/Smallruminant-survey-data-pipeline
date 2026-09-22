# Agricultural area vs zones: Kruskal-Wallis only
# Population logic is unchanged:
# main activity = Agriculture OR secondary activity = Agriculture,
# AND cultivated area is numeric and greater than zero.

packages <- c("readxl", "dplyr", "stringr", "stringi", "writexl")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) stop("Install: ", paste(missing_packages, collapse = ", "))
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(stringi); library(writexl)
})

DATA_FILE <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "agricultural_area_zones_kruskal_wallis_results.xlsx"
ALPHA <- 0.05

AREA_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Si pratique de l'agriculture, quelle est la superficie de terre agricole mise en valeur pour les cultures dans votre champ ? kanti ou ha"
MAIN_ACTIVITY_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelle est votre principale activité? : 1=Agriculture, 2=Artisanat, 3=Autre, 4=Commerce, 5=Elevage"
SECONDARY_AGRICULTURE_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont vos activites secondaires ?/Agriculture: 0=Non, 1=Oui"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

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
parse_number <- function(x) suppressWarnings(as.numeric(str_replace_all(str_squish(as.character(x)), ",", ".")))

if (!file.exists(DATA_FILE)) stop("File not found: ", DATA_FILE,
  ". Use the repaired workbook because the original data7.xlsx has malformed worksheet dimensions.")
if (!file.exists(ZONES_FILE)) stop("File not found: ", ZONES_FILE)

survey <- read_excel(DATA_FILE, sheet=1, col_types="text", .name_repair="minimal")
required <- c(AREA_COL, MAIN_ACTIVITY_COL, SECONDARY_AGRICULTURE_COL, COMMUNE_COL)
if (!all(required %in% names(survey))) stop("Missing required survey column(s): ", paste(setdiff(required,names(survey)),collapse=" | "))
if (nrow(survey) != 211L) stop("Expected 211 survey records; imported ", nrow(survey), ".")

# Same district-to-zone assignment used in the corrected age analysis.
zone_lookup <- data.frame(
  matched_district=c("Toffo","Zogbodomey","Dogbo","Athiémé","Kpomasse","Tori",
    "Kétou","Bohicon","Agbangninzoun","Djidja","Dassa","Glazoué","N'dali","Tchaourou","Savè"),
  vegetation_zone=c(rep("Guineo-Congolaise",9),rep("Guineo-Soudanienne",6)),
  phytogeographic_zone=c(rep("Vallée de l'Ouémé (VOZ)",4),rep("Plateau",5),
    rep("Zou",2),rep("Borgou-Sud",4)), stringsAsFactors=FALSE
) |>
  mutate(commune_key=normalize_place(matched_district)) |>
  select(commune_key,matched_district,vegetation_zone,phytogeographic_zone)

joined <- survey |>
  transmute(
    survey_row=row_number()+1L,
    commune_original=.data[[COMMUNE_COL]],
    commune_key=normalize_place(.data[[COMMUNE_COL]]),
    main_activity=parse_number(.data[[MAIN_ACTIVITY_COL]]),
    secondary_agriculture=parse_number(.data[[SECONDARY_AGRICULTURE_COL]]),
    area_raw=.data[[AREA_COL]],
    agricultural_area=parse_number(.data[[AREA_COL]])
  ) |>
  left_join(zone_lookup,by="commune_key") |>
  mutate(
    agriculture_practitioner=main_activity==1 | secondary_agriculture==1,
    included=agriculture_practitioner & !is.na(agricultural_area) & agricultural_area>0
  )

if (any(is.na(joined$matched_district))) stop("Unmatched district records remain: ",sum(is.na(joined$matched_district)))
analysis <- joined |> filter(included)
if (nrow(analysis) != 131L) stop("Population changed: expected 131 included farmers; obtained ",nrow(analysis),".")

vegetation_counts <- analysis |> count(vegetation_zone,name="N")
phytogeo_counts <- analysis |> count(phytogeographic_zone,name="N")
expected_vegetation <- c("Guineo-Congolaise"=60,"Guineo-Soudanienne"=71)
expected_phytogeo <- c("Borgou-Sud"=52,"Plateau"=21,"Vallée de l'Ouémé (VOZ)"=39,"Zou"=19)
observed_veg <- setNames(as.numeric(vegetation_counts$N),vegetation_counts$vegetation_zone)
observed_phy <- setNames(as.numeric(phytogeo_counts$N),phytogeo_counts$phytogeographic_zone)
if (!isTRUE(all.equal(unname(observed_veg[names(expected_vegetation)]),unname(expected_vegetation)))) stop("Vegetation group sizes changed.")
if (!isTRUE(all.equal(unname(observed_phy[names(expected_phytogeo)]),unname(expected_phytogeo)))) stop("Phytogeographic group sizes changed.")

describe_area <- function(zone_var) analysis |>
  group_by(Group=.data[[zone_var]]) |>
  summarise(N=n(),Mean=mean(agricultural_area),Median=median(agricultural_area),
    SD=sd(agricultural_area),Q1=quantile(agricultural_area,.25,names=FALSE),
    Q3=quantile(agricultural_area,.75,names=FALSE),IQR=IQR(agricultural_area),
    Minimum=min(agricultural_area),Maximum=max(agricultural_area),.groups="drop")

run_kw <- function(zone_var,label) {
  d <- analysis |> filter(!is.na(.data[[zone_var]]),.data[[zone_var]]!="")
  groups <- droplevels(factor(d[[zone_var]]))
  test <- kruskal.test(d$agricultural_area~groups)
  data.frame(Comparison=label,N=nrow(d),Groups=nlevels(groups),
    H_statistic=unname(test$statistic),Degrees_of_freedom=unname(test$parameter),
    P_value=test$p.value,Alpha=ALPHA,
    Decision=ifelse(test$p.value<ALPHA,"Reject H0","Do not reject H0"),
    stringsAsFactors=FALSE)
}

summary_results <- bind_rows(
  run_kw("vegetation_zone","Agricultural area vs vegetation zone"),
  run_kw("phytogeographic_zone","Agricultural area vs phytogeographic zone")
)

population_audit <- data.frame(
  Metric=c("All survey records","Agriculture practitioners with positive area",
    "Excluded records","Unmatched districts"),
  Value=c(nrow(joined),nrow(analysis),nrow(joined)-nrow(analysis),sum(is.na(joined$matched_district)))
)
method_notes <- data.frame(
  Item=c("Population definition","Primary test","Alpha"),
  Detail=c("Main activity Agriculture OR secondary activity Agriculture, with cultivated area > 0.",
    "Kruskal-Wallis test only for both geographical comparisons.",as.character(ALPHA))
)

write_xlsx(list(
  Summary=summary_results,
  Vegetation_descriptive=describe_area("vegetation_zone"),
  Phytogeo_descriptive=describe_area("phytogeographic_zone"),
  Vegetation_counts=vegetation_counts,
  Phytogeo_counts=phytogeo_counts,
  Population_audit=population_audit,
  Method_notes=method_notes
),OUTPUT_FILE)
message("Results exported to: ",normalizePath(OUTPUT_FILE,winslash="/",mustWork=FALSE))
