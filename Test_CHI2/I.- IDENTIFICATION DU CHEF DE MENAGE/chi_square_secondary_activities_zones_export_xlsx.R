# Multiple-response secondary activities vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: chi_square_results_secondary_activities_zones.xlsx

packages <- c("readxl","dplyr","tidyr","stringr","stringi","openxlsx")
missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) stop("Install packages first: install.packages(c(",paste(sprintf('"%s"',missing),collapse=", "),"))")
suppressPackageStartupMessages({library(readxl);library(dplyr);library(tidyr);library(stringr);library(stringi);library(openxlsx)})

DATA_FILE <- "data7.xlsx"; ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "chi_square_results_secondary_activities_zones.xlsx"
ALPHA <- 0.05; B <- 100000L; SEED <- 20260827L
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
ACTIVITY_COLUMNS <- c(
  Livestock="I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont vos activites secondaires ?/Elevage: 0=Non, 1=Oui",
  Agriculture="I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont vos activites secondaires ?/Agriculture: 0=Non, 1=Oui",
  Commerce="I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont vos activites secondaires ?/Commerce: 0=Non, 1=Oui",
  Crafts="I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont vos activites secondaires ?/Artisanat: 0=Non, 1=Oui",
  Other="I.- IDENTIFICATION DU CHEF DE MENAGE /Quelles sont vos activites secondaires ?/Autre: 0=Non, 1=Oui")
set.seed(SEED)

normalize_key <- function(x){
  x<-str_replace_all(as.character(x),"\\u00A0"," ");x<-stri_trans_general(x,"Latin-ASCII")|>str_to_lower()|>str_squish()
  x<-str_replace_all(x,"[’'`-]","")|>str_replace_all("[^a-z0-9]","")
  recode(x,"toribossito"="tori","dassazoume"="dassa","dassazounme"="dassa",.default=x)
}
raw<-read_excel(DATA_FILE,1,col_types="text",.name_repair="minimal")
zraw<-read_excel(ZONES_FILE,1,col_types="text",.name_repair="unique")
missing_cols<-setdiff(c(COMMUNE_COL,unname(ACTIVITY_COLUMNS)),names(raw));if(length(missing_cols)) stop("Columns not found: ",paste(missing_cols,collapse=" | "))
zones<-zraw|>transmute(vegetation_zone=str_squish(str_replace_all(`Vegetation zones`,"\\u00A0"," ")),
                       phytogeo_zone=str_squish(str_replace_all(`Phytogeographic zones`,"\\u00A0"," ")),district=str_squish(str_replace_all(District,"\\u00A0"," ")))|>
  fill(vegetation_zone,phytogeo_zone)|>filter(!is.na(district),district!="",!str_detect(str_to_lower(vegetation_zone),"^total"))|>
  mutate(commune_key=normalize_key(district))|>select(commune_key,district,vegetation_zone,phytogeo_zone)
base<-raw|>mutate(row_id=row_number(),commune_original=str_squish(.data[[COMMUNE_COL]]),commune_key=normalize_key(.data[[COMMUNE_COL]]))|>
  left_join(zones,by="commune_key")
long<-bind_rows(lapply(names(ACTIVITY_COLUMNS),function(a){
  data.frame(row_id=base$row_id,Activity=a,Response_code=str_squish(base[[ACTIVITY_COLUMNS[[a]]]]),
             vegetation_zone=base$vegetation_zone,phytogeo_zone=base$phytogeo_zone,stringsAsFactors=FALSE)
}))|>mutate(Response=factor(Response_code,levels=c("0","1"),labels=c("No","Yes")),
            invalid=!is.na(Response_code)&Response_code!=""&is.na(Response))

run_one<-function(dat,activity,zone_var,zone_label){
  d<-dat|>filter(Activity==activity,!is.na(Response),!is.na(.data[[zone_var]]))|>droplevels()
  obs<-table(d$Response,d[[zone_var]]);pearson<-suppressWarnings(chisq.test(obs,correct=FALSE));exp<-pearson$expected
  n<-sum(obs);cells<-length(exp);df<-(nrow(obs)-1)*(ncol(obs)-1);n5<-sum(exp<5);n1<-sum(exp<1);pct5<-100*n5/cells
  valid<-n1==0&&pct5<=20&&!any(rowSums(obs)==0)&&!any(colSums(obs)==0)
  if(valid){sel<-pearson;method<-"Pearson chi-square (asymptotic)";mcB<-NA_integer_;mcse<-mcl<-mch<-NA_real_
  }else{set.seed(SEED+match(activity,names(ACTIVITY_COLUMNS))+ifelse(zone_var=="phytogeo_zone",100,0));sel<-chisq.test(obs,simulate.p.value=TRUE,B=B)
  method<-paste0("Pearson chi-square with Monte Carlo p-value (B=",B,")");mcB<-B;mcse<-sqrt(sel$p.value*(1-sel$p.value)/(B+1));mcl<-max(0,sel$p.value-1.96*mcse);mch<-min(1,sel$p.value+1.96*mcse)}
  v<-sqrt(unname(pearson$statistic)/(n*min(nrow(obs)-1,ncol(obs)-1)))
  sm<-data.frame(Zone_comparison=zone_label,Activity=activity,N=n,Rows=nrow(obs),Columns=ncol(obs),Cell_count=cells,
                 Degrees_of_freedom=df,Pearson_chi_square=unname(pearson$statistic),Asymptotic_p_value=pearson$p.value,
                 Minimum_expected=min(exp),Cells_expected_below_5=n5,Percent_expected_below_5=pct5,Cells_expected_below_1=n1,
                 Asymptotic_chi_square_valid=ifelse(valid,"YES","NO"),Selected_test=method,Selected_p_value=sel$p.value,
                 Monte_Carlo_B=mcB,Monte_Carlo_SE=mcse,Monte_Carlo_95CI_low=mcl,Monte_Carlo_95CI_high=mch,Alpha=ALPHA,
                 Cramers_V=v,stringsAsFactors=FALSE)
  matlong <- function(x, type) {
    x <- as.matrix(x)
    out <- expand.grid(
      Response = rownames(x),
      Zone = colnames(x),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    out$Value <- as.vector(x)
    out <- out |>
      mutate(
        Zone_comparison = zone_label,
        Activity = activity,
        Table = type,
        .before = 1
      )
    out
  }
  list(summary=sm,tables=bind_rows(matlong(obs,"Observed"),matlong(round(exp,6),"Expected"),matlong(round(pearson$stdres,6),"Standardized residual")))
}
results <- list()
k <- 1
zone_variables <- c(
  vegetation_zone = "Vegetation zone",
  phytogeo_zone = "Phytogeographic zone"
)

for (zone_var in names(zone_variables)) {
  zone_label <- unname(zone_variables[[zone_var]])
  for (activity in names(ACTIVITY_COLUMNS)) {
    results[[k]] <- run_one(
      dat = long,
      activity = activity,
      zone_var = zone_var,
      zone_label = zone_label
    )
    k <- k + 1
  }
}
summary<-bind_rows(lapply(results,`[[`,"summary"))|>group_by(Zone_comparison)|>
  mutate(P_adjusted_BH=p.adjust(Selected_p_value,"BH"),P_adjusted_Bonferroni=p.adjust(Selected_p_value,"bonferroni"),
         Decision_raw=ifelse(Selected_p_value<ALPHA,"Significant","Not significant"),Decision_BH=ifelse(P_adjusted_BH<ALPHA,"Significant","Not significant"))|>ungroup()
tables<-bind_rows(lapply(results,`[[`,"tables"))
quality<-long|>group_by(Activity)|>summarise(Valid=sum(!is.na(Response)),Yes=sum(Response=="Yes",na.rm=TRUE),No=sum(Response=="No",na.rm=TRUE),
                                             Missing=sum(is.na(Response_code)|Response_code==""),Invalid=sum(invalid),Unmatched_zone=sum(is.na(vegetation_zone)),.groups="drop")
wide_binary<-base|>transmute(row_id,across(all_of(unname(ACTIVITY_COLUMNS)),~as.numeric(.x),.names="{.col}"))
selected_count<-rowSums(as.data.frame(wide_binary[-1]),na.rm=TRUE)
response_summary<-data.frame(Number_selected=sort(unique(selected_count)),Respondents=as.integer(table(factor(selected_count,levels=sort(unique(selected_count))))))
notes<-data.frame(Topic=c("Number of indicators","Multiple-response structure","Correct chi-square approach","Expected-count rule","Monte Carlo selection","Multiple testing","Global interpretation","Independence","Survey design"),
                  Assessment=c("Five indicators were supplied: Livestock, Agriculture, Commerce, Crafts, and Other.",
                               "The indicators are not mutually exclusive; one respondent can answer Yes to several activities.",
                               "Run a separate 2 x zone chi-square test for each binary Yes/No indicator. Do not combine Yes counts into one ordinary contingency table.",
                               "Asymptotic Pearson is accepted when no expected cell is below 1 and no more than 20% are below 5.",
                               "If that rule fails, the script reports a Monte Carlo chi-square p-value with 100,000 simulations.",
                               "Because five tests are performed per zone system, raw, Benjamini-Hochberg, and Bonferroni p-values are exported.",
                               "Each test concerns one activity. A single overall test of the full multiple-response pattern requires a multivariate or repeated-measures method.",
                               "Respondents must be independent, although activity indicators within the same respondent may be correlated.",
                               "If weights, strata, or village clusters apply, use survey-adjusted or multilevel methods."))

wb<-createWorkbook();hdr<-createStyle(fontColour="white",fgFill="#1F4E78",textDecoration="bold",halign="center",wrapText=TRUE)
write_sheet<-function(n,x){addWorksheet(wb,n,gridLines=FALSE);writeData(wb,n,x,headerStyle=hdr,withFilter=TRUE);freezePane(wb,n,firstRow=TRUE);setColWidths(wb,n,1:ncol(x),"auto")}
write_sheet("Summary",summary);write_sheet("All_tables",tables);write_sheet("Data_quality",quality);write_sheet("Selections_per_person",response_summary);write_sheet("Method_notes",notes)
setColWidths(wb,"Summary",1:ncol(summary),18);setColWidths(wb,"Summary",c(1,2,14,27,29),c(24,18,48,22,22));setColWidths(wb,"Method_notes",1:2,c(32,110))
numcols<-which(vapply(summary,is.numeric,logical(1)));addStyle(wb,"Summary",createStyle(numFmt="0.0000"),rows=2:(nrow(summary)+1),cols=numcols,gridExpand=TRUE)
saveWorkbook(wb,OUTPUT_FILE,overwrite=TRUE);cat("Results exported to",OUTPUT_FILE,"\n");print(summary)
