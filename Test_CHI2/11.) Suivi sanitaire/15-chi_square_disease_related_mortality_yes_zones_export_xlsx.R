# Disease-related animal mortality, Yes modality, vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: chi_square_results_disease_related_mortality_yes_zones.xlsx
packages <- c("readxl","dplyr","tidyr","stringr","stringi","writexl")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly=TRUE)]
if(length(missing)) stop("Install packages first: install.packages(c(", paste(sprintf('"%s"',missing),collapse=", "), "))")
suppressPackageStartupMessages({library(readxl);library(dplyr);library(tidyr);library(stringr);library(stringi);library(writexl)})
DATA_FILE <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "chi_square_results_disease_related_mortality_yes_zones.xlsx"
ALPHA <- 0.05
B <- 100000L
SEED <- 20260914L
CHILD_COL <- "11.) Suivi sanitaire/Y-a-t-il des cas de mortalités liées aux maladies /Oui"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
normalize_header <- function(x){x<-str_replace_all(as.character(x),"\\u00A0"," ");x<-stri_trans_general(x,"Latin-ASCII")|>str_to_lower()|>str_squish();str_replace_all(x,"[^a-z0-9]","")}
normalize_key <- function(x){x<-normalize_header(x);recode(x,"toribossito"="tori","dassazoume"="dassa","dassazounme"="dassa",.default=x)}
resolve_column <- function(expected,headers,terms,label){
  if(expected %in% headers) return(expected)
  nh <- normalize_header(headers); ne <- normalize_header(expected); hit <- which(nh==ne)
  if(length(hit)==1L){message(label," matched after normalization to: ",headers[hit]);return(headers[hit])}
  tt <- normalize_header(terms)
  hit <- which(vapply(nh,function(h) all(vapply(tt,function(t) str_detect(h,fixed(t)),logical(1))),logical(1)))
  if(length(hit)==1L){message(label," matched flexibly to: ",headers[hit]);return(headers[hit])}
  cand <- headers[str_detect(nh,"contexteenvironnemental|influence.*croisement|deparasitezvousvosanimauxoui")]
  stop(label," column could not be identified uniquely. Candidate headers: ",paste(cand,collapse=" | "))
}
raw <- read_excel(DATA_FILE,1,col_types="text",.name_repair="minimal")
zraw <- read_excel(ZONES_FILE,1,col_types="text",.name_repair="unique")
CHILD_COL <- resolve_column(CHILD_COL,names(raw),c("Suivi sanitaire","cas de mortalités","liées aux maladies","Oui"),"Disease-related mortality Yes selected")
COMMUNE_COL <- resolve_column(COMMUNE_COL,names(raw),c("CARACTERISTIQUES","UNITE","ELEVAGE","Commune"),"Commune")
zones <- zraw |>
  transmute(vegetation_zone=str_squish(str_replace_all(`Vegetation zones`,"\\u00A0"," ")),phytogeo_zone=str_squish(str_replace_all(`Phytogeographic zones`,"\\u00A0"," ")),district=str_squish(str_replace_all(District,"\\u00A0"," "))) |>
  fill(vegetation_zone,phytogeo_zone) |>
  filter(!is.na(district),district!="",!str_detect(str_to_lower(vegetation_zone),"^total")) |>
  mutate(commune_key=normalize_key(district)) |>
  select(commune_key,vegetation_zone,phytogeo_zone) |>
  distinct(commune_key,.keep_all=TRUE)
data <- raw |>
  transmute(row_id=row_number(),child_code=str_squish(.data[[CHILD_COL]]),commune_raw=str_squish(.data[[COMMUNE_COL]]),commune_key=normalize_key(.data[[COMMUNE_COL]])) |>
  left_join(zones,by="commune_key") |>
  mutate(eligible=TRUE,invalid_child=!is.na(child_code)&child_code!=""&!child_code%in%c("0","1"),disease_related_mortality_yes=factor(ifelse(child_code%in%c("0","1"),child_code,NA_character_),levels=c("0","1"),labels=c("No","Yes")))
run_test <- function(zone_var,label){
  d <- data |> filter(eligible,!is.na(disease_related_mortality_yes),!is.na(.data[[zone_var]]),.data[[zone_var]]!="") |> droplevels()
  obs <- table(d$disease_related_mortality_yes,d[[zone_var]]); n<-sum(obs); nr<-nrow(obs); nc<-ncol(obs); dfchi<-(nr-1)*(nc-1)
  if(n==0L||nr<2L||nc<2L){
    reason<-if(n==0L)"No eligible complete valid observations." else if(nr<2L)"Only one response category observed." else "Only one zone category observed."
    sm<-data.frame(Comparison=label,N=n,Rows=nr,Columns=nc,Degrees_of_freedom=ifelse(dfchi>0,dfchi,NA),Pearson_chi_square=NA,Asymptotic_p_value=NA,Minimum_expected=NA,Percent_expected_below_5=NA,Selected_test="Not testable",Selected_p_value=NA,Monte_Carlo_B=NA,Alpha=ALPHA,Decision="No statistical test performed",Cramers_V=NA,Recommendation=reason,stringsAsFactors=FALSE)
    return(list(summary=sm,observed=obs,expected=NULL,percent=NULL,stdres=NULL))
  }
  pearson<-suppressWarnings(chisq.test(obs,correct=FALSE)); exp<-pearson$expected; pct5<-100*sum(exp<5)/length(exp); valid<-all(exp>=1)&pct5<=20
  set.seed(SEED+ifelse(zone_var=="phytogeo_zone",100L,0L)); mc<-suppressWarnings(chisq.test(obs,simulate.p.value=TRUE,B=B))
  selected_p<-if(valid) pearson$p.value else mc$p.value; selected_test<-if(valid) "Pearson chi-square" else "Monte Carlo chi-square"
  v<-sqrt(as.numeric(pearson$statistic)/(n*min(nr-1,nc-1)))
  sm<-data.frame(Comparison=label,N=n,Rows=nr,Columns=nc,Degrees_of_freedom=dfchi,Pearson_chi_square=as.numeric(pearson$statistic),Asymptotic_p_value=pearson$p.value,Minimum_expected=min(exp),Percent_expected_below_5=pct5,Selected_test=selected_test,Selected_p_value=selected_p,Monte_Carlo_B=B,Alpha=ALPHA,Decision=ifelse(selected_p<ALPHA,"Reject H0: association detected","Do not reject H0: no association detected"),Cramers_V=v,Recommendation=ifelse(valid,"Pearson assumptions acceptable.","Sparse expected counts: Monte Carlo p-value selected."),stringsAsFactors=FALSE)
  list(summary=sm,observed=obs,expected=exp,percent=prop.table(obs,2)*100,stdres=pearson$stdres)
}
matrix_df <- function(x){
  if(is.null(x)||length(x)==0L||is.null(dim(x))) return(data.frame(Note="Not available",stringsAsFactors=FALSE))
  m<-as.matrix(x); rn<-rownames(m); if(is.null(rn)) rn<-as.character(seq_len(nrow(m))); cn<-colnames(m); if(is.null(cn)) cn<-paste0("Column_",seq_len(ncol(m)))
  out<-data.frame(Response=rn,stringsAsFactors=FALSE,check.names=FALSE)
  for(j in seq_len(ncol(m))) out[[cn[j]]]<-unname(m[,j])
  out
}
safe_round <- function(x,digits){if(is.null(x)||length(x)==0L)return(NULL);round(x,digits)}
veg<-run_test("vegetation_zone","Disease-related mortality Yes x vegetation zone")
phyto<-run_test("phytogeo_zone","Disease-related mortality Yes x phytogeographic zone")
quality<-data.frame(Metric=c("Total records","Valid eligible child responses","No (0)","Yes (1)","Blank child responses","Invalid child codes","Unmatched communes"),Value=c(nrow(data),sum(data$eligible&!is.na(data$disease_related_mortality_yes),na.rm=TRUE),sum(data$eligible&data$child_code=="0",na.rm=TRUE),sum(data$eligible&data$child_code=="1",na.rm=TRUE),sum(is.na(data$child_code)|data$child_code==""),sum(data$invalid_child),sum(is.na(data$vegetation_zone))))
notes<-data.frame(Parameter=c("Variable type","Analysis population","Coding","Null hypothesis","Expected-count rule","Monte Carlo","Effect size"),Assessment=c("Standalone binary multiple-response qualitative modality.","All records with a valid 0/1 response and matched zone are eligible.","0=option not selected; 1=disease-related mortality Yes selected selected.","Selection of disease-related mortality Yes selected and geographical zone are independent.","No expected count below 1 and no more than 20% below 5.","100,000 simulations, selected when Pearson assumptions fail.","Cramer's V."),stringsAsFactors=FALSE)
sheets<-list(Summary=bind_rows(veg$summary,phyto$summary),Veg_observed=matrix_df(veg$observed),Veg_expected=matrix_df(safe_round(veg$expected,4)),Veg_percent=matrix_df(safe_round(veg$percent,2)),Veg_std_residuals=matrix_df(safe_round(veg$stdres,4)),Phyto_observed=matrix_df(phyto$observed),Phyto_expected=matrix_df(safe_round(phyto$expected,4)),Phyto_percent=matrix_df(safe_round(phyto$percent,2)),Phyto_std_residuals=matrix_df(safe_round(phyto$stdres,4)),Data_quality=quality,Method_notes=notes)
writexl::write_xlsx(sheets,path=OUTPUT_FILE)
message("Created: ",OUTPUT_FILE)
