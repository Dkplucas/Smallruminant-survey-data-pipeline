# Agro-industrial by-products (SPAI) as supplements (No/Yes) vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: chi_square_results_agroindustrial_byproducts_spai_zones.xlsx
packages<-c("readxl","dplyr","tidyr","stringr","stringi","writexl")
missing<-packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing))stop("Install packages first: install.packages(c(",paste(sprintf('"%s"',missing),collapse=", "),"))")
suppressPackageStartupMessages({library(readxl);library(dplyr);library(tidyr);library(stringr);library(stringi);library(writexl)})
DATA_FILE<-"data7.xlsx";ZONES_FILE<-"zones.xlsx";OUTPUT_FILE<-"chi_square_results_agroindustrial_byproducts_spai_supplement_zones.xlsx"
ALPHA<-0.05;B<-100000L;SEED<-20260828L
AGROINDUSTRIAL_BYPRODUCTS_SPAI_COL<-"4.) Conduite des animaux/Si oui, quels sont ces compléments alimentaires ? /3= sous-produits agro-industriels (SPAI)/0=Non, 1=Oui"
PARENT_SUPPLEMENTATION_COL<-"4.) Conduite des animaux/Après le pâturage servez-vous de compléments alimentaires aux animaux ? 0=Non, 1=Oui"
COMMUNE_COL<-"II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
normalize_key<-function(x){x<-str_replace_all(as.character(x),"\\u00A0"," ");x<-stri_trans_general(x,"Latin-ASCII")|>str_to_lower()|>str_squish();x<-str_replace_all(x,"[’'`-]","")|>str_replace_all("[^a-z0-9]","");recode(x,"toribossito"="tori","dassazoume"="dassa","dassazounme"="dassa",.default=x)}
raw<-read_excel(DATA_FILE,1,col_types="text",.name_repair="minimal");zraw<-read_excel(ZONES_FILE,1,col_types="text",.name_repair="unique")
if(!AGROINDUSTRIAL_BYPRODUCTS_SPAI_COL%in%names(raw))stop("Market-gardening-by-products column not found exactly in data7.xlsx");if(!PARENT_SUPPLEMENTATION_COL%in%names(raw))stop("Parent pasture-conduct column not found exactly in data7.xlsx");if(!COMMUNE_COL%in%names(raw))stop("Commune column not found exactly")
zones<-zraw|>transmute(vegetation_zone=str_squish(str_replace_all(`Vegetation zones`,"\\u00A0"," ")),phytogeo_zone=str_squish(str_replace_all(`Phytogeographic zones`,"\\u00A0"," ")),district=str_squish(str_replace_all(District,"\\u00A0"," ")))|>fill(vegetation_zone,phytogeo_zone)|>filter(!is.na(district),district!="",!str_detect(str_to_lower(vegetation_zone),"^total"))|>mutate(commune_key=normalize_key(district))|>select(commune_key,district,vegetation_zone,phytogeo_zone)
data<-raw|>transmute(parent_supplementation=str_squish(.data[[PARENT_SUPPLEMENTATION_COL]]),agroindustrial_byproducts_spai_code=str_squish(.data[[AGROINDUSTRIAL_BYPRODUCTS_SPAI_COL]]),commune_original=str_squish(.data[[COMMUNE_COL]]),commune_key=normalize_key(.data[[COMMUNE_COL]]))|>mutate(eligible=parent_supplementation=="1",agroindustrial_byproducts_spai=factor(agroindustrial_byproducts_spai_code,levels=c("0","1"),labels=c("No","Yes")),invalid_agroindustrial_byproducts_spai=eligible&is.na(agroindustrial_byproducts_spai)&!is.na(agroindustrial_byproducts_spai_code)&agroindustrial_byproducts_spai_code!="",ineligible_positive=parent_supplementation=="0"&agroindustrial_byproducts_spai_code=="1")|>left_join(zones,by="commune_key")
run_test<-function(df,zone_var,label){
 d<-df|>filter(eligible,!is.na(agroindustrial_byproducts_spai),!is.na(.data[[zone_var]]))|>droplevels();obs<-table(d$agroindustrial_byproducts_spai,d[[zone_var]]);pearson<-suppressWarnings(chisq.test(obs,correct=FALSE));exp<-pearson$expected;n<-sum(obs);cells<-length(exp);dfchi<-(nrow(obs)-1)*(ncol(obs)-1);n5<-sum(exp<5);n1<-sum(exp<1);pct5<-100*n5/cells;margins<-!any(rowSums(obs)==0)&!any(colSums(obs)==0);valid<-n1==0&pct5<=20&margins
 set.seed(SEED+ifelse(zone_var=="phytogeo_zone",100,0));mc<-chisq.test(obs,simulate.p.value=TRUE,B=B);mcse<-sqrt(mc$p.value*(1-mc$p.value)/(B+1));mcl<-max(0,mc$p.value-1.96*mcse);mch<-min(1,mc$p.value+1.96*mcse)
 if(valid){selected<-pearson;method<-"Pearson chi-square (asymptotic)"}else{selected<-mc;method<-paste0("Pearson chi-square with Monte Carlo p-value (B=",B,")")}
 v<-sqrt(unname(pearson$statistic)/(n*min(nrow(obs)-1,ncol(obs)-1)));pct<-prop.table(obs,margin=2)*100
 sm<-data.frame(Comparison=label,N=n,Rows=nrow(obs),Columns=ncol(obs),Cell_count=cells,Degrees_of_freedom=dfchi,Pearson_chi_square=unname(pearson$statistic),Asymptotic_p_value=pearson$p.value,Minimum_expected=min(exp),Cells_expected_below_5=n5,Percent_expected_below_5=pct5,Cells_expected_below_1=n1,Nonzero_margins=ifelse(margins,"PASS","FAIL"),Asymptotic_chi_square_valid=ifelse(valid,"YES","NO"),Selected_test=method,Selected_p_value=selected$p.value,Monte_Carlo_B=B,Monte_Carlo_p_value=mc$p.value,Monte_Carlo_SE=mcse,Monte_Carlo_95CI_low=mcl,Monte_Carlo_95CI_high=mch,Alpha=ALPHA,Decision=ifelse(selected$p.value<ALPHA,"Statistically significant association","No statistically significant association"),Cramers_V=v,Recommendation=ifelse(valid,"Report ordinary Pearson chi-square; Monte Carlo is supplied as sensitivity analysis.","Report Monte Carlo p-value because expected-count conditions fail."),stringsAsFactors=FALSE)
 list(summary=sm,observed=obs,expected=exp,stdres=pearson$stdres,percent=pct)
}
veg<-run_test(data,"vegetation_zone","Agro-industrial by-products (SPAI) as supplements vs vegetation zone");phyto<-run_test(data,"phytogeo_zone","Agro-industrial by-products (SPAI) as supplements vs phytogeographic zone");summary<-bind_rows(veg$summary,phyto$summary)
matrix_df<-function(x){
  if(is.null(x)||length(x)==0L||is.null(dim(x))||length(dim(x))!=2L||any(is.na(dim(x)))){
    return(data.frame(Message="No matrix output available",stringsAsFactors=FALSE,check.names=FALSE))
  }
  m<-as.matrix(x)
  nr<-nrow(m);nc<-ncol(m)
  if(is.null(nr)||is.null(nc)||nr<1L||nc<1L){
    return(data.frame(Message="No matrix output available",stringsAsFactors=FALSE,check.names=FALSE))
  }
  rn<-rownames(m);if(is.null(rn))rn<-as.character(seq_len(nr))
  cn<-colnames(m);if(is.null(cn))cn<-paste0("Column_",seq_len(nc))
  out<-data.frame(`Agro-industrial by-products (SPAI) as supplements`=rn,stringsAsFactors=FALSE,check.names=FALSE)
  for(j in seq_len(nc))out[[cn[j]]]<-unname(m[,j])
  out
}
quality<-data.frame(Parameter=c("Rows read","Eligible units receiving post-pasture supplements","Eligible valid market-gardening by-product records","Eligible No","Eligible Yes","Eligible missing responses","Eligible invalid codes","Ineligible positive responses","Unmatched eligible zone records"),Value=c(nrow(data),sum(data$eligible),sum(data$eligible&!is.na(data$agroindustrial_byproducts_spai)),sum(data$eligible&data$agroindustrial_byproducts_spai=="No",na.rm=TRUE),sum(data$eligible&data$agroindustrial_byproducts_spai=="Yes",na.rm=TRUE),sum(data$eligible&(is.na(data$agroindustrial_byproducts_spai_code)|data$agroindustrial_byproducts_spai_code=="")),sum(data$invalid_agroindustrial_byproducts_spai),sum(data$ineligible_positive),sum(data$eligible&is.na(data$vegetation_zone))))
notes<-data.frame(Parameter=c("Variable type","Code 0 interpretation","Null hypothesis","Expected-count rule","Monte Carlo","Multiple-response caution","Independence","Survey design"),Assessment=c("Agro-industrial by-products (SPAI) as supplements and zone are qualitative variables, so chi-square is appropriate.","The source label defines 0=No and 1=Yes.","Agro-industrial by-products (SPAI) as supplements status and zone are independent.","No expected count below 1 and no more than 20% below 5.","Monte Carlo with 100,000 simulations is exported as sensitivity analysis and selected only if expected-count conditions fail.","This is a conditional modality of a multiple-response supplement-type question. Analysis is restricted to livestock units receiving post-pasture supplements. Analyze each supplement type separately and adjust p-values if several are tested.","Among eligible livestock units, each unit contributes once to the agro-industrial-by-products-spai table.","Use Rao-Scott chi-square if weights, strata, or village clusters apply."))
sheets<-list()
sheets[["Summary"]]<-as.data.frame(summary,check.names=FALSE,stringsAsFactors=FALSE)
sheets[["Veg_observed"]]<-matrix_df(veg$observed)
sheets[["Veg_expected"]]<-matrix_df(round(veg$expected,4))
sheets[["Veg_percent"]]<-matrix_df(round(veg$percent,2))
sheets[["Veg_std_residuals"]]<-matrix_df(round(veg$stdres,4))
sheets[["Phyto_observed"]]<-matrix_df(phyto$observed)
sheets[["Phyto_expected"]]<-matrix_df(round(phyto$expected,4))
sheets[["Phyto_percent"]]<-matrix_df(round(phyto$percent,2))
sheets[["Phyto_std_residuals"]]<-matrix_df(round(phyto$stdres,4))
sheets[["Data_quality"]]<-as.data.frame(quality,check.names=FALSE,stringsAsFactors=FALSE)
sheets[["Validity_notes"]]<-as.data.frame(notes,check.names=FALSE,stringsAsFactors=FALSE)
for(nm in names(sheets)){
  if(is.null(sheets[[nm]])||!is.data.frame(sheets[[nm]])||ncol(sheets[[nm]])<1L){
    sheets[[nm]]<-data.frame(Message="No output available",stringsAsFactors=FALSE,check.names=FALSE)
  }
  message("Prepared sheet: ",nm," [",nrow(sheets[[nm]])," rows x ",ncol(sheets[[nm]])," columns]")
}
writexl::write_xlsx(sheets,path=OUTPUT_FILE)
cat("Results exported to",OUTPUT_FILE,"\n")
print(summary)
