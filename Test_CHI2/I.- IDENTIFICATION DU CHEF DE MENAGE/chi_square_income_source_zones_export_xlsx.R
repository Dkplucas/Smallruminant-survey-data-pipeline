# Main income source vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: chi_square_results_income_source_zones.xlsx

packages <- c("readxl","dplyr","tidyr","stringr","stringi","openxlsx")
missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) stop("Install packages first: install.packages(c(",paste(sprintf('"%s"',missing),collapse=", "),"))")
suppressPackageStartupMessages({library(readxl);library(dplyr);library(tidyr);library(stringr);library(stringi);library(openxlsx)})
DATA_FILE <- "data7.xlsx"; ZONES_FILE <- "zones.xlsx"; OUTPUT_FILE <- "chi_square_results_income_source_zones.xlsx"
ALPHA <- 0.05; B <- 100000L; SEED <- 20260827L
INCOME_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelle est votre principale source de revenu? ou De quelle activité provient principalement vos revenus?: 1=Agriculture, 2=Artisanat, 3=Autre, 4=Commerce, 5=Elevage, 6=Vide"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
set.seed(SEED)

normalize_key <- function(x){
 x<-str_replace_all(as.character(x),"\\u00A0"," ");x<-stri_trans_general(x,"Latin-ASCII")|>str_to_lower()|>str_squish()
 x<-str_replace_all(x,"[’'`-]","")|>str_replace_all("[^a-z0-9]","")
 recode(x,"toribossito"="tori","dassazoume"="dassa","dassazounme"="dassa",.default=x)
}
raw<-read_excel(DATA_FILE,1,col_types="text",.name_repair="minimal")
zraw<-read_excel(ZONES_FILE,1,col_types="text",.name_repair="unique")
if(!INCOME_COL%in%names(raw))stop("Income-source column not found exactly in data7.xlsx")
if(!COMMUNE_COL%in%names(raw))stop("Commune column not found exactly in data7.xlsx")
zones<-zraw|>transmute(vegetation_zone=str_squish(str_replace_all(`Vegetation zones`,"\\u00A0"," ")),
 phytogeo_zone=str_squish(str_replace_all(`Phytogeographic zones`,"\\u00A0"," ")),district=str_squish(str_replace_all(District,"\\u00A0"," ")))|>
 fill(vegetation_zone,phytogeo_zone)|>filter(!is.na(district),district!="",!str_detect(str_to_lower(vegetation_zone),"^total"))|>
 mutate(commune_key=normalize_key(district))|>select(commune_key,district,vegetation_zone,phytogeo_zone)
data<-raw|>transmute(income_code=str_squish(.data[[INCOME_COL]]),commune_original=str_squish(.data[[COMMUNE_COL]]),commune_key=normalize_key(.data[[COMMUNE_COL]]))|>
 mutate(income_source=factor(income_code,levels=c("1","2","3","4","5","6"),labels=c("Agriculture","Crafts","Other","Commerce","Livestock","Blank/unspecified")),
 invalid_income=is.na(income_source)&!is.na(income_code)&income_code!="")|>left_join(zones,by="commune_key")

run_test<-function(df,zone_var,label){
 d<-df|>filter(!is.na(income_source),!is.na(.data[[zone_var]]))|>droplevels();obs<-table(d$income_source,d[[zone_var]])
 pearson<-suppressWarnings(chisq.test(obs,correct=FALSE));exp<-pearson$expected;n<-sum(obs);cells<-length(exp);dfchi<-(nrow(obs)-1)*(ncol(obs)-1)
 n5<-sum(exp<5);n1<-sum(exp<1);pct5<-100*n5/cells;margins<-!any(rowSums(obs)==0)&&!any(colSums(obs)==0);valid<-n1==0&&pct5<=20&&margins
 # Also calculate Monte Carlo for every comparison as a sensitivity analysis.
 set.seed(SEED+ifelse(zone_var=="phytogeo_zone",100,0));mc<-chisq.test(obs,simulate.p.value=TRUE,B=B)
 mcse<-sqrt(mc$p.value*(1-mc$p.value)/(B+1));mcl<-max(0,mc$p.value-1.96*mcse);mch<-min(1,mc$p.value+1.96*mcse)
 if(valid){selected<-pearson;method<-"Pearson chi-square (asymptotic)"}else{selected<-mc;method<-paste0("Pearson chi-square with Monte Carlo p-value (B=",B,")")}
 v<-sqrt(unname(pearson$statistic)/(n*min(nrow(obs)-1,ncol(obs)-1)))
 sm<-data.frame(Comparison=label,N=n,Rows=nrow(obs),Columns=ncol(obs),Cell_count=cells,Degrees_of_freedom=dfchi,
 Pearson_chi_square=unname(pearson$statistic),Asymptotic_p_value=pearson$p.value,Minimum_expected=min(exp),Cells_expected_below_5=n5,
 Percent_expected_below_5=pct5,Cells_expected_below_1=n1,Nonzero_margins=ifelse(margins,"PASS","FAIL"),
 Asymptotic_chi_square_valid=ifelse(valid,"YES","NO"),Selected_test=method,Selected_p_value=selected$p.value,
 Monte_Carlo_B=B,Monte_Carlo_p_value=mc$p.value,Monte_Carlo_SE=mcse,Monte_Carlo_95CI_low=mcl,Monte_Carlo_95CI_high=mch,
 Alpha=ALPHA,Decision=ifelse(selected$p.value<ALPHA,"Statistically significant association","No statistically significant association"),
 Cramers_V=v,Recommendation=ifelse(valid,"Report ordinary Pearson chi-square; Monte Carlo is supplied as sensitivity analysis.",
 "Report Monte Carlo p-value because expected-count conditions fail."),stringsAsFactors=FALSE)
 list(summary=sm,observed=obs,expected=exp,stdres=pearson$stdres)
}
veg<-run_test(data,"vegetation_zone","Main income source vs vegetation zone")
phyto<-run_test(data,"phytogeo_zone","Main income source vs phytogeographic zone")
summary<-bind_rows(veg$summary,phyto$summary)
matrix_df<-function(x){z<-as.data.frame.matrix(x);data.frame(`Income source`=rownames(z),z,check.names=FALSE,row.names=NULL)}
quality<-data.frame(Parameter=c("Rows read","Valid income-source records","Missing income-source records","Invalid income-source codes","Unmatched zone records"),
 Value=c(nrow(data),sum(!is.na(data$income_source)),sum(is.na(data$income_code)|data$income_code==""),sum(data$invalid_income),sum(is.na(data$vegetation_zone))))
notes<-data.frame(Parameter=c("Variable type","Code 6 treatment","Null hypothesis","Expected-count rule","Monte Carlo","Independence","Sampling design","Interpretation"),
 Assessment=c("Income source and zone are qualitative variables, so chi-square is appropriate.",
 "Code 6 is retained as Blank/unspecified because it is an explicit category in the source variable. Consider treating it as missing only if the questionnaire documentation defines it as nonresponse.",
 "Income source and zone are independent.","No expected count below 1 and no more than 20% below 5.",
 "Monte Carlo with 100,000 simulations is selected when the asymptotic rule fails and is also exported as a sensitivity analysis.",
 "Each farmer must contribute to one cell only.","Use Rao-Scott chi-square if weights, strata, or village clusters apply.","Association does not establish causation."))
wb<-createWorkbook();hdr<-createStyle(fontColour="white",fgFill="#1F4E78",textDecoration="bold",halign="center",wrapText=TRUE)
write_sheet<-function(n,x){addWorksheet(wb,n,gridLines=FALSE);writeData(wb,n,x,headerStyle=hdr,withFilter=TRUE);freezePane(wb,n,firstRow=TRUE);setColWidths(wb,n,1:ncol(x),"auto")}
write_sheet("Summary",summary);write_sheet("Veg_observed",matrix_df(veg$observed));write_sheet("Veg_expected",matrix_df(round(veg$expected,4)));write_sheet("Veg_std_residuals",matrix_df(round(veg$stdres,4)))
write_sheet("Phyto_observed",matrix_df(phyto$observed));write_sheet("Phyto_expected",matrix_df(round(phyto$expected,4)));write_sheet("Phyto_std_residuals",matrix_df(round(phyto$stdres,4)));write_sheet("Data_quality",quality);write_sheet("Validity_notes",notes)
setColWidths(wb,"Summary",1:ncol(summary),18);setColWidths(wb,"Summary",c(1,15,23,25),c(42,55,42,85));setColWidths(wb,"Validity_notes",1:2,c(35,105))
numcols<-which(vapply(summary,is.numeric,logical(1)));addStyle(wb,"Summary",createStyle(numFmt="0.0000"),rows=2:3,cols=numcols,gridExpand=TRUE)
saveWorkbook(wb,OUTPUT_FILE,overwrite=TRUE);cat("Results exported to",OUTPUT_FILE,"\n");print(summary)
