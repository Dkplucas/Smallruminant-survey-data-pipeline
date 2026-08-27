# Training length vs vegetation and phytogeographic zones
# Analysis population: respondents with training status = Yes (1)
# Inputs: data7.xlsx and zones.xlsx
# Output: training_length_zone_comparison_results.xlsx

packages <- c("readxl","dplyr","tidyr","stringr","stringi","openxlsx")
missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) stop("Install packages first: install.packages(c(",paste(sprintf('"%s"',missing),collapse=", "),"))")
suppressPackageStartupMessages({library(readxl);library(dplyr);library(tidyr);library(stringr);library(stringi);library(openxlsx)})

DATA_FILE <- "data7.xlsx"; ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "training_length_zone_comparison_results.xlsx"
ALPHA <- 0.05; B <- 100000L; SEED <- 20260827L
TRAINING_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Avez vous reçu oui suivi une Formation en élevage ?: 0=Non, 1=Oui"
LENGTH_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Si Oui, depuis quand ?"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
set.seed(SEED)

normalize_key <- function(x){
  x<-str_replace_all(as.character(x),"\\u00A0"," ");x<-stri_trans_general(x,"Latin-ASCII")|>str_to_lower()|>str_squish()
  x<-str_replace_all(x,"[’'`-]","")|>str_replace_all("[^a-z0-9]","")
  recode(x,"toribossito"="tori","dassazoume"="dassa","dassazounme"="dassa",.default=x)
}

brown_forsythe <- function(y,g){g<-droplevels(factor(g));m<-ave(y,g,FUN=median);a<-anova(lm(abs(y-m)~g));list(F=unname(a$`F value`[1]),df1=unname(a$Df[1]),df2=unname(a$Df[2]),p=unname(a$`Pr(>F)`[1]))}
welch_stat <- function(y,g){sp<-split(y,g);ni<-vapply(sp,length,numeric(1));mi<-vapply(sp,mean,numeric(1));vi<-vapply(sp,var,numeric(1));if(any(vi<=0)||any(ni<2))return(NA_real_);wi<-ni/vi;sum(wi*(mi-sum(wi*mi)/sum(wi))^2)/(length(sp)-1)}
permutation_test <- function(y,g,B){
  g<-droplevels(factor(g));k<-nlevels(g)
  if(k==2){statfun<-function(z)abs(diff(tapply(y,z,mean)));method<-"Monte Carlo permutation test of mean difference"}
  else{statfun<-function(z)welch_stat(y,z);method<-"Monte Carlo permutation test using Welch-type statistic"}
  observed<-statfun(g);perm<-replicate(B,statfun(sample(g,replace=FALSE)));ok<-!is.na(perm);extreme<-sum(perm[ok]>=observed)
  p<-(extreme+1)/(sum(ok)+1);se<-sqrt(p*(1-p)/(sum(ok)+1));ci<-pmax(0,pmin(1,p+c(-1,1)*1.96*se))
  list(method=method,statistic=observed,p=p,B=sum(ok),extreme=extreme,se=se,low=ci[1],high=ci[2])
}

raw<-read_excel(DATA_FILE,1,col_types="text",.name_repair="minimal");zraw<-read_excel(ZONES_FILE,1,col_types="text",.name_repair="unique")
missing_cols<-setdiff(c(TRAINING_COL,LENGTH_COL,COMMUNE_COL),names(raw));if(length(missing_cols))stop("Columns not found: ",paste(missing_cols,collapse=" | "))
zones<-zraw|>transmute(vegetation_zone=str_squish(str_replace_all(`Vegetation zones`,"\\u00A0"," ")),phytogeo_zone=str_squish(str_replace_all(`Phytogeographic zones`,"\\u00A0"," ")),district=str_squish(str_replace_all(District,"\\u00A0"," ")))|>fill(vegetation_zone,phytogeo_zone)|>filter(!is.na(district),district!="",!str_detect(str_to_lower(vegetation_zone),"^total"))|>mutate(commune_key=normalize_key(district))|>select(commune_key,district,vegetation_zone,phytogeo_zone)
data<-raw|>transmute(row_id=row_number(),training_code=str_squish(.data[[TRAINING_COL]]),length_text=str_squish(.data[[LENGTH_COL]]),training_length=suppressWarnings(as.numeric(str_replace_all(length_text,",","."))),commune_original=str_squish(.data[[COMMUNE_COL]]),commune_key=normalize_key(.data[[COMMUNE_COL]]))|>left_join(zones,by="commune_key")|>mutate(eligible=training_code=="1",inconsistent_nontrained_value=training_code=="0" & !is.na(training_length) & training_length>0,review_flag=eligible & !is.na(training_length) & (training_length<=0 | training_length>60))
analysis<-data|>filter(eligible,!is.na(training_length),training_length>0,!is.na(vegetation_zone))

analyse<-function(df,zone_var,label){
  d<-df|>transmute(value=training_length,group=factor(.data[[zone_var]]))|>droplevels();if(nlevels(d$group)<2)stop(label,": at least two groups required")
  desc<-d|>group_by(group)|>summarise(N=n(),Mean=mean(value),SD=sd(value),Median=median(value),Q1=quantile(value,.25),Q3=quantile(value,.75),IQR=IQR(value),Minimum=min(value),Maximum=max(value),.groups="drop")|>rename(Group=group)
  normal<-d|>group_by(group)|>summarise(N=n(),Shapiro_W=if(n()>=3)unname(shapiro.test(value)$statistic)else NA_real_,Shapiro_p=if(n()>=3)shapiro.test(value)$p.value else NA_real_,Normality=ifelse(is.na(Shapiro_p),"Not assessed",ifelse(Shapiro_p>=ALPHA,"PASS","FAIL")),.groups="drop")|>rename(Group=group)
  bf<-brown_forsythe(d$value,d$group);welch<-oneway.test(value~group,data=d,var.equal=FALSE);kw<-kruskal.test(value~group,data=d);set.seed(SEED+ifelse(zone_var=="phytogeo_zone",100,0));mc<-permutation_test(d$value,d$group,B)
  # Due to small trained-only samples and observed skew, Kruskal-Wallis is primary; Monte Carlo and Welch are sensitivity analyses.
  summary<-data.frame(Comparison=label,Analysis_population="Training=Yes only",Valid_N=nrow(d),Number_of_groups=nlevels(d$group),Alpha=ALPHA,
                      Primary_test="Kruskal-Wallis rank-sum test",Primary_statistic=unname(kw$statistic),Primary_df=unname(kw$parameter),Primary_p_value=kw$p.value,
                      Primary_decision=ifelse(kw$p.value<ALPHA,"Statistically significant difference","No statistically significant difference"),
                      Welch_test=ifelse(nlevels(d$group)==2,"Welch two-sample t-test","Welch one-way ANOVA"),Welch_statistic=unname(welch$statistic),Welch_df1=unname(welch$parameter[1]),Welch_df2=if(length(welch$parameter)>1)unname(welch$parameter[2])else NA_real_,Welch_p_value=welch$p.value,
                      Brown_Forsythe_F=bf$F,Brown_Forsythe_df1=bf$df1,Brown_Forsythe_df2=bf$df2,Brown_Forsythe_p=bf$p,
                      Monte_Carlo_method=mc$method,Monte_Carlo_statistic=mc$statistic,Monte_Carlo_B=mc$B,Monte_Carlo_extreme_count=mc$extreme,Monte_Carlo_p=mc$p,Monte_Carlo_SE=mc$se,Monte_Carlo_95CI_low=mc$low,Monte_Carlo_95CI_high=mc$high,
                      Recommendation="Report Kruskal-Wallis as primary because of small trained-only group sizes and skew; report Monte Carlo and Welch as sensitivity analyses.",stringsAsFactors=FALSE)
  list(summary=summary,desc=desc,normal=normal)
}
veg<-analyse(analysis,"vegetation_zone","Training length vs vegetation zone");phyto<-analyse(analysis,"phytogeo_zone","Training length vs phytogeographic zone");summary<-bind_rows(veg$summary,phyto$summary)
quality<-data.frame(Parameter=c("Rows read","Training=Yes respondents","Training=No respondents","Eligible with valid positive length","Eligible missing/non-numeric length","Unmatched eligible records","Non-trained respondents with positive length","Eligible values flagged <=0 or >60","Minimum analyzed length","Maximum analyzed length"),Value=c(nrow(data),sum(data$training_code=="1"),sum(data$training_code=="0"),nrow(analysis),sum(data$eligible & is.na(data$training_length)),sum(data$eligible & is.na(data$vegetation_zone)),sum(data$inconsistent_nontrained_value,na.rm=TRUE),sum(data$review_flag,na.rm=TRUE),min(analysis$training_length),max(analysis$training_length)))
inconsistent<-data|>filter(inconsistent_nontrained_value)|>select(row_id,training_code,length_text,training_length,commune_original)
notes<-data.frame(Topic=c("Outcome type","Analysis population","Zero values","Primary test","Monte Carlo role","Welch role","Independence","Unit interpretation","Survey design"),Assessment=c("The training-length field is quantitative.","Only respondents answering Yes to livestock training are analyzed.","Zeros belonging to respondents who answered No are structural not-applicable values and are excluded, not treated as zero years.","Kruskal-Wallis is primary because the trained-only sample is small and some group distributions are skewed.","Monte Carlo permutation p-values with 100,000 permutations provide a sensitivity analysis.","Welch tests compare means without assuming equal variances and are exported as sensitivity analyses.","Each farmer must provide one independent observation.","The script assumes the values represent duration in years. Confirm this from the questionnaire codebook.","If weights, strata, or village clusters apply, use survey-adjusted or multilevel methods."))

wb<-createWorkbook();hdr<-createStyle(fontColour="white",fgFill="#1F4E78",textDecoration="bold",halign="center",wrapText=TRUE)
write_sheet<-function(n,x){addWorksheet(wb,n,gridLines=FALSE);writeData(wb,n,x,headerStyle=hdr,withFilter=TRUE);freezePane(wb,n,firstRow=TRUE);setColWidths(wb,n,1:ncol(x),"auto")}
write_sheet("Summary",summary);write_sheet("Veg_descriptives",veg$desc);write_sheet("Veg_normality",veg$normal);write_sheet("Phyto_descriptives",phyto$desc);write_sheet("Phyto_normality",phyto$normal);write_sheet("Data_quality",quality);if(nrow(inconsistent)>0)write_sheet("Inconsistent_records",inconsistent);write_sheet("Method_notes",notes)
setColWidths(wb,"Summary",1:ncol(summary),18);setColWidths(wb,"Summary",c(1,2,6,11,20,28),c(40,22,32,28,55,95));setColWidths(wb,"Method_notes",1:2,c(32,110));numcols<-which(vapply(summary,is.numeric,logical(1)));addStyle(wb,"Summary",createStyle(numFmt="0.0000"),rows=2:3,cols=numcols,gridExpand=TRUE)
saveWorkbook(wb,OUTPUT_FILE,overwrite=TRUE);cat("Results exported to",OUTPUT_FILE,"\n");print(summary)
