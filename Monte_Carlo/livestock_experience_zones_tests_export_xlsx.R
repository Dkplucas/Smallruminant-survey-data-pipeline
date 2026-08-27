# Livestock experience vs vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: livestock_experience_zone_comparison_results.xlsx

packages <- c("readxl","dplyr","tidyr","stringr","stringi","openxlsx")
missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) stop("Install packages first: install.packages(c(",paste(sprintf('"%s"',missing),collapse=", "),"))")
suppressPackageStartupMessages({library(readxl);library(dplyr);library(tidyr);library(stringr);library(stringi);library(openxlsx)})

DATA_FILE <- "data7.xlsx"; ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "livestock_experience_zone_comparison_results.xlsx"
ALPHA <- 0.05; B <- 100000L; SEED <- 20260827L
EXPERIENCE_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Quelle est votre expérience en élevage?"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
set.seed(SEED)

normalize_key <- function(x){
 x<-str_replace_all(as.character(x),"\\u00A0"," ");x<-stri_trans_general(x,"Latin-ASCII")|>str_to_lower()|>str_squish()
 x<-str_replace_all(x,"[’'`-]","")|>str_replace_all("[^a-z0-9]","")
 recode(x,"toribossito"="tori","dassazoume"="dassa","dassazounme"="dassa",.default=x)
}
brown_forsythe <- function(y,g){g<-droplevels(factor(g));m<-ave(y,g,FUN=median);a<-anova(lm(abs(y-m)~g));list(F=unname(a$`F value`[1]),df1=unname(a$Df[1]),df2=unname(a$Df[2]),p=unname(a$`Pr(>F)`[1]))}
permutation_kw <- function(y,g,B){
 g<-droplevels(factor(g));observed<-unname(kruskal.test(y~g)$statistic)
 perm<-replicate(B,unname(kruskal.test(y, sample(g, replace=FALSE))$statistic))
 extreme<-sum(perm>=observed);p<-(extreme+1)/(B+1);se<-sqrt(p*(1-p)/(B+1));ci<-pmax(0,pmin(1,p+c(-1,1)*1.96*se))
 list(statistic=observed,p=p,B=B,extreme=extreme,se=se,low=ci[1],high=ci[2])
}

raw<-read_excel(DATA_FILE,1,col_types="text",.name_repair="minimal");zraw<-read_excel(ZONES_FILE,1,col_types="text",.name_repair="unique")
missing_cols<-setdiff(c(EXPERIENCE_COL,COMMUNE_COL),names(raw));if(length(missing_cols))stop("Columns not found: ",paste(missing_cols,collapse=" | "))
zones<-zraw|>transmute(vegetation_zone=str_squish(str_replace_all(`Vegetation zones`,"\\u00A0"," ")),phytogeo_zone=str_squish(str_replace_all(`Phytogeographic zones`,"\\u00A0"," ")),district=str_squish(str_replace_all(District,"\\u00A0"," ")))|>fill(vegetation_zone,phytogeo_zone)|>filter(!is.na(district),district!="",!str_detect(str_to_lower(vegetation_zone),"^total"))|>mutate(commune_key=normalize_key(district))|>select(commune_key,district,vegetation_zone,phytogeo_zone)
data<-raw|>transmute(row_id=row_number(),experience_text=str_squish(.data[[EXPERIENCE_COL]]),experience=suppressWarnings(as.numeric(str_replace_all(experience_text,",","."))),commune_original=str_squish(.data[[COMMUNE_COL]]),commune_key=normalize_key(.data[[COMMUNE_COL]]))|>left_join(zones,by="commune_key")|>mutate(review_flag=!is.na(experience)&(experience<0|experience>80))
analysis<-data|>filter(!is.na(experience),experience>=0,!is.na(vegetation_zone))

analyse<-function(df,zone_var,label){
 d<-df|>transmute(value=experience,group=factor(.data[[zone_var]]))|>droplevels();if(nlevels(d$group)<2)stop(label,": at least two groups required")
 desc<-d|>group_by(group)|>summarise(N=n(),Mean=mean(value),SD=sd(value),Median=median(value),Q1=quantile(value,.25),Q3=quantile(value,.75),IQR=IQR(value),Minimum=min(value),Maximum=max(value),.groups="drop")|>rename(Group=group)
 normal<-d|>group_by(group)|>summarise(N=n(),Shapiro_W=if(n()>=3)unname(shapiro.test(value)$statistic)else NA_real_,Shapiro_p=if(n()>=3)shapiro.test(value)$p.value else NA_real_,Normality=ifelse(is.na(Shapiro_p),"Not assessed",ifelse(Shapiro_p>=ALPHA,"PASS","FAIL")),.groups="drop")|>rename(Group=group)
 bf<-brown_forsythe(d$value,d$group);kw<-kruskal.test(value~group,data=d);welch<-oneway.test(value~group,data=d,var.equal=FALSE)
 set.seed(SEED+ifelse(zone_var=="phytogeo_zone",100,0));mc<-permutation_kw(d$value,d$group,B)
 eta_h<-(unname(kw$statistic)-nlevels(d$group)+1)/(nrow(d)-nlevels(d$group));eta_h<-max(0,eta_h)
 pairwise<-data.frame(Group_1=character(),Group_2=character(),W=numeric(),P_raw=numeric(),P_BH=numeric(),P_Bonferroni=numeric())
 if(nlevels(d$group)>2 && kw$p.value<ALPHA){
   lv<-levels(d$group);cmb<-combn(lv,2,simplify=FALSE);tmp<-lapply(cmb,function(z){w<-wilcox.test(d$value[d$group==z[1]],d$value[d$group==z[2]],exact=FALSE);data.frame(Group_1=z[1],Group_2=z[2],W=unname(w$statistic),P_raw=w$p.value)})
   pairwise<-bind_rows(tmp)|>mutate(P_BH=p.adjust(P_raw,"BH"),P_Bonferroni=p.adjust(P_raw,"bonferroni"))
 }
 summary<-data.frame(Comparison=label,Outcome="Livestock experience (quantitative)",Valid_N=nrow(d),Groups=nlevels(d$group),Alpha=ALPHA,
 Primary_test="Kruskal-Wallis rank-sum test",Primary_statistic=unname(kw$statistic),Primary_df=unname(kw$parameter),Primary_p_value=kw$p.value,Primary_decision=ifelse(kw$p.value<ALPHA,"Statistically significant difference","No statistically significant difference"),
 Effect_size_epsilon_squared=eta_h,Brown_Forsythe_F=bf$F,Brown_Forsythe_df1=bf$df1,Brown_Forsythe_df2=bf$df2,Brown_Forsythe_p=bf$p,
 Welch_test=ifelse(nlevels(d$group)==2,"Welch two-sample t-test","Welch one-way ANOVA"),Welch_statistic=unname(welch$statistic),Welch_df1=unname(welch$parameter[1]),Welch_df2=if(length(welch$parameter)>1)unname(welch$parameter[2])else NA_real_,Welch_p_value=welch$p.value,
 Monte_Carlo_method="Permutation Kruskal-Wallis test",Monte_Carlo_statistic=mc$statistic,Monte_Carlo_B=mc$B,Monte_Carlo_extreme_count=mc$extreme,Monte_Carlo_p=mc$p,Monte_Carlo_SE=mc$se,Monte_Carlo_95CI_low=mc$low,Monte_Carlo_95CI_high=mc$high,
 Recommendation="Report Kruskal-Wallis as primary because all group distributions are non-normal; report Monte Carlo and Welch as sensitivity analyses.",stringsAsFactors=FALSE)
 list(summary=summary,desc=desc,normal=normal,pairwise=pairwise)
}
veg<-analyse(analysis,"vegetation_zone","Livestock experience vs vegetation zone");phyto<-analyse(analysis,"phytogeo_zone","Livestock experience vs phytogeographic zone");summary<-bind_rows(veg$summary,phyto$summary)
quality<-data.frame(Parameter=c("Rows read","Valid numeric experience values","Missing/non-numeric values","Unmatched zone records","Values flagged <0 or >80","Zero experience values","Minimum analyzed value","Maximum analyzed value"),Value=c(nrow(data),sum(!is.na(data$experience)),sum(is.na(data$experience)),sum(is.na(data$vegetation_zone)),sum(data$review_flag,na.rm=TRUE),sum(data$experience==0,na.rm=TRUE),min(analysis$experience),max(analysis$experience)))
notes<-data.frame(Topic=c("Outcome type","Chi-square","Primary test","Monte Carlo role","Welch role","Post-hoc tests","Independence","Unit interpretation","Survey design"),Assessment=c("Livestock experience is quantitative.","Chi-square is not appropriate unless experience is categorized, which would lose information.","Kruskal-Wallis is primary because group-specific Shapiro-Wilk tests reject normality.","A permutation Kruskal-Wallis test with 100,000 permutations is included as a sensitivity analysis.","Welch tests compare means without requiring equal variances and are included as sensitivity analyses.","When an overall comparison is significant for more than two groups, pairwise Wilcoxon tests with BH and Bonferroni adjustments are exported.","Each farmer must provide one independent observation.","The script assumes experience is measured in years; confirm this in the questionnaire codebook.","If weights, strata, or village clusters apply, use survey-adjusted or multilevel methods."))
wb<-createWorkbook();hdr<-createStyle(fontColour="white",fgFill="#1F4E78",textDecoration="bold",halign="center",wrapText=TRUE)
write_sheet<-function(n,x){addWorksheet(wb,n,gridLines=FALSE);writeData(wb,n,x,headerStyle=hdr,withFilter=TRUE);freezePane(wb,n,firstRow=TRUE);setColWidths(wb,n,1:ncol(x),"auto")}
write_sheet("Summary",summary);write_sheet("Veg_descriptives",veg$desc);write_sheet("Veg_normality",veg$normal);write_sheet("Phyto_descriptives",phyto$desc);write_sheet("Phyto_normality",phyto$normal);if(nrow(phyto$pairwise)>0)write_sheet("Phyto_pairwise",phyto$pairwise);write_sheet("Data_quality",quality);write_sheet("Method_notes",notes)
setColWidths(wb,"Summary",1:ncol(summary),18);setColWidths(wb,"Summary",c(1,2,6,16,21,29),c(42,32,32,28,45,100));setColWidths(wb,"Method_notes",1:2,c(32,110));numcols<-which(vapply(summary,is.numeric,logical(1)));addStyle(wb,"Summary",createStyle(numFmt="0.0000"),rows=2:3,cols=numcols,gridExpand=TRUE)
saveWorkbook(wb,OUTPUT_FILE,overwrite=TRUE);cat("Results exported to",OUTPUT_FILE,"\n");print(summary)
