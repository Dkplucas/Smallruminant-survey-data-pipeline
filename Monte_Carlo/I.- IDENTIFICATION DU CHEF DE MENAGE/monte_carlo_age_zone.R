# Age comparisons across vegetation and phytogeographic zones
# Inputs: data7.xlsx and zones.xlsx
# Output: age_zone_comparison_results.xlsx

packages <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "openxlsx")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install packages first: install.packages(c(", paste(sprintf('"%s"', missing), collapse=", "), "))")
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(stringr)
  library(stringi); library(openxlsx)
})

DATA_FILE <- "data7.xlsx"
ZONES_FILE <- "zones.xlsx"
OUTPUT_FILE <- "age_zone_comparison_results.xlsx"
ALPHA <- 0.05
B <- 100000L                 # Monte Carlo permutations
SEED <- 20260827L
AGE_COL <- "I.- IDENTIFICATION DU CHEF DE MENAGE /Age :"
COMMUNE_COL <- "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"
set.seed(SEED)

normalize_key <- function(x) {
  x <- str_replace_all(as.character(x), "\\u00A0", " ")
  x <- stri_trans_general(x, "Latin-ASCII") |> str_to_lower() |> str_squish()
  x <- str_replace_all(x, "[’'`-]", "") |> str_replace_all("[^a-z0-9]", "")
  recode(x, "toribossito"="tori", "dassazoume"="dassa", "dassazounme"="dassa", .default=x)
}

# Brown-Forsythe test: equality of variances using absolute deviations from group medians.
brown_forsythe <- function(y, g) {
  g <- droplevels(factor(g)); med <- ave(y, g, FUN=median)
  fit <- lm(abs(y-med) ~ g); a <- anova(fit)
  list(statistic=unname(a$`F value`[1]), df1=unname(a$Df[1]),
       df2=unname(a$Df[2]), p.value=unname(a$`Pr(>F)`[1]))
}

# Welch ANOVA statistic, used for the Monte Carlo sensitivity test for 3+ groups.
welch_stat <- function(y, g) {
  split_y <- split(y, g); ni <- vapply(split_y, length, numeric(1))
  mi <- vapply(split_y, mean, numeric(1)); vi <- vapply(split_y, var, numeric(1))
  if (any(vi <= 0) || any(ni < 2)) return(NA_real_)
  wi <- ni/vi; sum(wi*(mi-sum(wi*mi)/sum(wi))^2)/(length(split_y)-1)
}

# Monte Carlo permutation test. The +1 correction prevents a zero p-value.
monte_carlo_test <- function(y, g, B=100000L) {
  g <- droplevels(factor(g)); k <- nlevels(g)
  if (k == 2) {
    stat_fun <- function(labels) abs(diff(tapply(y, labels, mean)))
    name <- "Monte Carlo permutation test of difference in means"
  } else {
    stat_fun <- function(labels) welch_stat(y, labels)
    name <- "Monte Carlo permutation test using Welch-type statistic"
  }
  observed <- stat_fun(g)
  permuted <- replicate(B, stat_fun(sample(g, replace=FALSE)))
  extreme <- sum(permuted >= observed, na.rm=TRUE)
  valid_B <- sum(!is.na(permuted))
  p <- (extreme+1)/(valid_B+1)
  mc_se <- sqrt(p*(1-p)/(valid_B+1))
  ci <- pmax(0, pmin(1, p + c(-1,1)*1.96*mc_se))
  list(method=name, statistic=observed, p.value=p, B=valid_B,
       extreme=extreme, mc_se=mc_se, ci_low=ci[1], ci_high=ci[2])
}

read_data <- read_excel(DATA_FILE, sheet=1, col_types="text", .name_repair="minimal")
zones_raw <- read_excel(ZONES_FILE, sheet=1, col_types="text", .name_repair="unique")
if (!AGE_COL %in% names(read_data)) stop("Age column not found exactly in data7.xlsx")
if (!COMMUNE_COL %in% names(read_data)) stop("Commune column not found exactly in data7.xlsx")

zones <- zones_raw |>
  transmute(
    vegetation_zone=str_squish(str_replace_all(`Vegetation zones`, "\\u00A0", " ")),
    phytogeo_zone=str_squish(str_replace_all(`Phytogeographic zones`, "\\u00A0", " ")),
    district=str_squish(str_replace_all(District, "\\u00A0", " "))
  ) |>
  fill(vegetation_zone, phytogeo_zone) |>
  filter(!is.na(district), district!="", !str_detect(str_to_lower(vegetation_zone), "^total")) |>
  mutate(commune_key=normalize_key(district)) |>
  select(commune_key, district, vegetation_zone, phytogeo_zone)

data <- read_data |>
  transmute(age_text=str_squish(.data[[AGE_COL]]),
            age=suppressWarnings(as.numeric(str_replace_all(age_text, ",", "."))),
            commune_original=str_squish(.data[[COMMUNE_COL]]),
            commune_key=normalize_key(.data[[COMMUNE_COL]])) |>
  left_join(zones, by="commune_key")

# Flag values for review. The tests use all numeric ages, but the workbook exposes implausible values.
data <- data |> mutate(age_review_flag=!is.na(age) & (age < 15 | age > 100))

analyse_zone <- function(df, zone_var, label) {
  d <- df |> filter(!is.na(age), !is.na(.data[[zone_var]])) |>
    transmute(age=age, group=factor(.data[[zone_var]])) |> droplevels()
  if (nlevels(d$group) < 2) stop(label, ": at least two groups are required")
  
  desc <- d |> group_by(group) |> summarise(
    N=n(), Mean=mean(age), SD=sd(age), Median=median(age),
    Q1=quantile(age,.25), Q3=quantile(age,.75), IQR=IQR(age),
    Minimum=min(age), Maximum=max(age), .groups="drop") |>
    rename(Group=group)
  
  normality <- d |> group_by(group) |> summarise(
    N=n(), Shapiro_W=if(n()>=3 && n()<=5000) unname(shapiro.test(age)$statistic) else NA_real_,
    Shapiro_p=if(n()>=3 && n()<=5000) shapiro.test(age)$p.value else NA_real_,
    Normality_at_alpha=ifelse(is.na(Shapiro_p),"Not assessed",ifelse(Shapiro_p>=ALPHA,"PASS","FAIL")),
    .groups="drop") |> rename(Group=group)
  
  bf <- brown_forsythe(d$age,d$group)
  welch <- oneway.test(age ~ group, data=d, var.equal=FALSE)
  kw <- kruskal.test(age ~ group, data=d)
  mc <- monte_carlo_test(d$age,d$group,B)
  
  if (nlevels(d$group)==2) {
    primary_name <- "Welch two-sample t-test"
    primary_stat <- unname(welch$statistic); primary_df1 <- unname(welch$parameter)
    primary_df2 <- NA_real_; primary_p <- welch$p.value
  } else {
    primary_name <- "Welch one-way ANOVA"
    primary_stat <- unname(welch$statistic); primary_df1 <- unname(welch$parameter[1])
    primary_df2 <- unname(welch$parameter[2]); primary_p <- welch$p.value
  }
  
  # Monte Carlo is a sensitivity analysis, not automatically superior. Welch is primary for comparing means.
  summary <- data.frame(
    Comparison=label, Outcome="Age (quantitative)", Number_of_groups=nlevels(d$group),
    Valid_N=nrow(d), Missing_or_excluded_N=nrow(df)-nrow(d), Alpha=ALPHA,
    Primary_test=primary_name, Primary_statistic=primary_stat,
    Primary_df1=primary_df1, Primary_df2=primary_df2, Primary_p_value=primary_p,
    Primary_decision=ifelse(primary_p<ALPHA,"Statistically significant age difference","No statistically significant age difference"),
    Brown_Forsythe_F=bf$statistic, Brown_Forsythe_df1=bf$df1,
    Brown_Forsythe_df2=bf$df2, Brown_Forsythe_p=bf$p.value,
    Equal_variance_assumption=ifelse(bf$p.value>=ALPHA,"Not rejected","Rejected; Welch test preferred"),
    Kruskal_Wallis_statistic=unname(kw$statistic), Kruskal_Wallis_df=unname(kw$parameter),
    Kruskal_Wallis_p=kw$p.value,
    Monte_Carlo_method=mc$method, Monte_Carlo_statistic=mc$statistic,
    Monte_Carlo_B=mc$B, Monte_Carlo_extreme_count=mc$extreme,
    Monte_Carlo_p=mc$p.value, Monte_Carlo_SE=mc$mc_se,
    Monte_Carlo_95CI_low=mc$ci_low, Monte_Carlo_95CI_high=mc$ci_high,
    Monte_Carlo_decision=ifelse(mc$p.value<ALPHA,"Statistically significant age difference","No statistically significant age difference"),
    Recommendation="Use Welch test as primary for mean age; report Monte Carlo and Kruskal-Wallis as sensitivity analyses.",
    stringsAsFactors=FALSE)
  list(summary=summary, descriptives=desc, normality=normality)
}

veg <- analyse_zone(data,"vegetation_zone","Age vs vegetation zone")
phyto <- analyse_zone(data,"phytogeo_zone","Age vs phytogeographic zone")
summary <- bind_rows(veg$summary,phyto$summary)

quality <- data.frame(
  Parameter=c("Rows read","Valid numeric ages","Missing/non-numeric ages","Unmatched zone records","Age values flagged (<15 or >100)","Minimum age","Maximum age"),
  Value=c(nrow(data),sum(!is.na(data$age)),sum(is.na(data$age)),sum(is.na(data$vegetation_zone)),sum(data$age_review_flag,na.rm=TRUE),min(data$age,na.rm=TRUE),max(data$age,na.rm=TRUE)))
notes <- data.frame(
  Topic=c("Why chi-square is not used","Primary test","Normality","Variance homogeneity","Monte Carlo role","Kruskal-Wallis role","Independence","Sampling design","Interpretation"),
  Assessment=c(
    "Age is quantitative; chi-square would require arbitrary categorization and loss of information.",
    "Welch t-test for two vegetation zones; Welch one-way ANOVA for phytogeographic zones.",
    "Group-specific Shapiro-Wilk results are reported. Large samples make Welch tests reasonably robust to moderate non-normality.",
    "Brown-Forsythe results are reported. Welch tests do not require equal variances.",
    "Monte Carlo permutation p-values are included as sensitivity analyses. They are not automatically better than Welch tests, especially with unequal variances.",
    "Kruskal-Wallis is included as a rank-based sensitivity analysis; it tests distribution/rank differences, not strictly equality of means.",
    "Each farmer must contribute one independent observation.",
    "If villages/districts are clusters or survey weights apply, use survey-design or multilevel methods.",
    "Statistical association/difference does not establish causation."))

wb <- createWorkbook(); hdr <- createStyle(fontColour="white",fgFill="#1F4E78",textDecoration="bold",halign="center",wrapText=TRUE)
write_sheet <- function(name,x) { addWorksheet(wb,name,gridLines=FALSE); writeData(wb,name,x,headerStyle=hdr,withFilter=TRUE); freezePane(wb,name,firstRow=TRUE); setColWidths(wb,name,1:ncol(x),"auto") }
write_sheet("Summary",summary)
write_sheet("Veg_descriptives",veg$descriptives)
write_sheet("Veg_normality",veg$normality)
write_sheet("Phyto_descriptives",phyto$descriptives)
write_sheet("Phyto_normality",phyto$normality)
write_sheet("Data_quality",quality)
write_sheet("Method_notes",notes)
setColWidths(wb,"Summary",1:ncol(summary),18); setColWidths(wb,"Summary",c(1,7,12,22,31),c(30,28,42,44,78)); setColWidths(wb,"Method_notes",1:2,c(30,100))
num_cols <- which(vapply(summary,is.numeric,logical(1)))
addStyle(wb,"Summary",createStyle(numFmt="0.0000"),rows=2:(nrow(summary)+1),cols=num_cols,gridExpand=TRUE)
saveWorkbook(wb,OUTPUT_FILE,overwrite=TRUE)
cat("Results exported to",OUTPUT_FILE,"\n"); print(summary)
