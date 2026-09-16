# Smallruminant-survey-data-pipeline

R scripts for cleaning and analysing KoboCollect survey data about small-ruminant
farming and goat crossbreeding practices in Benin. The project covers household
characteristics, livestock-unit management, housing, reproduction, crossbred-goat
performance, record keeping, animal health, environmental context and herd
composition.

The analysis compares survey responses with vegetation and phytogeographic zones.
It includes Pearson chi-squared tests, Monte Carlo chi-squared tests for sparse
contingency tables, and permutation-based tests for quantitative variables.

## Repository layout

```
Data_cleaning/
   unite1cleaning.R ... unite7cleaning.R
   Raw questionnaire, intermediate workbooks and the final data7.xlsx

Test_CHI2/
   Thematic folders containing chi-squared analysis scripts and Excel results
   data7.xlsx and zones.xlsx used by the analysis scripts

Monte_Carlo/
   Thematic folders containing Monte Carlo and permutation-test scripts
   data7.xlsx and zones.xlsx used by the analysis scripts
```

The thematic folders correspond to the questionnaire sections:

1. Household-head identification
2. Livestock-unit characteristics
3. Housing, equipment and facilities
4. Animal management
5. Reproduction
6. Crossbred-goat characteristics
7. Crossbred performance
8. Data recording and monitoring
9. Perceptions and adaptation
10. Environmental context
11. Animal health monitoring
12. Small-ruminant species, herd size and composition

## Data-cleaning pipeline

Run the cleaning scripts in this order:

```text
unite1cleaning.R -> data1.xlsx
unite2cleaning.R -> data2.xlsx
unite3cleaning.R -> data3.xlsx
unite4cleaning.R -> data4.xlsx
unite5cleaning.R -> data5.xlsx
unite6cleaning.R -> data6.xlsx
unite7cleaning.R -> data7.xlsx
```

The scripts generally perform the following operations:

- read the previous Excel workbook;
- remove identifiers, coordinates, free-text clarification fields or other
   columns that are not used in the analysis;
- rename questionnaire columns with explicit category codes;
- recode Yes/No and other categorical responses;
- replace unanswered multi-select fields with `0` where the questionnaire logic
   requires it;
- write the next intermediate workbook.

Example from an R session started in the cleaning directory:

```r
setwd("path/to/Smallruminant-survey-data-pipeline/Data_cleaning")
source("unite1cleaning.R")
source("unite2cleaning.R")
source("unite3cleaning.R")
source("unite4cleaning.R")
source("unite5cleaning.R")
source("unite6cleaning.R")
source("unite7cleaning.R")
```

The cleaning scripts currently contain machine-specific absolute paths in some
stages. Before running them on another machine, update `setwd()` and each
`xlsx_path` or `output_path` that points to the original Windows directory.

## Statistical analyses

Each analysis script is intended to be run independently. It normally expects
these files in its current working directory:

- `data7.xlsx`: final cleaned survey data;
- `zones.xlsx`: mapping of districts/communes to `Vegetation zones` and
   `Phytogeographic zones`.

The scripts normalize accents and commune names before joining the survey data
to the zone table. They write one or more `.xlsx` result workbooks containing
summaries, observed/expected tables, data-quality checks and method notes.

### Chi-squared tests

Use the scripts under `Test_CHI2/` for categorical variables such as education,
animal-health practices, reproduction, environmental perceptions and herd
composition. Scripts select the Pearson test when expected-count assumptions
are acceptable and use a simulated p-value when the table is sparse.

Example:

```r
setwd("path/to/Smallruminant-survey-data-pipeline/Test_CHI2/I.- IDENTIFICATION DU CHEF DE MENAGE")
source("chi2_education.R")
```

### Monte Carlo and permutation tests

Use the scripts under `Monte_Carlo/` when a Monte Carlo analysis is required or
when the outcome is quantitative. Depending on the variable, the scripts use
Monte Carlo chi-squared tests, Kruskal-Wallis permutation tests, or permutation
tests alongside Welch and Kruskal-Wallis sensitivity analyses.

Example:

```r
setwd("path/to/Smallruminant-survey-data-pipeline/Monte_Carlo/I.- IDENTIFICATION DU CHEF DE MENAGE")
source("monte_carlo_age_zone.R")
```

Simulation counts, significance level and random seeds are defined near the top
of each script. Increase the number of simulations for a final report when the
script documents that option, and keep the seed for reproducibility.

## Requirements

- R 4.0 or later recommended;
- R packages used across the project:
   `dplyr`, `stringr`, `readxl`, `openxlsx`, `tidyr`, `stringi`, and `writexl`.

Install the dependencies with:

```r
install.packages(c(
   "dplyr", "stringr", "readxl", "openxlsx", "tidyr", "stringi", "writexl"
))
```

Most analysis scripts check for missing packages and stop with an installation
message. They do not automatically download the survey data or the zone table.

## Reproducibility and interpretation

- Run the cleaning stages in order; later stages depend on the previous
   `dataN.xlsx` output.
- Run each statistical script from a directory containing the required input
   workbooks, or change its `DATA_FILE`, `ZONES_FILE` and `OUTPUT_FILE` values.
- Review the `Data_quality` and `Method_notes` sheets before interpreting a
   result. They report missing values, invalid codes, unmatched communes and the
   selected statistical method.
- Statistical association does not establish causation.
- If the survey design includes clusters, weights or stratification, consider
   a survey-design or multilevel analysis rather than treating every row as an
   independent observation.

## License

Distributed under the MIT License. See [LICENSE](LICENSE) for details.

