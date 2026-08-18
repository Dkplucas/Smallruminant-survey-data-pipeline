# Smallruminant-survey-data-pipeline

R-based data pipeline for cleaning, recoding, and statistically analyzing KoboCollect survey data on goat crossbreeding practices (Djallonké × Sahelian × exotic breeds) in Bénin. The pipeline covers household identification, reproduction, health/veterinary follow-up, environmental context, and herd composition, then applies Chi-squared and Monte Carlo tests to selected variables.

## Project Structure

```
Data_cleaning/     Sequential cleaning scripts (unite1cleaning.R → unite7cleaning.R)
                    Each script renames raw KoboCollect columns to coded,
                    analysis-ready variables (e.g. 1=Masculin, 2=Feminin) and
                    exports an intermediate file (data1.xlsx → data7.xlsx).

Test_CHI2/          Chi-squared independence tests on qualitative variables
                    (education, sex, marital status vs. zone, etc.), built on
                    a shared helper (chi2_shared.R) plus a qualitative-variable
                    extraction script (qualitative.R).

Monte_Carlo/        Monte Carlo simulation of Chi-squared tests
                    (chisq.test(..., simulate.p.value = TRUE)) used when
                    expected cell counts are too low for the standard
                    approximation.
```

## Pipeline Flow

1. **Data_cleaning** — Raw KoboCollect export is processed sequentially through `unite1cleaning.R` … `unite7cleaning.R`. Each script:
   - Loads the previous stage's output (`dataN.xlsx`)
   - Renames columns to embed their coding scheme directly in the header (e.g. `Sexe: 1=Masculin, 2=Feminin`)
   - Fills/recodes blank or free-text responses into standardized numeric codes
   - Verifies no unexpected empty cells remain
   - Exports the next stage (`dataN+1.xlsx`)
2. **Test_CHI2** — Consumes the final cleaned dataset to test independence between qualitative variables (e.g. education level, sex, marital status) and geographic zone.
3. **Monte_Carlo** — Re-runs Chi-squared tests with Monte Carlo p-value simulation for variables where expected counts violate the Chi-squared approximation's assumptions.

## Requirements

- R (≥ 4.0 recommended)
- R packages: `dplyr`, `stringr`, `readxl`, `openxlsx`

Install dependencies:
```r
install.packages(c("dplyr", "stringr", "readxl", "openxlsx"))
```

## Usage

Run the cleaning scripts in order from the `Data_cleaning/` folder:
```r
source("unite1cleaning.R")
source("unite2cleaning.R")
# ... through unite7cleaning.R
```

Each script expects its working directory to be set to its own folder and reads/writes local `.xlsx` files — update the hardcoded paths at the top of each script if your folder location differs.

Then run the statistical tests from `Test_CHI2/` or `Monte_Carlo/` as needed, e.g.:
```r
source("chi2_education.R")
source("marital_status_zone_test.R")
```

## License

Distributed under the MIT License — see [LICENSE](LICENSE) for details.

