# Older adults' intentions to adopt a Mobility-as-a-Service application

This repository contains the R analysis used to examine older adults' behavioural intentions to adopt a Mobility-as-a-Service (MaaS) mobile application. The study applies an integrated Technology Acceptance Model and Diffusion of Innovation Theory framework.

## Repository files

| File | Description |
| --- | --- |
| `analysis.R` | Reproducible analysis script |
| `data.csv` | Survey input file, with one row per participant |
| `.gitignore` | Excludes generated results and local R files |

The script creates a `results/` directory containing the analysis tables and diagnostics.

## Requirements

Install [R](https://cran.r-project.org/) and the required R packages:

```r
install.packages(c("cSEM", "openxlsx"))
```

## Run the analysis

1. Put `analysis.R` and `data.csv` in the same directory.
2. Open that directory as the working directory in R or RStudio.
3. Run:

```r
source("analysis.R")
```

The outputs will be saved automatically in the `results/` directory.

## Input data

The input is a comma-separated file named `data.csv`. Each row represents a survey respondent. Variable names are case-sensitive. The main model uses these indicator columns:

- **Attitude:** `ATT2`, `ATT3`, `ATT4`
- **Perceived ease of use:** `PEOU1`, `PEOU2`, `PEOU4`
- **Perceived usefulness:** `PU1`, `PU2`, `PU3`
- **Relative advantage:** `RA2`, `RA3`
- **Compatibility:** `COM1`, `COM2`, `COM3`
- **Complexity:** `CPX1`, `CPX2`, `CPX3`
- **Observability:** `OBS3`, `OBS4`
- **Trialability:** `TRI1`, `TRI3`, `TRI4`
- **Behavioural intention:** `BI1`, `BI2`, `BI4`
- **Age:** `Age`

For descriptive and group analyses, the dataset also uses `Sex`, `Education`, `Public_transport_Use`, `Ride_User`, and `Education_Level`. The last three analyses involving group comparisons use binary values `0` and `1` for `Ride_User`, `Sex` and `Education_Level`; other categories are retained as recorded.

Reverse-coded source items may be supplied as columns ending in `_R` (for example, `ATT4_R`). When present, the corresponding corrected item is calculated as `6 - original` on the five-point response scale. Otherwise, the script uses the corrected indicator column directly. A `Dummy` column may be supplied for the auxiliary full-collinearity model; if absent, the script generates a binary dummy variable using the fixed random seed.

## Analysis workflow

The script runs the following analyses in sequence:

1. Data preparation and descriptive statistics
2. PLS-SEM model estimation
3. Measurement quality (loadings, reliability, average variance extracted, HTMT2)
4. Structural paths, bootstrapping, explained variance and collinearity
5. Full-collinearity VIF assessment using an auxiliary single-indicator dummy-variable construct
6. Out-of-sample prediction with PLSpredict and a linear regression benchmark
7. MICOM assessment of measurement invariance
8. Exploratory Mann–Whitney group comparisons

## Reproducibility settings

The analysis uses random seed `123`. HTMT2 confidence intervals use 5,000 bootstrap draws and a 95% two-sided percentile interval. Structural inference uses 10,000 bootstrap draws. PLSpredict uses 10 cross-validation folds and one repetition. MICOM uses 5,000 resamples. The script saves R session information in `results/session_info.txt`.

## Software

The analyses use the [`cSEM`](https://cran.r-project.org/package=cSEM) and [`openxlsx`](https://cran.r-project.org/package=openxlsx) R packages.
