# Stratification_Frame_US2026

This repo contains code for constructing the Stratification Frame used to generate synthetic personas for political polling in the 2026 US midterm election. It creates:

- The cell-level (demographic) stratification frame
- Processed CES survey data with 2024 vote choice
- CD-level 2024 House vote shares (with CART-imputed shares for redistricted CDs)

These are the inputs to MrsP (Multilevel Regression with Synthetic Poststratification) modeling in the downstream pipeline that produces the full 2026 vote share predictions.

Research project at the AI Pop Lab, University of Amsterdam.

## Instructions for use

1. Clone this repository.
2. Download the raw data from Zenodo: https://doi.org/10.5281/zenodo.21285306
3. Extract the downloaded files into the `Data_Raw/` directory.
4. Get a free U.S. Census Bureau API key (https://api.census.gov/data/key_signup.html), install it via `census_api_key()`, and persist it to `.Renviron`. Required from Script 4 onwards.
5. Install the required R packages:
```r
   source("install_packages.R")
```
6. Run the scripts in the `Scripts/` folder using one of the following methods:
   - **Sequentially:** Run scripts in numerical order (e.g., `Script1_3...`, `Script4...`, ...). Open each script directly in R, or use `runner.R` for a convenient way to execute one at a time.
   - **As a batch:** Use `run_all.R` to execute all scripts automatically. Alternatively, configure `run_all.R` to run only a selected range (e.g., from Script *i* to Script *j*).

## Outputs

After running the pipeline, three deliverables appear in `Data_Final/`:

- **`stratification_frame_2026_preMrsP.csv/.rds`**: The demographic stratification frame — one row per (demographic × CD) cell with weighted population counts, ~498K cells covering 435 CDs.
- **`area_level_vote_shares.csv/.rds`**: CD-level 2024 House vote shares with CART-imputed values for the ~117 redistricted CDs where 2024 shares don't map onto 2026 boundaries.
- **`ces_2024_for_mrsp.csv/.rds`**: Processed CES 2024 survey data with harmonized demographics, 2026 CD assignments, and constructed vote_2024 variable.


## Repository structure

Each script (refactored and organized) and their readmes are in the `Scripts` folder. The full repo structure is as follows:

- `Scripts/` — Pipeline scripts (numbered by execution order) and their per-script READMEs
- `Data_Raw/` — Raw input files, downloaded from Zenodo (gitignored)
- `Data_Processed/` — Intermediate outputs, produced by scripts (gitignored)
- `Data_Final/` — Final data outputs (tracked)
- `Development_Script_Original/` — Original script file run to generate the data used for downstream pipelines
- `Final_Data_Original/` — Original final data outputs used for downstream pipelines

The original outputs used for our research can be found in the Final_Data_Original folder. The original script file can be found under the Development_Script_Original folder.

## Data Sources

The raw data was collected from various third party sources. Please see data_sources.md file at https://zenodo.org/records/21285306 

## Contact

Binam Poudyal: binampoudyal@gmail.com or binam.poudyal@student.uva.nl  |   Roberto Cerina: r.cerina@uva.nl
