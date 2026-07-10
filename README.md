# Stratification_Frame_US2026
This repo contains code for constructing Stratification Frame used to generate synthetic personas for political polling, for the 2026 US midterm election. It was used to create the cell level (demographics) stratification frame, process the CES data, and the congressional district level 2024 house vote shares; to be used for MrsP modeling in the downstream pipeline (which creates the full stratification frame).
Research project at the AI Pop Up Lab, University of Amsterdam.

## Instructions for use 

1. Clone this repository.
2. Download the raw data from Zenodo: https://zenodo.org/records/21285306.
3. Extract the downloaded files into the `Data_Raw/` directory.
4. Get a free U.S. Census Bureau API key (https://api.census.gov/data/key_signup.html), set once via census_api_key() and persisted in .Renviron. Will be required Script4 onwards.
5. Install the required R packages by running:
   ```r
   source("install_packages.R")
   ```
6. Run the scripts in the `Scripts/` folder using one of the following methods:
   - **Sequentially:** Run the scripts in numerical order (e.g., `Script1_3...`, `Script4...`,...). You can either open each script directly in R or use `runner.R`, which provides a convenient way to execute one script at a time.
   - **As a batch:** Use `run_all.R` to execute all scripts automatically. Alternatively, you can configure `run_all.R` to run only a selected range of scripts (e.g., from Script *i* to Script *j*).
