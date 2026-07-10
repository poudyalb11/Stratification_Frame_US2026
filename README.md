# Stratification_Frame_US2026
This repo contains code for constructing Stratification Frame used to generate synthetic personas for political polling, for the 2026 US midterm election. It was used to create the cell level (demographics) stratification frame, process the CES data, and the congressional district level 2024 house vote shares; to be used for MrsP modeling in the downstream pipeline (which creates the full stratification frame).
Research project at the AI Pop Up Lab, University of Amsterdam.

## Instructions for use 

1. Clone this repo
2. Download raw data from Zenodo https://zenodo.org/records/21285306, and extract to `Data_Raw/`
3. Install required R packages: by running `source("install_packages.R")` in R
4. Run scripts in the Scripts folder in numeric order (Script1_3, Script4, ...), or run them as a batch (using run_all.R file -- within which you can either run all scripts at once or optionally run a subset from Script i to Script j)
