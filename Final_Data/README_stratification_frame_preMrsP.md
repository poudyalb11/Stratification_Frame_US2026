# Stratification Frame for 2026 U.S. House Election Modeling -- Pre MrsP

## Files
- `stratification_frame_2026.csv` — 497,836 rows × 10 columns
- `stratification_frame_2026.rds` — same data, R-native format (preserves factor levels)

## Purpose
This file is the demographic stratification frame for poststratification in
the MrsP pipeline. Each row is a demographic cell within a 2026 congressional
district, with a weighted population count derived from the U.S. Census Bureau's
2023 ACS 5-year PUMS, harmonized to 2026 congressional district boundaries.

Total weighted citizen voting-age population across all cells: ~240.45 million.

## Geographic Coverage
- 50 U.S. states (DC and territories excluded)
- 435 congressional districts (2026 boundaries)
- For 7 states with substantial 2025 redistricting (CA, FL, MO, NC, OH, TX, UT),
  PUMS data was reassigned from PUMA to 2026 CD using a population-weighted
  block-level crosswalk.

## Columns

| Column        | Type    | Description |
|---------------|---------|-------------|
| state_cat     | numeric | State FIPS code (numeric, 1-56 possible; 1-50 in the dataset) |
| cd_cat        | integer | Congressional district number (1-52); CDs use 1 for at-large states |
| age_cat       | factor  | Age group (14 levels): 18-22, 23-27, 28-32, 33-37, 38-42, 43-47, 48-52, 53-57, 58-62, 63-67, 68-72, 73-77, 78-82, 83+ |
| gender_cat    | char    | Gender (2 levels): Female, Male |
| race_cat      | factor  | Race (5 levels): White, Black, Native American, Asian, Other/Multi |
| hispanic_cat  | factor  | Hispanic ethnicity (2 levels): Hispanic, Not Hispanic |
| educ_cat      | factor  | Educational attainment (6 levels): No HS, HS grad, Some college, 2-year, 4-year, Post-grad |
| cell_pop      | numeric | Weighted citizen voting-age population in this cell |
| state_abbrv   | char    | 2-letter state abbreviation (e.g., "CA") |
| state_cd      | char    | State-CD identifier (e.g., "CA-1"); Provides a unique national identifier for each congressional district (CD numbers reset within each state, so this combined key is needed to disambiguate) |

## Cell Structure

Each unique (state_cd, age_cat, gender_cat, race_cat, hispanic_cat, educ_cat)
combination appears exactly once. The full joint distribution of demographics
× geography is captured in the cell_pop column.

Cells with cell_pop == 0 (1,190 of 497,836) represent demographic-geographic
combinations that exist as possibilities but have no estimated population in
the ACS data. 

## Generation

Generated from harmonized 2023 ACS 5-year PUMS data, processed with a
PUMA-to-2026-CD population-weighted crosswalk built from Census block-level
boundaries. Detailed methodology will be available in the project documentation.

