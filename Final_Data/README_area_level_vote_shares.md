# Area-Level Vote Shares for 2026 U.S. House Election Modeling

## Files
- `area_level_vote_shares.csv` — 435 rows × 44 columns
- `area_level_vote_shares.rds` — same data, R-native format

## Purpose
This file provides CD-level area covariates for use in the MrsP pipeline.
Each row is a 2026 congressional district, with:
- 2024 House vote shares (real for stable CDs, imputed via CART for redistricted CDs)
- 2024 Presidential vote shares (state-level, constant within state)
- Demographic composition (29 marginal proportions, used to impute redistricted CDs)
- Modeling flags (redistricting status, contestation, imputation status)

## Geographic Coverage
- 50 U.S. states (DC and territories excluded)
- 435 congressional districts (2026 boundaries)

## Key Columns

### Identifiers
| Column      | Type     | Description |
|-------------|----------|-------------|
| state_cd    | char     | Unique national CD identifier (e.g., "CA-1"); primary key |
| state_abbrv | char     | 2-letter state abbreviation (e.g., "CA") |
| cd_pop      | numeric  | Total citizen voting-age population (CVAP) in the CD, from 2023 ACS 5-year PUMS |

### 2024 House Vote Shares (CD-level)
The 4 shares represent the share of CVAP that voted for each option in the 2024
House election. Computed against `cd_pop` (CVAP), not against total votes cast,
so they naturally include a no_vote_share. Shares sum to 1 for non-imputed CDs;
for imputed CDs they are raw CART predictions and may not sum to 1.

| Column        | Type     | Description |
|---------------|----------|-------------|
| dem_share     | numeric  | Share of CVAP that voted Democratic in 2024 House |
| rep_share     | numeric  | Share of CVAP that voted Republican in 2024 House |
| other_share   | numeric  | Share of CVAP that voted for other parties in 2024 House |
| no_vote_share | numeric  | Share of CVAP that did NOT cast a House vote in 2024 |

For 318 CDs (`is_imputed = FALSE`): values are computed from MIT House 1976-2024
election data. For 117 redistricted CDs (`is_imputed = TRUE`): values are CART-
imputed using demographic + state-pres covariates + contestation as predictors,
trained on the 318 non-redistricted CDs.

### 2024 Presidential Vote Shares (state-level)
State-level shares used as area-level covariates. Values are constant within
each state (repeated across that state's CDs). Computed against state-level
voting-age population (sum of cd_pop within state), so they include a
no_vote_share. Computed from MIT 1976-2024 presidential data.

| Column                    | Type     | Description |
|---------------------------|----------|-------------|
| state_pres_dem_share      | numeric  | State-level share of CVAP that voted Democratic for President in 2024 |
| state_pres_rep_share      | numeric  | State-level share of CVAP that voted Republican for President in 2024 |
| state_pres_other_share    | numeric  | State-level share of CVAP that voted for other parties for President in 2024 |
| state_pres_no_vote_share  | numeric  | State-level share of CVAP that did NOT cast a Presidential vote in 2024 |

### Modeling Flags
| Column               | Type     | Description |
|----------------------|----------|-------------|
| is_redistricted      | logical  | TRUE if this CD was substantially redrawn between 2024 and 2026 (i.e., its 2026 boundaries do not overlap any single 2024 CD by ≥95% population). 117 CDs flagged TRUE. |
| contestation         | logical  | TRUE if the 2024 House race (for non-redistricted CDs) or the expected 2026 House race (for redistricted CDs) has opposition from both major parties. FALSE for uncontested races (no real opposition from the other major party). 33 CDs flagged FALSE: 29 stable CDs with 2024 uncontested races + 4 known 2026-uncontested CDs (CA-14, CA-29, CA-40, FL-10). |
| is_imputed           | logical  | TRUE if the 4 vote share columns were generated via CART imputation rather than real 2024 data. Equivalent to `is_redistricted`. 117 CDs flagged TRUE. |
| training_eligibility | char     | Pipeline-internal label: "training_set" for non-redistricted CDs (used to fit CART), "prediction_set" for redistricted CDs (CART predictions used). |

### Demographic Composition (29 columns)
The columns `pct_*` give marginal demographic proportions for each CD, used as
predictors in the CART imputation. They are not joint distributions; the joint
distribution is in the stratification frame file. All values are proportions
of cd_pop, summing to 1 within each demographic category.

Categories represented:
- **Age** (14 columns: `pct_age_18_22`, `pct_age_23_27`, ..., `pct_age_83_plus`)
- **Gender** (2 columns: `pct_female`, `pct_male`)
- **Race** (5 columns: `pct_race_white`, `pct_race_black`, `pct_race_native_american`, `pct_race_asian`, `pct_race_other_multi`)
- **Hispanic ethnicity** (2 columns: `pct_hisp_hispanic`, `pct_hisp_not_hispanic`)
- **Educational attainment** (6 columns: `pct_educ_no_hs`, `pct_educ_hs_grad`, `pct_educ_some_college`, `pct_educ_two_year`, `pct_educ_four_year`, `pct_educ_post_grad`)

These match the demographic categories in the stratification frame
(stratification_frame_2026_preMrsP). Joining the two files on `state_cd`
allows cell-level use of the area covariates.

## Vote Share Sum Diagnostic
- Non-imputed CDs (318): all 4 shares sum to exactly 1.0
- Imputed CDs (117): raw CART predictions; sum ranges 0.45 to 1.35
  (no post-hoc normalization applied; the MrsP downstream raking step
  will handle the simplex constraint)

## Notes on Imputation
The 117 imputed CDs come from 7 states with substantial 2025 redistricting
(CA, FL, MO, NC, OH, TX, UT). For each redistricted CD, CART (rpart in R)
was used to predict 2024 vote shares from 29 demographic predictors + 4
state-pres predictors + 1 contestation predictor (34 total). Four separate
maximal trees (no pruning) were fit, one per outcome, on the 318 non-
redistricted CDs as training data. Predicted shares are reported raw
(without normalization to sum to 1).

## Coverage Statistics
- Total CDs: 435
- Training set (non-redistricted, real 2024 data): 318
- Prediction set (redistricted, CART-imputed): 117
- Contested races: 402
- Uncontested races: 33

