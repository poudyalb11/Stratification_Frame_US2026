# CES 2024 Individual-Level Data for MrsP Modeling

## Files
- `ces_2024_for_mrsp.csv` — 69,020 rows × 15 columns
- `ces_2024_for_mrsp.rds` — same data, R-native format (preserves factor levels)

## Purpose
This file contains individual-level Cooperative Election Study (CES) 2024
respondent data, prepared for use in the MrsP pipeline as the training data
for the multinomial vote-choice model. Each row is a (respondent × candidate
2026 CD) combination; respondents whose ZCTA spans multiple 2026 CDs appear
in multiple rows with an allocation factor (afact) for each candidate CD.

## Row Structure
- 59,280 unique respondents (caseid)
- 50,970 respondents with a single row (afact = 1, ZCTA contained entirely
  within one 2026 CD)
- 8,310 respondents with multiple rows (max 4), one row per candidate 2026 CD;
  afact values for one respondent sum to 1
- Total rows: 69,020

### Example of multi-CD respondent
A respondent in a ZCTA that spans 3 CDs (CA-12, CA-13, CA-14) with population
shares 60% / 30% / 10% appears as 3 rows:

| caseid    | state_cd | afact |
|-----------|----------|-------|
| 123456789 | CA-12    | 0.60  |
| 123456789 | CA-13    | 0.30  |
| 123456789 | CA-14    | 0.10  |

The afact values sum to 1 across all rows for a given respondent.

## Geographic Coverage
- 50 U.S. states (DC and territories excluded)
- 435 congressional districts (2026 boundaries)

## Columns

### Respondent identifier
| Column | Type    | Description |
|--------|---------|-------------|
| caseid | numeric | Unique respondent ID from CES (assigned by YouGov) |

### Survey weights
For population-representative analysis, observations must be weighted by
the appropriate survey weight. Three weight-related columns are provided.

| Column            | Type    | NAs    | Description |
|-------------------|---------|--------|-------------|
| commonweight      | numeric | 0      | CES pre-election wave survey weight, calibrated to the U.S. adult population |
| commonpostweight  | numeric | 12,328 | CES post-election wave survey weight; calibrated for respondents who completed both waves, adjusted for post-wave attrition. NA for respondents who did not complete the post wave |
| tookpost          | numeric | 0      | Post-wave completion flag (YouGov coding: 1 = did NOT complete post-wave, 2 = completed post-wave). 56,692 respondents completed the post wave |

Note: The choice between commonweight and commonpostweight is a methodological
decision pending team discussion. vote_2024 is derived from a waterfall through
both pre- and post-election variables, so respondents who did not complete the
post wave still have valid vote_2024 values.

### Demographic variables for cell mapping
These categories match the stratification frame
(stratification_frame_2026_preMrsP). Joining on these enables cell-level
poststratification.

| Column       | Type   | Levels | Description |
|--------------|--------|--------|-------------|
| age_cat      | factor | 14     | Age group: 18-22, 23-27, 28-32, 33-37, 38-42, 43-47, 48-52, 53-57, 58-62, 63-67, 68-72, 73-77, 78-82, 83+ |
| gender_cat   | char   | 2      | Female, Male |
| race_cat     | factor | 5      | White, Black, Native American, Asian, Other/Multi |
| hispanic_cat | factor | 2      | Hispanic, Not Hispanic (1 NA) |
| educ_cat     | factor | 6      | No HS, HS grad, Some college, 2-year, 4-year, Post-grad |

### Geographic identifiers
| Column      | Type    | Description |
|-------------|---------|-------------|
| state_cat   | numeric | State FIPS code (1-56 possible; 50 unique in data) |
| cd_cat      | integer | Congressional district number within state (1-52) |
| state_abbrv | char    | 2-letter state abbreviation (e.g., "CA") |
| state_cd    | char    | National CD identifier (e.g., "CA-1"); joins to `area_level_vote_shares.csv` |

### Allocation factor
| Column | Type    | Description |
|--------|---------|-------------|
| afact  | numeric | Allocation factor for respondents whose ZCTA spans multiple 2026 CDs. afact sums to 1 across a respondent's rows. For respondents in a ZCTA contained in a single 2026 CD, afact = 1. |

### Outcome
| Column    | Type   | Levels | Description |
|-----------|--------|--------|-------------|
| vote_2024 | factor | 4      | Self-reported 2024 House vote choice: Democratic, Republican, Other, No Vote. 3,449 NAs for respondents whose source variables (CES vote choice questions) were all missing |

## vote_2024 Construction
Derived from a waterfall through 5 CES variables, in priority order:
CC24_412 → CC24_401 → CC24_367_voted → CC24_367 → CC24_363

The first non-missing value (according to this priority) determines vote_2024.
Variables CC24_412 and CC24_401 are post-election wave (asked of all post-wave
respondents). Variables CC24_367_voted, CC24_367, and CC24_363 are pre-election
wave: CC24_367_voted captures vote choice for respondents who had already voted
early at the time of the pre-election interview, while CC24_367 and CC24_363
capture vote intent for respondents who had not yet voted. Source variables are
not included in this deliverable.

## Coverage Statistics
- Total respondents: 59,280
- Total rows (with multi-CD allocation): 69,020
- Post-wave completers: 56,692 (95.6%)
- vote_2024 valid: 65,571 (95.0% of rows)

## Survey Weighting for MrsP

For each row, the effective survey weight is:

  effective_weight = chosen_weight × afact

where:
- chosen_weight is either commonweight or commonpostweight, depending on the
  team's methodological decision (pending discussion)
- afact is the allocation factor (1.0 for single-CD respondents, fractional
  for multi-CD)

For a single-CD respondent: effective_weight = chosen_weight (afact = 1).

For a multi-CD respondent with N rows: the N effective weights sum to
chosen_weight (since afact sums to 1 across the rows). The respondent's total
contribution is the same as a single-CD respondent; it is just distributed
across multiple candidate CDs proportionally to the probability of residence.

## Cell Mapping
Each row maps to a stratification frame cell via:
(state_cd, age_cat, gender_cat, race_cat, hispanic_cat, educ_cat)

This combination matches the cell-level rows in
`stratification_frame_2026_preMrsP.csv`.

## Area-Level Covariate Join
Each row's state_cd joins to `area_level_vote_shares.csv` for CD-level area
covariates (2024 House shares, state-level 2024 Pres shares, contestation flag,
etc.).

