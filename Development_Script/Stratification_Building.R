library(tidyverse)
library(data.table)
library(ipumsr)
library(janitor)
library(haven)

# ══════════════════════════════════════════════════════════════════════════════
# Sub-script 1: Loading Files and Initial checks
# ══════════════════════════════════════════════════════════════════════════════
# ── 1. File paths ──────────────────────────────────────────────────────────────
# Update these to wherever you saved your files
ddi_path  <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/usa_00003.xml"
data_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/usa_00003.csv.gz"
geo_path  <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/geocorr2022_2610104623.csv"

# ── 2. Load IPUMS extract ──────────────────────────────────────────────────────
ddi  <- read_ipums_ddi(ddi_path)
pums <- read_ipums_micro(ddi, data_file = data_path)

# ── 3. Quick sanity checks ─────────────────────────────────────────────────────
# Row and column count
cat("Rows:", nrow(pums), "\n")
cat("Cols:", ncol(pums), "\n")

# Check key variables exist
cat("\nVariable names:\n")
print(names(pums))

# Distribution of GQ (group quarters) -- we'll filter these out next
cat("\nGQ distribution:\n")
print(table(pums$GQ))

# Check STATEFIP looks right (should be 1-56, no Puerto Rico)
cat("\nUnique states:", n_distinct(pums$STATEFIP), "\n")

# Quick check on person weights -- should all be positive
cat("\nPERWT summary:\n")
print(summary(pums$PERWT))


# Check code mappings for all variables of concern
ipums_val_labels(ddi, "GQ")
ipums_val_labels(ddi, "RACED")
ipums_val_labels(ddi, "HISPAND")
ipums_val_labels(ddi, "CITIZEN")
ipums_val_labels(ddi, "EDUCD")
ipums_val_labels(ddi, "SEX")


# Print full RACED codes
ipums_val_labels(ddi, "RACED") %>% print(n = Inf)

# Print full EDUCD codes
ipums_val_labels(ddi, "EDUCD") %>% print(n = Inf)

# Print full HISPAND codes
ipums_val_labels(ddi, "HISPAND") %>% print(n = Inf)



# ══════════════════════════════════════════════════════════════════════════════
# Subscript 2: Filtering 
# Purpose: Clean the raw PUMS extract 
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# DIAGNOSTIC: Weighted and unweighted counts by code for each variable
# Purpose: Understand the scale of each category before making drop decisions
# Using PERWT to get population estimates, not just sample counts
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)

# Helper function: weighted and unweighted counts for any variable
code_counts <- function(data, var) {
  data %>%
    group_by({{ var }}) %>%
    summarise(
      n_records    = n(),                    # raw sample count
      n_weighted   = sum(PERWT),             # population estimate
      pct_weighted = round(sum(PERWT) / sum(data$PERWT) * 100, 3)
    ) %>%
    arrange(desc(n_weighted))
}

# ── GQ ────────────────────────────────────────────────────────────────────────
cat("═══ GQ (Group Quarters) ═══\n")
code_counts(pums, GQ) %>% print()

# ── CITIZEN ───────────────────────────────────────────────────────────────────
cat("\n═══ CITIZEN ═══\n")
code_counts(pums, CITIZEN) %>% print()

# ── SEX ───────────────────────────────────────────────────────────────────────
cat("\n═══ SEX ═══\n")
code_counts(pums, SEX) %>% print()

# ── HISPAND ───────────────────────────────────────────────────────────────────
# Collapse to Hispanic vs Not Hispanic vs Not Reported for readability
cat("\n═══ HISPAND (collapsed) ═══\n")
pums %>%
  mutate(hisp_group = case_when(
    HISPAND == 0   ~ "Not Hispanic",
    HISPAND == 900 ~ "Not Reported",
    TRUE           ~ "Hispanic"
  )) %>%
  group_by(hisp_group) %>%
  summarise(
    n_records  = n(),
    n_weighted = sum(PERWT),
    pct        = round(sum(PERWT) / sum(pums$PERWT) * 100, 3)
  ) %>%
  arrange(desc(n_weighted)) %>%
  print()

# ── RACED (top-level groups only) ─────────────────────────────────────────────
cat("\n═══ RACED (collapsed to major groups) ═══\n")
pums %>%
  mutate(race_group = case_when(
    HISPAND > 0 & HISPAND < 900          ~ "Hispanic",
    HISPAND == 900                       ~ "Hispanic - Not Reported",
    RACED >= 100 & RACED <= 177          ~ "NH-White",
    RACED >= 200 & RACED <= 234          ~ "NH-Black",
    RACED >= 300 & RACED <= 399          ~ "NH-AIAN",
    RACED >= 400 & RACED <= 629          ~ "NH-Asian",
    RACED >= 635 & RACED <= 679          ~ "NH-Asian",
    RACED >= 630 & RACED <= 634          ~ "NH-NativeHawaiian_PI",
    RACED >= 680 & RACED <= 699          ~ "NH-NativeHawaiian_PI",
    RACED >= 700 & RACED <= 730          ~ "NH-Other",
    RACED >= 801 & RACED <= 997          ~ "NH-Multiracial",
    TRUE                                 ~ "Uncategorised"
  )) %>%
  group_by(race_group) %>%
  summarise(
    n_records  = n(),
    n_weighted = sum(PERWT),
    pct        = round(sum(PERWT) / sum(pums$PERWT) * 100, 3)
  ) %>%
  arrange(desc(n_weighted)) %>%
  print()

# ── EDUCD ─────────────────────────────────────────────────────────────────────
cat("\n═══ EDUCD (collapsed to major groups) ═══\n")
pums %>%
  mutate(educ_group = case_when(
    EDUCD >= 0   & EDUCD <= 61  ~ "No HS",
    EDUCD >= 62  & EDUCD <= 64  ~ "HS Grad",
    EDUCD >= 65  & EDUCD <= 83  ~ "Some College",
    EDUCD >= 100 & EDUCD <= 101 ~ "4-Year Degree",
    EDUCD >= 110 & EDUCD <= 116 ~ "Post-Grad",
    EDUCD == 999                ~ "Missing",
    TRUE                        ~ "Uncategorised"
  )) %>%
  group_by(educ_group) %>%
  summarise(
    n_records  = n(),
    n_weighted = sum(PERWT),
    pct        = round(sum(PERWT) / sum(pums$PERWT) * 100, 3)
  ) %>%
  arrange(desc(n_weighted)) %>%
  print()

# ── HHINCOME ──────────────────────────────────────────────────────────────────
cat("\n═══ HHINCOME special values ═══\n")
pums %>%
  mutate(inc_flag = case_when(
    HHINCOME == 9999999 ~ "N/A code",
    HHINCOME < 0        ~ "Negative income",
    HHINCOME == 0       ~ "Zero income",
    TRUE                ~ "Valid positive"
  )) %>%
  group_by(inc_flag) %>%
  summarise(
    n_records  = n(),
    n_weighted = sum(PERWT),
    pct        = round(sum(PERWT) / sum(pums$PERWT) * 100, 3)
  ) %>%
  arrange(desc(n_weighted)) %>%
  print()



# ══════════════════════════════════════════════════════════════════════════════
# DIAGNOSTIC: Full detailed category counts for RACED, HISPAND, EDUCD
# Purpose: Check for missing, NA, or unexpected codes before recoding
# ══════════════════════════════════════════════════════════════════════════════

# ── RACED full distribution ───────────────────────────────────────────────────
cat("═══ RACED full distribution ═══\n")
raced_labels <- ipums_val_labels(ddi, "RACED") %>% rename(raced_label = lbl)

pums_filtered %>%
  filter(HISPAND == 0) %>%   # non-Hispanics only for RACED
  left_join(raced_labels, by = c("RACED" = "val")) %>%
  group_by(RACED, raced_label) %>%
  summarise(
    n_records  = n(),
    n_weighted = sum(PERWT),
    pct        = round(sum(PERWT) / sum(pums_filtered$PERWT) * 100, 4),
    .groups = "drop"
  ) %>%
  arrange(desc(n_weighted)) %>%
  print(n = Inf)

# ── HISPAND full distribution ─────────────────────────────────────────────────
cat("\n═══ HISPAND full distribution ═══\n")
hispand_labels <- ipums_val_labels(ddi, "HISPAND") %>% rename(hispand_label = lbl)

pums_filtered %>%
  filter(HISPAND > 0) %>%    # Hispanics only
  left_join(hispand_labels, by = c("HISPAND" = "val")) %>%
  group_by(HISPAND, hispand_label) %>%
  summarise(
    n_records  = n(),
    n_weighted = sum(PERWT),
    pct        = round(sum(PERWT) / sum(pums_filtered$PERWT) * 100, 4),
    .groups = "drop"
  ) %>%
  arrange(desc(n_weighted)) %>%
  print(n = Inf)

# ── EDUCD full distribution ───────────────────────────────────────────────────
cat("\n═══ EDUCD full distribution ═══\n")
educd_labels <- ipums_val_labels(ddi, "EDUCD") %>% rename(educd_label = lbl)

pums_filtered %>%
  left_join(educd_labels, by = c("EDUCD" = "val")) %>%
  group_by(EDUCD, educd_label) %>%
  summarise(
    n_records  = n(),
    n_weighted = sum(PERWT),
    pct        = round(sum(PERWT) / sum(pums_filtered$PERWT) * 100, 4),
    .groups = "drop"
  ) %>%
  arrange(desc(n_weighted)) %>%
  print(n = Inf)

# ── Check for any RACED codes in data not in DDI labels ──────────────────────
cat("\n═══ RACED codes in data with no matching DDI label ═══\n")
pums_filtered %>%
  filter(HISPAND == 0) %>%
  anti_join(raced_labels, by = c("RACED" = "val")) %>%
  group_by(RACED) %>%
  summarise(
    n_records  = n(),
    n_weighted = sum(PERWT)
  ) %>%
  print()

# ── Check for any HISPAND codes in data not in DDI labels ────────────────────
cat("\n═══ HISPAND codes in data with no matching DDI label ═══\n")
pums_filtered %>%
  filter(HISPAND > 0) %>%
  anti_join(hispand_labels, by = c("HISPAND" = "val")) %>%
  group_by(HISPAND) %>%
  summarise(
    n_records  = n(),
    n_weighted = sum(PERWT)
  ) %>%
  print()


# DIAGNOSTIC: NA audit across all key variables
cat("\n═══ NA audit across all key variables ═══\n")
pums %>%
  summarise(
    across(
      c(GQ, CITIZEN, SEX, HISPAND, RACED, EDUCD, HHINCOME, AGE, PERWT, 
        STATEFIP, PUMA, CPUMA1020),
      list(
        n_na       = ~sum(is.na(.)),
        pct_na     = ~round(sum(is.na(.)) / n() * 100, 4),
        n_weighted_na = ~sum(PERWT[is.na(.)], na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to  = c("variable", "stat"),
    names_sep = "_(?=[^_]+$)"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(desc(na)) %>%
  print(n = Inf)

##---------------------------------------
##Filtering
##--------------------------------------
###Filtering GQ 
##-- drop 0: non vacant units -- existing data count:0
##-- drop 6: fragments  --- existing data count:0   
# Keep all valid person records
# Drop only vacant units (0) and geographic fragments (6)
# Retain institutional GQ 3 per Roberto's recommendation
pums_filtered <- pums %>%
  filter(!GQ %in% c(0, 6))

cat("Rows after GQ filter:", nrow(pums_filtered), "\n")
cat("Rows removed:", nrow(pums) - nrow(pums_filtered), "\n")
cat("Weighted pop removed:", 
    sum(pums$PERWT) - sum(pums_filtered$PERWT), "\n")
cat("Pct weighted pop removed:", 
    round((sum(pums$PERWT) - sum(pums_filtered$PERWT)) / 
            sum(pums$PERWT) * 100, 3), "%\n")

# Drop non-citizens (CITIZEN == 3)
# CITIZEN == 0 (N/A) = US-born citizens, question not applicable to them -- KEEP
# CITIZEN == 1 = Born abroad of American parents -- KEEP
# CITIZEN == 2 = Naturalized citizen -- KEEP
# Codes 4, 5, 8, 9 not present in data

pums_filtered <- pums_filtered %>%
  filter(!CITIZEN %in% c(3))

cat("Rows after citizenship filter:", nrow(pums_filtered), "\n")
cat("Rows removed:", nrow(pums %>% filter(!GQ %in% c(0,6))) - nrow(pums_filtered), "\n")
cat("Weighted pop removed:",
    sum(pums$PERWT) - sum(pums_filtered$PERWT), "\n")



# Keep only voting eligible age (18+)
# AGE is exact years in IPUMS, no special codes
pums_filtered <- pums_filtered %>%
  filter(AGE >= 18)

cat("Rows after age filter:", nrow(pums_filtered), "\n")
cat("Rows removed:", 15244470 - nrow(pums_filtered), "\n")
cat("Weighted pop removed:",
    sum(pums %>% filter(!CITIZEN %in% c(3)) %>% pull(PERWT)) - 
      sum(pums_filtered$PERWT), "\n")
cat("Pct weighted pop removed:",
    round((sum(pums %>% filter(!CITIZEN %in% c(3)) %>% pull(PERWT)) - 
             sum(pums_filtered$PERWT)) /
            sum(pums %>% filter(!CITIZEN %in% c(3)) %>% pull(PERWT)) * 100, 3), "%\n")



# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 3: Variable Recoding
# Purpose: Attach clean human-readable labels to all demographic variables
#          NO binning or aggregation -- Aggregation done in later stages/scripts
# Input:   pums_filtered (12,263,785 rows, post all filters)
# Output:  pums_clean
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(ipumsr)

# ── 1. Load label lookups from DDI ───────────────────────────────────────────
hispand_labels <- ipums_val_labels(ddi, "HISPAND") %>% rename(hispanic_detailed = lbl)
raced_labels   <- ipums_val_labels(ddi, "RACED")   %>% rename(race_detailed     = lbl)
educd_labels   <- ipums_val_labels(ddi, "EDUCD")   %>% rename(educ_detailed     = lbl)


# ── 2. Gender ─────────────────────────────────────────────────────────────────
# SEX codes: 1 = Male, 2 = Female
# Binary for merge integrity with CES
# Non-binary handled as post-hoc persona layer only

pums_clean <- pums_filtered %>%
  mutate(gender = case_when(
    SEX == 1 ~ "Male",
    SEX == 2 ~ "Female",
    TRUE     ~ NA_character_
  ))

cat("Gender distribution:\n")
print(table(pums_clean$gender, useNA = "always"))


# ── 3. Hispanic flag ──────────────────────────────────────────────────────────
# Binary flag: 0 = Not Hispanic, 1 = Hispanic
# Kept separate from hispanic_detailed for modeling flexibility

pums_clean <- pums_clean %>%
  mutate(hispanic_flag = case_when(
    HISPAND == 0 ~ 0L,
    HISPAND >  0 ~ 1L,
    TRUE         ~ NA_integer_
  ))

cat("\nHispanic flag distribution:\n")
print(table(pums_clean$hispanic_flag, useNA = "always"))


# ── 4. Hispanic detailed label ────────────────────────────────────────────────
# Full HISPAND label for every record -- includes "Not Hispanic"
# No aggregation -- all 24 detailed categories retained

pums_clean <- pums_clean %>%
  left_join(hispand_labels, by = c("HISPAND" = "val"))

cat("\nHispanic detailed -- top 10 by weighted count:\n")
pums_clean %>%
  group_by(hispanic_detailed) %>%
  summarise(
    n_records  = n(),
    n_weighted = sum(PERWT),
    pct        = round(sum(PERWT) / sum(pums_clean$PERWT) * 100, 3)
  ) %>%
  arrange(desc(n_weighted)) %>%
  print(n = 10)

cat("Distinct hispanic categories:", n_distinct(pums_clean$hispanic_detailed), "\n")
cat("Hispanic NAs:", sum(is.na(pums_clean$hispanic_detailed)), "\n")


# ── 5. Race detailed label ────────────────────────────────────────────────────
# Full RACED label for every record -- no aggregation, no Hispanic-first rule

pums_clean <- pums_clean %>%
  left_join(raced_labels, by = c("RACED" = "val"))

cat("\nRace detailed -- top 10 by weighted count:\n")
pums_clean %>%
  group_by(race_detailed) %>%
  summarise(
    n_records  = n(),
    n_weighted = sum(PERWT),
    pct        = round(sum(PERWT) / sum(pums_clean$PERWT) * 100, 3)
  ) %>%
  arrange(desc(n_weighted)) %>%
  print(n = 10)

cat("Distinct race categories:", n_distinct(pums_clean$race_detailed), "\n")
cat("Race NAs:", sum(is.na(pums_clean$race_detailed)), "\n")


# ── 6. Education detailed label ───────────────────────────────────────────────
# Full EDUCD label -- all 24 categories retained

pums_clean <- pums_clean %>%
  left_join(educd_labels, by = c("EDUCD" = "val"))

cat("\nEducation detailed -- top 10 by weighted count:\n")
pums_clean %>%
  group_by(educ_detailed) %>%
  summarise(
    n_records  = n(),
    n_weighted = sum(PERWT),
    pct        = round(sum(PERWT) / sum(pums_clean$PERWT) * 100, 3)
  ) %>%
  arrange(desc(n_weighted)) %>%
  print(n = 10)

cat("Distinct education categories:", n_distinct(pums_clean$educ_detailed), "\n")
cat("Education NAs:", sum(is.na(pums_clean$educ_detailed)), "\n")


# ── 7. Income ─────────────────────────────────────────────────────────────────
# HHINCOME already in 2024 dollars for this 5-year file
# Convert 9999999 (N/A placeholder) to proper R NA
# Zero and negative values are legitimate -- retained as-is
# Exact dollar amounts kept -- binning happens at modeling stage

pums_clean <- pums_clean %>%
  mutate(hhincome_clean = if_else(
    HHINCOME == 9999999, NA_real_, as.numeric(HHINCOME)
  ))

cat("\nIncome summary:\n")
print(summary(pums_clean$hhincome_clean))
cat("NA income records:      ", sum(is.na(pums_clean$hhincome_clean)), "\n")
cat("Zero income records:    ", sum(pums_clean$hhincome_clean == 0, na.rm = TRUE), "\n")
cat("Negative income records:", sum(pums_clean$hhincome_clean < 0,  na.rm = TRUE), "\n")


# ── 8. NA audit ───────────────────────────────────────────────────────────────
cat("\n── NA audit across all recoded variables ──\n")
pums_clean %>%
  select(gender, hispanic_flag, hispanic_detailed,
         race_detailed, educ_detailed, hhincome_clean) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  print()


# ── 9. Select final columns ───────────────────────────────────────────────────
# Raw variables retained for auditability alongside cleaned versions
# AGE kept as exact years -- binning at modeling stage

pums_clean <- pums_clean %>%
  select(
    # Identifiers and geography
    SERIAL, PERNUM, STATEFIP, PUMA, CPUMA1020,
    # Weights and variance estimation
    PERWT, HHWT, CLUSTER, STRATA,
    # Raw variables retained for auditability
    AGE, SEX, RACE, RACED, HISPAN, HISPAND, CITIZEN, EDUC, EDUCD, HHINCOME, GQ,
    # Cleaned and labeled variables
    gender, hispanic_flag, hispanic_detailed,
    race_detailed, educ_detailed, hhincome_clean
  )


# ── 10. Final summary ─────────────────────────────────────────────────────────
cat("\n── Final dataset ──\n")
cat("Dimensions:", nrow(pums_clean), "rows x", ncol(pums_clean), "cols\n")
cat("Columns:\n")
print(names(pums_clean))
cat("\nDistinct categories:\n")
cat("Race:     ", n_distinct(pums_clean$race_detailed), "\n")
cat("Hispanic: ", n_distinct(pums_clean$hispanic_detailed), "\n")
cat("Education:", n_distinct(pums_clean$educ_detailed), "\n")
cat("\nReady for PUMA-to-CD crosswalk join.\n")


#---- 11. Save to disk --------#
saveRDS(pums_clean, 
        file = "/Users/binampoudyal/Downloads/Stratification_Frame_Building/pums_clean.rds")

cat("Saved successfully.\n")
cat("File size:", 
    round(file.size("/Users/binampoudyal/Downloads/Stratification_Frame_Building/pums_clean.rds") / 1e6, 1), 
    "MB\n")





# ══════════════════════════════════════════════════════════════════════════════
# SubCRIPT 4: PUMA-to-CD Crosswalk
# ══════════════════════════════════════════════════════════════════════════════

#Loading geocorr data
# ── Read description row first ────────────────────────────────────────────────
crosswalk_desc <- read_csv(geo_path, n_max = 1)
cat("Column descriptions:\n")
print(crosswalk_desc)

# ── Now read actual data skipping the description row ─────────────────────────
crosswalk_raw <- read_csv(geo_path, skip = 1)
cat("\nDimensions:", nrow(crosswalk_raw), "rows x", ncol(crosswalk_raw), "cols\n")
cat("\nColumn names:\n")
print(names(crosswalk_raw))
cat("\nFirst 10 rows:\n")


print(head(crosswalk_raw, 10))



# ── 2. Rename columns to clean short names ────────────────────────────────────
crosswalk <- crosswalk_raw %>%
  rename(
    state     = `State code`,
    puma22    = `PUMA (2022)`,
    cd119     = `Congressional district code (119th Congress)`,
    stab      = `State abbr.`,
    puma_name = `PUMA22 name`,
    pop20     = `Total population (2020 Census)`,
    afact2    = `cd119-to-puma22 allocation factor`,
    afact     = `puma22-to-cd119 allocation factor`
  )

cat("\nColumn names after rename:\n")
print(names(crosswalk))


# ── 3. Check and fix data types ───────────────────────────────────────────────
# state and puma22 must be integer to match STATEFIP and PUMA in pums_clean
# cd119 must be integer for the final frame
# afact must be numeric (should already be from skip=1 read)

cat("\nData types before conversion:\n")
print(sapply(crosswalk, class))

crosswalk <- crosswalk %>%
  mutate(
    state  = as.integer(state),
    puma22 = as.integer(puma22),
    cd119  = as.integer(cd119),
    pop20  = as.numeric(pop20),
    afact  = as.numeric(afact),
    afact2 = as.numeric(afact2)
  )

cat("\nData types after conversion:\n")
print(sapply(crosswalk, class))


# ── 4. Basic diagnostics ──────────────────────────────────────────────────────
cat("\nDimensions:", nrow(crosswalk), "rows x", ncol(crosswalk), "cols\n")
cat("Unique states:", n_distinct(crosswalk$state), "\n")
cat("Unique PUMAs (state+puma):", n_distinct(paste(crosswalk$state, crosswalk$puma22)), "\n")
cat("Unique CDs:", n_distinct(crosswalk$cd119), "\n")
cat("\nFirst 10 rows:\n")
print(head(crosswalk, 10))


# ── 5. Validate afact sums to 1 per state+PUMA ───────────────────────────────
# Every PUMA's afact values across all its CDs must sum to 1.0
# If not, there is a data integrity problem in the crosswalk

cat("\n── afact validation ──\n")
afact_check <- crosswalk %>%
  group_by(state, puma22) %>%
  summarise(afact_sum = round(sum(afact), 4), .groups = "drop")

cat("PUMAs where afact does NOT sum to 1.0:\n")
afact_check %>%
  filter(afact_sum != 1.0) %>%
  print()

cat("afact sum range:", min(afact_check$afact_sum), 
    "to", max(afact_check$afact_sum), "\n")


# ── 6. Check for NAs ──────────────────────────────────────────────────────────
cat("\n── NA check ──\n")
crosswalk %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  print()


# ── 7. Check afact range ──────────────────────────────────────────────────────
cat("\nafact summary:\n")
print(summary(crosswalk$afact))
cat("\nafact2 summary:\n")
print(summary(crosswalk$afact2))




n_distinct(paste(crosswalk$state, crosswalk$cd119))

# Check near-zero afact values
crosswalk %>%
  filter(afact < 0.001) %>%
  arrange(afact) %>%
  print(n = 20)


# See all unique state+CD combinations and identify the extras
crosswalk %>%
  distinct(state, stab, cd119) %>%
  arrange(state, cd119) %>%
  group_by(state, stab) %>%
  summarise(
    n_cds = n(),
    cd_codes = paste(cd119, collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_cds > 1 | state %in% c(11)) %>%  # flag DC and multi-CD states
  print(n = Inf)


# Quick check -- which states have unusual CD codes
crosswalk %>%
  distinct(state, stab, cd119) %>%
  filter(cd119 == 0 | cd119 == 98 | cd119 == 99) %>%
  print()

# How many rows would be removed at different thresholds
cat("Rows with afact == 0:    ", sum(crosswalk$afact == 0), "\n")
cat("Rows with afact < 0.001:", sum(crosswalk$afact < 0.001), "\n")
cat("Rows with afact < 0.01: ", sum(crosswalk$afact < 0.01), "\n")
cat("Rows with afact < 0.05: ", sum(crosswalk$afact < 0.05), "\n")
cat("\nTotal rows:", nrow(crosswalk), "\n")
cat("Rows remaining after afact >= 0.001:", sum(crosswalk$afact >= 0.001), "\n")

#---------------------------------------------------------
#----------------------------------------
#Script for Block to Tract to PUMA to CD crosswalk building 
#------------------------------------
#----------------------------------------------------
library(readr)
#Census Tract to 2020 PUMA relationship file for all states
X2020_Census_Tract_to_2020_PUMA <- read_csv("/Users/binampoudyal/Downloads/rstudio-export/2020_Census_Tract_to_2020_PUMA.txt")
tract_to_puma <- X2020_Census_Tract_to_2020_PUMA

tract_to_puma_clean <- tract_to_puma %>%
  mutate(tract_geoid = paste0(STATEFP, COUNTYFP, TRACTCE)) %>%
  select(tract_geoid, statefp = STATEFP, puma = PUMA5CE)

##Tidycensus to extract 2020 population data
library(tidycensus)
census_api_key("1cb6990638d07553bb0999196d09156692f9621e", install = TRUE, overwrite =  TRUE)
readRenviron("~/.Renviron")


######## TEXAS ##############

tx_baf <- read_csv("/Users/binampoudyal/Downloads/rstudio-export/PLANC2333.csv",
                   col_types = cols(
                     SCTBKEY  = col_character(),
                     DISTRICT = col_integer()
                   ))




library(tidycensus)
library(tidyverse)

# ── 1. Pull Texas block-level 2020 population ────────────────────────────────
cat("Pulling Texas block populations (this may take 1-2 minutes)...\n")

tx_blocks <- get_decennial(
  geography = "block",
  variables = "P1_001N",
  year      = 2020,
  sumfile   = "pl",
  state     = "TX"
)

cat("Block records pulled:", nrow(tx_blocks), "\n")
cat("Total Texas pop:     ", sum(tx_blocks$value), "\n")

# Verify GEOID is 15 digits
cat("\nGEOID length distribution:\n")
print(table(nchar(tx_blocks$GEOID)))


# ── 2. Join BAF to block populations ─────────────────────────────────────────
# Each block now has both CD assignment and population

tx_blocks_full <- tx_baf %>%
  inner_join(
    tx_blocks %>% select(GEOID, pop = value),
    by = c("SCTBKEY" = "GEOID")
  )

cat("\nBlocks in BAF:                ", nrow(tx_baf), "\n")
cat("Blocks after join with pops:  ", nrow(tx_blocks_full), "\n")
cat("Texas pop accounted for:      ", sum(tx_blocks_full$pop), "\n")


# ── 3. Aggregate blocks to tract × CD level ──────────────────────────────────
# For each (tract, CD) intersection, sum block populations

tx_tract_cd_pop <- tx_blocks_full %>%
  mutate(tract_geoid = substr(SCTBKEY, 1, 11)) %>%
  group_by(tract_geoid, cd120 = DISTRICT) %>%
  summarise(
    pop_in_intersection = sum(pop),
    n_blocks            = n(),
    .groups = "drop"
  )

cat("\nTract x CD intersections:", nrow(tx_tract_cd_pop), "\n")


# ── 4. Join to tract → PUMA ──────────────────────────────────────────────────
# Now each tract×CD row has a PUMA assignment

tx_tract_puma_cd <- tx_tract_cd_pop %>%
  inner_join(
    tract_to_puma_clean %>% filter(statefp == "48"),
    by = "tract_geoid"
  )

cat("\nRows after PUMA join:", nrow(tx_tract_puma_cd), "\n")
cat("Unique PUMAs:        ", n_distinct(tx_tract_puma_cd$puma), "\n")


# ── 5. Aggregate to PUMA × CD level ──────────────────────────────────────────

tx_puma_cd <- tx_tract_puma_cd %>%
  group_by(state = "48", puma, cd120) %>%
  summarise(
    pop_intersection = sum(pop_in_intersection),
    .groups = "drop"
  ) %>%
  group_by(state, puma) %>%
  mutate(
    puma_pop = sum(pop_intersection),
    afact    = pop_intersection / puma_pop
  ) %>%
  ungroup()

cat("\nFinal PUMA x CD120 rows:", nrow(tx_puma_cd), "\n")
cat("Unique PUMAs:           ", n_distinct(tx_puma_cd$puma), "\n")
cat("Unique CDs:             ", n_distinct(tx_puma_cd$cd120), "\n")


# ── 6. Validate ──────────────────────────────────────────────────────────────
afact_check <- tx_puma_cd %>%
  group_by(state, puma) %>%
  summarise(afact_sum = sum(afact), .groups = "drop")

cat("\nafact sum range:", round(min(afact_check$afact_sum), 6), 
    "to", round(max(afact_check$afact_sum), 6), "\n")
cat("PUMAs where afact != 1:", sum(round(afact_check$afact_sum, 4) != 1), "\n")

cat("\nFirst 10 rows of TX PUMA-CD120 crosswalk:\n")
print(head(tx_puma_cd, 10))

cat("\nDistribution of CDs per PUMA:\n")
tx_puma_cd %>%
  group_by(state, puma) %>%
  summarise(n_cds = n(), .groups = "drop") %>%
  count(n_cds) %>%
  print()

sink()


saveRDS(tx_puma_cd, "tx_puma_cd_crosswalk.rds")
cat("File size:", round(file.size("tx_puma_cd_crosswalk.rds")/1e3, 1), "KB\n")


####General script for any state################

# ══════════════════════════════════════════════════════════════════════════════
# Build PUMA × CD120 crosswalk for redistricted states
# 
# Function: build_state_puma_cd_crosswalk()
# 
# Takes a state's Block Assignment File (BAF) plus identifying info,
# pulls block populations from tidycensus, and produces a PUMA × CD
# crosswalk with population-weighted afact values.
#
# Requires `tract_to_puma_clean` to be loaded in the environment.
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(tidycensus)


build_state_puma_cd_crosswalk <- function(
    baf_path,
    state_abb,
    state_fips,
    baf_block_col   = "SCTBKEY",
    baf_district_col = "DISTRICT",
    baf_delim       = ",",
    baf_has_header  = TRUE,
    output_dir      = ".",
    log_file        = NULL
) {
  
  if (!is.null(log_file)) sink(log_file)
  
  cat("══════════════════════════════════════════════════\n")
  cat("Building crosswalk for:", state_abb, "(FIPS", state_fips, ")\n")
  cat("══════════════════════════════════════════════════\n\n")
  
  # ── 1. Read BAF -- detect format from file extension ───────────────────────
  cat("Reading BAF from", baf_path, "...\n")
  
  ext <- tools::file_ext(baf_path)
  
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' required for Excel files. Run: install.packages('readxl')")
    }
    baf <- readxl::read_excel(baf_path) %>%
      mutate(across(everything(), as.character))
  } else if (baf_has_header) {
    baf <- read_delim(
      baf_path,
      delim = baf_delim,
      col_types = cols(.default = col_character())
    )
  } else {
    baf <- read_delim(
      baf_path,
      delim = baf_delim,
      col_names = c(baf_block_col, baf_district_col),
      col_types = cols(.default = col_character())
    )
  }
  
  baf <- baf %>%
    rename(
      block_geoid = !!baf_block_col,
      district    = !!baf_district_col
    ) %>%
    mutate(district = as.integer(district))
  
  cat("BAF rows:", nrow(baf), "\n")
  cat("Unique CDs in BAF:", n_distinct(baf$district), "\n\n")
  
  # ── 2. Pull block-level populations ────────────────────────────────────────
  cat("Pulling block populations for", state_abb, "(this may take 1-2 minutes)...\n")
  
  blocks <- get_decennial(
    geography = "block",
    variables = "P1_001N",
    year      = 2020,
    sumfile   = "pl",
    state     = state_abb
  )
  
  cat("Block records pulled:", nrow(blocks), "\n")
  cat("Total state pop:     ", sum(blocks$value), "\n\n")
  
  # ── 3. Join BAF to block populations ───────────────────────────────────────
  blocks_full <- baf %>%
    inner_join(
      blocks %>% select(GEOID, pop = value),
      by = c("block_geoid" = "GEOID")
    )
  
  cat("Blocks in BAF:               ", nrow(baf), "\n")
  cat("Blocks after join with pops: ", nrow(blocks_full), "\n")
  cat("Pop accounted for:           ", sum(blocks_full$pop), "\n\n")
  
  # ── 4. Aggregate blocks to tract × CD ──────────────────────────────────────
  tract_cd_pop <- blocks_full %>%
    mutate(tract_geoid = substr(block_geoid, 1, 11)) %>%
    group_by(tract_geoid, cd_new = district) %>%
    summarise(
      pop_in_intersection = sum(pop),
      n_blocks            = n(),
      .groups = "drop"
    )
  
  cat("Tract x CD intersections:", nrow(tract_cd_pop), "\n\n")
  
  # ── 5. Join to tract → PUMA ────────────────────────────────────────────────
  tract_puma_cd <- tract_cd_pop %>%
    inner_join(
      tract_to_puma_clean %>% filter(statefp == state_fips),
      by = "tract_geoid"
    )
  
  cat("Rows after PUMA join:", nrow(tract_puma_cd), "\n")
  cat("Unique PUMAs:        ", n_distinct(tract_puma_cd$puma), "\n\n")
  
  # ── 6. Aggregate to PUMA × CD ──────────────────────────────────────────────
  puma_cd <- tract_puma_cd %>%
    group_by(state = state_fips, puma, cd_new) %>%
    summarise(
      pop_intersection = sum(pop_in_intersection),
      .groups = "drop"
    ) %>%
    group_by(state, puma) %>%
    mutate(
      puma_pop = sum(pop_intersection),
      afact    = pop_intersection / puma_pop
    ) %>%
    ungroup()
  
  cat("Final PUMA × CD rows:", nrow(puma_cd), "\n")
  cat("Unique PUMAs:        ", n_distinct(puma_cd$puma), "\n")
  cat("Unique CDs:          ", n_distinct(puma_cd$cd_new), "\n\n")
  
  # ── 7. Validation ──────────────────────────────────────────────────────────
  afact_check <- puma_cd %>%
    group_by(state, puma) %>%
    summarise(afact_sum = sum(afact), .groups = "drop")
  
  cat("afact sum range:", round(min(afact_check$afact_sum), 6),
      "to", round(max(afact_check$afact_sum), 6), "\n")
  cat("PUMAs where afact != 1:", sum(round(afact_check$afact_sum, 4) != 1), "\n\n")
  
  cat("Distribution of CDs per PUMA:\n")
  puma_cd %>%
    group_by(state, puma) %>%
    summarise(n_cds = n(), .groups = "drop") %>%
    count(n_cds) %>%
    print()
  
  cat("\nFirst 10 rows:\n")
  print(head(puma_cd, 10))
  
  # ── 8. Save ─────────────────────────────────────────────────────────────────
  out_path <- file.path(
    output_dir,
    paste0(tolower(state_abb), "_puma_cd_crosswalk.rds")
  )
  saveRDS(puma_cd, out_path)
  cat("\nSaved to:", out_path, "\n")
  cat("File size:", round(file.size(out_path) / 1e3, 1), "KB\n")
  
  if (!is.null(log_file)) sink()
  
  return(puma_cd)
}

##California (when BAF downloaded)

# Peek at the raw file to confirm delimiter
ca_raw_peek <- readLines("/Users/binampoudyal/Downloads/ab604.csv", n = 5)
cat(ca_raw_peek, sep = "\n")

# Check the byte representation to see exact delimiter
cat("\nByte view of first line:\n")
charToRaw(ca_raw_peek[1])


ca_puma_cd <- build_state_puma_cd_crosswalk(
  baf_path        = "/Users/binampoudyal/Downloads/ab604.csv",
  state_abb       = "CA",
  state_fips      = "06",
  baf_delim       = ",",           # comma-separated
  baf_has_header  = FALSE,         # no header row
  output_dir      = "/Users/binampoudyal/Downloads/"
)

###Missouri
mo_puma_cd <- build_state_puma_cd_crosswalk(
  baf_path        = "/Users/binampoudyal/Downloads/HB1_Missouri_Congressional_Districts_2025_BEF.xlsx",  # update path
  state_abb       = "MO",
  state_fips      = "29",
  baf_block_col   = "Block",
  baf_district_col = "DistrictID",
  output_dir      = "/Users/binampoudyal/Downloads"
)

###North Carolina
nc_puma_cd <- build_state_puma_cd_crosswalk(
  baf_path        = "/Users/binampoudyal/Downloads/NCGA_CCM-2 .csv",  # update path
  state_abb       = "NC",
  state_fips      = "37",
  baf_block_col   = "Block",
  baf_district_col = "District",
  output_dir      = "/Users/binampoudyal/Downloads"
)

#Ohio
oh_puma_cd <- build_state_puma_cd_crosswalk(
  baf_path         = "/Users/binampoudyal/Downloads/October 31 2025 CD BAF.xlsx",
  state_abb        = "OH",
  state_fips       = "39",
  baf_block_col    = "Block",
  baf_district_col = "DistrictID:1",
  output_dir       = "/Users/binampoudyal/Downloads"
)

#Utah
utah_puma_cd <- build_state_puma_cd_crosswalk(
  baf_path         = "/Users/binampoudyal/Downloads/ut_cong_adopted_2025_baf.csv",
  state_abb        = "UT",
  state_fips       = "49",
  baf_block_col    = "GEOID20",
  baf_district_col = "DISTRICT",
  output_dir       = "/Users/binampoudyal/Downloads"
)

#florida
florida_puma_cd <- build_state_puma_cd_crosswalk(
  baf_path         = "/Users/binampoudyal/Downloads/EOGPCRP2026.csv",
  state_abb        = "FL",
  state_fips       = "12",
  baf_block_col    = "block_geoid",
  baf_district_col = "district",
  baf_has_header   = FALSE,
  output_dir       = "/Users/binampoudyal/Downloads"
)


#-------------------------------------------------------------
#Script: Merge Created and Geocorr crosswalks
#
#---------------------------------------------------------

# Load all 7 state-specific crosswalks
tx_xw  <- readRDS("/Users/binampoudyal/Downloads/tx_puma_cd_crosswalk.rds")
tx_xw <- rename(tx_xw, cd_new = cd120)

ca_xw  <- readRDS("/Users/binampoudyal/Downloads/ca_puma_cd_crosswalk.rds")
mo_xw  <- readRDS("/Users/binampoudyal/Downloads/mo_puma_cd_crosswalk.rds")
nc_xw  <- readRDS("/Users/binampoudyal/Downloads/nc_puma_cd_crosswalk.rds")
oh_xw  <- readRDS("/Users/binampoudyal/Downloads/oh_puma_cd_crosswalk.rds")
ut_xw  <- readRDS("/Users/binampoudyal/Downloads/ut_puma_cd_crosswalk.rds")
fl_xw  <- readRDS("/Users/binampoudyal/Downloads/fl_puma_cd_crosswalk.rds")

# Standardise the new state crosswalks to match Geocorr structure
new_states <- bind_rows(tx_xw, ca_xw, mo_xw, nc_xw, oh_xw, ut_xw, fl_xw) %>%
  transmute(
    state  = as.integer(state),
    puma22 = as.integer(puma),
    cd119  = as.integer(cd_new),         # keep column name 'cd119' for compatibility
    afact  = afact
  )

cat("New state crosswalk rows:", nrow(new_states), "\n")
cat("Unique state+CD combos: ", 
    n_distinct(paste(new_states$state, new_states$cd119)), "\n")


# FIPS codes of the redistricted states
redistricted_fips <- c(48, 6, 29, 37, 39, 49, 12)  # TX, CA, MO, NC, OH, UT, FL

# Trim Geocorr to non-redistricted states
geocorr_stable <- crosswalk %>%
  filter(!state %in% redistricted_fips) %>%
  select(state, puma22, cd119, afact)

cat("\nGeocorr stable-states rows:", nrow(geocorr_stable), "\n")

# Append new state crosswalks
unified_crosswalk <- bind_rows(geocorr_stable, new_states)

cat("\nUnified crosswalk rows:    ", nrow(unified_crosswalk), "\n")
cat("Unique states:             ", n_distinct(unified_crosswalk$state), "\n")
cat("Unique PUMAs:              ", 
    n_distinct(paste(unified_crosswalk$state, unified_crosswalk$puma22)), "\n")
cat("Unique state+CD combos:    ", 
    n_distinct(paste(unified_crosswalk$state, unified_crosswalk$cd119)), "\n")


# Validation
afact_check <- unified_crosswalk %>%
  group_by(state, puma22) %>%
  summarise(afact_sum = sum(afact), .groups = "drop")

cat("\nafact sum range:", round(min(afact_check$afact_sum), 6),
    "to", round(max(afact_check$afact_sum), 6), "\n")
cat("PUMAs where afact != 1:", sum(round(afact_check$afact_sum, 4) != 1), "\n")

#Drop PR
unified_crosswalk <- unified_crosswalk %>%
  filter(state != 72)

cat("Rows after dropping PR:", nrow(unified_crosswalk), "\n")
cat("Unique states:          ", n_distinct(unified_crosswalk$state), "\n")
cat("Unique state+CD combos: ", 
    n_distinct(paste(unified_crosswalk$state, unified_crosswalk$cd119)), "\n")

# Save
saveRDS(unified_crosswalk, 
        "/Users/binampoudyal/Downloads/unified_crosswalk_2026.rds")




# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 5: PUMA-to-CD Crosswalk Join (UPDATED)
# Purpose: Join pums_clean to UNIFIED crosswalk (Geocorr + redistricted state BAFs)
#
# Key logic:
#   - Join on STATEFIP + PUMA from pums_clean to state + puma22 in crosswalk
#   - left_join preserves all pums_clean records
#   - Person records in split PUMAs get multiple rows -- one per CD
#   - Each row's effective weight = PERWT * afact
#   - DC (state=11, cd=98) retained for now and will be filtered at frame
#     construction stage
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)

# Read cleaned PUMS file
pums_clean <- readRDS(
  "/Users/binampoudyal/Downloads/Stratification_Frame_Building/pums_clean.rds"
)

# Read unified crosswalk (Geocorr + redistricted states)
unified_crosswalk <- readRDS(
  "/Users/binampoudyal/Downloads/unified_crosswalk_2026.rds"
)


# ── 1. Join pums_clean to unified crosswalk ──────────────────────────────────
# Join key: STATEFIP (pums) = state (crosswalk)
#           PUMA (pums)     = puma22 (crosswalk)

pums_crosswalked <- pums_clean %>%
  left_join(
    unified_crosswalk,
    by = c("STATEFIP" = "state", "PUMA" = "puma22"),
    relationship = "many-to-many"
  )

cat("Rows before join:", nrow(pums_clean), "\n")
cat("Rows after join: ", nrow(pums_crosswalked), "\n")
cat("Row increase:    ", nrow(pums_crosswalked) - nrow(pums_clean), "\n")
cat("(increase expected -- split PUMA persons get multiple rows)\n")


# ── 2. Create adjusted person weight ─────────────────────────────────────────
# PERWT_adj = PERWT * afact
# Effective population weight for each person-CD assignment

pums_crosswalked <- pums_crosswalked %>%
  mutate(PERWT_adj = PERWT * afact)

cat("\nPERWT_adj summary:\n")
print(summary(pums_crosswalked$PERWT_adj))


# ── 3. Check for unmatched records ───────────────────────────────────────────
cat("\nUnmatched records (cd119 is NA):", 
    sum(is.na(pums_crosswalked$cd119)), "\n")

if(sum(is.na(pums_crosswalked$cd119)) > 0) {
  cat("\nUnmatched PUMA codes:\n")
  pums_crosswalked %>%
    filter(is.na(cd119)) %>%
    distinct(STATEFIP, PUMA) %>%
    print()
}


# ── 4. Verify weighted population preservation ───────────────────────────────
cat("\nWeighted population check:\n")
cat("Original PERWT sum:    ", sum(pums_clean$PERWT), "\n")
cat("PERWT_adj sum:         ", round(sum(pums_crosswalked$PERWT_adj), 0), "\n")
cat("Difference:            ", 
    round(sum(pums_crosswalked$PERWT_adj) - sum(pums_clean$PERWT), 0), "\n")


# ── 5. Quick sanity check on CD distribution ─────────────────────────────────
cat("\nTop 10 CDs by weighted population:\n")
pums_crosswalked %>%
  group_by(STATEFIP, cd119) %>%
  summarise(
    n_records  = n(),
    pop_weight = round(sum(PERWT_adj), 0),
    .groups = "drop"
  ) %>%
  arrange(desc(pop_weight)) %>%
  print(n = 10)

cat("\nTotal unique state+CD combinations:", 
    n_distinct(paste(pums_crosswalked$STATEFIP, pums_crosswalked$cd119)), "\n")


# ── 6. Final dimensions ──────────────────────────────────────────────────────
cat("\n── Crosswalked dataset ──\n")
cat("Dimensions:", nrow(pums_crosswalked), "rows x", 
    ncol(pums_crosswalked), "cols\n")
cat("Ready for cell aggregation.\n")

# Save
saveRDS(pums_crosswalked, 
        "/Users/binampoudyal/Downloads/Stratification_Frame_Building/pums_crosswalked.rds")


cat("Saved successfully.\n")
cat("RDS File size:", 
    round(file.size("/Users/binampoudyal/Downloads/Stratification_Frame_Building/pums_crosswalked.rds") / 1e6, 1), 
    "MB\n")

pums_crosswalked <- readRDS("/Users/binampoudyal/Downloads/Stratification_Frame_Building/pums_crosswalked.rds")


# ── Check overlap between hispanic_flag and race_detailed ─────────────────────

# 1. Does race_detailed have explicit Hispanic categories?
cat("══ Unique values in race_detailed containing 'Hispanic' or 'Latin' ══\n")
unique(pums_crosswalked$race_detailed)[
  grepl("Hispanic|Latin|Latino|Mexican|Puerto|Cuban", 
        unique(pums_crosswalked$race_detailed), 
        ignore.case = TRUE)
]

# 2. Cross-tabulation: hispanic_flag vs race_detailed (top 20 races)
cat("\n══ Cross-tab: hispanic_flag x race_detailed (top 20 by record count) ══\n")
pums_crosswalked %>%
  count(hispanic_flag, race_detailed, sort = TRUE) %>%
  head(20) %>%
  print()

# 3. How are Hispanic-flagged people distributed across race_detailed categories?
cat("\n══ Race breakdown of Hispanic-flagged respondents ══\n")
pums_crosswalked %>%
  filter(hispanic_flag == 1) %>%
  count(race_detailed, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  head(20) %>%
  print()

# 4. How are non-Hispanic-flagged people distributed across race_detailed?
cat("\n══ Race breakdown of NON-Hispanic-flagged respondents ══\n")
pums_crosswalked %>%
  filter(hispanic_flag == 0) %>%
  count(race_detailed, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  head(10) %>%
  print()

# value counts
table(pums_crosswalked$cd119)

# how many unique CDs
n_distinct(pums_crosswalked$cd119)

# any NAs?
sum(is.na(pums_crosswalked$cd119))

# range
range(pums_crosswalked$cd119, na.rm = TRUE)

# rows with 0 or 98
pums_crosswalked %>% filter(cd119 %in% c(0, 98)) %>% nrow()

# which states have cd119 == 0
pums_crosswalked %>% 
  filter(cd119 == 0) %>% 
  distinct(STATEFIP) %>% 
  arrange(STATEFIP)

# which states have cd119 == 98
pums_crosswalked %>% 
  filter(cd119 == 98) %>% 
  distinct(STATEFIP) %>% 
  arrange(STATEFIP)

n_distinct(paste(pums_crosswalked$STATEFIP, pums_crosswalked$cd119))


# 1. How many unique cd119 values overall?
n_distinct(crosswalk$cd119)

# 2. How many unique state + cd119 combinations?
n_distinct(paste(crosswalk$state, crosswalk$cd119))

# 3. Range of cd119 values
range(crosswalk$cd119)

# 4. Look at one specific state to see what cd119 looks like for it
crosswalk %>%
  filter(state == 6) %>%      # California -- should have 1 through 52
  distinct(cd119) %>%
  arrange(cd119) %>%
  print(n = Inf)

# 5. Same check for a smaller state
crosswalk %>%
  filter(state == 36) %>%     # New York -- should have 1 through 26
  distinct(cd119) %>%
  arrange(cd119) %>%
  print(n = Inf)

# 6. CDs per state -- should match the known House seat distribution
crosswalk %>%
  distinct(state, stab, cd119) %>%
  filter(!cd119 %in% c(0, 98)) %>%
  count(state, stab, name = "n_cds") %>%
  arrange(desc(n_cds)) %>%
  print(n = Inf)



# CDs per state in the UNIFIED crosswalk
unified_crosswalk %>%
  distinct(state, cd119) %>%
  filter(!cd119 %in% c(0, 98)) %>%
  count(state, name = "n_cds") %>%
  arrange(desc(n_cds)) %>%
  print(n = Inf)

# total seat count (should be 429 nationally + 6 at-large = 435)
unified_crosswalk %>%
  distinct(state, cd119) %>%
  filter(!cd119 %in% c(0, 98)) %>%
  nrow()
# plus 6 at-large = should equal 435



pums_crosswalked %>%
  distinct(STATEFIP, cd119) %>%
  filter(!cd119 %in% c(0, 98)) %>%
  count(STATEFIP, name = "n_cds") %>%
  arrange(desc(n_cds)) %>%
  print(n = Inf)


# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 7: Initial CES (CCES) Exploration
# Purpose: Load the CES cumulative file and inventory its variables,
#          types, and category distributions before deciding what to use
#          for the modeling stages.
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)

# ── 1. Read CES file ──────────────────────────────────────────────────────────
ces_path <- "/Users/binampoudyal/Downloads/dataverse_files/CCES24_Common_OUTPUT_vv_topost_final.csv"  # UPDATE PATH

ces <- read_csv(ces_path)


# ── 2. Top-level dimensions ──────────────────────────────────────────────────
cat("══ Dimensions ══\n")
cat("Rows:   ", nrow(ces), "\n")
cat("Columns:", ncol(ces), "\n\n")


# ── 3. Column inventory ──────────────────────────────────────────────────────
cat("══ All columns and types ══\n")
ces_summary <- tibble(
  column = names(ces),
  type   = sapply(ces, function(x) class(x)[1]),
  n_unique = sapply(ces, function(x) n_distinct(x)),
  n_na     = sapply(ces, function(x) sum(is.na(x)))
)
print(ces_summary, n = Inf)


# ── 4. Sample of first rows ──────────────────────────────────────────────────
cat("\n══ First 5 rows ══\n")
print(head(ces, 5))


# ── 5. Distributions of likely-key categorical variables ─────────────────────
# Focus on low-cardinality columns since these are likely categorical
cat("\n══ Distributions for variables with ≤ 30 unique values ══\n")

low_card_cols <- ces_summary %>%
  filter(n_unique <= 30 & n_unique > 1) %>%
  pull(column)

for (col in low_card_cols) {
  cat("\n--- ", col, " ---\n", sep = "")
  ces %>%
    count(.data[[col]], sort = TRUE) %>%
    print(n = Inf)
}


# ── 6. Year coverage if a year column exists ─────────────────────────────────
cat("\n══ Year coverage ══\n")
year_cols <- names(ces)[grepl("year|YEAR|Year", names(ces))]
if (length(year_cols) > 0) {
  for (col in year_cols) {
    cat("\n--- ", col, " ---\n", sep = "")
    print(table(ces[[col]], useNA = "ifany"))
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 8: Variable Harmonization for MrP Cell Aggregation
# 
# Purpose: Match variables across the ACS PUMS frame (pums_crosswalked) and
#          the CES survey data (ces). For each demographic variable used in
#          poststratification, we need:
#            (a) Identical category definitions in both datasets
#            (b) A new harmonized column with the same name and codings
#          This script processes variables one at a time. Each variable gets:
#            - A binning/recoding function applied to both datasets
#            - Distribution checks to verify the codings line up
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)


# ── 1. AGE → age_cat ─────────────────────────────────────────────────────────
#
# Raw formats:
#   pums_crosswalked$AGE  : integer years, 18+
#   ces$age               : integer years, 18+
#
# Binning scheme: 5-year bins starting at 18.
#   Each bin spans 5 ages, inclusive on both ends (e.g. "18-22" = 18, 19, 20, 21, 22).
#   The top bin is open-ended (83+) since exact age past ~80 is less informative
#   for political behavior and the upper tail has fewer respondents.
#
# Why 5-year bins:
#   - Captures meaningful age gradients in turnout and partisanship
#   - Keeping fine granularity at frame stage; we can collapse
#     to broader bins later at modeling stage if cells are too sparse
#   - Standard ccesMRPprep package offers 5-year and 10-year keys; we use 5
#
# How cut() works:
#   breaks = c(17, 22, ...) with right = TRUE means bins are (lower, upper]
#   - (17, 22] → 18, 19, 20, 21, 22 → labeled "18-22"
#   - (22, 27] → 23, 24, 25, 26, 27 → labeled "23-27"
#   - (82, Inf] → 83 and above → labeled "83+"
#   The lowest break is 17 (not 18) to cleanly exclude any stray under-18 records
#   while including 18 in the first bin.


#first, calculate age and store as column in ces
ces <- ces %>%
  mutate(age = 2024 - birthyr)

cat("Age summary:\n")
summary(ces$age)

bin_age <- function(age) {
  cut(age,
      breaks = c(17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, Inf),
      labels = c("18-22", "23-27", "28-32", "33-37", "38-42", "43-47",
                 "48-52", "53-57", "58-62", "63-67", "68-72", "73-77",
                 "78-82", "83+"),
      right  = TRUE)
}

# Apply to PUMS frame -- creates new column age_cat alongside raw AGE
pums_crosswalked <- pums_crosswalked %>%
  mutate(age_cat = bin_age(AGE))

# Apply to CES survey data -- same binning, same column name
ces <- ces %>%
  mutate(age_cat = bin_age(age))

# Distribution check on PUMS
# This shows the share of each age bin in the demographic frame.
# Expected: monotonically declining shares from middle-age peak toward older bins,
# with relatively even shares for working-age bins.
cat("══ PUMS age_cat distribution ══\n")
pums_crosswalked %>%
  count(age_cat) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()

# Distribution check on CES
# This shows the share of each age bin in the survey sample.
# Expected: similar shape to PUMS but with some sampling differences --
# CES tends to slightly overrepresent middle-age engaged voters.
cat("\n══ CES age_cat distribution ══\n")
ces %>%
  count(age_cat) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()

# Side by side comparison
pums_dist <- pums_crosswalked %>%
  count(age_cat, wt = PERWT_adj) %>%   # use weighted counts for fair comparison
  mutate(pct_pums = round(100 * n / sum(n), 2)) %>%
  select(age_cat, pct_pums)

ces_dist <- ces %>%
  count(age_cat) %>%
  mutate(pct_ces = round(100 * n / sum(n), 2)) %>%
  select(age_cat, pct_ces)

comparison <- pums_dist %>%
  left_join(ces_dist, by = "age_cat") %>%
  mutate(diff = pct_ces - pct_pums)

cat("══ PUMS vs CES age distribution comparison ══\n")
print(comparison, n = Inf)


# ── 2. NEXT VARIABLE -- to be added ──────────────────────────────────────────
# Variables remaining to harmonize:
#   - gender / sex
#   - race + Hispanic (combined per Hispanic-first rule)
#   - education
#   - household income
#   - state + congressional district (geography)



# Inspect gender4 column structure
cat("══ Class and type of gender4 ══\n")
print(class(ces$gender4))
print(typeof(ces$gender4))

cat("\n══ Distribution of gender4 ══\n")
ces %>%
  count(gender4) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()

# Check if there's a haven_labelled attribute that gives us the labels
cat("\n══ Value labels (if labelled) ══\n")
if ("haven_labelled" %in% class(ces$gender4)) {
  print(attr(ces$gender4, "labels"))
} else {
  cat("Not a labelled vector. Raw values shown above.\n")
}

# Also check gender4_t for any text responses
cat("\n══ Sample of non-empty gender4_t responses ══\n")
ces %>%
  filter(!is.na(gender4_t) & gender4_t != "") %>%
  count(gender4_t, sort = TRUE) %>%
  head(20) %>%
  print()


# ── 2. GENDER → harmonize CES gender4 to PUMS binary gender ──────────────────
#
# Raw formats:
#   pums_crosswalked$gender : "Male" / "Female" (binary, from ACS SEX)
#   ces$gender4             : 1=Man, 2=Woman, 3=Non-binary, 4=Other
#
# Harmonized column: gender_cat
#   Both datasets get a new gender_cat column with values "Male" / "Female".
#   This keeps the harmonized cell-defining variables consistently named
#   (age_cat, gender_cat, ...) and distinct from the raw source columns.
#
# ACS only collects binary sex (Male/Female). Non-binary and Other CES 
# respondents cannot be matched to PUMS poststratification cells under this 
# structural constraint. We drop these 554 respondents (~0.93% of CES) since
# they cannot contribute to MrP estimation regardless.


# PUMS: just copy existing gender column into gender_cat
pums_crosswalked <- pums_crosswalked %>%
  mutate(gender_cat = gender)

# CES: filter out non-binary/Other, then create gender_cat
ces <- ces %>%
  filter(gender4 %in% c(1, 2)) %>%
  mutate(gender_cat = case_when(
    gender4 == 1 ~ "Male",
    gender4 == 2 ~ "Female"
  ))

# Verify distributions match between PUMS and CES
cat("══ PUMS gender_cat distribution (weighted) ══\n")
pums_crosswalked %>%
  count(gender_cat, wt = PERWT_adj) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()

cat("\n══ CES gender_cat distribution ══\n")
ces %>%
  count(gender_cat) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()

cat("\nCES rows after dropping non-binary/Other:", nrow(ces), "\n")


####-----------Education------------------###

# ── 3. EDUCATION → harmonize PUMS EDUCD to CES 6-category scheme ─────────────

# Define a single helper to label educ values consistently across both datasets
label_educ <- function(code) {
  factor(code,
         levels = 1:6,
         labels = c("No HS",
                    "HS grad",
                    "Some college",
                    "2-year",
                    "4-year",
                    "Post-grad"))
}

# Apply to PUMS -- use zap_labels to strip haven_labelled before mapping
pums_crosswalked <- pums_crosswalked %>%
  mutate(EDUCD_num = haven::zap_labels(EDUCD)) %>%
  mutate(educ_cat = case_when(
    EDUCD_num %in% c(2, 11, 12, 14, 15, 16, 17, 22, 23, 25, 26, 
                     30, 40, 50, 61)                  ~ 1L,
    EDUCD_num %in% c(63, 64)                          ~ 2L,
    EDUCD_num %in% c(65, 71)                          ~ 3L,
    EDUCD_num == 81                                   ~ 4L,
    EDUCD_num == 101                                  ~ 5L,
    EDUCD_num %in% c(114, 115, 116)                   ~ 6L,
    TRUE                                              ~ NA_integer_
  )) %>%
  mutate(educ_cat = label_educ(educ_cat)) %>%
  select(-EDUCD_num)

# Apply to CES -- educ is already 1-6 numeric, just label it
ces <- ces %>%
  mutate(educ_cat = label_educ(as.integer(educ)))

# Verify distributions
cat("══ PUMS educ_cat distribution (weighted) ══\n")
pums_crosswalked %>%
  count(educ_cat, wt = PERWT_adj) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()

cat("\n══ CES educ_cat distribution ══\n")
ces %>%
  count(educ_cat) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()


###-----Hispanic---------##


# ── 4. HISPANIC → harmonize CES hispanic to PUMS hispanic_flag ───────────────
#
# Raw formats:
#   pums_crosswalked$hispanic_flag : 0 = Not Hispanic, 1 = Hispanic
#   ces$hispanic                   : 1 = Yes (Hispanic), 2 = No (Not Hispanic)
#
# Harmonized column: hispanic_cat ("Hispanic" / "Not Hispanic" as factor)

# Helper to label
label_hispanic <- function(code) {
  factor(code,
         levels = c(1L, 0L),
         labels = c("Hispanic", "Not Hispanic"))
}

# Apply to PUMS
pums_crosswalked <- pums_crosswalked %>%
  mutate(hispanic_cat = label_hispanic(hispanic_flag))

# Apply to CES -- map 1->1, 2->0, NA stays NA
ces <- ces %>%
  mutate(hispanic_cat = case_when(
    hispanic == 1 ~ 1L,
    hispanic == 2 ~ 0L,
    TRUE          ~ NA_integer_
  )) %>%
  mutate(hispanic_cat = label_hispanic(hispanic_cat))

# Verify distributions
cat("══ PUMS hispanic_cat distribution (weighted) ══\n")
pums_crosswalked %>%
  count(hispanic_cat, wt = PERWT_adj) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()

cat("\n══ CES hispanic_cat distribution ══\n")
ces %>%
  count(hispanic_cat) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()

##--------


cat("══ Does hispanic_cat exist in ces? ══\n")
"hispanic_cat" %in% names(ces)

cat("\n══ First few rows of ces$hispanic_cat ══\n")
head(ces$hispanic_cat, 10)

cat("\n══ Class of ces$hispanic_cat ══\n")
class(ces$hispanic_cat)

sink()

###-----Race---------##

# Check what's in race == 8
cat("══ Sample of race == 8 ══\n")
ces %>%
  filter(race == 8) %>%
  count(race_other, sort = TRUE) %>%
  head(20) %>%
  print()

# And check if multrace is populated for race == 8
cat("\n══ multrace columns when race == 8 ══\n")
ces %>%
  filter(race == 8) %>%
  summarise(across(starts_with("multrace_"), ~sum(!is.na(.) & . != 0))) %>%
  pivot_longer(everything()) %>%
  arrange(desc(value)) %>%
  print(n = Inf)


# Helper to label race_cat consistently
label_race <- function(code) {
  factor(code,
         levels = 1:6,
         labels = c("White", "Black", "Native American", "Asian", 
                    "Two or more races", "Other"))
}

# ── Apply to PUMS ────────────────────────────────────────────────────────────
# IPUMS RACE codes:
#   1 = White, 2 = Black, 3 = AI/AN, 4 = Chinese, 5 = Japanese,
#   6 = Other Asian/PI, 7 = Other race, 8 = Two major races, 9 = Three or more

pums_crosswalked <- pums_crosswalked %>%
  mutate(RACE_num = haven::zap_labels(RACE)) %>%
  mutate(race_cat = case_when(
    RACE_num == 1                ~ 1L,  # White
    RACE_num == 2                ~ 2L,  # Black
    RACE_num == 3                ~ 3L,  # Native American (AI/AN)
    RACE_num %in% c(4, 5, 6)     ~ 4L,  # Asian (Chinese, Japanese, Other Asian/PI)
    RACE_num %in% c(8, 9)        ~ 5L,  # Two or more (incl. three+)
    RACE_num == 7                ~ 6L,  # Other
    TRUE                         ~ NA_integer_
  )) %>%
  mutate(race_cat = label_race(race_cat)) %>%
  select(-RACE_num)

# ── Apply to CES ─────────────────────────────────────────────────────────────
# CES race codes:
#   1 = White, 2 = Black, 3 = Hispanic, 4 = Asian, 5 = Native American,
#   6 = Two or more, 7 = Other, 8 = Middle Eastern

ces <- ces %>%
  mutate(race_cat = case_when(
    race == 1                    ~ 1L,  # White
    race == 2                    ~ 2L,  # Black
    race == 5                    ~ 3L,  # Native American
    race == 4                    ~ 4L,  # Asian
    race == 6                    ~ 5L,  # Two or more
    race %in% c(3, 7, 8)         ~ 6L,  # Other (Hispanic-as-race, Other, Middle Eastern)
    TRUE                         ~ NA_integer_
  )) %>%
  mutate(race_cat = label_race(race_cat))


# ── Verify distributions ─────────────────────────────────────────────────────
cat("══ PUMS race_cat distribution (weighted) ══\n")
pums_crosswalked %>%
  count(race_cat, wt = PERWT_adj) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()

cat("\n══ CES race_cat distribution ══\n")
ces %>%
  count(race_cat) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()

# Cross-tab race_cat × hispanic_cat to verify they're independent
cat("\n══ PUMS race_cat × hispanic_cat (weighted) ══\n")
pums_crosswalked %>%
  count(race_cat, hispanic_cat, wt = PERWT_adj) %>%
  pivot_wider(names_from = hispanic_cat, values_from = n) %>%
  print()

cat("\n══ CES race_cat × hispanic_cat ══\n")
ces %>%
  count(race_cat, hispanic_cat) %>%
  pivot_wider(names_from = hispanic_cat, values_from = n) %>%
  print()

# Collapse Two or more races and Other into a single category for cleaner alignment (since hispanics go into either)
pums_crosswalked <- pums_crosswalked %>%
  mutate(race_cat = fct_collapse(race_cat, 
                                 "Other/Multi" = c("Two or more races", "Other")))

ces <- ces %>%
  mutate(race_cat = fct_collapse(race_cat, 
                                 "Other/Multi" = c("Two or more races", "Other")))

# Verify
cat("══ PUMS race_cat distribution (weighted) ══\n")
pums_crosswalked %>%
  count(race_cat, wt = PERWT_adj) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()

cat("\n══ CES race_cat distribution ══\n")
ces %>%
  count(race_cat) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()



#####---------------Geography harmonization ---------------------########

#Diagnostics


cat("══ inputstate ══\n")
print(class(ces$inputstate))
cat("First 10 values:\n")
print(head(ces$inputstate, 10))
cat("Unique count:", n_distinct(ces$inputstate), "\n\n")

cat("══ cdid119 ══\n")
print(class(ces$cdid119))
cat("First 10 values:\n")
print(head(ces$cdid119, 10))
cat("Unique count:", n_distinct(ces$cdid119), "\n\n")

cat("══ inputzip ══\n")
print(class(ces$inputzip))
cat("First 10 values:\n")
print(head(ces$inputzip, 10))
cat("Unique count:", n_distinct(ces$inputzip), "\n\n")

cat("══ countyfips ══\n")
print(class(ces$countyfips))
cat("First 10 values:\n")
print(head(ces$countyfips, 10))
cat("Unique count:", n_distinct(ces$countyfips), "\n\n")

cat("══ NA counts ══\n")
cat("inputstate NAs:", sum(is.na(ces$inputstate)), "\n")
cat("cdid119 NAs:   ", sum(is.na(ces$cdid119)), "\n")
cat("inputzip NAs:  ", sum(is.na(ces$inputzip)), "\n")
cat("countyfips NAs:", sum(is.na(ces$countyfips)), "\n")

sink()


library(tidyverse)
library(readxl)

# ── Load all 7 BAFs and standardize to block_geoid + district + state_fips ──

tx_baf <- read_csv("/Users/binampoudyal/Downloads/rstudio-export/PLANC2333.csv",
                   col_types = cols(SCTBKEY = col_character(), 
                                    DISTRICT = col_integer())) %>%
  transmute(block_geoid = SCTBKEY, district = DISTRICT, state_fips = "48")

ca_baf <- read_delim("/Users/binampoudyal/Downloads/ab604.csv",
                     delim = ",", col_names = c("block_geoid", "district"),
                     col_types = cols(block_geoid = col_character(),
                                      district = col_integer())) %>%
  mutate(state_fips = "06")

mo_baf <- read_excel("/Users/binampoudyal/Downloads/HB1_Missouri_Congressional_Districts_2025_BEF.xlsx") %>%
  transmute(block_geoid = as.character(Block), 
            district = as.integer(DistrictID),
            state_fips = "29")

nc_baf <- read_csv("/Users/binampoudyal/Downloads/NCGA_CCM-2 .csv",
                   col_types = cols(Block = col_character(),
                                    District = col_integer())) %>%
  transmute(block_geoid = Block, district = District, state_fips = "37")

oh_baf <- read_excel("/Users/binampoudyal/Downloads/October 31 2025 CD BAF.xlsx") %>%
  transmute(block_geoid = as.character(Block), 
            district = as.integer(`DistrictID:1`),
            state_fips = "39")

ut_baf <- read_csv("/Users/binampoudyal/Downloads/ut_cong_adopted_2025_baf.csv",
                   col_types = cols(GEOID20 = col_character(),
                                    DISTRICT = col_integer())) %>%
  transmute(block_geoid = GEOID20, district = DISTRICT, state_fips = "49")

fl_baf <- read_delim("/Users/binampoudyal/Downloads/EOGPCRP2026.csv",
                     delim = ",", col_names = c("block_geoid", "district"),
                     col_types = cols(block_geoid = col_character(),
                                      district = col_integer())) %>%
  mutate(state_fips = "12")

# Combine all BAFs
all_bafs <- bind_rows(tx_baf, ca_baf, mo_baf, nc_baf, oh_baf, ut_baf, fl_baf)

cat("Total blocks across 7 states:", nrow(all_bafs), "\n")
cat("States:\n")
print(table(all_bafs$state_fips))


# ── Now: how often do counties split across CDs in each redistricted state? ──
county_split_analysis <- all_bafs %>%
  mutate(county_fips = substr(block_geoid, 1, 5)) %>%
  group_by(state_fips, county_fips) %>%
  summarise(n_cds = n_distinct(district), .groups = "drop") %>%
  group_by(state_fips, n_cds) %>%
  summarise(n_counties = n(), .groups = "drop")

cat("\n══ County splits across CDs (by state) ══\n")
print(county_split_analysis, n = Inf)

# Aggregate summary
cat("\n══ Overall county split distribution ══\n")
all_bafs %>%
  mutate(county_fips = substr(block_geoid, 1, 5)) %>%
  group_by(state_fips, county_fips) %>%
  summarise(n_cds = n_distinct(district), .groups = "drop") %>%
  count(n_cds) %>%
  print()





library(tidyverse)

# Identify which counties in the 7 redistricted states split across CDs
county_cd_count <- all_bafs %>%
  mutate(county_fips = as.integer(substr(block_geoid, 1, 5))) %>%
  group_by(state_fips, county_fips) %>%
  summarise(n_cds = n_distinct(district), .groups = "drop")

# FIPS codes of the 7 redistricted states
redistricted_fips <- c(6, 12, 29, 37, 39, 48, 49)

# How many CES respondents are in each scenario?
ces_geo_breakdown <- ces %>%
  filter(inputstate %in% redistricted_fips) %>%
  left_join(county_cd_count %>% 
              mutate(state_fips_int = as.integer(state_fips)),
            by = c("inputstate" = "state_fips_int", "countyfips" = "county_fips")) %>%
  mutate(scenario = case_when(
    is.na(n_cds)  ~ "Unmatched (no county)",
    n_cds == 1    ~ "Clean (1 CD)",
    n_cds == 2    ~ "2 CDs",
    n_cds == 3    ~ "3 CDs",
    n_cds %in% 4:9 ~ "4-9 CDs",
    n_cds >= 10   ~ "10+ CDs (LA County)",
    TRUE          ~ "Unknown"
  ))

cat("══ CES respondents in redistricted states, by county scenario ══\n")
ces_geo_breakdown %>%
  count(scenario) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()

# By state breakdown
cat("\n══ Same breakdown by state ══\n")
ces_geo_breakdown %>%
  count(inputstate, scenario) %>%
  group_by(inputstate) %>%
  mutate(pct_within_state = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  arrange(inputstate, scenario) %>%
  print(n = Inf)

# Compare to all CES (including stable states)
cat("\n══ CES geography summary ══\n")
cat("CES total:                          ", nrow(ces), "\n")
cat("In stable states (cdid119 works):   ", 
    sum(!ces$inputstate %in% redistricted_fips), "\n")
cat("In redistricted states (need fix):  ", 
    sum(ces$inputstate %in% redistricted_fips), "\n")

sink()




library(tidyverse)

# ── First: overall ZIP availability ──────────────────────────────────────────
cat("══ Overall ZIP availability in CES ══\n")
ces %>%
  mutate(has_zip = !is.na(inputzip)) %>%
  count(has_zip) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# ── ZIP availability by state (redistricted vs stable) ──────────────────────
cat("\n══ ZIP availability: redistricted vs stable states ══\n")
ces %>%
  mutate(
    has_zip = !is.na(inputzip),
    state_type = if_else(inputstate %in% redistricted_fips, 
                         "Redistricted", "Stable")
  ) %>%
  count(state_type, has_zip) %>%
  group_by(state_type) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# ── ZIP availability for each redistricted state ────────────────────────────
cat("\n══ ZIP availability per redistricted state ══\n")
ces %>%
  filter(inputstate %in% redistricted_fips) %>%
  mutate(has_zip = !is.na(inputzip)) %>%
  count(inputstate, has_zip) %>%
  group_by(inputstate) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print(n = Inf)

# ── Most useful: ZIP availability by county split scenario ──────────────────
cat("\n══ ZIP availability by county split scenario (redistricted states) ══\n")
ces_geo_breakdown %>%
  mutate(has_zip = !is.na(inputzip)) %>%
  count(scenario, has_zip) %>%
  group_by(scenario) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print(n = Inf)




cat("══ lookupzip distribution ══\n")
cat("Class:", class(ces$lookupzip), "\n")
cat("NAs:", sum(is.na(ces$lookupzip)), "of", nrow(ces), "\n")
head(ces$lookupzip, 20)

cat("\n══ regzip ══\n")
cat("Class:", class(ces$regzip), "\n")
cat("NAs:", sum(is.na(ces$regzip)), "\n")
head(ces$regzip, 20)

cat("\n══ urbancity distribution ══\n")
print(class(ces$urbancity))
ces %>% count(urbancity) %>% print()

cat("\n══ Are post-wave ZIPs more complete? ══\n")
cat("inputzip NAs:      ", sum(is.na(ces$inputzip)), "\n")
cat("inputzip_post NAs: ", sum(is.na(ces$inputzip_post)), "\n")
cat("regzip NAs:        ", sum(is.na(ces$regzip)), "\n")
cat("regzip_post NAs:   ", sum(is.na(ces$regzip_post)), "\n")
cat("lookupzip NAs:     ", sum(is.na(ces$lookupzip)), "\n")
cat("lookupzip_post NAs:", sum(is.na(ces$lookupzip_post)), "\n")





# Verify ZIP format
cat("══ lookupzip character lengths ══\n")
ces %>%
  mutate(zip_len = nchar(lookupzip)) %>%
  count(zip_len) %>%
  print()

# Sample by state to verify reasonable ZIPs
cat("\n══ Sample lookupzips by state ══\n")
ces %>%
  filter(inputstate %in% c(48, 6, 12)) %>%
  select(inputstate, lookupzip) %>%
  group_by(inputstate) %>%
  slice_sample(n = 5) %>%
  print()

# Check distinct ZIPs
cat("\n══ Unique lookupzips ══\n")
cat("Total unique:", n_distinct(ces$lookupzip), "\n")


## ----------------Actual Processing --------------------##
##Load ZCTA - block relationship file

library(readr)
library(data.table)

# Read only needed columns -- much faster and less memory
zcta_block <- fread(
  "/Users/binampoudyal/Downloads/tab20_zcta520_tabblock20_natl.txt",
  sep = "|",
  select = c("GEOID_ZCTA5_20", "GEOID_TABBLOCK_20"),
  colClasses = list(character = c("GEOID_ZCTA5_20", "GEOID_TABBLOCK_20"))
)

cat("Dimensions:", nrow(zcta_block), "rows x", ncol(zcta_block), "cols\n")
cat("\nFirst 5 rows:\n")
print(head(zcta_block, 5))

# Drop blocks without ZCTA assignment (uninhabited/water)
cat("\nBlocks without ZCTA:", sum(is.na(zcta_block$GEOID_ZCTA5_20) | 
                                    zcta_block$GEOID_ZCTA5_20 == ""), "\n")

zcta_block <- zcta_block[GEOID_ZCTA5_20 != "" & !is.na(GEOID_ZCTA5_20)]

cat("After dropping unassigned:", nrow(zcta_block), "rows\n")

# Filter to 7 redistricted states (state FIPS = first 2 chars of block GEOID)
redistricted_state_fips <- c("06", "12", "29", "37", "39", "48", "49")
zcta_block_redistricted <- zcta_block[substr(GEOID_TABBLOCK_20, 1, 2) %in% redistricted_state_fips]

cat("\nFiltered to 7 redistricted states:", nrow(zcta_block_redistricted), "rows\n")

# Quick verify: how many unique ZCTAs in our 7 states?
cat("Unique ZCTAs in redistricted states:", 
    length(unique(zcta_block_redistricted$GEOID_ZCTA5_20)), "\n")

##Pull block populations for all 7 redistricted states

library(tidycensus)
library(tidyverse)

# Pull block-level 2020 population for the redistricted states


states_needed <- c("CA", "FL", "MO", "NC", "OH", "UT", "TX")

all_blocks_pop <- map_dfr(states_needed, function(st) {
  cat("Pulling", st, "blocks...\n")
  get_decennial(
    geography = "block",
    variables = "P1_001N",
    year      = 2020,
    sumfile   = "pl",
    state     = st
  ) %>% select(GEOID, value)
})

cat("\nDone. Total block-pop records pulled:", nrow(all_blocks_pop), "\n")
cat("Total population covered:", sum(all_blocks_pop$value), "\n")


####Building the ZCTA --> 2026 CD Crosswalk
# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 9: ZCTA → 2026 CD Crosswalk for Redistricted States
#
# Purpose: Build a ZCTA-level crosswalk to 2026 Congressional Districts for
#          the 7 redistricted states (CA, FL, MO, NC, OH, TX, UT). This is
#          used to assign CES survey respondents to their 2026 CD via the
#          lookupzip field (5-digit ZIP, available for all respondents).
#
# Why this exists:
#   - CES gives us each respondent's verified 5-digit ZIP (lookupzip) but
#     not their PUMA or block. For the 43 states with stable boundaries we
#     can use cdid119 directly. For the 7 redistricted states cdid119 is
#     stale -- it reflects 119th Congress boundaries, not 2026 ones.
#   - ZCTAs (Census's polygonal approximation of ZIP codes) are made of
#     2020 Census blocks. Each block nests cleanly inside exactly one ZCTA.
#   - We already have block → 2026 CD assignments (the BAFs) and block-level
#     2020 populations (tidycensus). Combining these with the block → ZCTA
#     relationship gives us a population-weighted ZCTA → 2026 CD crosswalk.
#
# Architecture (parallels the PUMA → CD work):
#
#     Block GEOID
#         |
#         |----> ZCTA (from Census ZCTA-Block relationship file)
#         |----> 2026 CD (from state BAFs)
#         |----> Population (from tidycensus 2020 Decennial P1)
#         |
#         ↓ aggregate by (ZCTA, CD)
#     ZCTA × CD intersections with summed population
#         |
#         ↓ afact = pop(ZCTA × CD) / pop(ZCTA)
#     Final crosswalk: each ZCTA → list of (CD, afact) pairs
# ══════════════════════════════════════════════════════════════════════════════


# ── 1. Join block-level data: BAF (CD) + ZCTA + population ──────────────────
#
# Three inputs:
#   all_bafs                 -- block_geoid, district, state_fips (from BAFs)
#   zcta_block_redistricted  -- block_geoid, zcta (Census relationship file)
#   all_blocks_pop           -- block_geoid, pop (tidycensus 2020 Decennial)
#
# Inner joins ensure each block has all three pieces of info. The expected
# loss of a few thousand blocks is from the ~145K nationwide uninhabited
# (water/empty) blocks that have no ZCTA assignment -- those drop out here.

baf_data <- all_bafs %>%
  rename(block_geoid = block_geoid, district = district)

block_zcta <- zcta_block_redistricted %>%
  rename(block_geoid = GEOID_TABBLOCK_20,
         zcta        = GEOID_ZCTA5_20)

block_pop <- all_blocks_pop %>%
  rename(block_geoid = GEOID,
         pop         = value)

# Each block now has: state_fips, district (CD), zcta, pop
block_full <- baf_data %>%
  inner_join(block_zcta, by = "block_geoid") %>%
  inner_join(block_pop,  by = "block_geoid")

cat("══ Block-level join results ══\n")
cat("Blocks in BAFs:       ", nrow(baf_data), "\n")
cat("Blocks in ZCTA file:  ", nrow(block_zcta), "\n")
cat("Blocks in pop file:   ", nrow(block_pop), "\n")
cat("After all joins:      ", nrow(block_full), "\n")
cat("Population covered:   ", sum(block_full$pop), "\n")


# ── 2. Aggregate to ZCTA × CD level ─────────────────────────────────────────
#
# For each (state, ZCTA, CD) combination, sum block populations. This gives
# us the population of each ZCTA × CD intersection cell.
#
# A ZCTA that sits entirely within one CD will have exactly one row here.
# A ZCTA that straddles a CD boundary will have multiple rows (one per CD).

zcta_cd_crosswalk <- block_full %>%
  group_by(state_fips, zcta, cd_new = district) %>%
  summarise(
    pop_intersection = sum(pop),
    n_blocks         = n(),
    .groups = "drop"
  )

cat("\n══ ZCTA × CD intersections ══\n")
cat("Rows:", nrow(zcta_cd_crosswalk), "\n")
cat("Unique ZCTAs:", n_distinct(zcta_cd_crosswalk$zcta), "\n")


# ── 3. Compute afact (ZCTA → CD allocation factor) ──────────────────────────
#
# afact = pop in this (ZCTA × CD) intersection / total ZCTA population
#
# This is the same logic as PUMA → CD allocation: a respondent in a split
# ZCTA gets multiple rows after the join, one per CD, weighted by the
# fraction of their ZCTA's population in that CD.
#
# afact values for any given ZCTA must sum to exactly 1.0 across its CDs
# (modulo floating-point rounding) -- validated in step 4.

zcta_cd_crosswalk <- zcta_cd_crosswalk %>%
  group_by(state_fips, zcta) %>%
  mutate(
    zcta_pop = sum(pop_intersection),
    afact    = pop_intersection / zcta_pop
  ) %>%
  ungroup()


# ── 4. Validate afact sums to 1.0 per ZCTA ──────────────────────────────────
#
# Sanity check on the math: every ZCTA's afact values across all its CDs
# should sum to 1.0. If any don't, there's a data integrity issue.

afact_check <- zcta_cd_crosswalk %>%
  group_by(state_fips, zcta) %>%
  summarise(afact_sum = sum(afact), .groups = "drop")

cat("\nafact sum range:", round(min(afact_check$afact_sum), 6), 
    "to", round(max(afact_check$afact_sum), 6), "\n")
cat("ZCTAs where afact != 1:", sum(round(afact_check$afact_sum, 4) != 1), "\n")


# ── 5. Distribution: how clean is ZCTA → CD assignment? ─────────────────────
#
# This is the key diagnostic for the CES geography problem.
#
# If most ZCTAs nest cleanly within one CD, our fractional allocation has
# very few entries and the ZCTA-based geography is essentially as good as
# precise individual-level assignment.
#
# If many ZCTAs split across multiple CDs (especially in urban areas), we
# get noise in CD-level estimates because each respondent gets diluted
# across multiple CDs. We expect this to be much better than the county
# breakdown we saw earlier (where 67% of redistricted-state respondents
# were in split counties).

cat("\n══ Distribution: how many CDs per ZCTA? ══\n")
zcta_cd_crosswalk %>%
  group_by(state_fips, zcta) %>%
  summarise(n_cds = n(), .groups = "drop") %>%
  count(n_cds, name = "n_zctas") %>%
  print()

# Breakdown by state -- shows which states have cleaner ZCTA assignments
cat("\n══ Same distribution by state ══\n")
zcta_cd_crosswalk %>%
  group_by(state_fips, zcta) %>%
  summarise(n_cds = n(), .groups = "drop") %>%
  count(state_fips, n_cds, name = "n_zctas") %>%
  print(n = Inf)


# ── 6. Save crosswalk ───────────────────────────────────────────────────────
#
# This is the final output of this script. It will be loaded later when
# we apply it to CES lookupzip values to assign each CES respondent
# (in a redistricted state) to their 2026 CD.

saveRDS(zcta_cd_crosswalk, 
        "/Users/binampoudyal/Downloads/zcta_cd_crosswalk_redistricted.rds")
cat("\nSaved ZCTA crosswalk\n")
cat("File size:", 
    round(file.size("/Users/binampoudyal/Downloads/zcta_cd_crosswalk_redistricted.rds") / 1e6, 2), 
    "MB\n")

##Post run diagnostics and bug clean up
#The bug: afact validation shows NaN
#This means some ZCTAs have zcta_pop = 0 causing division by zero. 
#These are ZCTAs that exist geographically but contain only 
#blocks with zero population. 
# Check for zero-pop ZCTAs
cat("══ ZCTAs with zero total population ══\n")
zero_pop_zctas <- zcta_cd_crosswalk %>%
  group_by(state_fips, zcta) %>%
  summarise(total_pop = sum(pop_intersection), .groups = "drop") %>%
  filter(total_pop == 0)

cat("Number of zero-pop ZCTAs:", nrow(zero_pop_zctas), "\n")
cat("\nFirst few:\n")
print(head(zero_pop_zctas, 10))

# These are ZCTAs with only empty blocks -- can't be assigned to a CD
# Drop them since no real CES respondent should be in them anyway
zcta_cd_crosswalk <- zcta_cd_crosswalk %>%
  filter(zcta_pop > 0)

# Re-validate
afact_check <- zcta_cd_crosswalk %>%
  group_by(state_fips, zcta) %>%
  summarise(afact_sum = sum(afact), .groups = "drop")

cat("\nAfter dropping zero-pop ZCTAs:\n")
cat("afact sum range:", round(min(afact_check$afact_sum), 6),
    "to", round(max(afact_check$afact_sum), 6), "\n")
cat("ZCTAs where afact != 1:", sum(round(afact_check$afact_sum, 4) != 1), "\n")

# Re-save
saveRDS(zcta_cd_crosswalk,
        "/Users/binampoudyal/Downloads/zcta_cd_crosswalk_redistricted.rds")

##Check if any of the CES respondents zip belongs to these zero-pop ZCTAs
# Check if any CES respondents have lookupzip values that are zero-pop ZCTAs
redistricted_state_fips_int <- c(6, 12, 29, 37, 39, 48, 49)

zero_pop_zcta_codes <- zero_pop_zctas$zcta

ces_in_zero_pop <- ces %>%
  filter(inputstate %in% redistricted_state_fips_int) %>%
  filter(lookupzip %in% zero_pop_zcta_codes)

cat("CES respondents in zero-pop ZCTAs:", nrow(ces_in_zero_pop), "\n")
if (nrow(ces_in_zero_pop) > 0) {
  cat("\nBreakdown:\n")
  print(table(ces_in_zero_pop$lookupzip))
}

# What does cdid119 say for this respondent? And what's the population context?
ces_in_zero_pop %>%
  select(inputstate, lookupzip, cdid119, countyfips, countyname) %>%
  print()



### Applying the ZCTA crosswalk to CES
## Assignment logic:
##For each CES respondent:

#IF respondent is in a non-redistricted state:
#    cd_2026 = cdid119  (CES's value is already correct)
#afact = 1.0

#IF respondent is in a redistricted state:
#  Look up their lookupzip in zcta_cd_crosswalk
#If match found:
#  Produce 1+ rows, one per CD their ZCTA spans, with afact weights
#If no match (e.g. zero-pop ZCTA like 75671):
#  Fall back to cdid119, afact = 1.0

# ──────────────────────────────────────────────────────────────────────────────
# SCRIPT 10: Apply ZCTA crosswalk to assign CES respondents to 2026 CDs
# ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)

# State FIPS for the 7 redistricted states (as integers to match ces$inputstate)
redistricted_state_fips_int <- c(6, 12, 29, 37, 39, 48, 49)

# ── 1. For stable-state respondents: cdid119 is correct ──────────────────────
# These get one row per respondent with afact = 1.0

ces_stable <- ces %>%
  filter(!inputstate %in% redistricted_state_fips_int) %>%
  mutate(
    cd_2026 = cdid119,
    afact   = 1.0
  )

cat("Stable-state respondents:", nrow(ces_stable), "\n")


# ── 2. For redistricted-state respondents: join via ZCTA ─────────────────────
# Respondents in split ZCTAs get multiple rows (one per CD their ZCTA spans),
# each with the corresponding afact weight.

# Prepare crosswalk for join: need character zcta, integer cd_new
zcta_xw_for_join <- zcta_cd_crosswalk %>%
  select(zcta, cd_2026 = cd_new, afact)

ces_redistricted_matched <- ces %>%
  filter(inputstate %in% redistricted_state_fips_int) %>%
  inner_join(zcta_xw_for_join, by = c("lookupzip" = "zcta"))

cat("Redistricted-state respondents matched via ZCTA:", nrow(ces_redistricted_matched), "\n")


# ── 3. Handle unmatched redistricted respondents (zero-pop ZCTA edge case) ──
# Fall back to cdid119 for these. Expected: ~1 respondent.

ces_redistricted_unmatched <- ces %>%
  filter(inputstate %in% redistricted_state_fips_int) %>%
  anti_join(zcta_xw_for_join, by = c("lookupzip" = "zcta")) %>%
  mutate(
    cd_2026 = cdid119,
    afact   = 1.0
  )

cat("Redistricted-state respondents using cdid119 fallback:", 
    nrow(ces_redistricted_unmatched), "\n")


# ── 4. Combine everything ────────────────────────────────────────────────────
ces_with_cd <- bind_rows(
  ces_stable,
  ces_redistricted_matched,
  ces_redistricted_unmatched
)

cat("\n══ Combined dataset ══\n")
cat("Total rows:", nrow(ces_with_cd), "\n")
cat("Distinct CES respondents:", n_distinct(ces_with_cd$caseid), "\n")
cat("Row inflation factor:", round(nrow(ces_with_cd) / n_distinct(ces_with_cd$caseid), 3), "\n")


# ── 5. Validate ──────────────────────────────────────────────────────────────
# Each respondent's afact values should sum to 1.0 across all their rows.
afact_check <- ces_with_cd %>%
  group_by(caseid) %>%
  summarise(afact_sum = sum(afact), .groups = "drop")

cat("\nafact sum range across respondents:", 
    round(min(afact_check$afact_sum), 4), "to",
    round(max(afact_check$afact_sum), 4), "\n")
cat("Respondents where afact != 1.0:", 
    sum(round(afact_check$afact_sum, 4) != 1), "\n")

# Should have no missing CDs
cat("\nMissing cd_2026:", sum(is.na(ces_with_cd$cd_2026)), "\n")


##Diagnostics for 59 respondents who defaulted to previous cds


cat("══ The 59 fallback respondents — breakdown by state and ZIP ══\n")
ces_redistricted_unmatched %>%
  select(inputstate, lookupzip, cdid119, countyfips, countyname) %>%
  group_by(inputstate, lookupzip) %>%
  summarise(
    n_respondents = n(),
    cdid119_values = paste(unique(cdid119), collapse = ","),
    .groups = "drop"
  ) %>%
  arrange(desc(n_respondents)) %>%
  print(n = Inf)

cat("\n══ Are these ZIPs in the full ZCTA file? ══\n")
fallback_zips <- unique(ces_redistricted_unmatched$lookupzip)
cat("Total fallback ZIPs:", length(fallback_zips), "\n")

# Convert to tibble first to use n = Inf with print, or filter differently
zips_in_zcta_file <- zcta_block %>%
  as_tibble() %>%
  filter(GEOID_ZCTA5_20 %in% fallback_zips) %>%
  distinct(GEOID_ZCTA5_20)

cat("\nThese ZIPs that DO appear as ZCTAs (but got dropped as zero-pop):\n")
print(zips_in_zcta_file, n = Inf)

cat("\nZIPs that do NOT appear as ZCTAs at all (PO Box, business-only, etc.):\n")
zips_not_in_zcta <- setdiff(fallback_zips, zips_in_zcta_file$GEOID_ZCTA5_20)
print(zips_not_in_zcta)




# Map state FIPS integer to state name and abbreviation
state_lookup <- tibble(
  fips = c(6, 12, 29, 37, 39, 48, 49),
  abb  = c("CA", "FL", "MO", "NC", "OH", "TX", "UT"),
  name = c("California", "Florida", "Missouri", "North Carolina", 
           "Ohio", "Texas", "Utah")
)

ces_redistricted_unmatched %>%
  select(inputstate, lookupzip, cdid119, countyfips, countyname) %>%
  left_join(state_lookup, by = c("inputstate" = "fips")) %>%
  select(state_abb = abb, state_name = name, lookupzip, cdid119, countyfips, countyname) %>%
  group_by(state_abb, state_name, lookupzip, cdid119, countyfips, countyname) %>%
  summarise(n_respondents = n(), .groups = "drop") %>%
  arrange(state_abb, lookupzip) %>%
  print(n = Inf)


####We will default to old cd119 for these Zips that couldn't be mapped to ZCTAs


# ── 6. GEOGRAPHY → finalize state_cat and cd_cat in both datasets ────────────
#
# PUMS: cd119 column already contains 2026 CD for redistricted states
#       (mixed: 119th boundaries for stable, 2026 boundaries for redistricted)
#       Rename cd119 -> cd_cat for clarity
#       STATEFIP -> state_cat
#
# CES:  cd_2026 already created via ZCTA crosswalk + cdid119 fallback
#       Rename cd_2026 -> cd_cat
#       inputstate -> state_cat

# PUMS
pums_crosswalked <- pums_crosswalked %>%
  mutate(
    state_cat = as.integer(STATEFIP),
    cd_cat    = as.integer(cd119)
  )

# CES (using ces_with_cd which has the cd_2026 column already)
ces_with_cd <- ces_with_cd %>%
  mutate(
    state_cat = as.integer(inputstate),
    cd_cat    = as.integer(cd_2026)
  )

# Verify column types match
cat("══ PUMS state_cat and cd_cat ══\n")
cat("state_cat class:", class(pums_crosswalked$state_cat), "\n")
cat("cd_cat class:   ", class(pums_crosswalked$cd_cat), "\n")
cat("Unique state+CD combos:", 
    n_distinct(paste(pums_crosswalked$state_cat, pums_crosswalked$cd_cat)), "\n")

cat("\n══ CES state_cat and cd_cat ══\n")
cat("state_cat class:", class(ces_with_cd$state_cat), "\n")
cat("cd_cat class:   ", class(ces_with_cd$cd_cat), "\n")
cat("Unique state+CD combos:", 
    n_distinct(paste(ces_with_cd$state_cat, ces_with_cd$cd_cat)), "\n")

# Sanity check: do both datasets reference the same set of CDs?
pums_combos <- pums_crosswalked %>% 
  distinct(state_cat, cd_cat)

ces_combos <- ces_with_cd %>% 
  distinct(state_cat, cd_cat)

cat("\n══ Combos in PUMS but not in CES (PUMS-only) ══\n")
anti_join(pums_combos, ces_combos, by = c("state_cat", "cd_cat")) %>%
  count() %>% print()

cat("\n══ Combos in CES but not in PUMS (CES-only) ══\n")
anti_join(ces_combos, pums_combos, by = c("state_cat", "cd_cat")) %>%
  print(n = Inf)

# What CD values appear for at-large states in CES?
at_large_fips <- c(2, 10, 38, 46, 50, 56)  # AK, DE, ND, SD, VT, WY

cat("══ cdid119 values in CES for at-large states ══\n")
ces_with_cd %>%
  filter(inputstate %in% at_large_fips) %>%
  count(inputstate, cdid119, cd_cat) %>%
  print()

# Also check unique cdid119 values to see if CES uses 0 anywhere
cat("\n══ Distinct cdid119 values across CES ══\n")
cat("Min:", min(ces$cdid119, na.rm = TRUE), "\n")
cat("Max:", max(ces$cdid119, na.rm = TRUE), "\n")
cat("Sorted distinct values:", paste(sort(unique(ces$cdid119)), collapse = ","), "\n")

# What CD value does CES use for DC respondents?
cat("══ CES values for DC respondents ══\n")
ces_with_cd %>%
  filter(inputstate == 11) %>%
  count(cdid119, cd_cat) %>%
  print()

cat("\nTotal DC respondents:", sum(ces_with_cd$inputstate == 11), "\n")


# ── 7. GEOGRAPHY → handle at-large states and DC ─────────────────────────────
#
# Two encoding issues to resolve:
#
# 1. At-large states (AK, DE, ND, SD, VT, WY):
#    PUMS (Geocorr convention): cd_cat = 0
#    CES (standard convention): cd_cat = 1
#    Standardize PUMS to use 1, matching CES.
#
# 2. DC:
#    PUMS (Geocorr): state = 11, cd = 98 (non-voting delegate)
#    CES: state = 11, cd = 1 (treats DC's delegate same as at-large)
#    Drop DC entirely from both datasets. DC has a non-voting delegate,
#    not a House seat, so it has no place in a 435-CD House prediction frame.

# PUMS: drop DC, then recode at-large CDs from 0 to 1
pums_crosswalked <- pums_crosswalked %>%
  filter(state_cat != 11) %>%
  mutate(cd_cat = if_else(cd_cat == 0L, 1L, cd_cat))

# CES: drop DC
ces_with_cd <- ces_with_cd %>%
  filter(state_cat != 11)


# ── Re-validate alignment ────────────────────────────────────────────────────
pums_combos <- pums_crosswalked %>% distinct(state_cat, cd_cat)
ces_combos <- ces_with_cd %>% distinct(state_cat, cd_cat)

cat("══ Combos in PUMS but not in CES ══\n")
anti_join(pums_combos, ces_combos, by = c("state_cat", "cd_cat")) %>%
  print(n = Inf)

cat("\n══ Combos in CES but not in PUMS ══\n")
anti_join(ces_combos, pums_combos, by = c("state_cat", "cd_cat")) %>%
  print(n = Inf)


# ── Final summary ────────────────────────────────────────────────────────────
cat("\n══ Final state+CD coverage ══\n")
cat("PUMS unique state+CD combos:", 
    n_distinct(paste(pums_crosswalked$state_cat, pums_crosswalked$cd_cat)), 
    "(expect 435)\n")
cat("CES unique state+CD combos: ", 
    n_distinct(paste(ces_with_cd$state_cat, ces_with_cd$cd_cat)), 
    "(expect 435)\n")
cat("\nPUMS rows:", nrow(pums_crosswalked), "\n")
cat("CES rows: ", nrow(ces_with_cd), "\n")

##Save files
saveRDS(pums_crosswalked, 
        "/Users/binampoudyal/Downloads/Stratification_Frame_Building/pums_crosswalked_harmonized.rds")

saveRDS(ces_with_cd, 
        "/Users/binampoudyal/Downloads/Stratification_Frame_Building/ces_harmonized.rds")


# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 11: Cell Aggregation for Poststratification
#
# Purpose: Collapse the row-level PUMS data into one row per demographic cell
#          (state × CD × age × gender × race × hispanic × education) with the
#          summed weighted population per cell. 
#
# Note: this is the demographic-only frame. The MrsP frame requires
# expanding this with past vote (vote_2024) as an additional cell dimension,
# weighted by P(vote | demographics, CD) from the CES regression.
#
# Logic:
#   - Each row in pums_crosswalked is a person × CD assignment, with
#     PERWT_adj = ACS person weight × afact (the CD allocation fraction).
#   - Aggregate by all cell-defining variables, summing PERWT_adj within
#     each unique cell.
#   - Output: one row per cell with cell_pop = sum of PERWT_adj.
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)

# ── 1. Load harmonized PUMS from disk ────────────────────────────────────────
pums_frame <- readRDS(
  "/Users/binampoudyal/Downloads/Stratification_Frame_Building/pums_crosswalked_harmonized.rds"
)

cat("══ Loaded PUMS ══\n")
cat("Rows:", nrow(pums_frame), "\n")
cat("Cols:", ncol(pums_frame), "\n")


# ── 2. Aggregate to cell level ───────────────────────────────────────────────
# Cell-defining variables (must match exactly what we'll use in modeling):
#   state_cat, cd_cat -- geography
#   age_cat -- 14 age bins
#   gender_cat -- Male / Female
#   race_cat -- 5 categories
#   hispanic_cat -- Hispanic / Not Hispanic
#   educ_cat -- 6 levels
#
# Total possible cells: 435 × 14 × 2 × 5 × 2 × 6 = 365,400
# Actual cells (after dropping empties): less because many demographic
# combinations don't exist in any geography (e.g. an 83+ Native American
# Hispanic post-grad in CD 27 might have zero population)

pums_demographic_cells <- pums_frame %>%
  group_by(state_cat, cd_cat, age_cat, gender_cat, 
           race_cat, hispanic_cat, educ_cat) %>%
  summarise(
    cell_pop = sum(PERWT_adj),
    .groups = "drop"
  )

cat("\n══ Cell aggregation results ══\n")
cat("Number of cells (non-empty):", nrow(pums_demographic_cells), "\n")
cat("Theoretical max cells:       365,400\n")
cat("Cells filled:                ", 
    round(100 * nrow(pums_demographic_cells) / 365400, 1), "%\n")


# ── 3. Validation: total population preserved ────────────────────────────────
cat("\n══ Population check ══\n")
cat("PUMS total PERWT_adj sum:    ", 
    round(sum(pums_frame$PERWT_adj), 0), "\n")
cat("Aggregated cell_pop sum:     ", 
    round(sum(pums_demographic_cells$cell_pop), 0), "\n")
cat("Difference:                  ", 
    round(sum(pums_demographic_cells$cell_pop) - sum(pums_frame$PERWT_adj), 0), "\n")


# ── 4. Cell-size distribution ────────────────────────────────────────────────
# Sparsity matters for MrP -- very small cells produce unstable estimates
# but partial pooling handles this. Worth knowing the distribution.

cat("\n══ Cell-size distribution ══\n")
cat("Min cell_pop:    ", round(min(pums_demographic_cells$cell_pop), 2), "\n")
cat("Median cell_pop: ", round(median(pums_demographic_cells$cell_pop), 2), "\n")
cat("Mean cell_pop:   ", round(mean(pums_demographic_cells$cell_pop), 2), "\n")
cat("Max cell_pop:    ", round(max(pums_demographic_cells$cell_pop), 0), "\n")

cat("\nCell-size quantiles:\n")
print(round(quantile(pums_demographic_cells$cell_pop, probs = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)), 2))

# How many cells have very tiny populations (< 1 person effectively)?
cat("\nCells with cell_pop < 1:    ", sum(pums_demographic_cells$cell_pop < 1), "\n")
cat("Cells with cell_pop < 10:   ", sum(pums_demographic_cells$cell_pop < 10), "\n")
cat("Cells with cell_pop < 100:  ", sum(pums_demographic_cells$cell_pop < 100), "\n")

# ── 5. Save the poststratification frame ─────────────────────────────────────
saveRDS(pums_demographic_cells, 
        "/Users/binampoudyal/Downloads/Stratification_Frame_Building/pums_demographic_cells.rds")

cat("\nSaved poststratification frame\n")
cat("File size:", 
    round(file.size("/Users/binampoudyal/Downloads/Stratification_Frame_Building/pums_demographic_cells.rds") / 1e6, 2), 
    "MB\n")
saveRDS(ces_with_cd, 
        "/Users/binampoudyal/Downloads/Stratification_Frame_Building/ces_with_cd.rds")


#Ignore this next block, not used ultimately
#---------------------------------------------------------#
# Check exact count of unique values per variable
cat("Unique values per cell-defining variable:\n")
cat("state_cat:    ", n_distinct(pums_demographic_cells$state_cat), "\n")
cat("cd_cat:       ", n_distinct(pums_demographic_cells$cd_cat), "\n")
cat("state+cd:     ", n_distinct(paste(pums_demographic_cells$state_cat, 
                                       pums_demographic_cells$cd_cat)), "\n")
cat("age_cat:      ", n_distinct(pums_demographic_cells$age_cat), "\n")
cat("gender_cat:   ", n_distinct(pums_demographic_cells$gender_cat), "\n")
cat("race_cat:     ", n_distinct(pums_demographic_cells$race_cat), "\n")
cat("hispanic_cat: ", n_distinct(pums_demographic_cells$hispanic_cat), "\n")
cat("educ_cat:     ", n_distinct(pums_demographic_cells$educ_cat), "\n")

# Real theoretical max
real_max <- 435 * 14 * 2 * 5 * 2 * 6
cat("\nReal theoretical max:", real_max, "\n")
cat("Actual cells:        ", nrow(pums_demographic_cells), "\n")





# Look at all column names related to House voting
cat("══ Vote-related column names in CES ══\n")
vote_cols <- names(ces_with_cd)[grepl("CC24_4|presvote|HouseCand|cdid", names(ces_with_cd))]
print(vote_cols)

# Check the main candidates for House vote
# CC24_412 is typically the House vote in 2024 CES post-election
cat("\n══ Looking for likely House vote columns ══\n")
likely_house_cols <- c("CC24_410", "CC24_412", "CC24_412_nv", "CC24_412_t", 
                       "CC24_412_nv_t", "CC24_412e")

# Show distribution for each that exists
for (col in likely_house_cols) {
  if (col %in% names(ces_with_cd)) {
    cat("\n--- ", col, " ---\n", sep = "")
    print(table(ces_with_cd[[col]], useNA = "always"))
  }
}



cat("══ Cross-tab: tookpost vs CC24_412 ══\n")
print(table(ces$tookpost, ces$CC24_412, useNA = "always"))






library(data.table)

# Read and inspect the national CD119 BEF
cd119_baf <- fread(
  "/Users/binampoudyal/Downloads/Stratification_Frame_Building/NationalCD119.txt",
  colClasses = list(character = 1:2)  # keep both as character to preserve leading zeros
)

cat("══ Dimensions ══\n")
cat("Rows:", nrow(cd119_baf), "\n")
cat("Cols:", ncol(cd119_baf), "\n\n")

cat("══ Column names ══\n")
print(names(cd119_baf))

cat("\n══ First 10 rows ══\n")
print(head(cd119_baf, 10))

cat("\n══ Unique CD values ══\n")
# Assuming 2nd column is the CD
cd_col <- names(cd119_baf)[2]
cat("CD column name:", cd_col, "\n")
print(sort(unique(cd119_baf[[cd_col]])))




library(tidyverse)
library(data.table)

# Filter CD119 BAF to redistricted states only and standardize columns
redistricted_state_fips <- c("06", "12", "29", "37", "39", "48", "49")

cd119_redistricted <- cd119_baf %>%
  as_tibble() %>%
  filter(substr(GEOID, 1, 2) %in% redistricted_state_fips,
         CDFP != "ZZ") %>%
  rename(block_geoid = GEOID, cd_119 = CDFP) %>%
  mutate(cd_119 = as.integer(cd_119))


# Build the renumbering map: for each (state, cd_119), find the 2026 CD with
# the largest population overlap.
#
# Inputs needed:
#   cd119_redistricted    -- block -> cd_119 (just built)
#   all_bafs              -- block -> cd_2026 (from earlier work)
#   all_blocks_pop        -- block -> population (from tidycensus)

cd_renumber_map <- cd119_redistricted %>%
  inner_join(all_bafs %>% rename(cd_2026 = district),
             by = c("block_geoid", "state_fips" = "state_fips") %>% {
               # We need state_fips on both sides; derive from block_geoid
               c("block_geoid")
             })

# Simpler version: derive state_fips from block_geoid on the fly
cd_renumber_map <- cd119_redistricted %>%
  mutate(state_fips = substr(block_geoid, 1, 2)) %>%
  inner_join(
    all_bafs %>% rename(cd_2026 = district),
    by = c("block_geoid", "state_fips")
  ) %>%
  inner_join(
    all_blocks_pop %>% rename(block_geoid = GEOID, pop = value),
    by = "block_geoid"
  )

cat("══ Block-level merged data ══\n")
cat("Rows:", nrow(cd_renumber_map), "\n")
cat("Population covered:", sum(cd_renumber_map$pop), "\n\n")


# Now aggregate: for each (state, cd_119), find the cd_2026 with max overlap
cd119_to_cd2026 <- cd_renumber_map %>%
  group_by(state_fips, cd_119, cd_2026) %>%
  summarise(overlap_pop = sum(pop), .groups = "drop") %>%
  group_by(state_fips, cd_119) %>%
  arrange(desc(overlap_pop)) %>%
  mutate(
    cd_119_total_pop  = sum(overlap_pop),
    overlap_pct       = round(100 * overlap_pop / cd_119_total_pop, 1)
  ) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(state_fips, cd_119, cd_2026, overlap_pct, overlap_pop, cd_119_total_pop)


cat("══ Renumbering map (CD119 -> CD2026 by max population overlap) ══\n")
print(cd119_to_cd2026, n = Inf)

# Cases where overlap is incomplete (i.e. old CD got carved up)
cat("\n══ Cases where overlap < 90% (old CD split substantially) ══\n")
cd119_to_cd2026 %>%
  filter(overlap_pct < 90) %>%
  print(n = Inf)

#---------------------------------------------------------------#

#diagnostic

library(tidyverse)

ces_with_cd <- readRDS("/Users/binampoudyal/Downloads/Stratification_Frame_Building/ces_with_cd.rds")

cat("Dimensions:", nrow(ces_with_cd), "rows x", ncol(ces_with_cd), "cols\n")
cat("Unique respondents:", n_distinct(ces_with_cd$caseid), "\n")


# What format is CC24_367_voted in?
cat("══ CC24_367_voted ══\n")
cat("Class:", class(ces$CC24_367_voted), "\n")
cat("Sample values:\n")
head(ces$CC24_367_voted, 20)

cat("\n══ Distribution ══\n")
print(table(ces$CC24_367_voted, useNA = "always"))

# Check whether CC24_412 might be the same data under a different name
cat("\n══ Are CC24_367_voted and CC24_412 identical? ══\n")
cat("Rows where they differ (excluding both NA):\n")
sum(ces$CC24_367_voted != ces$CC24_412, na.rm = TRUE)

# Sanity check on HouseCandNParty columns to confirm we can derive party
cat("\n══ HouseCand1Party distribution ══\n")
print(table(ces$HouseCand1Party, useNA = "always"))

cat("\n══ HouseCand2Party distribution ══\n")
print(table(ces$HouseCand2Party, useNA = "always"))

cat("\n══ HouseCand3Party distribution ══\n")
print(table(ces$HouseCand3Party, useNA = "always"))

# Confirm CC24_401 turnout distribution
cat("\n══ CC24_401 turnout distribution ══\n")
print(table(ces$CC24_401, useNA = "always"))





# Pre-election house preference codes
cat("══ CC24_367 distribution ══\n")
print(table(ces_with_cd %>% distinct(caseid, .keep_all = TRUE) %>% pull(CC24_367), useNA = "always"))

# Pre-election turnout codes
cat("\n══ CC24_363 distribution ══\n")
print(table(ces_with_cd %>% distinct(caseid, .keep_all = TRUE) %>% pull(CC24_363), useNA = "always"))
cat("══ CC24_363 distribution ══\n")
print(table(ces_with_cd %>% distinct(caseid, .keep_all = TRUE) %>% pull(CC24_363), useNA = "always"))

# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 12: Construct vote_2024 column for CES respondents
#
# Purpose: Create a single, clean party-level vote variable for each CES
#          respondent, drawn from the most reliable source available in the
#          CES survey instrument.
#
# Background -- the CES 2024 vote-related variables:
#
#   The CES has both pre-election and post-election waves. Vote-related
#   information is captured in multiple questions across these waves, with
#   different gating logic per question:
#
#   Post-election (only for respondents who took the post-wave, tookpost == 2):
#     CC24_412  -- "For whom did you vote for U.S. House?"
#                  Codes: 1-5 (candidate index), 10 (Other write-in),
#                         11 (did not vote in this race), 12 (did not vote),
#                         13 (not sure), 98 (skipped), 99 (not asked), NA.
#                  Only shown if at least one House candidate exists.
#     CC24_401  -- "Which of the following statements best describes you?"
#                  Codes: 1-4 (didn't vote / thought about but didn't /
#                              usually but didn't / attempted but couldn't),
#                         5 (definitely voted), 8 (skipped), 9 (not asked).
#                  Asked of all post-wave respondents.
#
#   Pre-election (asked of all respondents pre-wave, with branching):
#     CC24_363  -- "Do you intend to vote in the 2024 general election?"
#                  Codes: 1 (Yes definitely), 2 (Probably), 3 (Already voted),
#                         4 (Plan to vote before Nov 5), 5 (No),
#                         6 (Undecided), 8 (skipped), 9 (not asked).
#                  Asked of all respondents.
#     CC24_367_voted -- "For which House candidate did you vote?"
#                       Codes: 1-3 (candidate index), 10 (Other), 98 (not sure),
#                              99 (didn't vote), 998/999 (skipped/not asked).
#                       ONLY asked of respondents who said CC24_363 == 3
#                       (already voted before the pre-election survey).
#     CC24_367  -- "In the general election for U.S. House, who do you prefer?"
#                  Codes: 1-3 (candidate index), 10 (Other), 98 (not sure),
#                         99 (No one), 998/999 (skipped/not asked).
#                  Asked of respondents who hadn't already voted (so essentially
#                  the complement of CC24_367_voted).
#
#   Candidate party lookup:
#     HouseCand1Party, ..., HouseCand5Party hold the party affiliation of
#     each candidate. The candidate index in any of the vote questions
#     above maps directly to these columns. For example, if CC24_412 == 2
#     for a respondent, their vote went to HouseCand2 -- whose party is in
#     HouseCand2Party for that row.
#
# Waterfall logic -- in order of preference:
#
#   The waterfall prioritizes:
#     (a) Confirmed votes from the post-wave (CC24_412)
#     (b) Post-wave confirmation of non-voting (CC24_401)
#     (c) Pre-wave reported actual votes (CC24_367_voted, asked of early voters)
#     (d) Pre-wave reported vote preference (CC24_367, asked of non-early-voters)
#     (e) Pre-wave reported intention not to vote (CC24_363)
#     (f) NA if none of the above gives a clean answer
#
#   This ordering is justified because:
#     - Post-wave responses are retrospective ("what did you do?") and most
#       accurate when available
#     - CC24_401 confirms non-voting status; we trust it for "No Vote"
#     - CC24_367_voted is a reported actual vote (just collected pre-wave for
#       early voters); equivalent reliability to CC24_412 but with smaller
#       sample. Falls below CC24_412 only because CC24_412 is asked of more
#       people
#     - CC24_367 is a pre-election preference, not a confirmed vote, so it's
#       less reliable than actual vote reports
#     - CC24_363 only contributes "No Vote" cases (when respondent says they
#       won't vote) -- positive vote intent without a candidate name is too
#       weak to record
#
# Party label standardization:
#
#   The HouseCandNParty columns contain raw party names as ballot-designated.
#   We standardize them into 6 categories plus "No Vote" and NA:
#     Democratic   -- HouseCandNParty == "Democratic"
#     Republican   -- HouseCandNParty == "Republican"
#     Libertarian  -- HouseCandNParty == "Libertarian"
#     Green        -- HouseCandNParty == "Green"
#     Independent  -- HouseCandNParty == "Independent" OR "No Party Preference"
#     Other        -- any other party (Unity, etc.) OR write-ins (code 10)
#     No Vote      -- explicit non-vote indicators
#     NA           -- no clean information available
#
#   Note on "No Party Preference": this is the official ballot designation in
#   some states (notably California) for candidates running without major-party
#   affiliation. Functionally equivalent to Independent; folded together.
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)


# ── Helper: translate (candidate index, HouseCandNParty columns) → party label ─
#
# Takes the candidate index from a vote question (1-5) and the five party
# columns for that respondent. Returns a standardized party label.
# If idx is NA or outside 1-5, returns NA.

party_from_candidate <- function(idx, p1, p2, p3, p4, p5) {
  # Look up the party of the candidate the respondent voted for
  raw_party <- case_when(
    idx == 1 ~ p1,
    idx == 2 ~ p2,
    idx == 3 ~ p3,
    idx == 4 ~ p4,
    idx == 5 ~ p5,
    TRUE     ~ NA_character_
  )
  
  # Standardize the raw party name to our 6-category scheme
  case_when(
    raw_party == "Democratic"          ~ "Democratic",
    raw_party == "Republican"          ~ "Republican",
    raw_party == "Libertarian"         ~ "Libertarian",
    raw_party == "Green"               ~ "Green",
    raw_party == "Independent"         ~ "Independent",
    raw_party == "No Party Preference" ~ "Independent",
    is.na(raw_party)                   ~ NA_character_,
    TRUE                               ~ "Other"      # Unity, write-ins, fringe parties
  )
}


# ── Apply waterfall to construct vote_2024 ───────────────────────────────────
#
# Each step starts with the result of the previous step; if that's NA, this
# step attempts to fill it in using its own data source. The result of the
# final step is the final vote_2024 column.

ces_with_cd <- ces_with_cd %>%
  mutate(
    
    # ── STEP 1: CC24_412 (post-election House vote) ────────────────────────
    # Most reliable: a retrospective report from the post-wave.
    vote_step1 = case_when(
      CC24_412 %in% 1:5 ~ party_from_candidate(
        CC24_412,
        HouseCand1Party, HouseCand2Party,
        HouseCand3Party, HouseCand4Party, HouseCand5Party),
      CC24_412 == 10           ~ "Other",         # write-in
      CC24_412 %in% c(11, 12)  ~ "No Vote",       # didn't vote in race / didn't vote
      TRUE                     ~ NA_character_    # 13, 98, 99, NA -- fall through
    ),
    
    # ── STEP 2: CC24_401 (post-election turnout, "No Vote" fallback) ───────
    # If step 1 didn't yield a vote_2024 value, try to use post-election
    # turnout to confirm non-voting. Codes 1-4 = various forms of non-voting.
    vote_step2 = case_when(
      !is.na(vote_step1)  ~ vote_step1,
      CC24_401 %in% 1:4   ~ "No Vote",
      TRUE                ~ NA_character_   # 5, 8, 9, NA -- fall through
    ),
    
    # ── STEP 3a: CC24_367_voted (pre-election, asked of early voters) ─────
    # For respondents who already voted before pre-wave, this is their
    # actual vote. Only ever has codes 1-3 (no 4 or 5).
    vote_step3a = case_when(
      !is.na(vote_step2)       ~ vote_step2,
      CC24_367_voted %in% 1:3  ~ party_from_candidate(
        CC24_367_voted,
        HouseCand1Party, HouseCand2Party,
        HouseCand3Party, NA, NA),
      CC24_367_voted == 10     ~ "Other",
      CC24_367_voted == 99     ~ "No Vote",
      TRUE                     ~ NA_character_   # 98, 998, 999, NA -- fall through
    ),
    
    # ── STEP 3b: CC24_367 (pre-election preference) ───────────────────────
    # For respondents who hadn't voted yet at pre-wave time, this is their
    # stated preference. Less reliable than actual vote reports.
    vote_step3b = case_when(
      !is.na(vote_step3a)  ~ vote_step3a,
      CC24_367 %in% 1:3    ~ party_from_candidate(
        CC24_367,
        HouseCand1Party, HouseCand2Party,
        HouseCand3Party, NA, NA),
      CC24_367 == 10       ~ "Other",
      CC24_367 == 99       ~ "No Vote",
      TRUE                 ~ NA_character_   # 98, 998, 999, NA -- fall through
    ),
    
    # ── STEP 4: CC24_363 (pre-election turnout intention, "No Vote" only) ──
    # Last resort: if we have nothing else, only the explicit "No" answer
    # gives us usable information. All other intent codes (Yes/Probably/
    # Already voted/Plan to vote/Undecided) don't tell us who they support,
    # so they fall through to NA.
    vote_2024 = case_when(
      !is.na(vote_step3b)  ~ vote_step3b,
      CC24_363 == 5        ~ "No Vote",
      TRUE                 ~ NA_character_   # NA = no information available
    )
  ) %>%
  
  # Drop intermediate step columns to keep dataset tidy
  select(-vote_step1, -vote_step2, -vote_step3a, -vote_step3b)


# ── Convert to factor with explicit level ordering ───────────────────────────
# Ordering matters for default factor display and modeling -- placing
# "Democratic" and "Republican" first so they're the reference categories
# during regression.

ces_with_cd <- ces_with_cd %>%
  mutate(vote_2024 = factor(
    vote_2024,
    levels = c("Democratic", "Republican", "Libertarian",
               "Green", "Independent", "Other", "No Vote")
  ))


# ── Verify the final distribution ────────────────────────────────────────────
# Use distinct(caseid) to count each respondent once, since ces_with_cd
# may have row inflation from the ZCTA crosswalk.

cat("══ vote_2024 distribution (unique respondents) ══\n")
ces_with_cd %>%
  distinct(caseid, .keep_all = TRUE) %>%
  count(vote_2024) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()


# Summary statistics
cat("\n══ Summary ══\n")
n_unique <- n_distinct(ces_with_cd$caseid)
n_with_vote <- ces_with_cd %>%
  distinct(caseid, .keep_all = TRUE) %>%
  filter(!is.na(vote_2024)) %>%
  nrow()
cat("Total unique respondents:           ", n_unique, "\n")
cat("With non-NA vote_2024:              ", n_with_vote, "\n")
cat("Coverage:                           ",
    round(100 * n_with_vote / n_unique, 1), "%\n")

##Too few greens, independents etc, so collapse them to other

ces_with_cd <- ces_with_cd %>%
  mutate(vote_2024 = fct_collapse(
    vote_2024,
    "Other" = c("Libertarian", "Green", "Independent", "Other")
  ))

# Verify
cat("══ vote_2024 distribution after collapse ══\n")
ces_with_cd %>%
  distinct(caseid, .keep_all = TRUE) %>%
  count(vote_2024) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()


# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 12B: Apply CD119 → CD2026 renumbering map to fallback respondents
#
# Purpose: For the 59 CES respondents in redistricted states whose lookupzip
#          doesn't correspond to a populated ZCTA (PO Box, business, military
#          ZIPs), refine their cd_2026 assignment using Roberto's renumbering
#          heuristic: if the majority of an old CD's population now lives in a
#          new CD with a different number, assign the new number.
#
# Background:
#   Earlier in the pipeline, these 59 respondents had cd_2026 set to their
#   cdid119 value as a fallback. For most of them this is approximately right
#   (their underlying geographic area may have kept the same CD number under
#   2026 boundaries). For some, the CD got renumbered and their cdid119 value
#   is stale.
#
#   The cd119_to_cd2026 lookup table (built earlier from population overlap
#   analysis between 2020 blocks' CD119 assignments and their 2026 CD
#   assignments via the state BAFs) provides, for each (state, cd_119), the
#   2026 CD with the largest population overlap. We use this as the heuristic
#   for renumbering.
#
#   Per Roberto's guidance: this is an approximate fix, accepting that the
#   overlap is often partial (<90% in many cases) but still better than blindly
#   trusting cdid119. The approach is "if we don't know for a fact, assume
#   the majority overlap."
#
# What this script does:
#   1. Identifies the 59 fallback respondents (in redistricted states with
#      lookupzip not in the ZCTA crosswalk)
#   2. Joins them to the renumbering map on (state, cdid119)
#   3. Updates cd_2026 to the remapped value, but ONLY for fallback respondents
#   4. Non-fallback respondents are unaffected
# ══════════════════════════════════════════════════════════════════════════════


# ── 1. Prepare the renumbering lookup ────────────────────────────────────────
# cd119_to_cd2026 has columns: state_fips (chr), cd_119 (int), cd_2026 (int)
# We need state_fips as integer to match ces_with_cd$inputstate, and we rename
# cd_2026 → cd_2026_remapped to avoid collision with the existing cd_2026
# column in ces_with_cd.

cd119_lookup <- cd119_to_cd2026 %>%
  mutate(state_fips_int = as.integer(state_fips)) %>%
  select(state_fips_int, cd_119, cd_2026_remapped = cd_2026)


# ── 2. Apply renumbering to fallback respondents ─────────────────────────────
# Step-by-step:
#   - Flag each row as fallback or not (in redistricted state AND lookupzip
#     isn't in the ZCTA crosswalk)
#   - Left-join the renumbering lookup on (inputstate, cdid119). For non-
#     fallback respondents this still does a join but the result doesn't
#     get applied.
#   - Use if_else to update cd_2026 ONLY when the row is fallback AND a
#     remapping exists. Other rows keep their existing cd_2026 (either from
#     stable-state cdid119 or from ZCTA-based assignment).
#   - Drop the temporary helper columns to keep the dataset tidy.

zcta_codes_in_crosswalk <- unique(zcta_cd_crosswalk$zcta)

ces_with_cd <- ces_with_cd %>%
  
  # Flag fallback respondents (~59 rows total)
  mutate(
    is_fallback = inputstate %in% c(6, 12, 29, 37, 39, 48, 49) & 
      !(lookupzip %in% zcta_codes_in_crosswalk)
  ) %>%
  
  # Join the renumbering lookup on (state, old CD)
  # For fallback respondents, their current cd_2026 is their cdid119 value,
  # but we join on cdid119 explicitly to be unambiguous about semantic.
  left_join(cd119_lookup,
            by = c("inputstate" = "state_fips_int", "cdid119" = "cd_119")) %>%
  
  # Conditionally update cd_2026 for fallback respondents with a valid remapping
  mutate(
    cd_2026 = if_else(
      is_fallback & !is.na(cd_2026_remapped),
      as.integer(cd_2026_remapped),
      as.integer(cd_2026)
    )
  ) %>%
  
  # Clean up helper columns
  select(-cd_2026_remapped, -is_fallback)


# ── 3. Verification ──────────────────────────────────────────────────────────
# Confirm structure is preserved: 69,020 rows, 59,446 unique respondents,
# all afact values still sum to 1.0 per respondent.

cat("══ Verify after renumbering ══\n")
cat("Total rows in ces_with_cd:", nrow(ces_with_cd), "\n")
cat("Unique respondents:        ", n_distinct(ces_with_cd$caseid), "\n")

# afact integrity check (should sum to 1.0 per respondent)
afact_check <- ces_with_cd %>%
  group_by(caseid) %>%
  summarise(afact_sum = sum(afact), .groups = "drop")

cat("\nafact sum per respondent: ",
    round(min(afact_check$afact_sum), 4), "to",
    round(max(afact_check$afact_sum), 4), "\n")
cat("Respondents where afact != 1:", 
    sum(round(afact_check$afact_sum, 4) != 1), "\n")

# Check state+CD alignment with PUMS is still intact
pums_combos <- pums_crosswalked %>% distinct(state_cat, cd_cat)
ces_combos <- ces_with_cd %>% 
  mutate(state_cat = as.integer(inputstate),
         cd_cat = as.integer(cd_2026)) %>%
  distinct(state_cat, cd_cat)

cat("\nCDs in CES but not in PUMS:", 
    nrow(anti_join(ces_combos, pums_combos, by = c("state_cat", "cd_cat"))), "\n")
cat("CDs in PUMS but not in CES:",
    nrow(anti_join(pums_combos, ces_combos, by = c("state_cat", "cd_cat"))), "\n")


#Save Checkpoint
saveRDS(ces_with_cd, 
        "/Users/binampoudyal/Downloads/Stratification_Frame_Building/ces_with_cd_v2.rds")

cat("Saved ces_with_cd_v2\n")
cat("File size:", 
    round(file.size("/Users/binampoudyal/Downloads/Stratification_Frame_Building/ces_with_cd_v2.rds") / 1e6, 2), 
    "MB\n")
cat("Rows:                ", nrow(ces_with_cd), "\n")
cat("Unique respondents:  ", n_distinct(ces_with_cd$caseid), "\n")
cat("Columns:             ", ncol(ces_with_cd), "\n")


# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 13: Add state abbreviations and state_cd identifier
#
# Purpose: Create a human-readable unique identifier for each congressional
#          district, combining state abbreviation and CD number.
#
# Examples: TX-1, FL-2, PA-1, CA-12
#
# This will be applied to BOTH pums_demographic_cells (for the CD-level
# aggregation we're about to do) and ces_with_cd_v2 (for consistency).
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)


# ── Build state FIPS → abbreviation lookup ───────────────────────────────────
# All 50 states (DC already excluded from our data).
# Using the standard US state FIPS codes.

state_fips_to_abb <- tibble(
  state_cat = c(
    1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20,
    21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
    36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51,
    53, 54, 55, 56
  ),
  state_abbrv = c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
    "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
    "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
    "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
  )
)

cat("══ State FIPS → abbreviation lookup ══\n")
cat("States:", nrow(state_fips_to_abb), "\n")


# ── Apply to pums_demographic_cells ─────────────────────────────────────────
pums_demographic_cells <- pums_demographic_cells %>%
  left_join(state_fips_to_abb, by = "state_cat") %>%
  mutate(state_cd = paste0(state_abbrv, "-", cd_cat))


cat("\n══ Verify pums_demographic_cells ══\n")
cat("Rows:", nrow(pums_demographic_cells), "\n")
cat("Unique state_cd values:", n_distinct(pums_demographic_cells$state_cd), "(expect 435)\n")
cat("NAs in state_abbrv:", sum(is.na(pums_demographic_cells$state_abbrv)), "\n")
cat("NAs in state_cd:   ", sum(is.na(pums_demographic_cells$state_cd)), "\n")

# Show sample of new column
cat("\nSample state_cd values:\n")
pums_demographic_cells %>%
  distinct(state_cat, cd_cat, state_abbrv, state_cd) %>%
  slice_sample(n = 10) %>%
  print()

# ── Apply same to CES ────────────────────────────────────────────────────────
# Load if needed (using v2 which has the final renumbering applied)

if (!exists("ces_with_cd")) {
  cat("ces_with_cd not in memory -- loading...\n")
  ces_with_cd <- readRDS(
    "/Users/binampoudyal/Downloads/Stratification_Frame_Building/ces_with_cd_v2.rds"
  )
  cat("Loaded.\n")
} else {
  cat("ces_with_cd already in memory.\n")
}

ces_with_cd <- ces_with_cd %>%
  left_join(state_fips_to_abb, by = "state_cat") %>%
  mutate(state_cd = paste0(state_abbrv, "-", cd_cat))


cat("\n══ Verify ces_with_cd ══\n")
cat("Rows:", nrow(ces_with_cd), "\n")
cat("Unique respondents:", n_distinct(ces_with_cd$caseid), "\n")
cat("Unique state_cd:", n_distinct(ces_with_cd$state_cd), "(expect 435)\n")
cat("NAs in state_abbrv:", sum(is.na(ces_with_cd$state_abbrv)), "\n")
cat("NAs in state_cd:", sum(is.na(ces_with_cd$state_cd)), "\n")

cat("\nSample state_cd values:\n")
ces_with_cd %>%
  distinct(state_cat, cd_cat, state_abbrv, state_cd) %>%
  slice_sample(n = 10) %>%
  print()

# ── Save updated versions with state_abbrv and state_cd columns ──────────────

saveRDS(pums_demographic_cells, 
        "/Users/binampoudyal/Downloads/Stratification_Frame_Building/pums_demographic_cells.rds")

saveRDS(ces_with_cd, 
        "/Users/binampoudyal/Downloads/Stratification_Frame_Building/ces_with_cd_v2.rds")


# ── Verify file sizes and quick reload check ─────────────────────────────────
cat("══ Saved files ══\n")
cat("pums_demographic_cells.rds:", 
    round(file.size("/Users/binampoudyal/Downloads/Stratification_Frame_Building/pums_demographic_cells.rds") / 1e6, 2), 
    "MB\n")
cat("ces_with_cd_v2.rds:        ", 
    round(file.size("/Users/binampoudyal/Downloads/Stratification_Frame_Building/ces_with_cd_v2.rds") / 1e6, 2), 
    "MB\n")

# Quick verify columns exist
cat("\nColumns in saved pums_demographic_cells:\n")
print(names(pums_demographic_cells))

cat("\nColumns in saved ces_with_cd:\n")
print(grep("state_abbrv|state_cd", names(ces_with_cd), value = TRUE))


##--------------------------------------------------##
## Script to impute vote 2024
##--------------------------------------------------##

#--------------------------------------------
#Step 0: Impute CD level vote 2024 share
#------------------------------------------
# ── Load latest stratification frame/demographic cells if not already in memory ──────────────────────────
# pums_demographic_cells is the canonical poststratification frame: one row
# per (state, CD, age, gender, race, hispanic, education) cell, with cell_pop
# as the weighted population in that cell. This is derived from pums_frame
# (the row-level PUMS) and is the smaller, modeling-ready version.

if (!exists("pums_demographic_cells")) {
  cat("pums_demographic_cells not in memory -- loading from disk...\n")
  pums_demographic_cells <- readRDS(
    "/Users/binampoudyal/Downloads/Stratification_Frame_Building/pums_demographic_cells.rds"
  )
  cat("Loaded.\n")
} else {
  cat("pums_demographic_cells already in memory -- skipping reload.\n")
}

cat("Rows:", nrow(pums_demographic_cells), "\n")
cat("Cols:", ncol(pums_demographic_cells), "\n")
cat("Population sum:", round(sum(pums_demographic_cells$cell_pop)), "\n")
cat("Unique state+CD combos:", 
    n_distinct(paste(pums_demographic_cells$state_cat, 
                     pums_demographic_cells$cd_cat)), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 14: Compute CD-level demographic proportions from pums_demographic_cells
#
# Purpose: For each of the 435 congressional districts, compute the proportion
#          of weighted population in each demographic category. This produces
#          a CD-level dataset where each row is one CD with ~29 demographic
#          feature columns, used as predictors in Roberto's CART model.
#
# Input:  pums_demographic_cells (497,836 cells, one row per unique demographic
#         × geographic combination, with cell_pop = weighted population)
#
# Output: cd_demographics (435 rows, one per CD, with proportion columns)
#
# Strategy:
#   For each demographic variable in turn (age, gender, race, hispanic, educ):
#     1. Aggregate cell_pop by (state_cd, category)
#     2. Compute proportion within each state_cd (sums to 1 per CD)
#     3. Pivot wider so each category value becomes a column
#   Then join all five wide tables together on state_cd.
#
# Naming: pct_age_18_22, pct_male, pct_race_white, pct_hisp_hispanic,
#         pct_educ_post_grad, etc.
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)

# ── 1. Clean factor levels into column-name-friendly strings ────────────────
# Factor levels contain hyphens, spaces, slashes, and "+" symbols that don't
# work cleanly as R column names. We mutate the factor variables to use
# underscored, lowercase strings that will produce valid column names when
# we pivot wider.

pums_for_aggregation <- pums_demographic_cells %>%
  mutate(
    # Age: "18-22" → "18_22", "83+" → "83_plus"
    age_cat = as.character(age_cat) %>%
      str_replace_all("-", "_") %>%
      str_replace("\\+", "_plus"),
    
    # Gender: "Male" → "male", "Female" → "female"
    gender_cat = tolower(as.character(gender_cat)),
    
    # Race: "White" → "white", "Native American" → "native_american",
    #       "Other/Multi" → "other_multi"
    race_cat = as.character(race_cat) %>%
      tolower() %>%
      str_replace_all(" ", "_") %>%
      str_replace_all("/", "_"),
    
    # Hispanic: "Hispanic" → "hispanic", "Not Hispanic" → "not_hispanic"
    hispanic_cat = tolower(as.character(hispanic_cat)) %>%
      str_replace_all(" ", "_"),
    
    # Education: "No HS" → "no_hs", "HS grad" → "hs_grad", 
    #            "Some college" → "some_college", "2-year" → "two_year",
    #            "4-year" → "four_year", "Post-grad" → "post_grad"
    educ_cat = as.character(educ_cat) %>%
      tolower() %>%
      str_replace_all(" ", "_") %>%
      str_replace_all("-", "_") %>%
      str_replace("^2_year$", "two_year") %>%
      str_replace("^4_year$", "four_year")
  )

# Quick check: confirm the new factor levels look right
cat("══ Cleaned factor levels ══\n")
cat("age_cat:     ", paste(sort(unique(pums_for_aggregation$age_cat)), collapse = ", "), "\n")
cat("gender_cat:  ", paste(sort(unique(pums_for_aggregation$gender_cat)), collapse = ", "), "\n")
cat("race_cat:    ", paste(sort(unique(pums_for_aggregation$race_cat)), collapse = ", "), "\n")
cat("hispanic_cat:", paste(sort(unique(pums_for_aggregation$hispanic_cat)), collapse = ", "), "\n")
cat("educ_cat:    ", paste(sort(unique(pums_for_aggregation$educ_cat)), collapse = ", "), "\n")


# ── 2. Helper function to compute CD-level proportions for one variable ─────
# 
# Takes a data frame, the variable name (as a string), and a column-name
# prefix. Returns a wide-format table with one row per state_cd and one
# column per category of the variable, containing the proportion of that
# CD's population in that category.
#
# The !!sym(var_name) syntax is dplyr's "tidy evaluation" -- it lets us
# pass a variable name as a string and use it inside group_by() and
# pivot_wider() as if we'd typed the name directly.

compute_proportions <- function(data, var_name, prefix) {
  data %>%
    # Group by CD and the demographic variable
    group_by(state_cd, !!sym(var_name)) %>%
    
    # Sum population in each (CD, category) combination
    summarise(cat_pop = sum(cell_pop), .groups = "drop_last") %>%
    
    # Compute proportion within each CD
    # (still grouped by state_cd from drop_last, so sum() is per-CD)
    mutate(prop = cat_pop / sum(cat_pop)) %>%
    
    # Keep only what we need
    select(state_cd, !!sym(var_name), prop) %>%
    
    # Reshape: each category becomes a column with proportion as its value
    pivot_wider(
      names_from   = !!sym(var_name),
      values_from  = prop,
      names_prefix = prefix,
      values_fill  = 0   # missing (CD, category) combos get 0
    )
}


# ── 3. Compute proportions for each demographic variable ─────────────────────

age_props <- compute_proportions(
  pums_for_aggregation, "age_cat", "pct_age_"
)

gender_props <- compute_proportions(
  pums_for_aggregation, "gender_cat", "pct_"
)

race_props <- compute_proportions(
  pums_for_aggregation, "race_cat", "pct_race_"
)

hispanic_props <- compute_proportions(
  pums_for_aggregation, "hispanic_cat", "pct_hisp_"
)

educ_props <- compute_proportions(
  pums_for_aggregation, "educ_cat", "pct_educ_"
)


# ── 4. CD-level total population (useful for diagnostics) ────────────────────
cd_pops <- pums_demographic_cells %>%
  group_by(state_cd) %>%
  summarise(cd_pop = sum(cell_pop), .groups = "drop")


# ── 5. Combine all proportion tables and total pop into one CD-level dataset ─
cd_demographics <- cd_pops %>%
  left_join(age_props,      by = "state_cd") %>%
  left_join(gender_props,   by = "state_cd") %>%
  left_join(race_props,     by = "state_cd") %>%
  left_join(hispanic_props, by = "state_cd") %>%
  left_join(educ_props,     by = "state_cd")


# ── 6. Verification ──────────────────────────────────────────────────────────

cat("\n══ cd_demographics structure ══\n")
cat("Rows (CDs):", nrow(cd_demographics), "(expect 435)\n")
cat("Columns:   ", ncol(cd_demographics), "\n")

cat("\nColumn names:\n")
print(names(cd_demographics))

cat("\n══ First few rows ══\n")
print(head(cd_demographics, 3))

# Each demographic variable's proportions should sum to 1 within each CD
cat("\n══ Proportion sum checks ══\n")

# Get column names by prefix
age_cols      <- grep("^pct_age_",  names(cd_demographics), value = TRUE)
gender_cols   <- c("pct_male", "pct_female")
race_cols     <- grep("^pct_race_", names(cd_demographics), value = TRUE)
hispanic_cols <- grep("^pct_hisp_", names(cd_demographics), value = TRUE)
educ_cols     <- grep("^pct_educ_", names(cd_demographics), value = TRUE)

cat("Age proportions (n=", length(age_cols), "), CD sums (should all be 1):\n", sep = "")
cat("  Range:", round(min(rowSums(cd_demographics[age_cols])), 4), "to",
    round(max(rowSums(cd_demographics[age_cols])), 4), "\n")

cat("Gender proportions (n=", length(gender_cols), "), CD sums (should all be 1):\n", sep = "")
cat("  Range:", round(min(rowSums(cd_demographics[gender_cols])), 4), "to",
    round(max(rowSums(cd_demographics[gender_cols])), 4), "\n")

cat("Race proportions (n=", length(race_cols), "), CD sums (should all be 1):\n", sep = "")
cat("  Range:", round(min(rowSums(cd_demographics[race_cols])), 4), "to",
    round(max(rowSums(cd_demographics[race_cols])), 4), "\n")

cat("Hispanic proportions (n=", length(hispanic_cols), "), CD sums (should all be 1):\n", sep = "")
cat("  Range:", round(min(rowSums(cd_demographics[hispanic_cols])), 4), "to",
    round(max(rowSums(cd_demographics[hispanic_cols])), 4), "\n")

cat("Education proportions (n=", length(educ_cols), "), CD sums (should all be 1):\n", sep = "")
cat("  Range:", round(min(rowSums(cd_demographics[educ_cols])), 4), "to",
    round(max(rowSums(cd_demographics[educ_cols])), 4), "\n")

cat("\nTotal feature columns:", 
    length(age_cols) + length(gender_cols) + length(race_cols) + 
      length(hispanic_cols) + length(educ_cols), "\n")


# ── 7. Save ──────────────────────────────────────────────────────────────────
saveRDS(cd_demographics,
        "/Users/binampoudyal/Downloads/Stratification_Frame_Building/cd_demographics.rds")

cat("\nSaved cd_demographics.rds\n")
cat("File size:",
    round(file.size("/Users/binampoudyal/Downloads/Stratification_Frame_Building/cd_demographics.rds") / 1e6, 2),
    "MB\n")



###------------Constructing a column for 2024 house shares-----#

# Quick look at the codebook to confirm column names and codes
codebook_path <- "/Users/binampoudyal/Downloads/dataverse_files_house_votes_actual/codebook-us-house-1976–2024.md"

cat("══ Codebook contents ══\n")
cat(readLines(codebook_path), sep = "\n")

library(data.table)

house_raw <- fread(
  "/Users/binampoudyal/Downloads/dataverse_files_house_votes_actual/1976-2024-house.tab",
  sep = ","
)

cat("══ Data structure ══\n")
cat("Rows:", nrow(house_raw), "\n")
cat("Cols:", ncol(house_raw), "\n")

cat("\nColumn names:\n")
print(names(house_raw))

cat("\nFirst 5 rows:\n")
print(head(house_raw, 5))




library(data.table)
library(tidyverse)

# Convert to tibble for tidyverse-friendly printing
house_raw <- as_tibble(house_raw)

# Filter to 2024 general elections
house_2024 <- house_raw %>%
  filter(year == 2024, stage == "GEN")

cat("══ 2024 general elections ══\n")
cat("Rows:", nrow(house_2024), "\n")

# Party label distribution
cat("\n══ Party label distribution in 2024 ══\n")
house_2024 %>%
  count(party) %>%
  arrange(desc(n)) %>%
  print(n = Inf)

# Flags
cat("\n══ Flag counts in 2024 ══\n")
cat("special  TRUE:", sum(house_2024$special, na.rm = TRUE), "\n")
cat("runoff   TRUE:", sum(house_2024$runoff, na.rm = TRUE), "\n")
cat("writein  TRUE:", sum(house_2024$writein, na.rm = TRUE), "\n")
cat("fusion   TRUE:", sum(house_2024$fusion_ticket, na.rm = TRUE), "\n")

# State coverage
cat("\n══ States in 2024 ══\n")
cat("Unique states:", n_distinct(house_2024$state), "\n")
print(sort(unique(house_2024$state_po)))

# At-large districts
cat("\n══ At-large districts (district = 0) ══\n")
house_2024 %>%
  filter(district == 0) %>%
  count(state_po) %>%
  print()

# Sample
cat("\n══ Sample 2024 rows ══\n")
print(head(house_2024 %>% 
             select(state_po, district, candidate, party, candidatevotes, totalvotes, writein), 
           15))

sink("output.txt")

# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 15: Aggregate 2024 House election results to CD-level party shares
#
# Purpose: Process MIT Election Lab's 2024 House results into CD-level vote
#          counts and shares (Dem, Rep, Other) for use as the dependent
#          variable in Roberto's CART imputation model.
#
# Input:  house_raw (33,805 rows, 1976-2024 House results, MIT Election Lab)
# Output: cd_house_2024 (435 rows, one per state-district combo for 2024)
#
# Required output columns:
#   state_cd          -- e.g. "TX-1", "CA-12" (matches our harmonization)
#   dem_votes         -- total Democratic votes in CD
#   rep_votes         -- total Republican votes in CD
#   other_votes       -- everything else (third party + independent + write-in)
#   total_house_votes -- dem + rep + other (total votes cast in House race)
#
# Note: The "No Vote" share is computed in a later step by joining with the
# CD's voting-age population from PUMS:
#     no_vote_share = (cd_pop - total_house_votes) / cd_pop
#
# ──────────────────────────────────────────────────────────────────────────────
# Methodological decisions (documented for the methods section of the paper):
#
# 1. PARTY CATEGORIZATION (3-way: Dem / Rep / Other)
#
#    Democratic family (counted as "dem"):
#      - DEMOCRAT (main label, used in 48 states)
#      - DEMOCRATIC-FARMER-LABOR (Minnesota's Democratic Party affiliate)
#      - DEMOCRATIC-NONPARTISAN LEAGUE (North Dakota's Democratic Party affiliate)
#
#    Republican family (counted as "rep"):
#      - REPUBLICAN (sole label)
#
#    Everything else (counted as "other"):
#      - Third parties (LIBERTARIAN, GREEN, CONSTITUTION, etc.)
#      - Minor and write-in candidates (where party is NA)
#      - Independent candidates (INDEPENDENT, UNAFFILIATED, etc.)
#      - Fusion party labels (WORKING FAMILIES, CONSERVATIVE, etc.) when the
#        candidate's primary party is itself "other"
#
# 2. FUSION TICKETS (NY, CT, NJ, SC) -- candidate aggregation approach
#
#    In fusion-voting states, a single candidate can appear under multiple
#    party labels on the ballot. For example, in NY a Democratic candidate
#    might also appear under the WORKING FAMILIES party line. Each row in
#    the MIT data represents the votes received under one party line, so the
#    candidate's total votes are split across multiple rows.
#
#    Naive approach (sum by party label) would count the WORKING FAMILIES
#    votes as "Other", artificially inflating the third-party share and
#    deflating the major-party share. In NY, this can misestimate the
#    Democratic share by 2-5 percentage points per CD.
#
#    Our approach: aggregate by candidate first, identify each candidate's
#    primary party (the party label under which they received the most
#    votes), then attribute the candidate's total votes (summed across all
#    party lines) to that primary party.
#
#    Example: if Jane Smith got 80,000 votes as DEMOCRAT and 5,000 as
#    WORKING FAMILIES, we identify DEMOCRAT as her primary party and
#    attribute the full 85,000 to "dem". This recovers the meaningful
#    political alignment that fusion voters intended.
#
#    Tradeoff: "primary party" is a heuristic based on vote share. In rare
#    edge cases (e.g., a true independent who happens to win more votes
#    under a fusion party line than under the INDEPENDENT line), this could
#    misclassify. Such edge cases are rare and the heuristic produces
#    reasonable results in 99%+ of cases.
#
# 3. WRITE-INS
#
#    119 write-in rows in 2024 have candidate = "WRITEIN" and party = NA.
#    Under our candidate-aggregation approach, all write-ins in a single CD
#    get grouped together (they share the candidate name "WRITEIN"). Their
#    primary party is NA, which maps to "other". Write-in totals are
#    typically small (a handful to hundreds of votes per CD).
#
# 4. AT-LARGE DISTRICTS
#
#    The MIT dataset uses district = 0 for at-large states (AK, DE, ND, SD,
#    VT, WY, and DC's non-voting delegate). Our PUMS/CES geography uses
#    district = 1 for the same states. We recode district 0 → 1 for
#    consistency.
#
# 5. DC EXCLUSION
#
#    DC has only a non-voting delegate, not a House seat. Excluded.
#
# 6. SPECIAL ELECTIONS AND RUNOFFS
#
#    Filtered: stage == "GEN". The 2024 data has 0 special elections and
#    0 runoffs, so no additional handling needed.
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)


# ── 1. Filter raw data to 2024 general elections ────────────────────────────
# Drop DC, recode at-large district encoding (0 → 1), and create state_cd
# identifier consistent with our PUMS/CES harmonization.

house_2024 <- house_raw %>%
  filter(year == 2024, stage == "GEN") %>%
  filter(state_po != "DC") %>%
  mutate(
    district = if_else(district == 0L, 1L, as.integer(district)),
    state_cd = paste0(state_po, "-", district)
  )

cat("══ After initial filtering ══\n")
cat("Rows:", nrow(house_2024), "\n")
cat("Unique state_cd:", n_distinct(house_2024$state_cd), "(expect 435)\n")


# ── 2. Aggregate votes per candidate per CD ─────────────────────────────────
# 
# This is the first key step of the candidate-aggregation approach. For each
# unique (state_cd, candidate) combination, sum the votes across all party
# lines. This collapses the fusion-ticket multi-row representation into a
# single candidate total.
#
# For example, NY's Jane Smith with rows under DEMOCRAT (80,000) and
# WORKING FAMILIES (5,000) becomes one row with total_candidate_votes = 85,000.
#
# Note: For candidates listed under different name spellings or capitalizations
# this approach could fail, but the MIT data uses consistent spellings within
# a single year.

candidate_totals <- house_2024 %>%
  group_by(state_cd, candidate) %>%
  summarise(
    total_candidate_votes = sum(candidatevotes, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n══ Candidate aggregation ══\n")
cat("Rows after collapsing party lines per candidate:", nrow(candidate_totals), "\n")


# ── 3. Identify each candidate's primary party ──────────────────────────────
#
# For each (state_cd, candidate), find the party label under which they
# received the most votes. This becomes their "primary party" and represents
# the candidate's actual political alignment.
#
# Implementation: for each (state_cd, candidate), keep only the row with
# the maximum candidatevotes value (ties broken by first occurrence).
#
# Edge case: write-ins all share candidate = "WRITEIN" within a CD, but
# their party is NA. Their "primary party" is therefore NA, which we'll
# map to "other" in the next step.

candidate_primary_party <- house_2024 %>%
  group_by(state_cd, candidate) %>%
  slice_max(candidatevotes, n = 1, with_ties = FALSE) %>%
  select(state_cd, candidate, primary_party = party) %>%
  ungroup()

cat("\n══ Primary party identification ══\n")
cat("Candidates with assigned primary party:", nrow(candidate_primary_party), "\n")
cat("Candidates with NA primary party (write-ins):", 
    sum(is.na(candidate_primary_party$primary_party)), "\n")


# ── 4. Combine: candidate's full vote total + their primary party ───────────

candidates_with_party <- candidate_totals %>%
  left_join(candidate_primary_party, by = c("state_cd", "candidate"))


# ── 5. Map primary party to 3-way category (dem / rep / other) ─────────────
#
# Democratic-family parties (DEMOCRAT, DFL, DNL) → "dem"
# Republican → "rep"
# Everything else (third parties, independents, fusion parties, write-ins,
# and any NA from write-ins) → "other"

candidates_with_party <- candidates_with_party %>%
  mutate(party_category = case_when(
    primary_party == "DEMOCRAT"                      ~ "dem",
    primary_party == "DEMOCRATIC-FARMER-LABOR"       ~ "dem",
    primary_party == "DEMOCRATIC-NONPARTISAN LEAGUE" ~ "dem",
    primary_party == "REPUBLICAN"                    ~ "rep",
    TRUE                                              ~ "other"
  ))

cat("\n══ Party category breakdown ══\n")
candidates_with_party %>%
  count(party_category) %>%
  arrange(desc(n)) %>%
  print()


# ── 6. Aggregate by CD and party category ───────────────────────────────────
# Sum candidate totals within each (state_cd, party_category). This produces
# the cleaned vote counts per party group per CD.

cd_votes_long <- candidates_with_party %>%
  group_by(state_cd, party_category) %>%
  summarise(votes = sum(total_candidate_votes, na.rm = TRUE), .groups = "drop")


# ── 7. Reshape to wide: one row per CD with dem/rep/other vote columns ─────

cd_house_2024 <- cd_votes_long %>%
  pivot_wider(
    names_from   = party_category,
    values_from  = votes,
    names_prefix = "",
    values_fill  = 0   # CDs with no candidates in a category get 0
  ) %>%
  rename(
    dem_votes   = dem,
    rep_votes   = rep,
    other_votes = other
  ) %>%
  mutate(
    total_house_votes = dem_votes + rep_votes + other_votes
  )


# ── 8. Verification ─────────────────────────────────────────────────────────

cat("\n══ cd_house_2024 structure ══\n")
cat("Rows:", nrow(cd_house_2024), "(expect 435)\n")
cat("Cols:", ncol(cd_house_2024), "\n")
print(head(cd_house_2024, 5))


# CDs per state -- sanity check we have right number for each state
cat("\n══ CDs per state ══\n")
cd_house_2024 %>%
  mutate(state_po = sub("-.*", "", state_cd)) %>%
  count(state_po, name = "n_cds") %>%
  arrange(state_po) %>%
  print(n = Inf)


# Vote total sanity check
cat("\n══ Vote total sanity check ══\n")
cat("Min total_house_votes: ", min(cd_house_2024$total_house_votes), "\n")
cat("Max total_house_votes: ", max(cd_house_2024$total_house_votes), "\n")
cat("Mean total_house_votes:", round(mean(cd_house_2024$total_house_votes)), "\n")
cat("Median total_house_votes:", round(median(cd_house_2024$total_house_votes)), "\n")
cat("\nCDs with 0 votes total:", sum(cd_house_2024$total_house_votes == 0), "\n")


# Two-party Dem share -- check distribution looks sensible
cd_house_2024 <- cd_house_2024 %>%
  mutate(dem_two_party_share = dem_votes / (dem_votes + rep_votes))

cat("\n══ Two-party Dem share distribution ══\n")
print(summary(cd_house_2024$dem_two_party_share))


# Compare to expected: which CDs look heavily Dem or heavily Rep?
cat("\n══ Top 5 most Democratic CDs ══\n")
cd_house_2024 %>%
  arrange(desc(dem_two_party_share)) %>%
  head(5) %>%
  select(state_cd, dem_votes, rep_votes, dem_two_party_share) %>%
  print()

cat("\n══ Top 5 most Republican CDs ══\n")
cd_house_2024 %>%
  arrange(dem_two_party_share) %>%
  head(5) %>%
  select(state_cd, dem_votes, rep_votes, dem_two_party_share) %>%
  print()


# ── 9. Save ─────────────────────────────────────────────────────────────────

saveRDS(cd_house_2024,
        "/Users/binampoudyal/Downloads/Stratification_Frame_Building/cd_house_2024.rds")

cat("\nSaved cd_house_2024.rds\n")
cat("File size:",
    round(file.size("/Users/binampoudyal/Downloads/Stratification_Frame_Building/cd_house_2024.rds") / 1e6, 2),
    "MB\n")

#Problamatic cases diagnostic

cat("══ Potential training-data issues ══\n\n")

cat("CDs with abnormally low total votes (< 50,000):\n")
cd_house_2024 %>%
  filter(total_house_votes < 50000) %>%
  arrange(total_house_votes) %>%
  select(state_cd, dem_votes, rep_votes, other_votes, total_house_votes) %>%
  print(n = Inf)

cat("\nCDs with 0 Democratic votes (no Dem on ballot or all-Rep race):\n")
cd_house_2024 %>%
  filter(dem_votes == 0) %>%
  select(state_cd, dem_votes, rep_votes, other_votes, total_house_votes) %>%
  print(n = Inf)

cat("\nCDs with 0 Republican votes (top-two Dem-vs-Dem race or no Rep on ballot):\n")
cd_house_2024 %>%
  filter(rep_votes == 0) %>%
  select(state_cd, dem_votes, rep_votes, other_votes, total_house_votes) %>%
  print(n = Inf)


# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 15B: Flag training-eligible CDs (filter out uncontested races)
#
# Purpose: Identify CDs where both major parties had candidates in 2024.
# These contested-race CDs provide cleaner demographic-vote signal for
# training Roberto's CART model. The "training_eligible" flag is added to
# cd_house_2024 — we keep all 435 CDs in the dataset (for downstream
# operations) but only use the contested ones for model fitting.
#
# Filtering criterion: both dem_votes > 100 AND rep_votes > 100
#   - The >100 threshold (rather than >0) catches the MIT placeholder values
#     of 1 vote used for uncontested races in some states (e.g., FL, OK)
#   - Real contested races have tens of thousands of votes per major party,
#     so the threshold of 100 is generous and easily distinguishes
#     placeholders/zeros from real contested results
#
# Excluded from training (~38 CDs):
#   - 20 CDs with dem_votes = 0 (no Democrat ran)
#   - 18 CDs with rep_votes = 0 (no Republican ran or top-two California
#     race with two Democrats)
#   - 2 CDs (FL-20, OK-3) with MIT placeholder of 1 vote total
#
# Methodological note: while these uncontested-race outcomes do contain
# information about district partisan character, the vote-share
# distribution for uncontested races is structurally different from
# contested races. Since the 2026 redistricted CDs will presumably have
# contested races, training on contested CDs only provides a more
# representative mapping from demographics to House vote shares.
# ══════════════════════════════════════════════════════════════════════════════


# ── Add training_eligible flag ───────────────────────────────────────────────
# 
# Logical column that is TRUE only when BOTH major parties received more
# than 100 votes in the district:
#   - dem_votes > 100  AND  rep_votes > 100
# 
# Why both conditions (with logical AND)?
#   - "dem_votes > 100" excludes CDs where Democrats didn't field a candidate
#     (dem_votes = 0) or where only a placeholder was recorded (dem_votes = 1)
#   - "rep_votes > 100" similarly excludes Republican-uncontested cases
#   - Combining with AND keeps only CDs where both parties had meaningful
#     vote totals, which is our definition of a "contested" race
#
# Why 100 and not 0?
#   - Catches the MIT placeholder values (FL, OK uncontested races use 1 as
#     the vote total marker, per their codebook)
#   - 100 is well below normal contested-race minimums (real contested
#     races have ~50,000+ votes per major party), so no real contested
#     race is excluded by this threshold

cd_house_2024 <- cd_house_2024 %>%
  mutate(training_eligible = dem_votes > 100 & rep_votes > 100)


# ── Verify the flag was applied correctly ───────────────────────────────────
# Count TRUE and FALSE values, and compute percentages.
# pct = (n / total) * 100 with rounding to 1 decimal place.

cat("══ Training eligibility breakdown ══\n")
cd_house_2024 %>%
  count(training_eligible) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()


# Summary statistics on training set size
cat("\nTotal CDs:                 ", nrow(cd_house_2024), "\n")
cat("Training-eligible CDs:     ", sum(cd_house_2024$training_eligible), "\n")
cat("Excluded from training:    ", sum(!cd_house_2024$training_eligible), "\n")


# ── Diagnostic: which states have the most excluded CDs? ────────────────────
# This helps us understand where the uncontested races concentrate
# (typically deep-red rural states and California's top-two primary races).
#
# Step-by-step:
#   1. Filter to only excluded (training_eligible == FALSE) rows
#   2. Extract state postal code from "state_cd" (e.g. "TX-1" → "TX")
#      using sub() to remove everything from "-" onward
#   3. Count by state and sort descending

cat("\n══ Excluded CDs by state ══\n")
cd_house_2024 %>%
  filter(!training_eligible) %>%
  mutate(state_po = sub("-.*", "", state_cd)) %>%
  count(state_po, name = "n_excluded") %>%
  arrange(desc(n_excluded)) %>%
  print()


# ── Quality check: two-party Dem share distribution ─────────────────────────
# 
# dem_two_party_share = dem_votes / (dem_votes + rep_votes)
#   - Computed in the previous script (script 15)
#   - Ranges from 0 to 1
#   - 0.5 = exactly tied
#   - 1.0 = pure Democratic (would be the all-Dem races we just filtered out)
#   - 0.0 = pure Republican (would be the all-Rep races we just filtered out)
#
# After filtering to contested races, we should see:
#   - Tighter distribution centered around 0.5
#   - No extreme values at 0 or 1 (since those required uncontested races)
#   - Typical range from ~0.10 (very Republican but with some Dem support)
#     to ~0.90 (very Democratic but with some Rep support)

cat("\n══ Two-party Dem share among training-eligible CDs ══\n")
cd_house_2024 %>%
  filter(training_eligible) %>%
  pull(dem_two_party_share) %>%
  summary() %>%
  print()


# ── Save updated file with training_eligible flag ────────────────────────────
# Overwrites previous version of cd_house_2024.rds with the new column added.

saveRDS(cd_house_2024,
        "/Users/binampoudyal/Downloads/Stratification_Frame_Building/cd_house_2024.rds")

cat("\nSaved cd_house_2024.rds with training_eligible column\n")


# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 15D: Add 4-way vote shares to cd_house_2024 (cd_pop denominator)
#
# Purpose: Extend cd_house_2024 with four vote shares computed against the
#          citizen voting-age population (cd_pop) rather than total House
#          votes. This adds no_vote_share as a fourth outcome and changes
#          the denominator for the existing three.
#
# Why this change:
#   The earlier 3-way shares (Script 15C) were conditional on voting:
#     dem_share = dem_votes / total_house_votes
#   This couldn't express the "no vote" outcome -- people eligible to vote
#   but who didn't participate. Roberto's revision asks us to model turnout
#   as a 4th outcome alongside the three party shares.
#
#   With cd_pop as the denominator:
#     dem_share     = dem_votes   / cd_pop
#     rep_share     = rep_votes   / cd_pop
#     other_share   = other_votes / cd_pop
#     no_vote_share = (cd_pop - total_house_votes) / cd_pop
#   All four sum to 1 per CD.
#
#   These NEW share columns OVERWRITE the conditional ones from Script 15C.
#
# Input:
#   cd_house_2024.rds (from Script 15C)
#     - existing columns including dem_share, rep_share, other_share
#       (conditional on voting; will be overwritten)
#     - dem_votes, rep_votes, other_votes, total_house_votes
#     - training_eligible
#
#   cd_demographics.rds
#     - state_cd + cd_pop (used as denominator)
#
# Output:
#   cd_house_2024.rds (overwritten with 4 shares against cd_pop)
#
# Note on training_eligible:
#   We do NOT modify training_eligible in this script. The flag was set
#   correctly in Script 15B based on contested-race criteria (both major
#   parties got >100 votes). The 38 uncontested CDs remain training-
#   ineligible. Their inflated no_vote_share values are still computed
#   here (we're just doing arithmetic), but they won't enter the training
#   set when we get to CART fitting.
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)


# ── 1. Load inputs ──────────────────────────────────────────────────────────
# Both files should already exist from earlier scripts.

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"

cd_house_2024   <- readRDS(paste0(base_path, "cd_house_2024.rds"))
cd_demographics <- readRDS(paste0(base_path, "cd_demographics.rds"))

cat("══ Inputs loaded ══\n")
cat("cd_house_2024 rows:  ", nrow(cd_house_2024),   "(expect 435)\n")
cat("cd_demographics rows:", nrow(cd_demographics), "(expect 435)\n")

cat("\nCurrent cd_house_2024 columns:\n")
print(names(cd_house_2024))


# ── 2. Attach cd_pop and rebuild the 4 shares ───────────────────────────────
#
# Step 2a: drop existing share columns if present (defensive — in case we
#          re-run this script). Keeps the file's state predictable.
# Step 2b: drop cd_pop if already attached (so we don't get cd_pop.x / .y
#          from a prior join).
# Step 2c: bring in cd_pop from cd_demographics.
# Step 2d: compute the 4 shares.

cd_house_2024 <- cd_house_2024 %>%
  select(-any_of(c("dem_share", "rep_share", "other_share", "no_vote_share",
                   "cd_pop"))) %>%
  left_join(cd_demographics %>% select(state_cd, cd_pop), by = "state_cd") %>%
  mutate(
    dem_share     = dem_votes   / cd_pop,
    rep_share     = rep_votes   / cd_pop,
    other_share   = other_votes / cd_pop,
    no_vote_share = (cd_pop - total_house_votes) / cd_pop
  )


# ── 3. Verify shares sum to 1 per CD ────────────────────────────────────────

share_check <- cd_house_2024 %>%
  mutate(sum_check = dem_share + rep_share + other_share + no_vote_share) %>%
  pull(sum_check)

cat("\n══ 4-way share sum check ══\n")
cat("Range:", round(min(share_check, na.rm = TRUE), 6),
    "to", round(max(share_check, na.rm = TRUE), 6), "\n")
cat("(Should be 1.0 to 1.0)\n")
cat("NAs in sum:", sum(is.na(share_check)), "\n")


# ── 4. Inspect distribution of each share ───────────────────────────────────

cat("\n══ Distribution of 4 shares across all 435 CDs ══\n")
cd_house_2024 %>%
  select(dem_share, rep_share, other_share, no_vote_share) %>%
  pivot_longer(everything(), names_to = "share", values_to = "value") %>%
  group_by(share) %>%
  summarise(
    min    = round(min(value),    4),
    median = round(median(value), 4),
    mean   = round(mean(value),   4),
    max    = round(max(value),    4),
    .groups = "drop"
  ) %>%
  print()


# Distribution among training-eligible CDs (the clean subset)
cat("\n══ Distribution of shares (training-eligible CDs only) ══\n")
cd_house_2024 %>%
  filter(training_eligible) %>%
  select(dem_share, rep_share, other_share, no_vote_share) %>%
  pivot_longer(everything(), names_to = "share", values_to = "value") %>%
  group_by(share) %>%
  summarise(
    min    = round(min(value),    4),
    median = round(median(value), 4),
    mean   = round(mean(value),   4),
    max    = round(max(value),    4),
    .groups = "drop"
  ) %>%
  print()


# ── 5. Spot check: a few specific CDs ────────────────────────────────────────
# Spot check both contested and uncontested CDs.

cat("\n══ Spot check: random training-eligible CDs ══\n")
set.seed(42)
cd_house_2024 %>%
  filter(training_eligible) %>%
  slice_sample(n = 5) %>%
  select(state_cd, cd_pop, total_house_votes,
         dem_share, rep_share, other_share, no_vote_share) %>%
  print()

cat("\n══ Spot check: training-ineligible CDs (inflated no_vote expected) ══\n")
cd_house_2024 %>%
  filter(!training_eligible) %>%
  arrange(desc(no_vote_share)) %>%
  head(5) %>%
  select(state_cd, cd_pop, total_house_votes,
         dem_share, rep_share, other_share, no_vote_share) %>%
  print()


# ── 6. Save updated cd_house_2024 ──────────────────────────────────────────

saveRDS(cd_house_2024, paste0(base_path, "cd_house_2024.rds"))

cat("\nSaved cd_house_2024.rds with 4-way shares (cd_pop denominator)\n")
cat("Final columns:\n")
print(names(cd_house_2024))







##-----State level presidential election results 2024 -----#

library(tidyverse)

pres_path <- "/Users/binampoudyal/Downloads/1976-2024-president.csv"

pres_raw <- read_csv(pres_path)

cat("══ Data structure ══\n")
cat("Rows:", nrow(pres_raw), "\n")
cat("Cols:", ncol(pres_raw), "\n")

cat("\nColumn names:\n")
print(names(pres_raw))

cat("\nFirst 5 rows:\n")
print(head(pres_raw, 5))

cat("\nUnique years (most recent 5):\n")
print(tail(sort(unique(pres_raw$year)), 5))

# Specifically inspect 2024
cat("\n══ 2024 data ══\n")
pres_2024 <- pres_raw %>% filter(year == 2024)

cat("Rows in 2024:", nrow(pres_2024), "\n")
cat("Unique states:", n_distinct(pres_2024$state), "\n")
cat("Party label distribution:\n")
print(table(pres_2024$party_simplified, useNA = "always"))
# (or whatever party column name they use)


# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 16B: Rebuild state_pres_2024 with 4-feature shares (VAP denominator)
#
# Purpose: Replace the 3-feature conditional state-level pres shares from
#          Script 16 with a 4-feature version that includes
#          state_pres_no_vote_share, computed against state-level voting-age
#          population (VAP).
#
# Why this change:
#   In Script 16, state-level pres shares were conditional on voting:
#     state_pres_dem_share = dem_pres_votes / total_pres_votes
#   To maintain symmetry with the new 4-outcome CD-level shares (which now
#   include no_vote as a 4th outcome), we recompute the state pres shares
#   against state VAP so they also sum to 1.0 across 4 categories.
#
# Methodology:
#
#   1. State-level VAP comes from cd_demographics:
#        state_vap = sum(cd_pop) within state
#      This uses our PUMS-derived citizen voting-age population, consistent
#      with the denominator used in CD-level shares.
#
#   2. For each state, compute 4 shares from 2024 pres data:
#        state_pres_dem_share      = dem_pres_votes      / state_vap
#        state_pres_rep_share      = rep_pres_votes      / state_vap
#        state_pres_other_share    = other_pres_votes    / state_vap
#        state_pres_no_vote_share  = (state_vap - total_pres_votes) / state_vap
#      These sum to 1 per state.
#
#   3. Party categorization: matches Script 16
#        DEMOCRAT   → dem
#        REPUBLICAN → rep
#        everything else → other
#
#   4. DC excluded: matches Script 16
#
# Input:
#   pres_raw           (in memory; or reloaded from 1976-2024-president.csv)
#   cd_demographics.rds (used to compute state_vap)
#
# Output:
#   state_pres_2024.rds  (overwrites the 3-feature version)
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)


# ── 1. Load inputs ──────────────────────────────────────────────────────────

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"

# Reload pres_raw if not in memory
if (!exists("pres_raw")) {
  cat("pres_raw not in memory -- loading from disk...\n")
  pres_raw <- read_csv("/Users/binampoudyal/Downloads/1976-2024-president.csv",
                       show_col_types = FALSE)
}

# Reload cd_demographics if not in memory
if (!exists("cd_demographics")) {
  cat("cd_demographics not in memory -- loading from disk...\n")
  cd_demographics <- readRDS(paste0(base_path, "cd_demographics.rds"))
}

cat("══ Inputs loaded ══\n")
cat("pres_raw rows:        ", nrow(pres_raw), "\n")
cat("cd_demographics rows: ", nrow(cd_demographics), "\n\n")


# ── 2. Build state-level VAP table ──────────────────────────────────────────
#
# Sum cd_pop across CDs within each state. This gives state-level citizen
# voting-age population (CVAP), matching the denominator used in CD-level
# 4-way shares. Total US VAP should be ~240M.
#
# state_abbrv is derived from state_cd (e.g. "TX-1" → "TX") since
# cd_demographics doesn't have it as a separate column.

state_vap <- cd_demographics %>%
  mutate(state_abbrv = sub("-.*", "", state_cd)) %>%
  group_by(state_abbrv) %>%
  summarise(state_vap = sum(cd_pop), .groups = "drop")

cat("══ State VAP ══\n")
cat("States:    ", nrow(state_vap), "(expect 50)\n")
cat("Total VAP: ", round(sum(state_vap$state_vap) / 1e6, 1), "M (expect ~240M)\n\n")


# ── 3. Filter pres data to 2024, drop DC ────────────────────────────────────

pres_2024 <- pres_raw %>%
  filter(year == 2024) %>%
  filter(state_po != "DC")

cat("══ 2024 presidential data ══\n")
cat("Rows:          ", nrow(pres_2024), "\n")
cat("Unique states: ", n_distinct(pres_2024$state_po), "(expect 50)\n\n")


# ── 4. Map party_simplified to 3-way category ───────────────────────────────
# DEMOCRAT / REPUBLICAN / OTHER (catches LIBERTARIAN, OTHER, NA write-ins)

pres_2024 <- pres_2024 %>%
  mutate(party_category = case_when(
    party_simplified == "DEMOCRAT"   ~ "dem",
    party_simplified == "REPUBLICAN" ~ "rep",
    TRUE                              ~ "other"
  ))


# ── 5. Aggregate votes by state + party category, then pivot wide ──────────
#
# After this step, each state has one row with three vote-count columns
# (dem, rep, other). total_pres_votes is the sum across the three.

state_pres_2024 <- pres_2024 %>%
  group_by(state_po, party_category) %>%
  summarise(votes = sum(candidatevotes, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = party_category,
    values_from  = votes,
    names_prefix = "",
    values_fill  = 0
  ) %>%
  rename(
    state_abbrv      = state_po,
    dem_pres_votes   = dem,
    rep_pres_votes   = rep,
    other_pres_votes = other
  ) %>%
  mutate(total_pres_votes = dem_pres_votes + rep_pres_votes + other_pres_votes)


# ── 6. Attach state_vap and compute 4 shares ────────────────────────────────
#
# Inner_join here would have the same effect since we have 50 in both;
# left_join is safer in case state codes diverge unexpectedly.

state_pres_2024 <- state_pres_2024 %>%
  left_join(state_vap, by = "state_abbrv") %>%
  mutate(
    state_pres_dem_share      = dem_pres_votes      / state_vap,
    state_pres_rep_share      = rep_pres_votes      / state_vap,
    state_pres_other_share    = other_pres_votes    / state_vap,
    state_pres_no_vote_share  = (state_vap - total_pres_votes) / state_vap
  )


# ── 7. Keep only the columns we need for downstream ─────────────────────────

state_pres_2024 <- state_pres_2024 %>%
  select(state_abbrv,
         state_pres_dem_share,
         state_pres_rep_share,
         state_pres_other_share,
         state_pres_no_vote_share)


# ── 8. Verification ─────────────────────────────────────────────────────────

cat("══ state_pres_2024 structure ══\n")
cat("Rows:", nrow(state_pres_2024), "(expect 50)\n")
cat("Cols:", ncol(state_pres_2024), "(expect 5)\n\n")
print(head(state_pres_2024, 10))


# Share sums should all be 1 (within rounding)
cat("\n══ Share sum validation ══\n")
share_sums <- state_pres_2024 %>%
  mutate(s = state_pres_dem_share + state_pres_rep_share + 
           state_pres_other_share + state_pres_no_vote_share) %>%
  pull(s)
cat("Range:", round(min(share_sums), 6), "to", round(max(share_sums), 6), "\n")
cat("States where sum != 1.0:", 
    sum(round(share_sums, 4) != 1), "\n")


# NA check
cat("\n══ NA check ══\n")
print(state_pres_2024 %>% summarise(across(everything(), ~sum(is.na(.x)))))


# Sanity: top Dem states (presidential 2024)
cat("\n══ Top 5 most Democratic states (presidential 2024) ══\n")
state_pres_2024 %>%
  arrange(desc(state_pres_dem_share)) %>%
  head(5) %>%
  print()

# Sanity: top Rep states
cat("\n══ Top 5 most Republican states (presidential 2024) ══\n")
state_pres_2024 %>%
  arrange(desc(state_pres_rep_share)) %>%
  head(5) %>%
  print()

# Highest turnout (lowest no_vote_share)
cat("\n══ Top 5 highest-turnout states (lowest no_vote share) ══\n")
state_pres_2024 %>%
  arrange(state_pres_no_vote_share) %>%
  head(5) %>%
  print()

# Lowest turnout (highest no_vote_share)
cat("\n══ Top 5 lowest-turnout states (highest no_vote share) ══\n")
state_pres_2024 %>%
  arrange(desc(state_pres_no_vote_share)) %>%
  head(5) %>%
  print()


# ── 9. Save ─────────────────────────────────────────────────────────────────

saveRDS(state_pres_2024, paste0(base_path, "state_pres_2024.rds"))

cat("\nSaved state_pres_2024.rds with 4-feature shares (VAP denominator)\n")
cat("\nFinal columns:\n")
print(names(state_pres_2024))






# Step 1: Block-level table with cd_119, cd_2026, and population
#   - Join all_bafs (block → cd_2026), cd119_redistricted (block → cd_119),
#     all_blocks_pop (block → pop)
#   - Only for the 7 redistricted states
#
# Step 2: For each (state, cd_2026), aggregate by source cd_119
#   - Get population of (cd_2026, cd_119) intersection
#   - Get total population of cd_2026
#
# Step 3: For each cd_2026, find the dominant cd_119 (largest overlap)
#   - Compute overlap_pct = pop in dominant intersection / total cd_2026 pop
#   - If overlap_pct ≥ 0.95, flag as "essentially unchanged from this cd_119"
#
# Step 4: For each "essentially unchanged" cd_2026:
#   - Look up the dominant cd_119's 2024 House results
#   - Assign those results to the new cd_2026
#
# Step 5: Update training_table:
#   - For essentially-unchanged CDs, set is_redistricted = FALSE
#     (so they enter the training set instead of prediction set)
#   - Update their dem_share, rep_share, other_share to the inherited values


# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 18: Identify essentially-unchanged 2026 CDs in redistricted states
#
# Purpose: Within each of the 7 redistricted states, identify 2026 CDs that
#          are essentially unchanged from a single 2024 (119th Congress) CD.
#          For these CDs, we can use the 2024 House results from the old CD
#          as the area-level covariate for the new CD -- no CART imputation
#          needed.
#
# Why: The 7 redistricted states had legal/political changes to their CD
#      maps for 2026, but not every district was substantially redrawn. Many
#      were renumbered slightly, shifted at the margins, or left effectively
#      unchanged. Treating all 159 CDs in the 7 states as "redistricted"
#      throws away genuine information about those whose populations are
#      effectively the same as in 2024.
#
# Output:
#   cd_2026_inheritance -- one row per 2026 CD in the 7 states, with:
#       state_fips, cd_2026, dominant_cd_119, overlap_pct, essentially_unchanged
#
# After this we update training_table:
#   - For "essentially_unchanged" CDs: set is_redistricted = FALSE, and
#     overwrite their House outcomes with the dominant_cd_119's 2024 results.
#   - For "substantially redrawn" CDs: leave as is_redistricted = TRUE
#     (these still need CART imputation).
#
# Methodology:
#   1. For each block in the 7 redistricted states: we know cd_2026 (from BAFs),
#      cd_119 (from the 119th CD BEF), and population (from tidycensus).
#   2. For each (state, cd_2026), compute population from each source cd_119.
#   3. Compute overlap_pct = pop from dominant cd_119 / total cd_2026 pop.
#      (Note: this is "new-CD-side" perspective; we ask "of the people now in
#       this new CD, what fraction came from one specific old CD?")
#   4. Flag as essentially_unchanged if overlap_pct >= 0.95.
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)


# ── 1. Build block-level table: state, cd_2026, cd_119, pop ─────────────────
#
# Three sources, all joined on block_geoid:
#   all_bafs              -- block → cd_2026 (and state_fips)
#   cd119_redistricted    -- block → cd_119
#   all_blocks_pop        -- block → 2020 population
#
# Inner joins drop blocks not in all three (very few, e.g. uninhabited
# blocks that may have CD assignments but no population, etc.)
#
# Note: cd119_redistricted already has state_fips derived from block_geoid;
# all_bafs also has state_fips. We can use either; results will match.

block_full <- all_bafs %>%
  rename(cd_2026 = district) %>%
  inner_join(
    cd119_redistricted %>% select(block_geoid, cd_119),
    by = "block_geoid"
  ) %>%
  inner_join(
    all_blocks_pop %>% rename(block_geoid = GEOID, pop = value),
    by = "block_geoid"
  )

cat("══ Block-level table ══\n")
cat("Rows:", nrow(block_full), "\n")
cat("Population covered:", sum(block_full$pop), "\n")
cat("States:\n")
print(table(block_full$state_fips))


# ── 2. Aggregate to (state, cd_2026, cd_119) population intersections ───────
#
# For each combination of (state, new CD, old CD), sum block populations.
# This tells us "how many people in new CD X used to be in old CD Y."

intersection_pop <- block_full %>%
  group_by(state_fips, cd_2026, cd_119) %>%
  summarise(pop_intersection = sum(pop), .groups = "drop")


# ── 3. For each cd_2026, find dominant source cd_119 and overlap_pct ───────
#
# overlap_pct is "new-CD-side": of the population NOW in cd_2026, what
# fraction came from the largest source cd_119?
#
# Formula:
#   overlap_pct = pop in (cd_2026, dominant_cd_119) / total pop in cd_2026
#
# If overlap_pct = 1.0: 100% of the new CD's population came from one
# specific old CD. The new CD is essentially the old CD (possibly with a
# different number).
#
# If overlap_pct = 0.6: 60% came from one old CD, 40% from others. The
# new CD is substantially different from any single old CD.

cd_2026_inheritance <- intersection_pop %>%
  group_by(state_fips, cd_2026) %>%
  # Compute total pop of this 2026 CD (for the denominator)
  mutate(cd_2026_total_pop = sum(pop_intersection)) %>%
  # Find the row with the largest intersection (the dominant source cd_119)
  slice_max(pop_intersection, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # Compute the new-CD-side overlap percentage
  mutate(
    overlap_pct           = pop_intersection / cd_2026_total_pop,
    essentially_unchanged = overlap_pct >= 0.95
  ) %>%
  rename(dominant_cd_119 = cd_119) %>%
  select(state_fips, cd_2026, dominant_cd_119, overlap_pct, 
         essentially_unchanged, cd_2026_total_pop)


cat("\n══ cd_2026_inheritance summary ══\n")
cat("Rows:", nrow(cd_2026_inheritance), "(expect ~159 across 7 states)\n")

cat("\n══ essentially_unchanged breakdown ══\n")
cd_2026_inheritance %>%
  count(essentially_unchanged) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\n══ Breakdown by state ══\n")
cd_2026_inheritance %>%
  count(state_fips, essentially_unchanged) %>%
  pivot_wider(names_from = essentially_unchanged, 
              values_from = n,
              values_fill = 0,
              names_prefix = "unchanged_") %>%
  rename(unchanged_TRUE_count = unchanged_TRUE, 
         unchanged_FALSE_count = unchanged_FALSE) %>%
  mutate(total = unchanged_TRUE_count + unchanged_FALSE_count) %>%
  print()


# ── 4. Examine the distribution of overlap_pct values ──────────────────────
cat("\n══ overlap_pct distribution ══\n")
cat("Min:    ", round(min(cd_2026_inheritance$overlap_pct), 4), "\n")
cat("Median: ", round(median(cd_2026_inheritance$overlap_pct), 4), "\n")
cat("Max:    ", round(max(cd_2026_inheritance$overlap_pct), 4), "\n")

cat("\nDistribution by threshold:\n")
cat("Overlap >= 0.99:", sum(cd_2026_inheritance$overlap_pct >= 0.99), "\n")
cat("Overlap >= 0.95:", sum(cd_2026_inheritance$overlap_pct >= 0.95), "\n")
cat("Overlap >= 0.90:", sum(cd_2026_inheritance$overlap_pct >= 0.90), "\n")
cat("Overlap >= 0.80:", sum(cd_2026_inheritance$overlap_pct >= 0.80), "\n")
cat("Overlap >= 0.50:", sum(cd_2026_inheritance$overlap_pct >= 0.50), "\n")


# ── 5. Save inheritance table ──────────────────────────────────────────────
saveRDS(cd_2026_inheritance,
        "/Users/binampoudyal/Downloads/Stratification_Frame_Building/cd_2026_inheritance.rds")

cat("\nSaved cd_2026_inheritance.rds\n")

# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 18A: Assemble training table (base) with 4-way shares
#
# Purpose: Combine cd_demographics, the updated cd_house_2024 (with 4-way
#          shares), and the updated state_pres_2024 (with 4-feature shares)
#          into a single training table. This is the base version BEFORE
#          inheritance is applied; that happens in Script 19B.
#
# What changes from Script 17 (the 3-way version):
#   - cd_house_2024 brings in 4 share columns (incl. no_vote_share),
#     computed against cd_pop. (Previously: 3 shares against total_house_votes.)
#   - state_pres_2024 brings in 4 features (incl. state_pres_no_vote_share),
#     computed against state_vap. (Previously: 3 features against total_pres_votes.)
#   - The selected output columns now include no_vote_share and
#     state_pres_no_vote_share.
#   - Otherwise the structure (joins, redistricted flag, column selection)
#     is unchanged from Script 17.
#
# Inputs:
#   cd_demographics.rds   (435 CDs, demographic proportions + cd_pop)
#   cd_house_2024.rds     (435 CDs, 4 shares + training_eligible, from 15D)
#   state_pres_2024.rds   (50 states, 4 shares, from 16B)
#
# Output:
#   training_table.rds    (435 CDs, base version, no inheritance applied yet)
#
# Join architecture:
#
#   cd_demographics (435 rows)
#       │
#       │ left_join by state_cd
#       ▼
#   + cd_house_2024 (435 rows, brings in 4 shares + training_eligible)
#       │
#       │ left_join by state_abbrv = state_abbrv
#       ▼
#   + state_pres_2024 (50 rows, brings in 4 state-level pres features)
#       │
#       ▼
#   training_table (435 rows)
#
# Note on cd_pop: cd_house_2024 already has cd_pop after Script 15D. To avoid
# duplicate columns (cd_pop.x / cd_pop.y), we drop cd_pop from one side
# before joining.
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)


# ── 1. Load source tables if not in memory ──────────────────────────────────

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"

if (!exists("cd_demographics")) {
  cat("Loading cd_demographics from disk...\n")
  cd_demographics <- readRDS(paste0(base_path, "cd_demographics.rds"))
}

if (!exists("cd_house_2024")) {
  cat("Loading cd_house_2024 from disk...\n")
  cd_house_2024 <- readRDS(paste0(base_path, "cd_house_2024.rds"))
}

if (!exists("state_pres_2024")) {
  cat("Loading state_pres_2024 from disk...\n")
  state_pres_2024 <- readRDS(paste0(base_path, "state_pres_2024.rds"))
}

cat("\n══ Input tables loaded ══\n")
cat("cd_demographics:  ", nrow(cd_demographics),  "rows\n")
cat("cd_house_2024:    ", nrow(cd_house_2024),    "rows\n")
cat("state_pres_2024:  ", nrow(state_pres_2024),  "rows\n")


# ── 2. Define which states are redistricted ─────────────────────────────────

redistricted_states <- c("CA", "FL", "MO", "NC", "OH", "TX", "UT")


# ── 3. Extract state_abbrv in cd_demographics if not already present ────────

if (!"state_abbrv" %in% names(cd_demographics)) {
  cd_demographics <- cd_demographics %>%
    mutate(state_abbrv = sub("-.*", "", state_cd))
}

cat("\n══ cd_demographics: state_abbrv ══\n")
cat("Unique values:", n_distinct(cd_demographics$state_abbrv), "(expect 50)\n")


# ── 4. Join the three tables ────────────────────────────────────────────────
#
# Step A: cd_demographics + cd_house_2024 on state_cd
#   - One-to-one join (both 435 rows keyed on state_cd)
#   - We drop cd_pop from cd_house_2024 since it's already in cd_demographics
#     (Script 15D added it there). This avoids cd_pop.x / cd_pop.y collision.
#
# Step B: + state_pres_2024 on state_abbrv
#   - One-to-many: each state's 4 pres features are replicated to all its CDs.

training_table <- cd_demographics %>%
  
  # Step A: bring in House outcomes + training_eligible
  left_join(
    cd_house_2024 %>% select(-cd_pop),  # drop cd_pop to avoid duplicate
    by = "state_cd"
  ) %>%
  
  # Step B: bring in state-level presidential features
  left_join(state_pres_2024, by = "state_abbrv")

cat("\n══ After joins ══\n")
cat("Rows:", nrow(training_table), "(expect 435)\n")
cat("Cols:", ncol(training_table), "\n")


# ── 5. Add is_redistricted flag ─────────────────────────────────────────────

training_table <- training_table %>%
  mutate(is_redistricted = state_abbrv %in% redistricted_states)


# ── 6. Keep only the columns we need for modeling ──────────────────────────
#
# Identifiers and flags:
#   state_cd, state_abbrv, cd_pop, training_eligible, is_redistricted
# Demographic predictors (29):
#   pct_age_*, pct_male, pct_female, pct_race_*, pct_hisp_*, pct_educ_*
# State-level pres predictors (4):
#   state_pres_dem_share, state_pres_rep_share, state_pres_other_share,
#   state_pres_no_vote_share
# Outcomes (4):
#   dem_share, rep_share, other_share, no_vote_share

training_table <- training_table %>%
  select(
    # Identifiers / flags
    state_cd, state_abbrv, cd_pop,
    training_eligible, is_redistricted,
    
    # Demographic predictors
    starts_with("pct_"),
    
    # State-level presidential predictors (now 4)
    state_pres_dem_share,
    state_pres_rep_share,
    state_pres_other_share,
    state_pres_no_vote_share,
    
    # Outcomes (now 4)
    dem_share,
    rep_share,
    other_share,
    no_vote_share
  )


# ── 7. Verification ─────────────────────────────────────────────────────────

cat("\n══ Final training_table (base) structure ══\n")
cat("Rows:", nrow(training_table), "(expect 435)\n")
cat("Cols:", ncol(training_table), "\n\n")

cat("Column names:\n")
print(names(training_table))


# Distribution of flags
cat("\n══ is_redistricted breakdown ══\n")
training_table %>%
  count(is_redistricted) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\n══ training_eligible breakdown ══\n")
training_table %>%
  count(training_eligible) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\n══ Cross-tab: redistricted × eligible ══\n")
training_table %>%
  count(is_redistricted, training_eligible) %>%
  print()


# Sanity check: outcomes sum to 1 per CD
cat("\n══ Outcome sum check (per CD) ══\n")
share_sums <- training_table %>%
  mutate(s = dem_share + rep_share + other_share + no_vote_share) %>%
  pull(s)
cat("Range:", round(min(share_sums, na.rm = TRUE), 6), "to",
    round(max(share_sums, na.rm = TRUE), 6), "\n")
cat("NAs:", sum(is.na(share_sums)), "\n")


# Sanity check: state-level pres shares sum to 1 per CD
cat("\n══ State pres feature sum check (per CD) ══\n")
state_pres_sums <- training_table %>%
  mutate(s = state_pres_dem_share + state_pres_rep_share + 
           state_pres_other_share + state_pres_no_vote_share) %>%
  pull(s)
cat("Range:", round(min(state_pres_sums, na.rm = TRUE), 6), "to",
    round(max(state_pres_sums, na.rm = TRUE), 6), "\n")
cat("NAs:", sum(is.na(state_pres_sums)), "\n")


# NA check across all columns
cat("\n══ NA check (any column) ══\n")
na_counts <- training_table %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_na") %>%
  filter(n_na > 0)
if (nrow(na_counts) == 0) {
  cat("No NAs in any column ✓\n")
} else {
  cat("Columns with NAs:\n")
  print(na_counts)
}


# ── 8. Save ─────────────────────────────────────────────────────────────────

saveRDS(training_table, paste0(base_path, "training_table.rds"))

cat("\nSaved training_table.rds (BASE version, no inheritance yet)\n")
cat("Next step: Script 19B applies inheritance.\n")





# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 18B: Add is_redistricted and contested_2024 flags to training_table
#
# Purpose: Build the two key categorization flags for each CD. This is a
#          standalone step before inheritance is applied to shares.
#
# Two flags built here:
#
#   is_redistricted (logical):
#     TRUE  if the CD is in a redistricted state AND does NOT have ≥95%
#           population overlap with a single 2024 CD.
#     FALSE if the CD is in a stable (non-redistricted) state, OR if it has
#           ≥95% overlap with a single 2024 CD (essentially-unchanged).
#
#     Interpretation: TRUE means "genuinely new geography" — needs CART
#     imputation. FALSE means "we have or can inherit valid 2024 results."
#
#   contested_2024 (logical):
#     TRUE  if this CD (or its dominant 2024 ancestor for inherited CDs)
#           had a contested race in 2024 (both major parties got > 100 votes).
#     FALSE otherwise.
#
#     Interpretation: cleanly separates whether the CD has reliable 2024
#     vote-share information.
#
# Logic table for assigning the flags:
#
#   For STABLE-STATE CDs (not in 7 redistricted states):
#     is_redistricted = FALSE
#     contested_2024 = training_eligible (already correctly set in 15B)
#
#   For REDISTRICTED-STATE CDs with ≥95% overlap with a single 2024 CD:
#     is_redistricted = FALSE  (essentially unchanged)
#     contested_2024 = training_eligible of the dominant_cd_119
#
#   For REDISTRICTED-STATE CDs without clean overlap (overlap < 95%):
#     is_redistricted = TRUE  (genuinely new)
#     contested_2024 = NA  (no meaningful 2024 ancestor)
#
# Inputs:
#   training_table.rds       (base version from 17B; has is_redistricted +
#                            training_eligible already, but is_redistricted
#                            is set to TRUE for ALL redistricted-state CDs,
#                            which we now refine)
#   cd_house_2024.rds        (used for contested-race info via training_eligible)
#   cd_2026_inheritance.rds  (overlap info for redistricted-state CDs)
#
# Output:
#   training_table.rds       (overwritten with refined flags)
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)


# ── 1. Load inputs ──────────────────────────────────────────────────────────

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"

if (!exists("training_table")) {
  cat("Loading training_table from disk...\n")
  training_table <- readRDS(paste0(base_path, "training_table.rds"))
}

if (!exists("cd_house_2024")) {
  cat("Loading cd_house_2024 from disk...\n")
  cd_house_2024 <- readRDS(paste0(base_path, "cd_house_2024.rds"))
}

if (!exists("cd_2026_inheritance")) {
  cat("Loading cd_2026_inheritance from disk...\n")
  cd_2026_inheritance <- readRDS(paste0(base_path, "cd_2026_inheritance.rds"))
}

cat("══ Inputs loaded ══\n")
cat("training_table:        ", nrow(training_table),       "rows\n")
cat("cd_house_2024:         ", nrow(cd_house_2024),        "rows\n")
cat("cd_2026_inheritance:   ", nrow(cd_2026_inheritance),  "rows\n\n")


# ── 2. Inspect cd_2026_inheritance columns ──────────────────────────────────
# 
# Verify the column names match what we expect: cd_2026_inheritance was built
# in Script 18 and has these columns:
#   state_fips, cd_2026, dominant_cd_119, overlap_pct,
#   essentially_unchanged, cd_2026_total_pop

cat("══ cd_2026_inheritance columns ══\n")
print(names(cd_2026_inheritance))


# ── 3. Build the inheritance lookup for ≥95% clean matches ──────────────────
#
# For each essentially-unchanged 2026 CD, we need:
#   - its state_cd (the new 2026 identifier, e.g. "CA-12")
#   - the state_cd of its dominant 2024 ancestor (e.g. "CA-9")
# Then we look up the ancestor's training_eligible value (which is TRUE if
# the 2024 race was contested) to populate contested_2024 for the inheritor.

# Map state_fips → state_abbrv for the 7 redistricted states
state_fips_to_abb <- tibble(
  state_fips  = c("06", "12", "29", "37", "39", "48", "49"),
  state_abbrv = c("CA", "FL", "MO", "NC", "OH", "TX", "UT")
)

# Inheritance lookup: state_cd (new 2026) → state_cd_119 (old 2024 ancestor)
inheritance_lookup <- cd_2026_inheritance %>%
  left_join(state_fips_to_abb, by = "state_fips") %>%
  mutate(
    state_cd     = paste0(state_abbrv, "-", cd_2026),
    state_cd_119 = paste0(state_abbrv, "-", dominant_cd_119)
  ) %>%
  filter(essentially_unchanged) %>%
  select(state_cd, state_cd_119, overlap_pct)

cat("\n══ Inheritance lookup ══\n")
cat("Rows (essentially-unchanged CDs):", nrow(inheritance_lookup), 
    "(expect ~42)\n")


# Look up training_eligible value of the ancestor (the 2024 CD) for each
# inheriting new CD. This becomes contested_2024 for the new CD.
inheritance_with_contested <- inheritance_lookup %>%
  left_join(
    cd_house_2024 %>% select(state_cd_119 = state_cd, 
                             ancestor_contested = training_eligible),
    by = "state_cd_119"
  )

cat("\nSample of inheritance lookup with ancestor contested status:\n")
print(head(inheritance_with_contested, 10))


# ── 4. Apply the flags to training_table ────────────────────────────────────
#
# We use a left_join + conditional logic:
#
#   - For CDs in inheritance_with_contested (the 42 essentially-unchanged
#     redistricted CDs): is_redistricted = FALSE, contested_2024 = inherited
#   - For CDs NOT in the lookup but in redistricted states (no clean overlap):
#     is_redistricted = TRUE, contested_2024 = NA
#   - For CDs NOT in the lookup AND in stable states:
#     is_redistricted = FALSE, contested_2024 = own training_eligible
#
# Note: the base training_table from 17B has is_redistricted set to TRUE
# for all redistricted-state CDs. We refine this to FALSE for the 42
# essentially-unchanged ones below.

training_table <- training_table %>%
  left_join(inheritance_with_contested %>% select(state_cd, ancestor_contested),
            by = "state_cd") %>%
  mutate(
    # is_redistricted: refine the base flag.
    # If the CD has a clean inheritance match (ancestor_contested is not NA,
    # meaning it was in the inheritance_lookup), it's no longer redistricted.
    # Otherwise, keep is_redistricted as is.
    is_redistricted = if_else(!is.na(ancestor_contested), FALSE, is_redistricted),
    
    # contested_2024: 
    # - For inheritors (ancestor_contested not NA): use ancestor's status
    # - For redistricted-state CDs without clean overlap (is_redistricted = TRUE):
    #   NA (no meaningful 2024 ancestor)
    # - For everyone else (stable states): use their own training_eligible
    contested_2024 = case_when(
      !is.na(ancestor_contested) ~ ancestor_contested,
      is_redistricted            ~ NA,
      TRUE                       ~ training_eligible
    )
  ) %>%
  select(-ancestor_contested)


# ── 5. Verification ─────────────────────────────────────────────────────────

cat("\n══ Final flag distribution ══\n")
cat("is_redistricted breakdown:\n")
training_table %>%
  count(is_redistricted) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\ncontested_2024 breakdown (including NA):\n")
training_table %>%
  count(contested_2024) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\nCross-tab: is_redistricted × contested_2024 ══\n")
training_table %>%
  count(is_redistricted, contested_2024) %>%
  print()


# Sanity check: by state, count by flag combo
cat("\n══ By state (redistricted states only) ══\n")
training_table %>%
  filter(state_abbrv %in% c("CA", "FL", "MO", "NC", "OH", "TX", "UT")) %>%
  count(state_abbrv, is_redistricted, contested_2024) %>%
  arrange(state_abbrv, is_redistricted, contested_2024) %>%
  print(n = Inf)


# ── 6. Save ─────────────────────────────────────────────────────────────────

saveRDS(training_table, paste0(base_path, "training_table.rds"))

cat("\nSaved training_table.rds with refined is_redistricted + new contested_2024\n")
cat("Columns now include contested_2024.\n")



# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 18C: Build training_eligibility column and finalize contested_2024
#
# Purpose: Convert the existing logical training_eligible (TRUE/FALSE) into a
#          3-value training_eligibility column that explicitly classifies each
#          CD's role in the pipeline. Also finalize contested_2024 by
#          replacing NA with TRUE for prediction-set CDs (assumption: 2026
#          races in genuinely new districts will be contested).
#
# Final classification:
#
#   training_eligibility = "training_set"
#     For CDs in stable states with a contested 2024 race, OR redistricted-
#     state CDs that inherit cleanly from a contested 2024 ancestor.
#     i.e. is_redistricted = FALSE AND contested_2024 = TRUE.
#     These CDs have valid 2024 vote shares we can train CART on.
#
#   training_eligibility = "prediction_set"
#     For genuinely new redistricted-state CDs (is_redistricted = TRUE).
#     CART will impute their 2026 vote shares. We assume their 2026 races
#     will be contested (set contested_2024 = TRUE for these).
#
#   training_eligibility = "exclude"
#     For CDs in stable states with an uncontested 2024 race, OR redistricted-
#     state CDs that inherit from an uncontested 2024 ancestor.
#     i.e. is_redistricted = FALSE AND contested_2024 = FALSE.
#     These CDs have unreliable 2024 vote shares (uncontested races inflate
#     no_vote_share) and aren't suitable for training.
#
# Note on dropping training_eligible:
#   The original training_eligible (logical) is no longer needed once
#   training_eligibility (categorical) is built. We drop it to avoid having
#   two redundant flag columns.
#
# Input:
#   training_table.rds   (from 18B, with is_redistricted + contested_2024)
#
# Output:
#   training_table.rds   (overwritten with training_eligibility column,
#                         training_eligible removed)
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)


# ── 1. Load input ───────────────────────────────────────────────────────────

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"

if (!exists("training_table")) {
  cat("Loading training_table from disk...\n")
  training_table <- readRDS(paste0(base_path, "training_table.rds"))
}

cat("══ Input loaded ══\n")
cat("training_table rows:", nrow(training_table), "\n")
cat("Current relevant columns:\n")
print(intersect(names(training_table),
                c("is_redistricted", "training_eligible", "contested_2024")))


# ── 2. Build training_eligibility and finalize contested_2024 ──────────────
#
# Step A: set contested_2024 = TRUE for prediction-set CDs (was NA).
#         This codifies the assumption that 2026 races in newly drawn
#         districts will be contested.
#
# Step B: build training_eligibility from is_redistricted + contested_2024.
#
# Step C: drop the now-redundant training_eligible column.

training_table <- training_table %>%
  mutate(
    # A: assume contested for prediction-set CDs
    contested_2024 = if_else(is_redistricted & is.na(contested_2024),
                             TRUE, contested_2024),
    # B: 3-value classification
    training_eligibility = case_when(
      !is_redistricted &  contested_2024 ~ "training_set",
      is_redistricted                   ~ "prediction_set",
      !is_redistricted & !contested_2024 ~ "exclude",
      TRUE                                ~ NA_character_
    )
  ) %>%
  # C: drop the redundant logical flag
  select(-any_of("training_eligible"))


# ── 3. Verification ─────────────────────────────────────────────────────────

cat("\n══ training_eligibility distribution ══\n")
training_table %>%
  count(training_eligibility) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\n══ Cross-tab: training_eligibility × contested_2024 × is_redistricted ══\n")
training_table %>%
  count(training_eligibility, is_redistricted, contested_2024) %>%
  print()


# By state for redistricted states
cat("\n══ By state (redistricted states only) ══\n")
training_table %>%
  filter(state_abbrv %in% c("CA", "FL", "MO", "NC", "OH", "TX", "UT")) %>%
  count(state_abbrv, training_eligibility) %>%
  pivot_wider(names_from = training_eligibility, values_from = n, 
              values_fill = 0) %>%
  print()


# Sanity check: NA in training_eligibility (should be 0)
cat("\n══ NA check ══\n")
cat("NAs in training_eligibility:", sum(is.na(training_table$training_eligibility)), "\n")
cat("NAs in contested_2024:       ", sum(is.na(training_table$contested_2024)), "\n")


# Final column names
cat("\n══ Final training_table columns ══\n")
print(names(training_table))


# ── 4. Save ─────────────────────────────────────────────────────────────────

saveRDS(training_table, paste0(base_path, "training_table.rds"))

cat("\nSaved training_table.rds with training_eligibility (3-value)\n")
cat("Removed: training_eligible (redundant logical flag)\n")
sink()


sink("output.txt")
# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 20D: Predicted vs actual plot with RMSE + R² annotations
#
# Purpose: Same as 20C but with RMSE added alongside R² in the annotation,
#          plus an overall summary table.
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"


# ── 1. Generate in-sample predictions for training set ──────────────────────

train_data <- training_table %>%
  filter(training_eligibility == "training_set")

train_predictions <- train_data %>%
  mutate(
    pred_dem_share     = predict(trees$dem$tree,     newdata = .),
    pred_rep_share     = predict(trees$rep$tree,     newdata = .),
    pred_other_share   = predict(trees$other$tree,   newdata = .),
    pred_no_vote_share = predict(trees$no_vote$tree, newdata = .)
  )


# ── 2. Reshape to long format ──────────────────────────────────────────────

plot_data <- train_predictions %>%
  select(state_cd,
         actual_dem     = dem_share,     pred_dem     = pred_dem_share,
         actual_rep     = rep_share,     pred_rep     = pred_rep_share,
         actual_other   = other_share,   pred_other   = pred_other_share,
         actual_no_vote = no_vote_share, pred_no_vote = pred_no_vote_share) %>%
  pivot_longer(
    cols = -state_cd,
    names_to = c(".value", "outcome"),
    names_pattern = "(actual|pred)_(.+)"
  ) %>%
  mutate(
    outcome = factor(outcome,
                     levels = c("dem", "rep", "other", "no_vote"),
                     labels = c("Democratic", "Republican", "Other", "No Vote"))
  )


# ── 3. Compute in-sample diagnostics per outcome ───────────────────────────
#
# RMSE = sqrt(mean(squared errors))
# R²   = 1 - SS_residual / SS_total

diagnostics <- plot_data %>%
  group_by(outcome) %>%
  summarise(
    rmse = sqrt(mean((actual - pred)^2)),
    rsq  = 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2),
    .groups = "drop"
  ) %>%
  mutate(label = sprintf("RMSE = %.4f\nR² = %.3f", rmse, rsq))

cat("══ In-sample diagnostics (training set, n = 289) ══\n")
print(diagnostics %>% select(outcome, rmse, rsq))


# ── 4. Also compute CV RMSE (more useful for assessing out-of-sample fit) ──
#
# rpart's cptable reports xerror in units of root-node MSE (variance of
# the outcome). So CV RMSE = sqrt(xerror * var(outcome_in_training)).

cv_rmse <- tibble(
  outcome_name = c("dem", "rep", "other", "no_vote"),
  label = c("Democratic", "Republican", "Other", "No Vote")
) %>%
  rowwise() %>%
  mutate(
    outcome_col = paste0(outcome_name, "_share"),
    var_outcome = var(train_data[[outcome_col]]),
    xerror_min  = min(as.data.frame(trees[[outcome_name]]$tree$cptable)[, 4]),
    cv_rmse     = sqrt(xerror_min * var_outcome)
  ) %>%
  ungroup()

cat("\n══ CV RMSE (out-of-sample estimate) ══\n")
print(cv_rmse %>% select(outcome = label, cv_rmse))


# ── 5. Build the plot with RMSE in annotation ──────────────────────────────

plot_pred_vs_actual <- ggplot(plot_data, aes(x = actual, y = pred)) +
  geom_abline(slope = 1, intercept = 0,
              color = "grey50", linetype = "dashed", linewidth = 0.4) +
  geom_point(alpha = 0.5, size = 1.5, color = "steelblue") +
  geom_text(data = diagnostics, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.05, vjust = 1.2, size = 3.5, inherit.aes = FALSE,
            lineheight = 1) +
  facet_wrap(~ outcome, scales = "free", nrow = 2) +
  labs(
    title = "Predicted vs. actual 2024 House vote shares (training set, n = 289)",
    subtitle = "Maximal CART trees. Dashed line shows y = x.",
    x = "Actual share",
    y = "Predicted share"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )


# ── 6. Save ────────────────────────────────────────────────────────────────

ggsave(
  filename = paste0(base_path, "tree_diagnostics_predicted_vs_actual.pdf"),
  plot     = plot_pred_vs_actual,
  width    = 10,
  height   = 8
)

cat("\nSaved tree_diagnostics_predicted_vs_actual.pdf\n")



sink("output.txt")
# ══════════════════════════════════════════════════════════════════════════════
# Diagnostic: who would have won (predicted vs actual) for training CDs
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)

# Generate in-sample predictions for training set
train_data <- training_table %>%
  filter(training_eligibility == "training_set")

train_predictions <- train_data %>%
  mutate(
    pred_dem_share = predict(trees$dem$tree, newdata = .),
    pred_rep_share = predict(trees$rep$tree, newdata = .)
  ) %>%
  mutate(
    actual_winner = if_else(dem_share > rep_share, "D", "R"),
    pred_winner   = if_else(pred_dem_share > pred_rep_share, "D", "R"),
    match         = actual_winner == pred_winner
  )


# ── Overall confusion matrix ────────────────────────────────────────────────

cat("══ Predicted vs actual winner (training set, n = 289) ══\n")
confusion <- train_predictions %>%
  count(actual_winner, pred_winner) %>%
  pivot_wider(names_from = pred_winner, values_from = n, 
              values_fill = 0, names_prefix = "pred_")

print(confusion)

cat("\nOverall accuracy:", 
    round(100 * mean(train_predictions$match), 1), "%\n")

cat("\nTotal actual D wins:", sum(train_predictions$actual_winner == "D"), "\n")
cat("Total actual R wins:", sum(train_predictions$actual_winner == "R"), "\n")
cat("Total predicted D wins:", sum(train_predictions$pred_winner == "D"), "\n")
cat("Total predicted R wins:", sum(train_predictions$pred_winner == "R"), "\n")


# ── Mismatches: which CDs did the model get wrong? ─────────────────────────

mismatches <- train_predictions %>%
  filter(!match) %>%
  select(state_cd, dem_share, rep_share, pred_dem_share, pred_rep_share,
         actual_winner, pred_winner) %>%
  mutate(
    actual_margin = dem_share - rep_share,
    pred_margin   = pred_dem_share - pred_rep_share
  )

cat("\n══ Mismatched CDs (n =", nrow(mismatches), ") ══\n")
if (nrow(mismatches) > 0) {
  print(mismatches %>%
          arrange(abs(actual_margin)) %>%
          select(state_cd, actual_winner, pred_winner,
                 dem_share, rep_share, actual_margin, pred_margin))
}


# ── Margin comparison: how close are predicted margins to actual? ──────────

cat("\n══ Margin (D minus R) summary ══\n")
margin_summary <- train_predictions %>%
  mutate(
    actual_margin = dem_share - rep_share,
    pred_margin   = pred_dem_share - pred_rep_share,
    margin_error  = pred_margin - actual_margin
  )

cat("Actual margin:\n")
print(summary(margin_summary$actual_margin))
cat("\nPredicted margin:\n")
print(summary(margin_summary$pred_margin))
cat("\nMargin error (predicted - actual):\n")
print(summary(margin_summary$margin_error))
cat("\nRMSE of margin:", round(sqrt(mean(margin_summary$margin_error^2)), 4), "\n")

sink("output.txt")
# Recompute raw sums for the 117 prediction-set CDs
predict_data_check <- training_table %>%
  filter(training_eligibility == "prediction_set") %>%
  mutate(
    pred_dem     = predict(trees$dem$tree,     newdata = .),
    pred_rep     = predict(trees$rep$tree,     newdata = .),
    pred_other   = predict(trees$other$tree,   newdata = .),
    pred_no_vote = predict(trees$no_vote$tree, newdata = .),
    raw_sum      = pred_dem + pred_rep + pred_other + pred_no_vote
  )

# Full distribution summary
cat("══ Raw sum distribution (n =", nrow(predict_data_check), ") ══\n")
print(summary(predict_data_check$raw_sum))

cat("\nQuantiles:\n")
print(quantile(predict_data_check$raw_sum, probs = c(0, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 1.0)))

cat("\nHow many CDs in each band:\n")
predict_data_check %>%
  mutate(band = cut(raw_sum, 
                    breaks = c(0, 0.85, 0.95, 1.05, 1.15, 1.25),
                    labels = c("0.73-0.85", "0.85-0.95", "0.95-1.05", "1.05-1.15", "1.15-1.25"))) %>%
  count(band) %>%
  print()
sink()

dev.off()
ggplot(margin_summary,
       aes(x = actual_margin,
           y = pred_margin)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              linetype = "dashed") +
  coord_equal() +
  labs(
    x = "Actual Margin",
    y = "Predicted Margin",
    title = "Predicted vs Actual Margin"
  ) +
  theme_minimal()

ggplot(plot_df,
       aes(x = 1, y = raw_sum)) +
  geom_point()



#Holdout validation
# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 20F: Training-set vs hold-out predicted-vs-actual plot
#
# Purpose: For each of the 4 outcomes, plot predicted vs actual on the 289
#          training CDs. Each CD gets two points:
#            - Training (in-sample): prediction from the main trees fit on
#              all 289 CDs
#            - Hold-out (out-of-sample): prediction from trees fit during
#              k-fold CV when this CD was in the held-out fold
#
# Methodology:
#   10-fold CV on training set. For each fold, fit the 4 trees on 9/10 of
#   the data, predict on the held-out 1/10. After all 10 folds, each
#   training CD has exactly one hold-out prediction.
#
# Output: 4-panel plot showing in-sample vs hold-out points with RMSE per outcome
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)
library(rpart)

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"


# ── 1. Setup ────────────────────────────────────────────────────────────────

train_data <- training_table %>%
  filter(training_eligibility == "training_set")

demographic_predictors <- grep("^pct_",        names(train_data), value = TRUE)
state_predictors       <- grep("^state_pres_", names(train_data), value = TRUE)
all_predictors         <- c(demographic_predictors, state_predictors)

# Match Script 20B settings exactly
rpart_ctrl <- rpart.control(cp = 0, minsplit = 3, minbucket = 1, xval = 0)


# ── 2. In-sample predictions (from main trees) ──────────────────────────────

train_predictions_insample <- train_data %>%
  mutate(
    pred_dem_share     = predict(trees$dem$tree,     newdata = .),
    pred_rep_share     = predict(trees$rep$tree,     newdata = .),
    pred_other_share   = predict(trees$other$tree,   newdata = .),
    pred_no_vote_share = predict(trees$no_vote$tree, newdata = .),
    fit_type           = "Training (in-sample)"
  )


# ── 3. 10-fold CV: hold-out predictions ─────────────────────────────────────
#
# Randomly assign each training CD to one of 10 folds. For each fold:
#   - Fit 4 trees on the other 9 folds (~260 CDs)
#   - Predict on the held-out fold (~29 CDs)
# After all 10 folds, each CD has one hold-out prediction.

set.seed(2026)
n_folds <- 10
fold_assignments <- sample(rep(1:n_folds, length.out = nrow(train_data)))

holdout_predictions <- list()

cat("Running 10-fold CV...\n")
for (fold in 1:n_folds) {
  
  train_subset   <- train_data[fold_assignments != fold, ]
  holdout_subset <- train_data[fold_assignments == fold, ]
  
  # Fit one tree per outcome on the training portion
  fit_one <- function(outcome) {
    fml <- as.formula(paste(outcome, "~", paste(all_predictors, collapse = " + ")))
    rpart(fml, data = train_subset, method = "anova", control = rpart_ctrl)
  }
  
  fold_trees <- list(
    dem     = fit_one("dem_share"),
    rep     = fit_one("rep_share"),
    other   = fit_one("other_share"),
    no_vote = fit_one("no_vote_share")
  )
  
  # Predict on hold-out
  holdout_predictions[[fold]] <- holdout_subset %>%
    mutate(
      pred_dem_share     = predict(fold_trees$dem,     newdata = .),
      pred_rep_share     = predict(fold_trees$rep,     newdata = .),
      pred_other_share   = predict(fold_trees$other,   newdata = .),
      pred_no_vote_share = predict(fold_trees$no_vote, newdata = .),
      fit_type           = "Hold-out (out-of-sample)"
    )
  
  cat("  Fold", fold, "complete (n =", nrow(holdout_subset), "CDs)\n")
}

train_predictions_holdout <- bind_rows(holdout_predictions)


# ── 4. Combine in-sample + hold-out into one long-format table ─────────────

all_preds <- bind_rows(train_predictions_insample, train_predictions_holdout) %>%
  select(state_cd, fit_type,
         actual_dem     = dem_share,     pred_dem     = pred_dem_share,
         actual_rep     = rep_share,     pred_rep     = pred_rep_share,
         actual_other   = other_share,   pred_other   = pred_other_share,
         actual_no_vote = no_vote_share, pred_no_vote = pred_no_vote_share) %>%
  pivot_longer(
    cols = -c(state_cd, fit_type),
    names_to = c(".value", "outcome"),
    names_pattern = "(actual|pred)_(.+)"
  ) %>%
  mutate(
    outcome = factor(outcome,
                     levels = c("dem", "rep", "other", "no_vote"),
                     labels = c("Democratic", "Republican", "Other", "No Vote")),
    fit_type = factor(fit_type,
                      levels = c("Training (in-sample)", "Hold-out (out-of-sample)"))
  )


# ── 5. Compute RMSE per (outcome, fit_type) for annotation ──────────────────

diagnostics <- all_preds %>%
  group_by(outcome, fit_type) %>%
  summarise(
    rmse = sqrt(mean((actual - pred)^2)),
    .groups = "drop"
  ) %>%
  mutate(
    label = sprintf("%s: RMSE = %.3f", 
                    if_else(str_detect(fit_type, "Training"), "Train", "Hold-out"),
                    rmse)
  )

# Combine train + hold-out labels into one annotation per facet
annotation_text <- diagnostics %>%
  group_by(outcome) %>%
  summarise(label = paste(label, collapse = "\n"), .groups = "drop")

cat("\n══ RMSE comparison ══\n")
print(diagnostics %>% select(outcome, fit_type, rmse))


# ── 6. Plot ────────────────────────────────────────────────────────────────

plot_train_vs_holdout <- ggplot(all_preds, aes(x = actual, y = pred, color = fit_type)) +
  geom_abline(slope = 1, intercept = 0,
              color = "grey50", linetype = "dashed", linewidth = 0.4) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_text(data = annotation_text, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.05, vjust = 1.2, size = 3.2, inherit.aes = FALSE,
            lineheight = 1) +
  facet_wrap(~ outcome, scales = "free", nrow = 2) +
  scale_color_manual(values = c("Training (in-sample)" = "steelblue",
                                "Hold-out (out-of-sample)" = "tomato")) +
  labs(
    title = "Training vs hold-out predicted shares (n = 289)",
    subtitle = "10-fold CV. Maximal CART trees. Dashed line shows y = x.",
    x = "Actual share",
    y = "Predicted share",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )


# ── 7. Save ────────────────────────────────────────────────────────────────

ggsave(
  filename = paste0(base_path, "training_vs_holdout_pred_vs_actual.pdf"),
  plot     = plot_train_vs_holdout,
  width    = 11,
  height   = 8.5
)

cat("\nSaved training_vs_holdout_pred_vs_actual.pdf\n")

sink("output.txt")
# ══════════════════════════════════════════════════════════════════════════════
# Update training_table to use RAW (un-normalized) predictions for prediction set
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"

# Generate raw predictions on the prediction set
predict_data_raw <- training_table %>%
  filter(training_eligibility == "prediction_set") %>%
  mutate(
    pred_dem_share     = predict(trees$dem$tree,     newdata = .),
    pred_rep_share     = predict(trees$rep$tree,     newdata = .),
    pred_other_share   = predict(trees$other$tree,   newdata = .),
    pred_no_vote_share = predict(trees$no_vote$tree, newdata = .)
  )

# Apply RAW predictions back to training_table (no normalization step)
training_table <- training_table %>%
  select(-any_of(c("is_imputed"))) %>%  # drop existing is_imputed flag
  left_join(
    predict_data_raw %>%
      select(state_cd, pred_dem_share, pred_rep_share,
             pred_other_share, pred_no_vote_share),
    by = "state_cd"
  ) %>%
  mutate(
    is_imputed    = !is.na(pred_dem_share),
    dem_share     = if_else(is_imputed, pred_dem_share,     dem_share),
    rep_share     = if_else(is_imputed, pred_rep_share,     rep_share),
    other_share   = if_else(is_imputed, pred_other_share,   other_share),
    no_vote_share = if_else(is_imputed, pred_no_vote_share, no_vote_share)
  ) %>%
  select(-pred_dem_share, -pred_rep_share, -pred_other_share, -pred_no_vote_share)


# Verify
cat("══ Verification ══\n")
cat("Is_imputed breakdown:\n")
print(table(training_table$is_imputed))

cat("\nShare sums per CD (should NOT all be 1 for imputed CDs):\n")
training_table %>%
  mutate(s = dem_share + rep_share + other_share + no_vote_share) %>%
  group_by(is_imputed) %>%
  summarise(
    min = round(min(s), 4),
    median = round(median(s), 4),
    max = round(max(s), 4),
    .groups = "drop"
  ) %>%
  print()

saveRDS(training_table, paste0(base_path, "training_table.rds"))
cat("\nSaved training_table.rds with RAW (un-normalized) imputed shares\n")
sink()

sink("output.txt")
library(tidyverse)

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"

training_table <- readRDS(paste0(base_path, "training_table.rds"))

# Pull redistricted CDs
redistricted_cds <- training_table %>%
  filter(is_redistricted) %>%
  arrange(state_abbrv, state_cd)

cat("══ Redistricted CDs ══\n")
cat("Total:", nrow(redistricted_cds), "\n\n")

cat("By state:\n")
print(redistricted_cds %>% count(state_abbrv))

cat("\nAll redistricted CDs with shares:\n")
print(redistricted_cds %>%
        select(state_cd, state_abbrv, dem_share, rep_share, other_share, no_vote_share, is_imputed),
      n = Inf)
sink()

library(tidyverse)

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"

training_table <- readRDS(paste0(base_path, "training_table.rds"))

# Pull redistricted CDs and select relevant columns
redistricted_cds <- training_table %>%
  filter(is_redistricted) %>%
  arrange(state_abbrv, state_cd) %>%
  select(state_cd, state_abbrv, cd_pop,
         dem_share, rep_share, other_share, no_vote_share,
         is_imputed)

# Save as CSV
output_path <- paste0(base_path, "redistricted_cds_imputed_2024_shares.csv")
write_csv(redistricted_cds, output_path)

cat("Saved", nrow(redistricted_cds), "redistricted CDs to:\n")
cat(output_path, "\n")

#####Check and modifications/fine-tuning####

#Inheritance check
sink("output.txt")
library(tidyverse)

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"
inheritance     <- readRDS(paste0(base_path, "cd_2026_inheritance.rds"))
cd_house_2024   <- readRDS(paste0(base_path, "cd_house_2024.rds"))

# Look at the 42 essentially-unchanged CDs: did 2026 number match 2024 number?
inheritance %>%
  filter(essentially_unchanged) %>%
  mutate(
    state_fips_to_abb = case_when(
      state_fips == "06" ~ "CA", state_fips == "12" ~ "FL",
      state_fips == "29" ~ "MO", state_fips == "37" ~ "NC",
      state_fips == "39" ~ "OH", state_fips == "48" ~ "TX",
      state_fips == "49" ~ "UT"
    ),
    state_cd_2026 = paste0(state_fips_to_abb, "-", cd_2026),
    state_cd_119  = paste0(state_fips_to_abb, "-", dominant_cd_119),
    name_matches  = state_cd_2026 == state_cd_119
  ) %>%
  count(name_matches)

#Output: True
sink("output.txt")


# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 18D: Build contestation flag + updated training_eligibility
#
# Purpose:
#   1. Build a fresh contestation column from cd_house_2024 vote counts
#      using a >10 vote threshold (Roberto's definition: no opposition from
#      the other major party).
#   2. For 4 known 2026-uncontested CDs (CA-14, CA-29, CA-40, FL-10),
#      override contestation to FALSE.
#   3. For genuinely redistricted CDs (is_redistricted = TRUE) without a
#      Roberto-specified override, default to contestation = TRUE.
#      (We do NOT inherit contestation from the old same-named district,
#       since the old district has been substantially redrawn.)
#   4. Update training_eligibility:
#        !is_redistricted → "training_set"  (318 CDs)
#        is_redistricted  → "prediction_set" (117 CDs)
#      No CDs excluded. Contestation is a FEATURE in the CART model.
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"

training_table <- readRDS(paste0(base_path, "training_table.rds"))
cd_house_2024  <- readRDS(paste0(base_path, "cd_house_2024.rds"))


# ── 1. Drop any stale contestation columns from prior runs ─────────────────

training_table <- training_table %>%
  select(-any_of(c("contested_2024", "contested_2024_v2", "contestation")))


# ── 2. Compute 2024 contestation from raw vote counts (>10 threshold) ──────
#
# Only meaningful for stable / essentially-unchanged CDs. Will be ignored
# for genuinely redistricted CDs in the next step.

cd_house_2024_contestation <- cd_house_2024 %>%
  mutate(contestation_2024 = (dem_votes > 10 & rep_votes > 10)) %>%
  select(state_cd, contestation_2024)

cat("══ 2024 contestation under >10 threshold (stable CDs only) ══\n")
print(table(cd_house_2024_contestation$contestation_2024))


# ── 3. Build the unified contestation column ───────────────────────────────
#
# Priority order:
#   (a) For 4 known 2026-uncontested CDs: FALSE (Roberto's specification)
#   (b) For redistricted CDs (not in (a)): TRUE (assume contested in 2026,
#       do NOT inherit from old same-named district)
#   (c) For stable / essentially-unchanged CDs: use 2024 contestation

known_uncontested_2026 <- c("CA-14", "CA-29", "CA-40", "FL-10")

training_table <- training_table %>%
  left_join(cd_house_2024_contestation, by = "state_cd") %>%
  mutate(
    contestation = case_when(
      state_cd %in% known_uncontested_2026 ~ FALSE,
      is_redistricted                      ~ TRUE,
      !is.na(contestation_2024)            ~ contestation_2024,
      TRUE                                 ~ TRUE
    )
  ) %>%
  select(-contestation_2024)


# ── 4. Updated training_eligibility (simplified) ──────────────────────────

training_table <- training_table %>%
  mutate(
    training_eligibility = case_when(
      !is_redistricted ~ "training_set",
      is_redistricted  ~ "prediction_set",
      TRUE             ~ NA_character_
    )
  )


# ── 5. Verification ────────────────────────────────────────────────────────

cat("\n══ contestation breakdown ══\n")
print(table(training_table$contestation, useNA = "always"))

cat("\n══ training_eligibility breakdown ══\n")
print(table(training_table$training_eligibility, useNA = "always"))

cat("\n══ Cross-tab: is_redistricted × contestation × training_eligibility ══\n")
print(training_table %>% count(is_redistricted, contestation, training_eligibility))

cat("\n══ The 4 known 2026-uncontested CDs ══\n")
training_table %>%
  filter(state_cd %in% known_uncontested_2026) %>%
  select(state_cd, is_redistricted, contestation, training_eligibility,
         dem_share, rep_share, other_share, no_vote_share, is_imputed) %>%
  print()

cat("\n══ All uncontested CDs in training_set (stable CDs only) ══\n")
training_table %>%
  filter(training_eligibility == "training_set", !contestation) %>%
  arrange(state_cd) %>%
  select(state_cd, state_abbrv, contestation,
         dem_share, rep_share, other_share, no_vote_share) %>%
  print(n = Inf)


# ── 6. Save ────────────────────────────────────────────────────────────────

saveRDS(training_table, paste0(base_path, "training_table.rds"))
cat("\nSaved training_table.rds with corrected contestation column + updated training_eligibility\n")
sink()


##Some inheritance diagnostic
sink("output.txt")

# List all data frames currently in workspace, sorted by size
sapply(ls(), function(x) {
  obj <- get(x)
  if (is.data.frame(obj)) {
    paste0(nrow(obj), " rows × ", ncol(obj), " cols")
  } else {
    NA
  }
}) %>%
  na.omit() %>%
  sort(decreasing = TRUE)
sink()


sink("output.txt")
# ══════════════════════════════════════════════════════════════════════════════
# SANITY CHECK: Verify symmetry of overlap for essentially-unchanged 2026 CDs
#
# For each 2026 CD flagged essentially_unchanged (overlap_pct >= 0.95):
#   - Forward overlap: % of NEW CD's population from dominant OLD CD
#     (this is what overlap_pct in cd_2026_inheritance measures)
#   - Reverse overlap: % of dominant OLD CD's population that ended up in NEW CD
#
# Under the equal-population constraint, these should be approximately equal.
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)

# block_full has columns: state_fips, cd_119, cd_2026, population (and one more?)
# Let me check its structure first
cat("══ block_full structure ══\n")
print(names(block_full))
print(head(block_full, 3))


# ── 1. Compute reverse overlap (per OLD CD) ────────────────────────────────
#
# For each (state_fips, cd_119), sum population by cd_2026 to see where the
# old CD's people ended up.

reverse_overlap <- block_full %>%
  group_by(state_fips, cd_119) %>%
  mutate(cd_119_total_pop = sum(pop)) %>%
  group_by(state_fips, cd_119, cd_2026) %>%
  summarise(
    pop_to_2026 = sum(pop),
    cd_119_total_pop = first(cd_119_total_pop),
    reverse_overlap_pct = pop_to_2026 / cd_119_total_pop,
    .groups = "drop"
  )


# ── 2. Join forward + reverse for essentially-unchanged 2026 CDs ───────────

inheritance <- readRDS("/Users/binampoudyal/Downloads/Stratification_Frame_Building/cd_2026_inheritance.rds")

state_fips_to_abb <- tibble(
  state_fips = c("06", "12", "29", "37", "39", "48", "49"),
  state_abbrv = c("CA", "FL", "MO", "NC", "OH", "TX", "UT")
)

comparison <- inheritance %>%
  filter(essentially_unchanged) %>%
  left_join(state_fips_to_abb, by = "state_fips") %>%
  mutate(
    state_cd_2026 = paste0(state_abbrv, "-", cd_2026),
    state_cd_119  = paste0(state_abbrv, "-", dominant_cd_119)
  ) %>%
  left_join(
    reverse_overlap %>%
      rename(dominant_cd_119 = cd_119, cd_2026_match = cd_2026),
    by = c("state_fips", "dominant_cd_119")
  ) %>%
  filter(cd_2026 == cd_2026_match) %>%
  select(state_cd_2026, state_cd_119,
         forward_overlap_pct = overlap_pct,
         reverse_overlap_pct,
         cd_2026_total_pop, cd_119_total_pop, pop_to_2026)


# ── 3. Inspect ─────────────────────────────────────────────────────────────

cat("\n══ Symmetry check: forward vs reverse overlap ══\n")
cat("All 42 essentially-unchanged CDs:\n")
print(comparison %>%
        arrange(forward_overlap_pct) %>%
        mutate(across(ends_with("pct"), ~round(., 4))),
      n = Inf)

cat("\n══ Summary statistics ══\n")
cat("Forward overlap range:", 
    round(min(comparison$forward_overlap_pct), 3), "to",
    round(max(comparison$forward_overlap_pct), 3), "\n")
cat("Reverse overlap range:", 
    round(min(comparison$reverse_overlap_pct), 3), "to",
    round(max(comparison$reverse_overlap_pct), 3), "\n")

cat("\nDifference between forward and reverse (abs):\n")
print(summary(abs(comparison$forward_overlap_pct - comparison$reverse_overlap_pct)))


# ── 4. Spot check: CA-10 specifically ──────────────────────────────────────

if ("CA-10" %in% comparison$state_cd_2026) {
  cat("\n══ CA-10 specifically ══\n")
  comparison %>% filter(state_cd_2026 == "CA-10") %>% print()
  
  cat("\nFull distribution of OLD CA-10 population across NEW CDs:\n")
  reverse_overlap %>%
    filter(state_fips == "06", cd_119 == 10) %>%
    arrange(desc(reverse_overlap_pct)) %>%
    mutate(reverse_overlap_pct = round(reverse_overlap_pct, 4)) %>%
    print()
}
sink()


sink("output.txt")
# Check column types
str(block_full)
sink()

sink("output.txt")
# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 20B (UPDATED): Fit 4-tree CART with contestation feature
#
# Changes from previous version:
#   1. Training set is now 318 CDs (all !is_redistricted, contested + uncontested)
#   2. contestation is included as a 34th predictor
#   3. Simple hold-out validation: 15 random CDs held out for OOD diagnostic
#
# Inputs:
#   training_table.rds  (435 CDs with training_eligibility and contestation)
#
# Outputs:
#   trees.rds                                — list of 4 maximal rpart models
#   training_table.rds                       — updated with raw imputed shares
#   tree_diagnostics_predicted_vs_actual.pdf — in-sample fit (4 panels)
#   tree_diagnostics_holdout.pdf             — hold-out fit (4 panels)
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)
library(rpart)

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"


# ── 1. Load training_table ──────────────────────────────────────────────────

training_table <- readRDS(paste0(base_path, "training_table.rds"))

cat("══ training_eligibility distribution ══\n")
print(table(training_table$training_eligibility))

cat("\n══ contestation distribution ══\n")
print(table(training_table$contestation, useNA = "always"))


# ── 2. Define training and prediction sets ──────────────────────────────────

train_data <- training_table %>%
  filter(training_eligibility == "training_set")

predict_data <- training_table %>%
  filter(training_eligibility == "prediction_set")

cat("\n══ Modeling samples ══\n")
cat("Training set:   ", nrow(train_data),   "CDs (expect 318)\n")
cat("Prediction set: ", nrow(predict_data), "CDs (expect 117)\n")


# ── 3. Define predictor variables ────────────────────────────────────────────
#
# 29 demographic + 4 state pres + 1 contestation = 34 predictors.

demographic_predictors <- grep("^pct_",        names(training_table), value = TRUE)
state_predictors       <- grep("^state_pres_", names(training_table), value = TRUE)
all_predictors         <- c(demographic_predictors, state_predictors, "contestation")

cat("\n══ Predictor variables ══\n")
cat("Total predictors:", length(all_predictors), "\n")
cat("Including contestation as a feature.\n")


# ── 4. Helper: fit one maximal tree ────────────────────────────────────────

fit_tree <- function(outcome_name, data, predictors) {
  formula_obj <- as.formula(
    paste(outcome_name, "~", paste(predictors, collapse = " + "))
  )
  rpart(
    formula = formula_obj,
    data    = data,
    method  = "anova",
    control = rpart.control(cp = 0, xval = 0, minsplit = 3, minbucket = 1)
  )
}


# ── 5. SIMPLE HOLD-OUT VALIDATION ──────────────────────────────────────────
#
# Randomly hold out 15 CDs from the 318 training CDs.
# Fit trees on the remaining 303. Predict the 15.
# Compare predicted vs actual on the hold-out for OOD diagnostic.

set.seed(2026)
n_holdout <- 15
holdout_idx <- sample(nrow(train_data), n_holdout)

train_subset_for_holdout   <- train_data[-holdout_idx, ]
holdout_subset             <- train_data[ holdout_idx, ]

cat("\n══ Hold-out validation ══\n")
cat("Hold-out size:", n_holdout, "CDs (out of 318)\n")
cat("Held-out CDs:\n")
print(holdout_subset$state_cd)

# Fit 4 trees on the 303 non-held-out CDs
holdout_trees <- list(
  dem     = fit_tree("dem_share",     train_subset_for_holdout, all_predictors),
  rep     = fit_tree("rep_share",     train_subset_for_holdout, all_predictors),
  other   = fit_tree("other_share",   train_subset_for_holdout, all_predictors),
  no_vote = fit_tree("no_vote_share", train_subset_for_holdout, all_predictors)
)

# Predict on the 15 held-out CDs
holdout_predictions <- holdout_subset %>%
  mutate(
    pred_dem_share     = predict(holdout_trees$dem,     newdata = .),
    pred_rep_share     = predict(holdout_trees$rep,     newdata = .),
    pred_other_share   = predict(holdout_trees$other,   newdata = .),
    pred_no_vote_share = predict(holdout_trees$no_vote, newdata = .)
  )

# Hold-out diagnostics
cat("\n══ Hold-out diagnostics ══\n")
for (outcome in c("dem_share", "rep_share", "other_share", "no_vote_share")) {
  pred_col <- paste0("pred_", outcome)
  actual <- holdout_predictions[[outcome]]
  pred   <- holdout_predictions[[pred_col]]
  rmse <- sqrt(mean((actual - pred)^2))
  rsq  <- 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)
  cat(sprintf("%-15s: RMSE = %.4f, R² = %.3f\n", outcome, rmse, rsq))
}


# ── 6. Fit MAIN trees on all 318 training CDs ─────────────────────────────

cat("\n══ Fitting main trees on all 318 training CDs ══\n")

trees <- list(
  dem     = fit_tree("dem_share",     train_data, all_predictors),
  rep     = fit_tree("rep_share",     train_data, all_predictors),
  other   = fit_tree("other_share",   train_data, all_predictors),
  no_vote = fit_tree("no_vote_share", train_data, all_predictors)
)

# In-sample diagnostics
cat("\n══ Main tree diagnostics ══\n")
cat(sprintf("%-9s | %7s | %7s | %12s | %12s\n",
            "outcome", "splits", "leaves", "in-sample R²", "RMSE"))
cat(strrep("-", 70), "\n")

for (outcome_name in names(trees)) {
  tr <- trees[[outcome_name]]
  cptab <- as.data.frame(tr$cptable)
  n_leaves <- sum(tr$frame$var == "<leaf>")
  n_splits <- nrow(tr$frame) - n_leaves
  
  # Compute in-sample RMSE
  outcome_col <- paste0(outcome_name, "_share")
  preds <- predict(tr, newdata = train_data)
  actuals <- train_data[[outcome_col]]
  rmse <- sqrt(mean((preds - actuals)^2))
  rsq <- 1 - sum((preds - actuals)^2) / sum((actuals - mean(actuals))^2)
  
  cat(sprintf("%-9s | %7d | %7d | %12.3f | %12.4f\n",
              outcome_name, n_splits, n_leaves, rsq, rmse))
}

saveRDS(trees, paste0(base_path, "trees.rds"))


# ── 7. Predict for the prediction set (raw, NO normalization) ──────────────

cat("\n══ Predicting for prediction_set CDs (RAW, no normalization) ══\n")

predict_data <- predict_data %>%
  mutate(
    pred_dem_share     = predict(trees$dem,     newdata = .),
    pred_rep_share     = predict(trees$rep,     newdata = .),
    pred_other_share   = predict(trees$other,   newdata = .),
    pred_no_vote_share = predict(trees$no_vote, newdata = .)
  )

cat("Raw prediction sums (NO normalization applied):\n")
predict_data %>%
  mutate(raw_sum = pred_dem_share + pred_rep_share +
           pred_other_share + pred_no_vote_share) %>%
  summarise(min = min(raw_sum), median = median(raw_sum), max = max(raw_sum)) %>%
  print()


# ── 8. Apply raw predictions to training_table ─────────────────────────────

training_table <- training_table %>%
  select(-any_of("is_imputed")) %>%
  left_join(
    predict_data %>%
      select(state_cd, pred_dem_share, pred_rep_share,
             pred_other_share, pred_no_vote_share),
    by = "state_cd"
  ) %>%
  mutate(
    is_imputed    = !is.na(pred_dem_share),
    dem_share     = if_else(is_imputed, pred_dem_share,     dem_share),
    rep_share     = if_else(is_imputed, pred_rep_share,     rep_share),
    other_share   = if_else(is_imputed, pred_other_share,   other_share),
    no_vote_share = if_else(is_imputed, pred_no_vote_share, no_vote_share)
  ) %>%
  select(-pred_dem_share, -pred_rep_share, -pred_other_share, -pred_no_vote_share)


# ── 9. Verification ───────────────────────────────────────────────────────

cat("\n══ Updated training_table breakdown ══\n")
print(training_table %>% count(is_imputed))


# ── 10. In-sample diagnostic plot ──────────────────────────────────────────

plot_data_insample <- train_data %>%
  mutate(
    pred_dem     = predict(trees$dem,     newdata = .),
    pred_rep     = predict(trees$rep,     newdata = .),
    pred_other   = predict(trees$other,   newdata = .),
    pred_no_vote = predict(trees$no_vote, newdata = .)
  ) %>%
  select(state_cd,
         actual_dem = dem_share,         pred_dem,
         actual_rep = rep_share,         pred_rep,
         actual_other = other_share,     pred_other,
         actual_no_vote = no_vote_share, pred_no_vote) %>%
  pivot_longer(cols = -state_cd,
               names_to = c(".value", "outcome"),
               names_pattern = "(actual|pred)_(.+)") %>%
  mutate(outcome = factor(outcome,
                          levels = c("dem", "rep", "other", "no_vote"),
                          labels = c("Democratic", "Republican", "Other", "No Vote")))

insample_diag <- plot_data_insample %>%
  group_by(outcome) %>%
  summarise(
    rmse = sqrt(mean((actual - pred)^2)),
    rsq  = 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2),
    .groups = "drop"
  ) %>%
  mutate(label = sprintf("RMSE = %.4f\nR² = %.3f", rmse, rsq))

plot_insample <- ggplot(plot_data_insample, aes(x = actual, y = pred)) +
  geom_abline(slope = 1, intercept = 0, color = "grey50",
              linetype = "dashed", linewidth = 0.4) +
  geom_point(alpha = 0.5, size = 1.5, color = "steelblue") +
  geom_text(data = insample_diag, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.05, vjust = 1.2, size = 3.5, inherit.aes = FALSE,
            lineheight = 1) +
  facet_wrap(~ outcome, scales = "free", nrow = 2) +
  labs(
    title = "In-sample predicted vs. actual (training set, n = 318)",
    subtitle = "Maximal CART. Dashed line = y = x.",
    x = "Actual share", y = "Predicted share"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(paste0(base_path, "tree_diagnostics_predicted_vs_actual.pdf"),
       plot_insample, width = 10, height = 8)


# ── 11. Hold-out diagnostic plot ──────────────────────────────────────────

plot_data_holdout <- holdout_predictions %>%
  select(state_cd,
         actual_dem = dem_share,         pred_dem     = pred_dem_share,
         actual_rep = rep_share,         pred_rep     = pred_rep_share,
         actual_other = other_share,     pred_other   = pred_other_share,
         actual_no_vote = no_vote_share, pred_no_vote = pred_no_vote_share) %>%
  pivot_longer(cols = -state_cd,
               names_to = c(".value", "outcome"),
               names_pattern = "(actual|pred)_(.+)") %>%
  mutate(outcome = factor(outcome,
                          levels = c("dem", "rep", "other", "no_vote"),
                          labels = c("Democratic", "Republican", "Other", "No Vote")))

holdout_diag <- plot_data_holdout %>%
  group_by(outcome) %>%
  summarise(
    rmse = sqrt(mean((actual - pred)^2)),
    rsq  = 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2),
    .groups = "drop"
  ) %>%
  mutate(label = sprintf("RMSE = %.4f\nR² = %.3f", rmse, rsq))

plot_holdout <- ggplot(plot_data_holdout, aes(x = actual, y = pred)) +
  geom_abline(slope = 1, intercept = 0, color = "grey50",
              linetype = "dashed", linewidth = 0.4) +
  geom_point(alpha = 0.7, size = 2.5, color = "tomato") +
  geom_text(aes(label = state_cd), vjust = -1, size = 2.5, alpha = 0.6) +
  geom_text(data = holdout_diag, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.05, vjust = 1.2, size = 3.5, inherit.aes = FALSE,
            lineheight = 1) +
  facet_wrap(~ outcome, scales = "free", nrow = 2) +
  labs(
    title = "Hold-out predicted vs. actual (n = 15 random CDs)",
    subtitle = "OOD diagnostic. Trees fit on 303 CDs, predicted on 15 held-out.",
    x = "Actual share", y = "Predicted share"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(paste0(base_path, "tree_diagnostics_holdout.pdf"),
       plot_holdout, width = 10, height = 8)


# ── 12. Save ───────────────────────────────────────────────────────────────

saveRDS(training_table, paste0(base_path, "training_table.rds"))

cat("\nSaved:\n")
cat("- trees.rds (4 maximal trees with contestation feature)\n")
cat("- training_table.rds (with raw imputed shares for 117 prediction CDs)\n")
cat("- tree_diagnostics_predicted_vs_actual.pdf (in-sample)\n")
cat("- tree_diagnostics_holdout.pdf (15-CD hold-out)\n")
sink()


sink("output.txt")
library(tidyverse)
holdout_predictions %>%
  select(state_cd, contestation, no_vote_share, pred_no_vote_share) %>%
  mutate(error = pred_no_vote_share - no_vote_share) %>%
  arrange(desc(abs(error))) %>%
  print()
sink()




##Final stratification check before sending to Danielius:
sink("output.txt")
# ══════════════════════════════════════════════════════════════════════════════
# Diagnostic: Stratification frame (pums_demographic_cells.rds)
# Run this first to gather info for the README.
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"

strat_frame <- readRDS(paste0(base_path, "pums_demographic_cells.rds"))


# ── 1. Structure ────────────────────────────────────────────────────────────

cat("══ Structure ══\n")
cat("Rows:", nrow(strat_frame), "\n")
cat("Cols:", ncol(strat_frame), "\n\n")
cat("Column names:\n")
print(names(strat_frame))
cat("\nColumn types:\n")
print(sapply(strat_frame, class))


# ── 2. Levels per categorical variable ──────────────────────────────────────

cat("\n══ Unique values per categorical column ══\n")

for (col in c("state_cat", "cd_cat", "age_cat", "gender_cat", 
              "race_cat", "hispanic_cat", "educ_cat", 
              "state_abbrv", "state_cd")) {
  if (col %in% names(strat_frame)) {
    uniq <- unique(strat_frame[[col]])
    cat(sprintf("\n%s (%d unique values):\n", col, length(uniq)))
    if (length(uniq) <= 60) {
      print(sort(uniq))
    } else {
      cat("  (sample of first 20):", paste(head(sort(uniq), 20), collapse = ", "), "\n")
    }
  }
}


# ── 3. Cell population summary ──────────────────────────────────────────────

cat("\n══ cell_pop summary ══\n")
print(summary(strat_frame$cell_pop))

cat("\nTotal weighted population:", round(sum(strat_frame$cell_pop) / 1e6, 2), "M\n")

cat("\nCells with cell_pop == 0:", sum(strat_frame$cell_pop == 0), "\n")
cat("Cells with cell_pop > 0:",  sum(strat_frame$cell_pop > 0), "\n")


# ── 4. CDs covered ──────────────────────────────────────────────────────────

cat("\n══ Coverage ══\n")
cat("Unique state_abbrv:", n_distinct(strat_frame$state_abbrv), "\n")
cat("Unique state_cd:",    n_distinct(strat_frame$state_cd),    "\n")

cat("\nCells per state (first 10):\n")
print(strat_frame %>% count(state_abbrv) %>% head(10))


# ── 5. Population per CD ────────────────────────────────────────────────────

cat("\n══ Population per CD (sanity check, should be ~500K each) ══\n")

cd_pops_check <- strat_frame %>%
  group_by(state_cd) %>%
  summarise(total_cd_pop = sum(cell_pop), .groups = "drop")

print(summary(cd_pops_check$total_cd_pop))


# ── 6. Sample row ──────────────────────────────────────────────────────────

cat("\n══ Sample rows ══\n")
print(head(strat_frame, 5))


# ── 7. NA check ────────────────────────────────────────────────────────────

cat("\n══ NA counts per column ══\n")
print(colSums(is.na(strat_frame)))


# ── 8. Cross-tab: are all cells unique? ─────────────────────────────────────

cat("\n══ Cell uniqueness ══\n")
n_unique <- strat_frame %>%
  distinct(state_cd, age_cat, gender_cat, race_cat, hispanic_cat, educ_cat) %>%
  nrow()

cat("Total rows:", nrow(strat_frame), "\n")
cat("Unique (state_cd × age × gender × race × hispanic × educ) combos:", n_unique, "\n")
cat("All unique:", n_unique == nrow(strat_frame), "\n")
sink()

sink("output.txt")
# ══════════════════════════════════════════════════════════════════════════════
# Save stratification frame as CSV + write README
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"
output_dir <- paste0(base_path, "deliverables_for_danielius/")
dir.create(output_dir, showWarnings = FALSE)


# ── 1. Save stratification frame as CSV ────────────────────────────────────

strat_frame <- readRDS(paste0(base_path, "pums_demographic_cells.rds"))

write_csv(strat_frame,
          paste0(output_dir, "stratification_frame_2026.csv"))

# Also save as RDS for R users (faster load, preserves factor levels)
saveRDS(strat_frame,
        paste0(output_dir, "stratification_frame_2026.rds"))

cat("Saved stratification_frame_2026.csv and .rds\n")
cat("Rows:", nrow(strat_frame), "\n")
cat("File size (CSV):",
    round(file.info(paste0(output_dir, "stratification_frame_2026.csv"))$size / 1e6, 1),
    "MB\n")


# ── 2. Write README ────────────────────────────────────────────────────────

readme_text <- '# Stratification Frame for 2026 U.S. House Election Modeling

## Files
- `stratification_frame_2026_preMrsP.csv` — 497,836 rows × 10 columns
- `stratification_frame_2026_preMrsP.rds` — same data, R-native format (preserves factor levels)

## Purpose
This file is the demographic stratification frame for poststratification in
the MrsP pipeline. Each row is a demographic cell within a 2026 congressional
district, with a weighted population count derived from the U.S. Census Bureau\'s
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
| state_cat     | numeric | State FIPS code (numeric, 1-56) |
| cd_cat        | integer | Congressional district number (1-52); CDs use 1 for at-large states |
| age_cat       | factor  | Age group (14 levels): 18-22, 23-27, 28-32, 33-37, 38-42, 43-47, 48-52, 53-57, 58-62, 63-67, 68-72, 73-77, 78-82, 83+ |
| gender_cat    | char    | Gender (2 levels): Female, Male |
| race_cat      | factor  | Race (5 levels): White, Black, Native American, Asian, Other/Multi |
| hispanic_cat  | factor  | Hispanic ethnicity (2 levels): Hispanic, Not Hispanic |
| educ_cat      | factor  | Educational attainment (6 levels): No HS, HS grad, Some college, 2-year, 4-year, Post-grad |
| cell_pop      | numeric | Weighted citizen voting-age population in this cell |
| state_abbrv   | char    | 2-letter state abbreviation (e.g., "CA") |
| state_cd      | char    | State-CD identifier (e.g., "CA-1"); primary key combined with the 5 demographic categories |

## Cell Structure

Each unique (state_cd, age_cat, gender_cat, race_cat, hispanic_cat, educ_cat)
combination appears exactly once. The full joint distribution of demographics
× geography is captured in the cell_pop column.

Cells with cell_pop == 0 (1,190 of 497,836) represent demographic-geographic
combinations that exist as possibilities but have no estimated population in
the ACS data. These can be safely filtered or kept (their contribution to
poststratified estimates is zero either way).

## Notes for Use in MrsP

For multilevel regression with poststratification:
- Fit a multinomial logit model on individual-level CES data with these same
  demographic categories.
- Use this stratification frame to compute cell-level probabilities for each
  vote outcome (Dem, Rep, Other, No Vote).
- Weight cell-level probabilities by cell_pop and aggregate to CD or state level.

## Generation

Generated from harmonized 2023 ACS 5-year PUMS data, processed with a
PUMA-to-2026-CD population-weighted crosswalk built from Census block-level
boundaries. Detailed methodology available in the project documentation.
'
#--Note edited the readme file for the final version and renamed the file as well

writeLines(readme_text, paste0(output_dir, "README_stratification_frame.md"))

cat("\nSaved README_stratification_frame_preMrsP.md\n")
sink()

sink("output.txt")
# ══════════════════════════════════════════════════════════════════════════════
# Diagnostic: training_table for Danielius
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"

training_table <- readRDS(paste0(base_path, "training_table.rds"))


# ── 1. Structure ────────────────────────────────────────────────────────────

cat("══ Structure ══\n")
cat("Rows:", nrow(training_table), "\n")
cat("Cols:", ncol(training_table), "\n\n")

cat("All column names:\n")
print(names(training_table))

cat("\nColumn types:\n")
print(sapply(training_table, class))


# ── 2. Sample row ──────────────────────────────────────────────────────────

cat("\n══ Sample rows (3 random CDs) ══\n")
print(training_table %>% slice_sample(n = 3))


# ── 3. Key flag distributions ──────────────────────────────────────────────

cat("\n══ Flag distributions ══\n")
cat("\nis_redistricted:\n");        print(table(training_table$is_redistricted))
cat("\ncontestation:\n");           print(table(training_table$contestation))
cat("\nis_imputed:\n");             print(table(training_table$is_imputed))
cat("\ntraining_eligibility:\n");   print(table(training_table$training_eligibility))


# ── 4. Vote share columns ──────────────────────────────────────────────────

cat("\n══ Vote share summaries ══\n")

cat("\n4 CD-level 2024 House shares (real or imputed):\n")
print(summary(training_table %>% 
                select(dem_share, rep_share, other_share, no_vote_share)))

cat("\n4 state-level 2024 Pres shares (constant within state):\n")
print(summary(training_table %>%
                select(starts_with("state_pres_"))))


# ── 5. Share sum check ─────────────────────────────────────────────────────

cat("\n══ House share sum per CD ══\n")
share_sums <- training_table %>%
  mutate(s = dem_share + rep_share + other_share + no_vote_share) %>%
  select(state_cd, is_imputed, s)

cat("Sum distribution by is_imputed:\n")
print(share_sums %>% group_by(is_imputed) %>% 
        summarise(min = round(min(s), 4),
                  median = round(median(s), 4),
                  max = round(max(s), 4)))


# ── 6. Demographic columns ─────────────────────────────────────────────────

cat("\n══ Demographic feature columns ══\n")
demo_cols <- grep("^pct_", names(training_table), value = TRUE)
cat("Number of pct_* columns:", length(demo_cols), "\n")
print(demo_cols)

cat("\nExample summaries:\n")
print(summary(training_table %>% select(all_of(demo_cols[1:5]))))


# ── 7. NA check ────────────────────────────────────────────────────────────

cat("\n══ NA counts per column ══\n")
print(colSums(is.na(training_table)))


# ── 8. Geographic coverage ─────────────────────────────────────────────────

cat("\n══ Geographic coverage ══\n")
cat("Unique state_abbrv:", n_distinct(training_table$state_abbrv), "\n")
cat("Unique state_cd:",    n_distinct(training_table$state_cd), "\n")

cat("\ncd_pop summary (should be ~500K each):\n")
print(summary(training_table$cd_pop))

sink()

sink("output.txt")
# ══════════════════════════════════════════════════════════════════════════════
# Save training_table as area_level_vote_shares.csv + write README
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"
output_dir <- paste0(base_path, "deliverables_for_danielius/")
dir.create(output_dir, showWarnings = FALSE)


# ── 1. Save ───────────────────────────────────────────────────────────────

training_table <- readRDS(paste0(base_path, "training_table.rds"))

write_csv(training_table,
          paste0(output_dir, "area_level_vote_shares.csv"))

saveRDS(training_table,
        paste0(output_dir, "area_level_vote_shares.rds"))

cat("Saved area_level_vote_shares.csv and .rds\n")
cat("Rows:", nrow(training_table), "\n")
cat("File size (CSV):",
    round(file.info(paste0(output_dir, "area_level_vote_shares.csv"))$size / 1e6, 2),
    "MB\n")


# ── 2. README ─────────────────────────────────────────────────────────────

readme_text <- '# Area-Level Vote Shares for 2026 U.S. House Election Modeling

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
each state (repeated across that state\'s CDs). Computed against state-level
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
'

writeLines(readme_text, paste0(output_dir, "README_area_level_vote_shares.md"))

cat("\nSaved README_area_level_vote_shares.md\n")
sink()

sink("output.txt")
library(tidyverse)
ces <- readRDS("/Users/binampoudyal/Downloads/Stratification_Frame_Building/ces_with_cd_v2.rds")

# Check which weight columns exist
weight_cols <- grep("weight", names(ces), value = TRUE, ignore.case = TRUE)
cat("Weight columns in CES:\n")
print(weight_cols)

# Check distribution of each
for (col in weight_cols) {
  cat(sprintf("\n%s:\n", col))
  print(summary(ces[[col]]))
  cat("  NAs:", sum(is.na(ces[[col]])), "\n")
}
sink()

sink("output.txt")
# ══════════════════════════════════════════════════════════════════════════════
# CES file for Danielius: diagnostic + save
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"
output_dir <- paste0(base_path, "deliverables_for_danielius/")
dir.create(output_dir, showWarnings = FALSE)


# ── 1. Load and select columns ────────────────────────────────────────────

ces <- readRDS(paste0(base_path, "ces_with_cd_v2.rds"))

wanted_cols <- c(
  "caseid",
  "commonweight", "commonpostweight", "tookpost",
  "age_cat", "gender_cat", "race_cat", "hispanic_cat", "educ_cat",
  "state_cat", "cd_cat", "state_abbrv", "state_cd",
  "afact",
  "vote_2024"
)

# Verify all columns exist
missing <- setdiff(wanted_cols, names(ces))
if (length(missing) > 0) {
  stop("Missing columns in CES: ", paste(missing, collapse = ", "))
}

ces_clean <- ces %>% select(all_of(wanted_cols))


# ── 2. Diagnostic ─────────────────────────────────────────────────────────

cat("══ Structure ══\n")
cat("Rows:", nrow(ces_clean), "\n")
cat("Cols:", ncol(ces_clean), "\n")
cat("Unique caseids:", n_distinct(ces_clean$caseid), "\n\n")

cat("Column types:\n")
print(sapply(ces_clean, class))


cat("\n══ Sample rows ══\n")
print(head(ces_clean, 5))


cat("\n══ Weight columns ══\n")
for (col in c("commonweight", "commonpostweight")) {
  cat(sprintf("\n%s:\n", col))
  print(summary(ces_clean[[col]]))
  cat("  NAs:", sum(is.na(ces_clean[[col]])), "\n")
}


cat("\n══ tookpost distribution ══\n")
print(table(ces_clean$tookpost, useNA = "always"))


cat("\n══ Demographic distributions ══\n")
for (col in c("age_cat", "gender_cat", "race_cat", "hispanic_cat", "educ_cat")) {
  cat(sprintf("\n%s:\n", col))
  print(table(ces_clean[[col]], useNA = "always"))
}


cat("\n══ vote_2024 distribution ══\n")
print(table(ces_clean$vote_2024, useNA = "always"))


cat("\n══ afact distribution ══\n")
print(summary(ces_clean$afact))


cat("\n══ Multi-CD respondents ══\n")
multi_cd <- ces_clean %>% count(caseid) %>% filter(n > 1)
single_cd <- ces_clean %>% count(caseid) %>% filter(n == 1)
cat("Respondents with multiple rows (multi-CD):", nrow(multi_cd), "\n")
cat("Respondents with single row:", nrow(single_cd), "\n")
cat("Max rows per respondent:", max(table(ces_clean$caseid)), "\n")


cat("\n══ Geographic coverage ══\n")
cat("Unique state_abbrv:", n_distinct(ces_clean$state_abbrv), "\n")
cat("Unique state_cd:",    n_distinct(ces_clean$state_cd),    "\n")


cat("\n══ NA counts per column ══\n")
print(colSums(is.na(ces_clean)))


# ── 3. Save ───────────────────────────────────────────────────────────────

write_csv(ces_clean,
          paste0(output_dir, "ces_2024_for_mrsp.csv"))

saveRDS(ces_clean,
        paste0(output_dir, "ces_2024_for_mrsp.rds"))

cat("\nSaved ces_2024_for_mrsp.csv and .rds\n")
cat("File size (CSV):",
    round(file.info(paste0(output_dir, "ces_2024_for_mrsp.csv"))$size / 1e6, 2),
    "MB\n")
sink()

sink("output.txt")
library(tidyverse)

base_path <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/"
output_dir <- paste0(base_path, "deliverables_for_danielius/")


readme_text <- '# CES 2024 Individual-Level Data for MrsP Modeling

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
| afact  | numeric | Allocation factor for respondents whose ZCTA spans multiple 2026 CDs. afact sums to 1 across a respondent\'s rows. For respondents in a ZCTA contained in a single 2026 CD, afact = 1. |

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
  team\'s methodological decision (pending discussion)
- afact is the allocation factor (1.0 for single-CD respondents, fractional
  for multi-CD)

For a single-CD respondent: effective_weight = chosen_weight (afact = 1).

For a multi-CD respondent with N rows: the N effective weights sum to
chosen_weight (since afact sums to 1 across the rows). The respondent\'s total
contribution is the same as a single-CD respondent; it is just distributed
across multiple candidate CDs proportionally to the probability of residence.

## Cell Mapping
Each row maps to a stratification frame cell via:
(state_cd, age_cat, gender_cat, race_cat, hispanic_cat, educ_cat)

This combination matches the cell-level rows in
`stratification_frame_2026_preMrsP.csv`.

## Area-Level Covariate Join
Each row\'s state_cd joins to `area_level_vote_shares.csv` for CD-level area
covariates (2024 House shares, state-level 2024 Pres shares, contestation flag,
etc.).
'

writeLines(readme_text, paste0(output_dir, "README_ces_2024_for_mrsp.md"))

cat("Saved README_ces_2024_for_mrsp.md\n")
sink()


sink("output.txt")
#diagnostics
library(tidyverse)
xw <- read_csv("/Users/binampoudyal/Downloads/zip_to_cd_2026.csv")

cat("Total rows:", nrow(xw), "\n")
cat("Unique ZIPs:", n_distinct(xw$zip), "\n\n")

# How many ZIPs map to multiple CDs?
zip_cd_counts <- xw %>% count(zip, name = "n_cds")

cat("ZIPs in single CD:", sum(zip_cd_counts$n_cds == 1), "\n")
cat("ZIPs split across multiple CDs:", sum(zip_cd_counts$n_cds > 1), "\n")
cat("Percent split:", 
    round(100 * mean(zip_cd_counts$n_cds > 1), 1), "%\n\n")

cat("Distribution of n_cds per ZIP:\n")
print(table(zip_cd_counts$n_cds))
sink()
