# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT 04: Build unified PUMA-to-2026 CD crosswalk
#
# Purpose:
#   Build a single PUMA × 2026 CD crosswalk covering all 50 states + DC,
#   for joining to ACS PUMS records in Script 05. The crosswalk provides,
#   for each PUMA-CD intersection, the fraction of the PUMA's population
#   that resides in that CD (afact). This allocation factor enables
#   probabilistic CD assignment for PUMS person records.
#
# Two crosswalk files Geocorr 2022 loaded from external file, and a crosswalk file built within these scripts for the redistricted CDs :
#   The Geocorr 2022 PUMA × CD crosswalk maps PUMAs to 119th Congress
#   (2024) CDs. For 43 states, 2024 and 2026 boundaries are identical, so
#   Geocorr is used directly. For 7 states with substantial 2025
#   redistricting (CA, FL, MO, NC, OH, TX, UT), boundaries differ between
#   2024 and 2026, so we build state-specific crosswalks from official
#   Block Assignment Files using population-weighted aggregation.
#
# Stages:
#   Stage 1 — Load Geocorr 2022 (covers 43 stable states + DC)
#   Stage 2 — Build BAF-based crosswalks for 7 redistricted states
#             via block → tract → PUMA → CD aggregation
#   Stage 3 — Merge: drop redistricted-state rows from Geocorr, append
#             new state crosswalks, drop Puerto Rico, save as unified file
#
# Inputs:
#   - Geocorr 2022 PUMA × CD crosswalk (CSV)
#   - 7 state Block Assignment Files (CSV / Excel)
#   - 2020 Census Tract-to-PUMA Relationship File (Census Bureau)
#   - 2020 Census block populations (via tidycensus)
#
# Output:
#   - unified_crosswalk_2026.rds
#     Columns: state (int FIPS), puma22 (int), cd_2026 (int), afact (num)
#     ~4,144 rows covering 51 jurisdictions, 436 state+CD combinations
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# STAGE 1: Load Geocorr 2022 PUMA-to-CD crosswalk
#
# Purpose:
#   Load the Geocorr 2022 PUMA × CD crosswalk and prepare it for use with
#   the PUMS data. The file maps each 2022 PUMA to its overlapping 119th
#   Congress (2024) congressional districts, with population-weighted
#   allocation factors. Used directly for the 43 states with stable 2024-2026
#   boundaries; the 7 redistricted states will be replaced in Stage 2.
#
# Input:
#   - geo_path → geocorr2022_2610104623.csv 
#   (set in Subscript 1; crosswalk file downloaded from **Source**: Geocorr 2022 PUMA × CD crosswalk from the Missouri Census Data Center (MCDC), accessible at https://mcdc.missouri.edu/applications/geocorr2022.html)                                        
#
# Output (in-memory):
#   - crosswalk : data frame with columns
#       state     int     State FIPS code
#       puma22    int     2022 PUMA code (unique within state)
#       cd119     int     119th Congress CD code (also valid for 2026 in stable states)
#       stab      chr     State abbreviation
#       puma_name chr     Human-readable PUMA name
#       pop20     num     2020 Census population in PUMA × CD intersection
#       afact     num     Fraction of PUMA's population in this CD (primary use)
#       afact2    num     Fraction of CD's population in this PUMA (reverse direction)
#
# Diagnostics:
#   - afact sums per (state, puma22) — should equal 1.0 (rounding tolerance)
#   - NA audit
#   - Unique state, PUMA, and CD counts
#   - Identification of unusual CD codes (0, 98, 99) and at-large states
#   - Distribution of near-zero afact rows
# ══════════════════════════════════════════════════════════════════════════════
#-- 1. Loading geocorr data ----------------------------------------------
# ── Read description row first ────────────────────────────────────────────────
geo_path  <- "/Users/binampoudyal/Downloads/Stratification_Frame_Building/geocorr2022_2610104623.csv" #path to crosswalk file
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
# afact must be numeric 

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

# ══════════════════════════════════════════════════════════════════════════════
# STAGE 2: Build PUMA-to-2026 CD crosswalks for 7 redistricted states
#
# Purpose:
#   For 7 states with substantial 2025 redistricting (CA, FL, MO, NC, OH, TX,
#   UT), build PUMA × CD crosswalks reflecting 2026 boundaries. The Geocorr
#   2022 crosswalk from Stage 1 reflects 2024 (119th Congress) boundaries
#   which are no longer accurate for these states.
#
#   The method recreates Geocorr's internal methodology: aggregate official
#   block-to-CD assignments up through the geographic hierarchy
#   (block → tract → PUMA) using population weighting, producing a
#   population-weighted PUMA × CD allocation factor (afact).
#
# Inputs:
#   - tract_to_puma_clean : built once from Census Bureau's 2020 Tract to
#                           2020 PUMA Relationship File (loaded at top of
#                           stage). Tracts nest completely within PUMAs.
#   - tidycensus API      : used to pull 2020 Census block populations
#                           (variable P1_001N from PL 94-171 redistricting file)
#   - 7 state Block Assignment Files (BAFs) — official mappings of each
#     Census block to its 2026 CD, published by each state's redistricting
#     authority. Sources documented in Stage 2 methodology section.
#
# Outputs (one RDS per state, saved to /Users/binampoudyal/Downloads):
#   tx_puma_cd_crosswalk.rds, ca_puma_cd_crosswalk.rds, mo_puma_cd_crosswalk.rds,
#   nc_puma_cd_crosswalk.rds, oh_puma_cd_crosswalk.rds, ut_puma_cd_crosswalk.rds,
#   fl_puma_cd_crosswalk.rds
#
#   Each contains: state (FIPS chr), puma (chr), cd_new (int),
#                  pop_intersection (num), puma_pop (num), afact (num)
#
# Helper function: build_state_puma_cd_crosswalk()
#
#   Pipeline for one state:
#     1. Read BAF (auto-detects CSV/Excel from extension; configurable
#        delimiter, header presence, column names)
#     2. Pull 2020 Census block populations via tidycensus
#     3. Join BAF to block populations → each block has (CD, population)
#     4. Aggregate to (tract × CD) intersections by summing block populations
#     5. Join tract → PUMA via tract_to_puma_clean (clean lookup, no splits)
#     6. Aggregate to (PUMA × CD); compute afact = intersection_pop / puma_pop
#     7. Validate (afact sums to 1.0 per PUMA) and save state-specific RDS
#
#   Configurable arguments per state:
#     baf_path          : file path
#     state_abb         : 2-letter postal code (e.g. "CA")
#     state_fips        : 2-digit FIPS as character (e.g. "06")
#     baf_block_col     : name of block GEOID column in BAF
#     baf_district_col  : name of CD column in BAF
#     baf_delim         : delimiter for CSV (default ",")
#     baf_has_header    : whether BAF has header row (default TRUE)
#     output_dir        : where to save state-specific RDS
#
# Validation checks per state:
#   - All blocks match between BAF and tidycensus population pull
#   - State total population matches 2020 Census
#   - Unique CD count matches state's known House delegation size
#   - afact sums to 1.0 (within rounding) per PUMA
#   - Distribution of CDs per PUMA reported as sanity check
#
# Note on Virginia (8th potentially-redistricted state):
#   Virginia's 2025 redistricting amendment was voter-approved on April 21,
#   2026 but faced legal challenges. Virginia is therefore covered by Geocorr in Stage 1
#   (no custom crosswalk built). If the legal challenge resolves before
#   the 2026 election, a Virginia BAF would be added to this stage.
# ══════════════════════════════════════════════════════════════════════════════

###Initial file and packages load
library(tidyverse)
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


# ══════════════════════════════════════════════════════════════════════════════
# Function: build_state_puma_cd_crosswalk()
#
# Purpose:
#   Build a PUMA × CD crosswalk for a single state from its Block Assignment
#   File (BAF), using population-weighted aggregation. Recreates Geocorr's
#   internal methodology so the output is interchangeable with Geocorr rows
#   in the unified national crosswalk.
#
# Arguments:
#   baf_path         : path to the state's BAF file (CSV or Excel)
#   state_abb        : 2-letter postal code, e.g., "CA"
#   state_fips       : 2-digit FIPS code as character, e.g., "06"
#   baf_block_col    : name of block GEOID column in BAF
#                      (default "SCTBKEY"; override per state)
#   baf_district_col : name of CD column in BAF
#                      (default "DISTRICT"; override per state)
#   baf_delim        : delimiter for CSV files (default ",")
#   baf_has_header   : whether the BAF file has a header row (default TRUE)
#   output_dir       : directory to save the resulting RDS (default ".")
#   log_file         : optional file path to capture diagnostic output
#                      via sink(); NULL means print to console
#
# Pipeline (executed in 8 steps):
#   1. Read BAF — auto-detects CSV vs Excel from extension; handles header
#      and delimiter variations
#   2. Pull 2020 Census block populations via tidycensus (P1_001N from PL
#      94-171 redistricting file)
#   3. Join BAF to block populations → each block has (CD, population)
#   4. Aggregate to (tract × CD) by summing block populations within each
#      (tract, CD) intersection
#   5. Join tract → PUMA via tract_to_puma_clean (clean one-to-one lookup;
#      tracts nest within PUMAs)
#   6. Aggregate to (PUMA × CD); compute afact = pop_intersection / puma_pop
#   7. Validate: afact sums to 1.0 per PUMA; report CD-per-PUMA distribution
#   8. Save state-specific RDS to output_dir
#
# Output:
#   - RDS file: {state_abb_lowercase}_puma_cd_crosswalk.rds
#     Columns: state (chr FIPS), puma (chr), cd_new (int),
#              pop_intersection (num), puma_pop (num), afact (num)
#   - Function also returns the data frame (for interactive inspection)
#
# Requirements:
#   - tract_to_puma_clean must be loaded in the environment (built once
#     above from Census Bureau's 2020 Tract-to-PUMA Relationship File)
#   - tidycensus API key must be set (via census_api_key() and Renviron)
#   - readxl package required for Excel BAFs (loaded conditionally)
#
# Usage examples:
#   tx_puma_cd <- build_state_puma_cd_crosswalk(
#     baf_path         = "/path/to/PLANC2333.csv",
#     state_abb        = "TX",
#     state_fips       = "48",
#     baf_block_col    = "SCTBKEY",
#     baf_district_col = "DISTRICT",
#     output_dir       = "/Users/binampoudyal/Downloads"
#   )
#
#   ca_puma_cd <- build_state_puma_cd_crosswalk(
#     baf_path         = "/path/to/ab604.csv",
#     state_abb        = "CA",
#     state_fips       = "06",
#     baf_delim        = ",",
#     baf_has_header   = FALSE,    # no header in CA file
#     output_dir       = "/Users/binampoudyal/Downloads"
#   )
# ══════════════════════════════════════════════════════════════════════════════

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

# ══════════════════════════════════════════════════════════════════════════════
# Function calls to create crosswalk for each redistricted state
# ══════════════════════════════════════════════════════════════════════════════

###Texas
tx_puma_cd <- build_state_puma_cd_crosswalk(
  baf_path         = "/Users/binampoudyal/Downloads/rstudio-export/PLANC2333.csv",
  state_abb        = "TX",
  state_fips       = "48",
  baf_block_col    = "SCTBKEY",
  baf_district_col = "DISTRICT",
  output_dir       = "/Users/binampoudyal/Downloads"
)


###California 
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

#Florida
florida_puma_cd <- build_state_puma_cd_crosswalk(
  baf_path         = "/Users/binampoudyal/Downloads/EOGPCRP2026.csv",
  state_abb        = "FL",
  state_fips       = "12",
  baf_block_col    = "block_geoid",
  baf_district_col = "district",
  baf_has_header   = FALSE,
  output_dir       = "/Users/binampoudyal/Downloads"
)


# ══════════════════════════════════════════════════════════════════════════════
# STAGE 3: Merge state-specific crosswalks into a unified national crosswalk
#
# Purpose:
#   Combine the Stage 1 Geocorr crosswalk (43 stable states + DC) with the
#   Stage 2 state-specific crosswalks (7 redistricted states with 2026
#   boundaries) into a single unified PUMA × CD crosswalk covering all 50
#   states + DC. Drop Puerto Rico as it has no voting House representation.
#
# Inputs:
#   - crosswalk : Geocorr 2022 data frame loaded in Stage 1
#                 (columns: state, puma22, cd119, stab, puma_name, pop20,
#                  afact, afact2)
#   - 7 state-specific RDS files from Stage 2:
#       tx_puma_cd_crosswalk.rds, ca_puma_cd_crosswalk.rds,
#       mo_puma_cd_crosswalk.rds, nc_puma_cd_crosswalk.rds,
#       oh_puma_cd_crosswalk.rds, ut_puma_cd_crosswalk.rds,
#       fl_puma_cd_crosswalk.rds
#
# Output:
#   - unified_crosswalk_2026.rds
#     Columns: state (int FIPS), puma22 (int), cd_2026 (int), afact (num)
#     Rows: ~4,144 PUMA × CD intersections covering 51 jurisdictions
#     (50 states + DC)
#
# Pipeline:
#   1. Load all 7 state-specific crosswalks
#   2. Standardize their column structure to match Geocorr
#      (state→integer, puma→puma22 integer, cd_new→cd119 integer)
#   3. Filter Geocorr to non-redistricted states (drop FIPS 6, 12, 29, 37,
#      39, 48, 49 — those rows are replaced by the new BAF-derived versions)
#   4. Append the 7 new state crosswalks to the trimmed Geocorr
#   5. Drop Puerto Rico (FIPS 72) — no voting House representation; not
#      present in PUMS data anyway
#   6. Validate: afact sums to 1.0 per PUMA, expected state and CD counts
#   7. Rename cd119 to cd_2026
#   8. Save as unified_crosswalk_2026.rds
#
# Validation targets:
#   - Total rows: ~4,144
#   - Unique states: 51 (50 + DC)
#   - Unique state+CD combinations: 436 (435 voting House districts + DC)
#   - afact sum range: 1.0 ± rounding (≈ 0.9999 to 1.0001)
# ══════════════════════════════════════════════════════════════════════════════

# Load all 7 state-specific crosswalks
tx_xw  <- readRDS("/Users/binampoudyal/Downloads/tx_puma_cd_crosswalk.rds")
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

#Rename cd119 to cd_2026 since the congressional districts correspond to 2026 boundaries
unified_crosswalk <- unified_crosswalk %>%
  rename(cd_2026 = cd119)

# Save
saveRDS(unified_crosswalk, 
        "/Users/binampoudyal/Downloads/unified_crosswalk_2026.rds")