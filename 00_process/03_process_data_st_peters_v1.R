# This is a data pre-processing script for later synthetic control method (SCM) analysis using St. Peter's as the treated school.
# The script copies and loads data files, processes data for treated and control schools,
# creates outcome and predictor time series, and handles academy trust status over time.
# Finally, it combines the outcome and predictor time series for SCM analysis.


#### SETUP ####

# Set scientific notation penalty to avoid scientific notation in outputs
options(scipen = 999)

# Clear the workspace and run garbage collection
# rm(list = ls())
gc()

# Load necessary libraries
library(kableExtra)
library(dplyr)
library(data.table)

# Define directories based on the current working directory
dir <- normalizePath(dir, winslash = "/")
dir_data <- file.path(dir, "data")
dir_misc <- file.path(dir, "misc")

# Copy data files from the source directory to the target directory
file.copy(
  file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_swf.csv"), 
  dir_data, 
  overwrite = T
)
file.copy(
  file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_pupils.csv"),
  dir_data,
  overwrite = TRUE
)
file.copy(
  file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_establishments_search.csv"),
  dir_data,
  overwrite = TRUE
)

# Load data from CSV files into data tables
swf <- fread(file = file.path(dir_data, "data_swf.csv"))
pup <- fread(file = file.path(dir_data, "data_pupils.csv"))
est <- fread(file = file.path(dir_data, "data_establishments_search.csv"), na.strings = "")

#### PROCESS DATA FOR ESTABLISHMENTS ####

# Get data for the treated school based on the ID
id_treated <- 3344650
est_treated <- est %>%
  filter(laestab == id_treated) %>%
  mutate(status = "treated") %>%
  as.data.frame()

# Get donor pool data excluding the treated school
est_cont <- est %>%
  filter(
    ! grepl("Boarding school", boarders_name),
    laestab != id_treated,
    phaseofeducation_name %in% unique(c(est_treated$phaseofeducation_name)),
    gor_name %in% unique(c(est_treated$gor_name)),
    urbanicity %in% c(unique(est_treated$urbanicity)), # coded as urban vs rural
    religiouscharacter_christian %in% unique(c(est_treated$religiouscharacter_christian))
  ) %>%
  mutate(status = "untreated") %>%
  as.data.frame()

# Save unique lists of laestab and urn
list_laestab <- c(unique(est_cont[, "laestab"]), unique(est_treated[, "laestab"]))
list_urn <- c(unique(est_cont[, "urn"]), unique(est_treated[, "urn"]))

#### CREATE OUTCOME DATASET ####

# Filter SWF data to create outcome dataset
z <- swf[laestab %in% list_laestab]
z[, status := fifelse(laestab %in% unique(est_treated[, "laestab"]), "treated", "untreated")]
z[, idx_treat := fifelse(laestab %in% unique(est_treated[, "laestab"]), TRUE, FALSE)]
z[, idx_donor := fifelse(laestab %in% unique(est_treated[, "laestab"]), FALSE, TRUE)]

# Remove rows with NA for the dependent variable and add observation count
var <- "pupil_to_qual_teacher_ratio"
z <- z %>%
  filter(!is.na(get(var))) %>%
  group_by(laestab) %>%
  mutate(
    obs_count = sum(!is.na(get(var)))
  ) %>%
  ungroup() %>%
  select(-one_of(c("region_code", "new_la_code", "old_la_code")))

# Filter for rows where observation count matches the treated ID and select columns
z <- z %>%
  filter(obs_count == unique(z$obs_count[z$laestab == id_treated])) %>%
  select(time_period, laestab, school, pupil_to_qual_teacher_ratio) %>%
  group_by(laestab) %>%
  arrange(laestab, desc(time_period)) %>%
  mutate(school = first(school)) %>%
  arrange(laestab, time_period) %>%
  ungroup() %>%
  as.data.frame()

# Update list of laestab numbers based on the dependent variable
list_laestab_dv <- unique(z$laestab)

#### CREATE PREDICTOR DATASET ####

# Filter to create predictor dataset
x <- pup[laestab %in% list_laestab_dv]
x[, status := fifelse(laestab %in% unique(est_treated[, "laestab"]), "treated", "untreated")]
x[, idx_treat := fifelse(laestab %in% unique(est_treated[, "laestab"]), TRUE, FALSE)]
x[, idx_donor := fifelse(laestab %in% unique(est_treated[, "laestab"]), FALSE, TRUE)]
x[, idaci_decile := NULL] # Remove idaci_decile

# Create temporary data frame with unique laestab and idaci decile
tmp <- est[, .(laestab, idaci_decile)]
tmp <- tmp[laestab %in% list_laestab_dv]
tmp <- tmp[!duplicated(tmp)]
tmp$time_period <- 201920 # Add time_period column

# Merge temporary data with predictor dataset
x <- merge(x, tmp, by = c("laestab", "time_period"), all.x = TRUE)
rm(tmp)

# Add more variables as predictors from establishment data
tmp <- est[, .(laestab, sex_students, urbanrural_name)]
tmp <- tmp[laestab %in% list_laestab_dv]
tmp <- tmp[!duplicated(tmp)]

# Dummy-code coed variable
tmp$coed <- ifelse(tmp$sex_students == "Co-ed", 1, ifelse(tmp$sex_students == "Single-sex", 0, NA))

# Create dummy variables for urbanicity categories
tmp$urban_minor_conurbation <- ifelse(tmp$urbanrural_name == "(England/Wales) Urban minor conurbation", 1, 0)
tmp$urban_city_town <- ifelse(tmp$urbanrural_name == "(England/Wales) Urban city and town", 1, 0)
tmp$urban_major_conurbation <- ifelse(tmp$urbanrural_name == "(England/Wales) Urban major conurbation", 1, 0)

# Select relevant columns and merge with predictor dataset
tmp <- tmp[, .(laestab, coed, urban_major_conurbation, urban_minor_conurbation, urban_city_town)]
x <- merge(x, tmp, by = c("laestab"), all.x = TRUE)
rm(tmp)

# PROCESS ACADEMY TRUST STATUS 
# Step 1: Initial Input

tmp <- est[, .(laestab, trustschoolflag_name, opendate, closedate)]
tmp <- tmp[laestab %in% list_laestab_dv]
tmp <- tmp[!duplicated(tmp)]

# Step 2a: Identify Schools with Multiple Entries

laestab_m <- tmp %>%
  group_by(laestab) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  pull(laestab)

# Step 2b: Identify Schools with Single Entries

laestab_s <- tmp %>%
  group_by(laestab) %>%
  summarise(n = n()) %>%
  filter(n == 1) %>%
  pull(laestab)

# Step 3: Define Academic Years
academic_years <- seq(2010, 2023)

# Step 4: process schools with single entry

scaffold <- merge(data.frame(time_period = academic_years),
                  data.frame(laestab = laestab_s))
tmp_s <- tmp %>%
  filter(laestab %in% laestab_s) %>%
  select(laestab, trustschoolflag_name) %>%
  full_join(scaffold, by = "laestab") %>% 
  as.data.frame()


# Step 5: process schools with multiple entries
tmp_m <- tmp %>%
  filter(laestab %in% laestab_m) %>%
  arrange(laestab) %>%
  as.data.frame()

# extract status for schools with multiple entries
tmp_m <- create_status_df(tmp_m, 2010, 2023)

# combine schools 
tmp <- rbind(tmp_s, tmp_m)

# Create dummy variables excluding the reference category (Not applicable)
tmp$multi_academy_trust <- ifelse(tmp$trustschoolflag_name == "Supported by a multi-academy trust", 1, 0)
tmp$single_academy_trust <- ifelse(tmp$trustschoolflag_name == "Supported by a single-academy trust", 1, 0)
tmp$trustschoolflag_name <- NULL

# Function to convert time_period in tmp to match x
convert_time_period <- function(year) {
  paste0(year, substr(year + 1, 3, 4))
}

# Apply the function to tmp
tmp <- tmp %>%
  mutate(time_period = as.numeric(convert_time_period(time_period))) %>%
  as.data.frame()

# merge
x <- merge(x, tmp, by = c("laestab", "time_period"), all.x = TRUE)
rm(tmp, tmp_s, tmp_m, scaffold)

# select relevant variables 
x <- x %>% 
  select(time_period, laestab, 
         # covariates instead of filter
         idaci_decile, pnpupf, coed,
         urban_city_town, urban_minor_conurbation, urban_major_conurbation,
         single_academy_trust, multi_academy_trust,
         # other covariates
         pnpupfsm_e, pnpupeal, pnpupeowb, pnpupeobl, pnpupeoas, pnpupsen, ks2a_zscore 
  )


# combine outcome and predictor
df <- merge(z, x, by = c("laestab", "time_period"), all = T)

# remove the last two digits
df$time_period <- as.numeric(substr(df$time_period, 0, 4))

# count the non-NA observations for each variable in the dataframe
id_remove <- df %>%
  # count non-NA
  group_by(laestab) %>%
  summarise(across(everything(), ~ sum(!is.na(.)), .names = "count_{col}")) %>%
  # Identify laestab groups that have zero in any cells
  filter(if_any(everything(), ~ . == 0)) %>%
  pull(laestab)

# remove from df
df <- df[!df$laestab %in% id_remove, ]

# determine ids of control schools
id_cont <- unique(df$laestab[df$laestab != 3344650])

# change name to include laestab to navigate duplicates
df$school <- paste(df$laestab, df$school)