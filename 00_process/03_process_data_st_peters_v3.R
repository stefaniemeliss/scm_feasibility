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
  overwrite = TRUE
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
    laestab != id_treated,
    phaseofeducation_name %in% unique(c(est_treated$phaseofeducation_name)),
    #gor_name %in% unique(c(est_treated$gor_name, "East Midlands")),
    ! parliamentaryconstituency_name %in% unique(c(est_treated$parliamentaryconstituency_name)),
    ! grepl("Boarding school", boarders_name),
    admissionspolicy_name != "Selective"
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
  select(time_period, laestab, school, pupil_to_qual_teacher_ratio, fte_avg_age, fte_perc_age_under_25, fte_perc_age_60_and_over) %>%
  group_by(laestab) %>%
  arrange(laestab, desc(time_period)) %>%
  mutate(school = first(school)) %>%
  arrange(laestab, time_period) %>%
  ungroup() %>%
  as.data.frame()


# Update list of laestab numbers based on the dependent variable
check <- z %>% group_by(laestab) %>% summarise(n = n()) # identify estabs with more than 1 idaci decile (school has moved locations)
id_remove <- check$laestab[check$n != 14] # identify estab numbers
list_laestab_dv <- unique(z$laestab)

#### CREATE PREDICTOR DATASET ####

# Filter to create predictor dataset
x <- pup[laestab %in% list_laestab_dv]
x[, status := fifelse(laestab %in% unique(est_treated[, "laestab"]), "treated", "untreated")]
x[, idx_treat := fifelse(laestab %in% unique(est_treated[, "laestab"]), TRUE, FALSE)]
x[, idx_donor := fifelse(laestab %in% unique(est_treated[, "laestab"]), FALSE, TRUE)]
x[, idaci_decile := NULL] # Remove idaci_decile

# Add more variables as predictors from establishment data
tmp <- est[, .(laestab, sex_students, urbanrural_name, lat, long)]
tmp <- tmp[laestab %in% list_laestab_dv]
tmp <- tmp[!duplicated(tmp)]

check <- tmp %>% group_by(laestab) %>% summarise(n = n()) # identify estabs where student_sex or urban_rural changed across establishments)
id_remove <- check$laestab[check$n != 1] # identify estab numbers
list_laestab_dv <- setdiff(list_laestab_dv, id_remove)# update list_laestab_dv to exclude the ids that should be removed
tmp <- tmp[laestab %in% list_laestab_dv, ] # update data
x <- x[laestab %in% list_laestab_dv, ] # remove from x
z <- z[z$laestab %in% list_laestab_dv, ] # remove from z

# Dummy-code coed variable
tmp$coed <- ifelse(tmp$sex_students == "Co-ed", 1, ifelse(tmp$sex_students == "Single-sex", 0, NA))

# Create dummy variables for urbanicity categories
tmp$urban_minor_conurbation <- ifelse(tmp$urbanrural_name == "(England/Wales) Urban minor conurbation", 1, 0)
tmp$urban_city_town <- ifelse(tmp$urbanrural_name == "(England/Wales) Urban city and town", 1, 0)
tmp$urban_major_conurbation <- ifelse(tmp$urbanrural_name == "(England/Wales) Urban major conurbation", 1, 0)

# Select relevant columns and merge with predictor dataset
tmp <- tmp[, .(laestab, coed, urban_major_conurbation, urban_minor_conurbation, urban_city_town, lat, long)]
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

# process region data #

# determine col name of region of treated unit
region_treated <- unique(est_treated$gor_name)
region_treated <- paste0("region_dummy_", tolower(gsub(" ", "", region_treated)))

x <- x %>%
  # dummy-code all regions
  mutate(region_dummy_london = ifelse(grepl("london", tolower(region)), 1, 0 ),
         region_dummy_westmidlands = ifelse(grepl("west midlands", tolower(region)), 1, 0 ),
         region_dummy_eastmidlands = ifelse(grepl("east midlands", tolower(region)), 1, 0 ),
         region_dummy_northwest = ifelse(grepl("north west", tolower(region)), 1, 0 ),
         region_dummy_southwest = ifelse(grepl("south west", tolower(region)), 1, 0 ),
         region_dummy_yorkshire = ifelse(grepl("yorkshire", tolower(region)), 1, 0 ),
         region_dummy_northeast = ifelse(grepl("north east", tolower(region)), 1, 0 ),
         region_dummy_southeast = ifelse(grepl("south east", tolower(region)), 1, 0 ),
         region_dummy_eastofengland = ifelse(grepl("east of england", tolower(region)), 1, 0 )
  ) %>%
  select(! all_of(region_treated)) # remove dummy-var for treated unit's regions, so that this becomes reference category

# select relevant variables 
vars <- names(x)[
  grepl("time_per|laestab|idaci|pnpupf|pnpupe|pnpupsen|coed|urban|academy_trust|region_dummy|ks2a_zscore|lat|long", 
        names(x))]

vars <- setdiff(vars, c("pnpupfsm_t", "pnpupfsm_e_spt", "urban_rural"))

x <- x %>% 
  select(all_of(vars))

# combine outcome and predictor
df <- merge(z, x, by = c("laestab", "time_period"), all = T)

# remove the last two digits
df$time_period <- as.numeric(substr(df$time_period, 0, 4))

# remove any schools that do not have any observations for any of the variables
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

# compute time series average per school #

timeseries_ave_per_school <- function(data = df, var = "var", perc = NULL){
  
  # pre-treatment outcome average and sd for treated school
  mean_treated <- mean(data[data$laestab == id_treated, paste(var)])
  sd_treated <- sd(data[data$laestab == id_treated, var])

  # pre-treatment outcome average and sd for control schools
  school_ave <- data %>% 
    filter(laestab != id_treated) %>%
    group_by(laestab) %>%
    summarise(
      mean_dv = mean(get(var), na.rm = T),
      sd_dv = sd(get(var), na.rm = T)
    ) %>%
    mutate(
      # check if average pre-treatment outcome is within [X] SDs of the average for treated school 
      crit_sd_100 = ifelse(mean_dv > (mean_treated + 1.0 * sd_treated) | mean_dv < (mean_treated - 1.0 * sd_treated), FALSE, TRUE),
      crit_sd_90 = ifelse(mean_dv > (mean_treated + .90 * sd_treated) | mean_dv < (mean_treated - .90 * sd_treated), FALSE, TRUE),
      crit_sd_80 = ifelse(mean_dv > (mean_treated + .80 * sd_treated) | mean_dv < (mean_treated - .80 * sd_treated), FALSE, TRUE),
      crit_sd_75 = ifelse(mean_dv > (mean_treated + .75 * sd_treated) | mean_dv < (mean_treated - .75 * sd_treated), FALSE, TRUE),
      crit_sd_70 = ifelse(mean_dv > (mean_treated + .70 * sd_treated) | mean_dv < (mean_treated - .70 * sd_treated), FALSE, TRUE),
      crit_sd_60 = ifelse(mean_dv > (mean_treated + .60 * sd_treated) | mean_dv < (mean_treated - .60 * sd_treated), FALSE, TRUE),
      crit_sd_50 = ifelse(mean_dv > (mean_treated + .50 * sd_treated) | mean_dv < (mean_treated - .50 * sd_treated), FALSE, TRUE),
      crit_sd_40 = ifelse(mean_dv > (mean_treated + .40 * sd_treated) | mean_dv < (mean_treated - .40 * sd_treated), FALSE, TRUE),
      crit_sd_30 = ifelse(mean_dv > (mean_treated + .30 * sd_treated) | mean_dv < (mean_treated - .30 * sd_treated), FALSE, TRUE),
      crit_sd_25 = ifelse(mean_dv > (mean_treated + .25 * sd_treated) | mean_dv < (mean_treated - .25 * sd_treated), FALSE, TRUE),
      crit_sd_20 = ifelse(mean_dv > (mean_treated + .20 * sd_treated) | mean_dv < (mean_treated - .20 * sd_treated), FALSE, TRUE),
      crit_sd_10 = ifelse(mean_dv > (mean_treated + .10 * sd_treated) | mean_dv < (mean_treated - .10 * sd_treated), FALSE, TRUE),
    )
  
  # summarise results: numbers show count kept
  print(colSums(school_ave[, c(-1:-3)]))
  
  if (!is.null(perc)) {
    tmp <- school_ave[, c("laestab", paste0("crit_sd_", perc))]
    names(tmp)[2] <- paste0("crit_sd_", var)
    data <- merge(data, tmp, by = "laestab", all.x = T)
    
    # update the original df in the parent environment
    assign("df", data, envir = .GlobalEnv)
    
  }
  
}

timeseries_ave_per_school(data = df, perc = 100, var = "pupil_to_qual_teacher_ratio")
timeseries_ave_per_school(data = df, perc = 100, var = "fte_avg_age")
timeseries_ave_per_school(data = df, perc = 100, var = "pnpupfsm_e")

# determine schools not passing threshold
id_remove <- unique(df$laestab[df$crit_sd_pupil_to_qual_teacher_ratio == F | df$crit_sd_fte_avg_age == F | df$crit_sd_pnpupfsm_e])

# remove from df
df <- df[!df$laestab %in% id_remove, ]

# determine ids of control schools
id_cont <- unique(df$laestab[df$laestab != 3344650])
