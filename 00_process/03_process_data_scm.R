# This is a data pre-processing script for later synthetic control method (SCM) analysis using [id_treated] as the treated school.
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

#### DETERMINE AVAILABLE TIMESERIES DATA ####

# Identify years for which there are observations for treated school
dv <- "pupil_to_qual_teacher_ratio"
data_avail_dv <- swf %>%
  filter(!is.na(get(dv))) %>%
  filter(laestab == id_treated) %>%
  select(time_period, laestab, paste(dv)) %>%
  distinct(time_period) %>%
  pull(time_period)

# Identify years for which there are observations for treated school
var1 <- "fte_avg_age"
data_avail_age <- swf %>%
  filter(!is.na(get(var1))) %>%
  filter(laestab == id_treated) %>%
  select(time_period, laestab, paste(var1)) %>%
  distinct(time_period) %>%
  pull(time_period)

# Identify years for which there are observations for treated school
var2 <- "pnpupfsm_e"
data_avail_fsm <- pup %>%
  filter(!is.na(get(var2))) %>%
  filter(laestab == id_treated) %>%
  select(time_period, laestab, paste(var2)) %>%
  distinct(time_period) %>%
  pull(time_period)

# combine, excluding duplicates
data_avail <- intersect(data_avail_dv, intersect(data_avail_age, data_avail_fsm))


#### PROCESS DATA FOR ESTABLISHMENTS ####

# Get data for the treated school based on the ID
est_treated <- est %>%
  filter(laestab == id_treated) %>%
  mutate(status = "treated") %>%
  as.data.frame()

# overwrite region with default if not previously specified
if (is.null(regions)) {
  regions <- unique(c(est_treated$gor_name))
}

# Get donor pool data excluding the treated school
est_cont <- est %>%
  filter(
    laestab != id_treated,
    phaseofeducation_name %in% unique(c(est_treated$phaseofeducation_name)),
    gor_name %in% regions,
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
z <- swf[laestab %in% list_laestab & time_period %in% data_avail]


# Remove rows for years for which the treated school has no data, 
# rows with NA for the dependent variable and age predictor 
# and add observation count
z <- z %>%
  filter(!is.na(get(dv))) %>%
  group_by(laestab) %>%
  mutate(
    obs_count_dv = sum(!is.na(get(dv))),
    obs_count_var1 = sum(!is.na(get(var1)))
  ) %>%
  ungroup()

# Filter for rows where observation count matches the treated ID and select columns
z <- z %>%
  filter(obs_count_dv == unique(z$obs_count_dv[z$laestab == id_treated])) %>%
  filter(obs_count_var1 == unique(z$obs_count_var1[z$laestab == id_treated])) %>%
  select(time_period, laestab, school, pupil_to_qual_teacher_ratio, pupil_to_qual_unqual_teacher_ratio, fte_avg_age) %>%
  group_by(laestab) %>%
  arrange(laestab, desc(time_period)) %>%
  mutate(school = first(school),
         fte_avg_age_roll = zoo::rollapply(fte_avg_age, width = 2, mean, align = "left", partial = T),
         pupil_to_qual_teacher_ratio_roll = zoo::rollapply(pupil_to_qual_teacher_ratio, width = 2, mean, align = "left", partial = T)
  ) %>%
  ungroup() %>%
  as.data.frame()

id_name <- unique(z$school[z$laestab == id_treated])

# Update list of laestab numbers based on the dependent dviable
check <- z %>% group_by(laestab) %>% summarise(n = n()) # identify estabs with more than 1 idaci decile (school has moved locations)
id_remove <- check$laestab[check$n != check$n[check$laestab == id_treated]] # identify estab numbers
list_laestab_dv <- unique(z$laestab)
list_laestab_dv <- setdiff(list_laestab_dv, id_remove)# update list_laestab_dv to exclude the ids that should be removed
rm(check)

#### CREATE PREDICTOR DATASET ####

# Filter SWF data to create outcome dataset
x <- pup[laestab %in% list_laestab_dv & time_period %in% data_avail]

# Remove rows for years for which the treated school has no data, 
# rows with NA for the dependent variable and age predictor 
# and add observation count
# x <- x %>%
#   filter(!is.na(get(var2))) %>%
#   group_by(laestab) %>%
#   mutate(
#     obs_count_var2 = sum(!is.na(get(var2)))
#   ) %>%
#   ungroup()

# Filter for rows where observation count matches the treated ID and select columns
x <- x %>%
  #filter(obs_count_var2 == unique(x$obs_count_var2[x$laestab == id_treated])) %>%
  select(time_period, laestab, urn, pnpupfsm_e) %>%
  group_by(laestab) %>%
  arrange(laestab, desc(time_period)) %>%
  mutate(
    pnpupfsm_e_roll = zoo::rollapply(pnpupfsm_e, width = 2, mean, align = "left", partial = T)
  ) %>%
  as.data.frame()

# combine outcome and predictor
df <- merge(z, x, by = c("laestab", "time_period"), all.x = T)

# apply(df, 2, function(x) sum(is.na(x)))
# 
# df <- df %>%
#   group_by(laestab) %>%
#   mutate(
#     obs_count_dv = sum(!is.na(get(dv))),
#     obs_count_var1 = sum(!is.na(get(var1))),
#     obs_count_var2 = sum(!is.na(get(var2)))
#   ) %>%
#   ungroup()

# arrange data
df <- df %>%
  relocate(urn, .after = laestab) %>%
  arrange(laestab, time_period) %>%
  mutate(
    # # change name to include laestab to navigate duplicates
    # school = paste(laestab, school),
    # add slash and use as string
    time_period_str = insert_slash(time_period),
    # remove the last two digits
    time_period = as.numeric(substr(time_period, 0, 4))
  )

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
  colSums(school_ave[, c(-1:-3)]) %>% 
    kbl(caption = paste(var)) %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T) %>% 
    add_footnote("Data shows the number of schools with average within crit_sd_[percentage].") %>%
    print()

  if (!is.null(perc)) {
    tmp <- school_ave[, c("laestab", paste0("crit_sd_", perc))]
    names(tmp)[2] <- paste0("crit_sd_", var)
    data <- merge(data, tmp, by = "laestab", all.x = T)
    
    # update the original df in the parent environment
    assign("df", data, envir = .GlobalEnv)
    
  }
  
}

if (show_sd_crit) {
  timeseries_ave_per_school(data = df, var = "pupil_to_qual_teacher_ratio")
  # timeseries_ave_per_school(data = df, var = "fte_avg_age")
  # timeseries_ave_per_school(data = df, var = "pnpupfsm_e")
  
}
