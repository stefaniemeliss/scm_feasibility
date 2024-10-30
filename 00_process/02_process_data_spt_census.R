# school performance tables - census data #

# Data source: the DfE’s January school census for 2024.

options(scipen = 999)
# empty work space
rm(list = ls())

# load libraries
library(kableExtra)
library(dplyr)

devtools::source_url("https://github.com/stefaniemeliss/scm_feasibility/blob/main/functions.R?raw=TRUE")

# define directories
dir <- getwd()
dir_data <- file.path(dir, "data")
dir_misc <- file.path(dir, "misc")
dir_in <- file.path(dir_data, "performance-tables")

# derive URNs
school_list <- read.csv(file = file.path(dir_misc, "schools_list.csv"))
urn_list <- school_list$urn

# determine year list (akin to other data sources)
years_list <- paste0(20, 10:22, 11:23)

##### performance tables #####

# determine years of interest
start <- 2010
finish <- 2022

# create scaffold to safe data
scaffold <- merge(data.frame(time_period = years_list),
                  data.frame(urn = urn_list))
id_cols <- names(scaffold)

for (year in start:finish) {
  
  # skip covid years
  if(year == 2019 | year == 2020) next
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  
  # determine folder for academic year
  dir_year <- file.path(dir_in, academic_year)
  
  # read in census data
  tmp <- read.csv(file = file.path(dir_year, paste0(academic_year, "_england_census.csv")))
  names(tmp) <- gsub("X...", "", names(tmp), fixed = T)
  
  # subset data to only include relevant schools
  names(tmp) <- tolower(names(tmp))
  tmp <- tmp %>% filter(urn %in% urn_list)
  
  # replace spaces and %
  tmp <- apply(tmp, 2, function(x) {ifelse(grepl(" |%", x), gsub(" |%", "", x), x)}) %>% 
    as.data.frame()
  
  # Figures are suppressed (“supp”) where they concern fewer than 10 pupils.
  tmp <- apply(tmp, 2, function(x) {ifelse(x == "SUPP" | x == "NE" | x == "" | x == " ", NA, as.numeric(x))}) %>%
    as.data.frame()
  
  # add year
  tmp$time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # get meta data
  tmp_meta <- read.csv(file = file.path(dir_year, paste0(academic_year, "_census_meta.csv")))
  names(tmp_meta) <- gsub("X...", "", tolower(names(tmp_meta)), fixed = T)
  names(tmp_meta)[names(tmp_meta) == "field.reference"] <- "variable"
  names(tmp_meta)[names(tmp_meta) == "field.name"] <- "label"
  tmp_meta <- tmp_meta[, c("variable", "label")]
  tmp_meta$time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # combine across years
  if (year == start) {
    census <- tmp
    meta <- tmp_meta
  } else {
    census <- rbind.all.columns(census, tmp)
    meta <- rbind.all.columns(meta, tmp_meta)
  }
  
}

# process meta data #
meta <- meta[with(meta, order(variable, time_period)), ]
# save meta data #
write.csv(meta, file = file.path(dir_misc, "meta_spt_census.csv"), row.names = F)

# process census data #

# Total number of pupils on roll (all ages)
# TOTPUPSENDN - Total number of pupils on roll (all ages) - 2011-11 to 2013/14
# NOR - Total number of pupils on roll from 2014/15 onwards
cols_to_merge <- c("totpupsendn", "nor")
new_col <- "npuptot__sptcensus"

df <- merge_timelines_across_columns(data_in = census, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = scaffold)

# NUMFSM - Number of pupils eligible for free school meals
# PNUMFSM - Percentage of pupils eligible for free school meals
# NUMFSMEVER - Number of pupils eligible for FSM at any time during the past 6 years
# PNUMFSMEVER - Percentage of pupils eligible for FSM at any time during the past 6 years
# TOTPUPFSMDN - Number of pupils used in FSM calculation
# NUMEAL - Number of pupils with English not as first language
# PNUMEAL - Percentage of pupils with English not as first language
# TOTPUPEALDN - Number of pupils of compulsory school age and above
# TOTPUPSENDN	Total number of pupils on roll (all ages)

df <- merge(df, census[, c(id_cols, "numfsm", "pnumfsm", 
                           "numfsmever", "pnumfsmever",
                           "totpupfsmdn",
                           "numeal", "pnumeal",
                           "totpupealdn")], by = id_cols, all = T)

# special educational needs #

# TSENA - Number of pupils on roll with SEN on School Action
# PSENA - Percentage of pupils on roll with SEN on School Action
# TOTSENAP - Total pupils with school  action+
# PTOTSENAP - Percentage pupils with school  action+
# TOTSENST - Total pupils with SEN statement
# PTOTSENST - Percentage pupils with SEN statement
# TSENSAP - Number of pupils SEN statement or on School Action Plus 
# PSENSAP - Percentage of pupils SEN statement or on School Action Plus 
# TSENELK - Number of eligible pupils with SEN support
# PSENELK - Percentage of eligible pupils with SEN support
# TSENELSE - Number of SEN pupils with a statement or EHC plan
# PSENELSE - Percenatge of SEN pupils with a statement or EHC plan

df <- merge(df, census[, c(id_cols, 
                           "tsena", "psena", # pupils on roll with SEN on School Action
                           "totsenap", "ptotsenap", # pupils on roll with school action plus
                           "totsenst", "ptotsenst", # pupils on roll with SEN statement
                           #"tsensap", "psensap", # SEN statement or on School Action Plus - IGNORE?
                           "tsenelk", "psenelk", # eligible pupils with SEN support
                           "tsenelse", "psenelse" # SEN pupils with a statement or EHC plan
                           )], by = id_cols, all = T)

# save data
df <- df[with(df, order(urn, time_period)),]
write.csv(df, file = file.path(dir_data, "data_spt_census.csv"), row.names = F)
