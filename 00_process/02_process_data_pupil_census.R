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
write.csv(meta, file = file.path(dir_misc, "meta_pupilcensus.csv"), row.names = F)

# process census data #

# Total number of pupils on roll (all ages)
# TOTPUPSENDN - Total number of pupils on roll (all ages) - 2011-11 to 2013/14
# NOR - Total number of pupils on roll from 2014/15 onwards
cols_to_merge <- c("totpupsendn", "nor")
new_col <- "pupils_on_roll"

df <- merge_timelines_across_columns(data_in = census, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = scaffold)

# PNUMFSM - Percentage of pupils eligible for free school meals
# PNUMFSMEVER - Percentage of pupils eligible for FSM at any time during the past 6 years
# PNUMEAL - Percentage of pupils with English not as first language

df <- merge(df, census[, c(id_cols, "pnumfsm", "pnumfsmever", "pnumeal")], by = id_cols, all.x = T)

# special educational needs
# PSENA - Percentage of pupils on roll with SEN on School Action
# proportion of students enrolled at a school who have been identified as having Special Educational Needs (SEN) and are receiving support through the School Action programme
# PSENELK - Percentage of eligible pupils with SEN support
# The percentage of eligible pupils with SEN support indicates the proportion of students who are receiving this broader range of support, rather than the more narrowly defined 'School Action' category.
cols_to_merge <- c("psena", "psenelk")
new_col <- "psen"

df <- merge_timelines_across_columns(data_in = census, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# PSENELSE - Percentage of SEN pupils with a statement or EHC plan
# PSENSAP - Percentage of pupils with SEN statement or on School Action Plus

cols_to_merge <- c("psensap", "psenelse")
new_col <- "psenst"

df <- merge_timelines_across_columns(data_in = census, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)



# merge with scaffold
df <- merge(scaffold, df, by = c("time_period", "urn"), all.x = T)
df <- df[with(df, order(urn, time_period)),]
names(df)[names(df) == "urn"] <- "school_urn"

# save data
write.csv(df, file = file.path(dir_data, "data_pupilcensus.csv"), row.names = F)
