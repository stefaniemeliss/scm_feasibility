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
in_dir <- file.path(dir_data, "performance-tables")

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
scaffold <- merge(data.frame(time_period = years_list), data.frame(urn = urn_list))

for (year in start:finish) {
  
  # skip covid years
  if(year == 2019 | year == 2020) next
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  
  # determine folder for academic year
  dir_year <- file.path(in_dir, academic_year)

  # read in absences data
  abs <- read.csv(file = file.path(dir_year, paste0(academic_year, "_england_abs.csv")))
  names(abs) <- gsub("X...", "", names(abs), fixed = T)
  
  # subset data to only include relevant schools
  names(abs) <- tolower(names(abs))
  abs <- abs %>% filter(urn %in% urn_list)
  
  # Figures are suppressed (“supp”) where they concern fewer than 10 pupils.
  abs <- apply(abs, 2, function(x) {ifelse(x == "SUPP", NA, as.numeric(x))}) %>% as.data.frame()
  
  # add year
  abs$time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # combine across years
  if (year == start) {
    df <- abs
  } else {
    df <- rbind.all.columns(df, abs)
  }
  
}

# merge with scaffold
df <- merge(scaffold, df, by = c("time_period", "urn"), all.x = T)
#names(df)[names(df) == "urn"] <- "school_urn"

# save data
df <- df[with(df, order(urn, time_period)),]
write.csv(df, file = file.path(dir_data, "data_spt_absences.csv"), row.names = F)
