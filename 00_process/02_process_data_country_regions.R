# School Workforce Census (SWC) #

# Data source: SWC 2023 (November).

options(scipen = 999)
# empty work space
rm(list = ls())
gc()

# load libraries
library(kableExtra)
library(dplyr)

#devtools::source_url("https://github.com/stefaniemeliss/edu_stats/blob/main/functions.R?raw=TRUE")

# define directories
dir <- getwd()
dir_data_out <- file.path(dir, "data")
dir_data_in <- gsub("scm_feasibility", "edu_stats", dir_data_out)
dir_misc <- file.path(dir, "misc")
dir_in <- file.path(dir_data_in, "school-workforce-in-england")

# determine year list (akin to other data sources)
years_list <- paste0(20, 10:23, 11:24)
id_cols <- c("time_period", "urn")

# Pupil to teacher ratios - Local authority; National; Regional #

# read in data
ptrs <- read.csv(file.path(dir_in, "2023", "data", "workforce_ptrs_2010_2023_nat_reg_la.csv"))

ptrs <- ptrs %>%
  # rename columns 
  rename_with(., ~tolower(gsub("X...", "", ., fixed = T))) %>% 
  # drop columns
  select(-region_code) %>%
  # filter
  filter(geographic_level %in% c("National", "Regional")) %>%
  filter(grepl("tate-funded", school_type)) %>%
  # remove columns that are uninformative
  select(where(~length(unique(na.omit(.x))) > 1)) %>%
  # mutate
  mutate(
    # replace empty cells
    region_name = ifelse(geographic_level == "National", "England", region_name),
    # make FTE numeric
    pupils_fte = as.numeric(gsub(",", "", pupils_fte)),
    qualified_teachers_fte = as.numeric(gsub(",", "", qualified_teachers_fte)),
    teachers_fte = as.numeric(gsub(",", "", teachers_fte)),
    adults_fte = as.numeric(gsub(",", "", adults_fte))) %>%
  # re-compute ratios
  mutate(pupil_to_qual_teacher_ratio = pupils_fte / qualified_teachers_fte,
         pupil_to_qual_unqual_teacher_ratio = pupils_fte / teachers_fte,
         pupil_to_adult_ratio = pupils_fte / adults_fte) %>%
  as.data.frame()


# Size of the school workforce - Local authority; National; Regional #

# read in data
swf <- read.csv(file.path(dir_in, "2023", "data", "workforce_2010_2023_fte_hc_nat_reg_la.csv"))
names(swf) <- tolower(gsub("X...", "", names(swf), fixed = T))

# select columns
swf <- swf[, grepl("time_period|geog|region_name|school_type|teach", names(swf))]
swf <- swf[, !grepl("fte_ft|fte_pt|hc_pt|hc_ft|leader|head", names(swf))]


swf <- swf %>%
  # filter rows
  filter(geographic_level %in% c("National", "Regional")) %>%
  filter(grepl("tate-funded", school_type)) %>%
  mutate(
    # replace empty cells
    region_name = ifelse(geographic_level == "National", "England", region_name)) %>%
  # make numeric
  mutate_at(vars(-c("geographic_level", "region_name", "school_type")), as.numeric) %>%
  # return df
  as.data.frame()


# combine both
df <- merge(ptrs, swf, by = intersect(names(swf), names(ptrs)))

# save data
df <- df[with(df, order(time_period, geographic_level)),]
data.table::fwrite(df, file = file.path(dir_data_out, "data_swf_nat_reg.csv"), row.names = F)
