options(scipen = 999)
# empty work space
rm(list = ls())
gc()

# load libraries
library(kableExtra)
library(dplyr)
library(data.table)

#devtools::source_url("https://github.com/stefaniemeliss/scm_feasibility/blob/main/functions.R?raw=TRUE")

# define directories
dir <- getwd()
dir_data <- file.path(dir, "data")
dir_misc <- file.path(dir, "misc")

# copy data #
file.copy(
  file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_swf.csv"), dir_data, overwrite = T)
file.copy(
  file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_pupils.csv"), dir_data, overwrite = T)
file.copy(
  file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_establishments_search.csv"), dir_data, overwrite = T)
# file.copy(
#   file.path(gsub("scm_feasibility", "edu_stats", dir_misc), "lookup_postcodearea_region.csv"), dir_misc, overwrite = T)

# load data #
 
swf <- fread(file.path(dir_data, "data_swf.csv"))
pup <- fread(file.path(dir_data, "data_pupils.csv"))
est <- fread(file.path(dir_data, "data_establishments_search.csv"), na.strings = "")
#look <- fread(file.path(dir_misc, "lookup_postcodearea_region.csv"))

id_cols <- c("urn", "time_period")

# process data establishments #

# exclude closed schools
# est <- est[
#   establishmentstatus_name != "Closed" # exclude closed schools
# ]

# get spc data from treated school
id_treated <- 3344650

est_treated <- est %>% 
  filter(laestab == id_treated) %>%
  mutate(status = "treated") %>% 
  as.data.frame()

# get donor pool data
est_cont <- est %>%
  filter(laestab != id_treated,
         phaseofeducation_name %in% unique(c(est_treated$phaseofeducation_name)),
         gor_name %in% unique(c(est_treated$gor_name)),
         urbanicity %in% c(unique(est_treated$urbanicity)), # coded as urban vs rural
         #idaci_decile %in% unique(c(est_treated$idaci_decile)), 
         religiouscharacter_christian %in% unique(c(est_treated$religiouscharacter_christian))) %>%
  mutate(status = "untreated") %>% 
  as.data.frame()

# save urn and laebstab
list_laestab <- c(unique(est_cont[, "laestab"]), unique(est_treated[, "laestab"]))
list_urn <- c(unique(est_cont[, "urn"]), unique(est_treated[, "urn"]))

# filter outcome variable #

l <- swf[laestab %in% list_laestab]
u <- swf[urn %in% list_urn]



# check filter variables at school level #

pup <- pup[urn %in% urn_list]

# create dt with columns on which data will be filtered on
filter <- pup[, .(urn, time_period, laestab, school, school_postcode, phase_of_education, phase_type_grouping, type_of_establishment, urban_rural, idaci_decile, denomination, sex_of_school_description)]

# make all empty cells NAs
filter[, (names(filter)) := lapply(.SD, function(x) {
  if (is.character(x)) {
    na_if(x, "")
  } else {
    x
  }
})]
# replace all "0" with NAs
filter[, (names(filter)) := lapply(.SD, function(x) {
  if (is.character(x)) {
    na_if(x, "0")
  } else {
    x
  }
})]

# check for NA
apply(filter, 2, function(x){sum(is.na(x))})

filter <- as.data.frame(filter)
str(filter)

problematic_column <- filter %>%
  as.data.frame() %>%
  group_by(urn) %>%
  summarise(problematic_values = sum(is.na(laestab))) %>%
  filter(problematic_values > 0)


# last observations carried forward/backward
filter2 <- filter %>%
  group_by(urn) %>%
  # fill missing values: observations to be carried forward/backward
  mutate(
    across(c(laestab, school, school_postcode, phase_of_education, phase_type_grouping, type_of_establishment, urban_rural, idaci_decile, denomination, sex_of_school_description),
           # foreward
           ~zoo::na.locf(., na.rm = FALSE, fromLast = FALSE)),
    across(c(laestab, school, school_postcode, phase_of_education, phase_type_grouping, type_of_establishment, urban_rural, idaci_decile, denomination, sex_of_school_description),
           # backward
           ~zoo::na.locf(., na.rm = FALSE, fromLast = TRUE))) %>%  
  ungroup() %>%
  as.data.frame()


apply(filter2, 2, function(x){
  sum(is.na(x))
})


check <- filter2[filter2$urn %in% filter2$urn[is.na(filter2$school_postcode)], ]



# create postcode area variable
filter[, pcd_area := sub(" .*", "", school_postcode)]

# merge with lookup to add region variable correctly to data
filter <- merge(filter, look, by = "pcd_area", all.x = TRUE, allow.cartesian = T)
setorderv(filter, id_cols)


# phase
unique(filter[, phase_of_education])
check <- filter[phase_of_education == "" | phase_of_education == "0"]
check <- filter[phase_of_education == "0"]


# combine #
df <- merge(swf, pup, by = id_cols, all = T)

# check
tmp <- df[, c(..id_cols, "region.x", "region.y"), with = F]
tmp[, region := paste(region.x, region.y, sep = " | ")]
unique(tmp[, region])
