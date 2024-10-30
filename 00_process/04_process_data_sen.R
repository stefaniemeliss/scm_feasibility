# Special educational needs in England #

# This publication contains information about pupils with special educational needs. 
# This information is derived from school census returns, general hospital school census 
# and school level annual school census (SALSC, independent schools) returns made to the department in January each year. 
# The school census contains pupil level data covering a wide range of information on the characteristics of schools and the pupils. 

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
dir_in <- file.path(dir_data, "special-educational-needs-in-england")

# derive URNs
school_list <- read.csv(file = file.path(dir_misc, "schools_list.csv"))
urn_list <- school_list$urn

# determine year list (akin to other data sources)
years_list <- paste0(20, 10:23, 11:24)

# create scaffold to safe data
scaffold <- merge(data.frame(time_period = years_list),
                  data.frame(urn = urn_list))
id_cols <- names(scaffold)

# rename folders that currently only have one year included (data collected in Jan)
rename_folders <- F
if (rename_folders) {
  # save df with old and new folder names
  start <- 2010
  finish <- 2019
  tmp <- data.frame(old = c(start:finish))
  tmp$new <- paste0(paste0(tmp$old-1,"-", gsub(20, "",tmp$old)))
  # add dirs
  tmp$from <- file.path(dir_in, tmp$old)
  tmp$to <- file.path(dir_in, tmp$new)
  # rename
  file.rename(from = c(tmp$from), to = c(tmp$to))
}

# determine years of interest
start <- 2010 # no school level data for 2009-10
finish <- 2023

files <- list.files(path = dir_in,
                    pattern = "UD|ud|nderlying",
                    recursive = T,
                    full.names = T)
files <- files[!grepl("meta|Meta", files)]
files

#### combine data ####

# loop over years #
for (i in seq_along(start:finish)) {
  
  year = c(start:finish)[i]
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  
  # read in data
  tmp <- read.csv(file = files[i])
  names(tmp) <- gsub("X...", "", names(tmp), fixed = T)
  
  # subset data to only include relevant schools
  names(tmp) <- tolower(names(tmp))
  tmp <- tmp %>% filter(urn %in% urn_list)
  
  # filter to remove columns
  tmp <- tmp[, !grepl("primary|prov", names(tmp))]
  
  # Figures are suppressed (“supp”) where they concern fewer than 10 pupils.
  tmp <- apply(tmp, 2, function(x) {ifelse(x == "x" | x == "z" | x == "." | x == "..", NA, as.numeric(x))}) %>%
    as.data.frame()
  
  # remove columns where all rows contain NA values
  tmp <- tmp[,colSums(is.na(tmp))<nrow(tmp)]
  
  # add year
  tmp$time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # combine across years
  if (year == start) {
    sen <- tmp
  } else {
    sen <- rbind.all.columns(sen, tmp)
  }
  
}


#### extract relevant data ####

# number of pupils
cols_to_merge <- c("pupils", "total.pupils")
new_col <- "npuptot__sen"

df <- merge_timelines_across_columns(data_in = sen, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = scaffold)

# School Action (SEN Code of Practice 2001)
# This stage involved the school providing additional or different support to help the child progress. 
# SEN Code of Practice 2014 replaced the terms "School Action" and "School Action Plus" with "SEN support."
cols_to_merge <- c("schoolaction", "school_action")
new_col <- "npupsena" # pupils on roll with SEN on School Action

df <- merge_timelines_across_columns(data_in = sen, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# School Action Plus (SEN Code of Practice 2001)
# This stage involved external specialists providing additional advice and support to the school to help meet the child's needs.
# SEN Code of Practice 2014 replaced the terms "School Action" and "School Action Plus" with "SEN support."
cols_to_merge <- c("schoolactionplus", "school_action_plus")
new_col <- "npupsenap" # pupils on roll with school  action plus

df <- merge_timelines_across_columns(data_in = sen, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# SEN support = registered pupils with SEN without a statement or EHC plan
# combines "SEN support," "School Action," and "School Action Plus"
# encompasses the various levels and types of support provided to children with special educational needs
# in SPT: TSENEL*K* stands for "Total number of pupils with Special Educational Needs (SEN) who receive SEN support." 
# K = SEN support
# This metric indicates the total count of pupils within a school who have been identified as having special educational needs 
# and are receiving additional support, but do not have an Education, Health and Care (EHC) plan or a Statement of SEN.

# combine School Action and School Action Plus (SEN Code of Practice 2001)
df[, "npupsenelk2001"] <- df$npupsena + df$npupsenap

# SEN support (SEN Code of Practice 2014)
# "SEN support" is the current system used in schools to help children with special educational needs 
# who do not have an Education, Health and Care (EHC) plan. 
new_col <- "npupsenelk2014"
sen[, new_col] <- sen$sen.support
df <- merge(df, sen[, c(id_cols, new_col)], by = id_cols, all = T)


cols_to_merge <- c("npupsenelk2001", "npupsenelk2014")
new_col <- "npupsenelk" # eligible pupils on roll with SEN support

df <- merge_timelines_across_columns(data_in = df, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# statements of SEN (SEN Code of Practice 2001)
# A Statement of Special Educational Needs was the previous system used in England to outline the educational needs 
# and the provision required for children with significant special educational needs.
cols_to_merge <- c("statements", "statement")
new_col <- "npupsenst" # pupils on roll with SEN statement

df <- merge_timelines_across_columns(data_in = sen, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)


# EHC plan (SEN Code of Practice 2014)
# An Education, Health and Care plan is a legal document that describes a child or young person's special educational, health, and social care needs. 
# It also specifies the support they need and the outcomes they are working towards.
# EHC plans were introduced under the Children and Families Act 2014, replacing the Statements of SEN.
# statements or EHC plan (transfer of statements to an EHC plan is due to take place by April 2018)

cols_to_merge <- c("statement..ehc.plan", "statement...ehc.plan", "ehc.plan")
new_col <- "npupsenehcst" # pupils on roll with EHC plan or statement

df <- merge_timelines_across_columns(data_in = sen, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# "SEND plan" (Special Educational Needs and Disabilities plan) = statements or EHC plan combined
# in SPT: TSENEL*SE* = Number of SEN pupils with a statement or EHC plan
# S = Statement
# E = Education, Health and Care (EHC) plan
# This metric indicates the total count of pupils within a school who have significant special educational needs 
# that require a formal plan or statement to ensure they receive the appropriate support and resources.
cols_to_merge <- c("npupsenst", "npupsenehcst")
new_col <- "npupsenelse" # pupils on roll with SEN statement or EHC plan

df <- merge_timelines_across_columns(data_in = df, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# pupil SEN status: Plan or intervention
# SEN provision - EHC plan/Statement of SEN or SEN support/School Action/School Action plus
df[, "npupsen"] <- df$npupsenelse + df$npupsenelk


#### save data ####
df <- df[with(df, order(urn, time_period)),]
write.csv(df, file = file.path(dir_data, "data_sen.csv"), row.names = F)

#### create var dict ####
dict <- data.frame(variable = names(df)[!grepl("_tag", names(df))])
dict$explanation <- c("academic year",
                      "unique reference number",
                      "total number of pupils",
                      "number of pupils with SEN on School Action",
                      "number of pupils with SEN on School Action Plus",
                      "number of pupils with SEN on School Action or School Action Plus (SEN Code of Practice 2001)",
                      "number of pupils with SEN support (SEN Code of Practice 2014)",
                      "number of pupils with SEN support/School Action/School Action plus (all years)",
                      "number of pupils with SEN statememnt (SEN Code of Practice 2001)",
                      "number of pupils with EHC plan or Statement of SEN (SEN Code of Practice 2014)",
                      "number of pupils with EHC plan/Statement of SEN (all years)",
                      "number of pupils with SEN provision (EHC plan/Statement of SEN or SEN support/School Action/School Action plus)"
                      )
# save file
write.csv(dict, file = file.path(dir_misc, "meta_sen.csv"), row.names = F)
