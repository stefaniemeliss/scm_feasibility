### combine all pupil characteristics ####

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

# derive URNs
school_list <- read.csv(file = file.path(dir_misc, "schools_list.csv"))
urn_list <- school_list$urn

# determine year list (akin to other data sources)
years_list <- paste0(20, 10:23, 11:24)

# create scaffold to safe data
scaffold <- merge(data.frame(time_period = as.numeric(years_list)),
                  data.frame(urn = urn_list))
id_cols <- names(scaffold)

#### data on school capacity ####

# data reported by local authorities in England, in the annual School Capacity (SCAP) survey, as of 1 May 2023

# 1. Includes mainstream state schools with capacity in any of the year groups from reception to year 11 on 1 May for the relevant academic year.
# 2. Primary places include all reported capacity in primary and middle deemed primary schools. Capacity excludes nursery places.
# 3. Secondary places include all reported capacity in secondary, middle-deemed secondary and all-through schools. Capacity includes sixth form places.
# 4. Number of pupils on roll for reception year group and above. Taken from the May school census for the relevant academic year, or gathered during the school capacity collection if census data was not available for a school.
# 5. 2019/20 data not available due to the cancellation of the 2020 School Capacity survey due to COVID-19.
# 6. Number of pupils in places that exceed their school's capacity is the difference between school places and number of pupils on roll, for schools where the number of pupils on roll is higher than the school’s reported capacity. Calculated at school level and then summed to national, regional or local authority level.

cap <- read.csv(file.path(dir_data, "school-capacity", "2022-23", "data", "school-capacity_200910-202223.csv"))
names(cap) <- tolower(gsub("X...", "", names(cap), fixed = T))

# change names
names(cap)[names(cap) == "school_urn"] <- "urn"

# subset data to only include relevant schools
cap <- cap %>% filter(urn %in% urn_list)
cap <- cap %>% filter(time_period != 200910)

# remove columns
cap <- cap[, grepl("time_period|urn|pupils|capacity", names(cap))]
cap <- cap[, !grepl("percent", names(cap))]

# Figures are suppressed (“supp”) where they concern fewer than 10 pupils.
cap <- apply(cap, 2, function(x) {ifelse(x == "z" | x == "x", NA, as.numeric(x))}) %>%
  as.data.frame()

# rename
names(cap)[c(-1, -2)] <- c("npuptot__cap", "cap_prim", "cap_sec", "npupovcap")
# "npuptot__cap" = Number of pupils on roll

# percentage of pupils over capacity
cap$pnpupovcap <- cap$npupovcap / cap$npuptot__cap

# combine with scaffold
cap <- merge(scaffold, cap, by = id_cols, all = T)

#### read in previously created files ####

spc <- read.csv(file.path(dir_data, "data_spc.csv")) # census data collected in January of academic year - Spring census
sen <- read.csv(file.path(dir_data, "data_sen.csv"))
spt <- read.csv(file.path(dir_data, "data_spt_census.csv")) 

df <- merge(spc, sen, by = id_cols, all = T)
df <- merge(df, spt, by = id_cols, all = T)
df <- merge(df, cap, by = id_cols, all = T)

#### process combined data ####

# get total number of pupils #

# possible variables to use
tmp <- df[, c(id_cols, "npuptot__spc", "npuptot__sen", "npuptot__cap", "npuptot__sptcensus")]

# rounding applied to nearest 5 in total pupil headcount data collected in 201011 / 201112 / 201213 in SPC
# there is a difference between npuptot__spc & npuptot__sen only in the years 2013/14 - 2016/17
#   data in  npuptot__sen is rounded to nearest 5, data in npuptot__spc is not rounded
#   NOTE: data on total number of pupils is the same for SPC and SEN tables once SEN data is rounded to the nearest 5 for the years 2013/14 - 2016/17 (!)
# there is a difference between npuptot__spc & npuptot__sptcensus only in the years 2010/11 - 2012/13
#   data in  npuptot__spc is rounded to nearest 5, data in npuptot__sptcensus is not rounded
#   NOTE: data on total number of pupils is the same for SPC and SPT tables once SPT data is rounded to the nearest 5 for the years 2010/11 - 2012/13 (!)

col_tot <- "npuptot"

tmp <- fix_roundings(var_rd = "npuptot__spc", var_nrd = "npuptot__sptcensus",
                     new_var = col_tot,
                     identifier_columns = id_cols,
                     col_to_filter = "time_period",
                     filter = c(201011, 201112, 201213),
                     rounding_factor = 5,
                     data_in = df)

# select rows
df <- merge(df, tmp[, c(id_cols, col_tot)], by = id_cols, all = T)


# select columns
tmp <- df[, c(id_cols, col_tot, "npuptot__sen", "npuptot__cap", "npuptot__sptcensus")]

# check NAs
apply(tmp, 2, FUN = function(x){sum(is.na(x))})
tmpp <- tmp[is.na(tmp[, col_tot]), ] # subset variable with smallest number of NAs

# for those that have NAs in npuptot, fill with information included in capacity data
tmp[, col_tot] <- ifelse(is.na(tmp[, col_tot]), tmp$npuptot__cap, tmp[, col_tot])
apply(tmp, 2, FUN = function(x){sum(is.na(x))})

out <- tmp[, c(id_cols, col_tot)]

# with the total number of pupils on role, calculate percentage of pupils based on SPC data #

# % of pupils known to be eligible for free school meals	
# Number of pupils know to be eligible for FSM expressed as a percentage of the total number of pupils

col_n <- "npupfsm_e"
col_p <- "pnpupfsm_e"

out <- merge(out, df[, c(id_cols, col_n)], by = id_cols)
out[, col_p] <- out[, col_n] / out[, col_tot] * 100

# percentage of pupils taking a free school meal on census day	

col_n <- "npupfsm_t"
col_p <- "pnpupfsm_t"

out <- merge(out, df[, c(id_cols, col_n)], by = id_cols)
out[, col_p] <- out[, col_n] / out[, col_tot] * 100

# FSM calculation in Performance Tables
# npup_calcspt - Number of pupils (used for FSM calculation in Performance Tables)
# npupfsm_e_spt - number of pupils known to be eligible for free school meals (School Performance Tables)
# pnpupfsm_e_spt - percentage of pupils known to be eligible for free school meals (School Performance Tables)
out <- merge(out, df[, c(id_cols, "npup_calcspt", "npupfsm_e_spt", "pnpupfsm_e_spt")], by = id_cols)

# % of pupils whose first language is known or believed to be other than English
col_n <- "npupeal"
col_p <- "pnpupeal"

out <- merge(out, df[, c(id_cols, col_n)], by = id_cols)
out[, col_p] <- out[, col_n] / out[, col_tot] * 100

# % of pupils classified as white British ethnic origin

col_n <- "numeowb"
col_p <- "pnumeowb"

out <- merge(out, df[, c(id_cols, col_n)], by = id_cols)
out[, col_p] <- out[, col_n] / out[, col_tot] * 100

# % of pupils classified as Black ethnic origin

col_n <- "numeobl"
col_p <- "pnumeobl"

out <- merge(out, df[, c(id_cols, col_n)], by = id_cols)
out[, col_p] <- out[, col_n] / out[, col_tot] * 100

# % of pupils classified as Asian ethnic origin

col_n <- "numeoas"
col_p <- "pnumeoas"

out <- merge(out, df[, c(id_cols, col_n)], by = id_cols)
out[, col_p] <- out[, col_n] / out[, col_tot] * 100

# class size data
# nclt1t - total number of classes taught by one teacher
# npupclt1t - total number of pupils in classes taught by one teacher

col_n <- "npupclt1t"
col_tot <- "nclt1t"

out <- merge(out, df[, c(id_cols, col_tot, col_n)], by = id_cols)

# avgclsize - average size of one teacher classes = Number of pupils divided by number of classes
col_p <- "avgclsize"
out[, col_p] <- out[, col_n] / out[, col_tot]

# special educational needs data #

col_tot <- "npuptot"

# School Action and School Action Plus (SEN Code of Practice 2001)

col_n <- "npupsena" # pupils on roll with SEN on School Action
col_p <- "pnpupsena"

# fix roundings
# data for 2012/13 - 2013/14 reported in SEN is rounded in comparison to numbers reported in SPT
tmp <- fix_roundings(var_rd = col_n, var_nrd = "tsena",
                     identifier_columns = id_cols,
                     col_to_filter = "time_period",
                     filter = c(201213, 201314),
                     rounding_factor = 5,
                     data_in = df)

# select rows
out <- merge(out, tmp[, c(id_cols, col_n)], by = id_cols, all = T)
# compute perc
out[, col_p] <- out[, col_n] / out[, col_tot] * 100
# check <- c("tsena", "psena")
# out <- merge(out, df[, c(id_cols, check)], by = id_cols)

col_n <- "npupsenap" # pupils on roll with SEN on School Action Plus
col_p <- "pnpupsenap"

# fix roundings
# data for 2012/13 - 2013/14 reported in SEN is rounded in comparison to numbers reported in SPT
tmp <- fix_roundings(var_rd = col_n, var_nrd = "totsenap",
                     identifier_columns = id_cols,
                     col_to_filter = "time_period",
                     filter = c(201213, 201314),
                     rounding_factor = 5,
                     data_in = df)

# select rows
out <- merge(out, tmp[, c(id_cols, col_n)], by = id_cols, all = T)
# compute perc
out[, col_p] <- out[, col_n] / out[, col_tot] * 100

# SEN Statement (SEN Code of Practice 2001)

col_n <- "npupsenst" # pupils on roll with SEN statement
col_p <- "pnpupsenst"

# fix roundings
# data for 2012/13 - 2013/14 reported in SEN is rounded in comparison to numbers reported in SPT
tmp <- fix_roundings(var_rd = col_n, var_nrd = "totsenst",
                     identifier_columns = id_cols,
                     col_to_filter = "time_period",
                     filter = c(201213, 201314),
                     rounding_factor = 5,
                     data_in = df)

# select rows
out <- merge(out, tmp[, c(id_cols, col_n)], by = id_cols, all = T)
# compute perc
out[, col_p] <- out[, col_n] / out[, col_tot] * 100

# eligible pupils with SEN support (SEN Code of Practice 2014)

col_n <- "npupsenelk2014" # pupils on roll with SEN support 
col_p <- "pnpupsenelk2014"

# fix roundings
# data for 2014/15 - 2016/17 reported in SEN is rounded in comparison to numbers reported in SPT
tmp <- fix_roundings(var_rd = col_n, var_nrd = "tsenelk",
                     identifier_columns = id_cols,
                     col_to_filter = "time_period",
                     filter = c(201415, 201516, 201617),
                     rounding_factor = 5,
                     data_in = df)

# select rows
out <- merge(out, tmp[, c(id_cols, col_n)], by = id_cols, all = T)
# compute perc
out[, col_p] <- out[, col_n] / out[, col_tot] * 100
# check <- c("tsenelk", "psenelk")
# out <- merge(out, df[, c(id_cols, check)], by = id_cols)

# SEN pupils with a statement or EHC plan (SEN Code of Practice 2014)
col_n <- "npupsenehcst" # pupils on roll with SEN statement or EHC plan
col_p <- "nnpupsenehcst"

# fix roundings
# data for 2014/15 - 2016/17 reported in SEN is rounded in comparison to numbers reported in SPT
tmp <- fix_roundings(var_rd = col_n, var_nrd = "tsenelse",
                     identifier_columns = id_cols,
                     col_to_filter = "time_period",
                     filter = c(201415, 201516, 201617),
                     rounding_factor = 5,
                     data_in = df)

# select rows
out <- merge(out, tmp[, c(id_cols, col_n)], by = id_cols, all = T)
# compute perc
out[, col_p] <- out[, col_n] / out[, col_tot] * 100
# check <- c("tsenelse", "psenelse")
# out <- merge(out, df[, c(id_cols, check)], by = id_cols)

# pupils with SEN support (all years)

col_n <- "npupsenelk" # eligible pupils on roll with SEN support
col_p <- "pnpupsenelk"

# combine School Action and School Action Plus (SEN Code of Practice 2001)
out[, "npupsenelk2001"] <- out$npupsena + out$npupsenap # both were fixed above

cols_to_merge <- c("npupsenelk2001", "npupsenelk2014") # also fixed above

tmp <- out[, c(id_cols, cols_to_merge)]
tmp <- merge_timelines_across_columns(data_in = tmp, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = col_n,
                                     data_out = tmp)


out <- merge(out, tmp[, c(id_cols, col_n)], by = id_cols)
out[, col_p] <- out[, col_n] / out[, col_tot] * 100

# pupils with EHC plan/Statement of SEN (all years)

col_n <- "npupsenelse" # pupils on roll with SEN statement or EHC plan
col_p <- "pnpupsenelse"

# combine School Action and School Action Plus (SEN Code of Practice 2001)

cols_to_merge <- c("npupsenst", "npupsenehcst") # fixed above

tmp <- out[, c(id_cols, cols_to_merge)]
tmp <- merge_timelines_across_columns(data_in = tmp, 
                                      identifier_columns = id_cols, 
                                      column_vector = cols_to_merge,
                                      stem = col_n,
                                      data_out = tmp)

out <- merge(out, tmp[, c(id_cols, col_n)], by = id_cols)
out[, col_p] <- out[, col_n] / out[, col_tot] * 100

# pupils with SEN provision - EHC plan/Statement of SEN or SEN support/School Action/School Action plus

col_n <- "npupsen" # pupils on roll with SEN statement or EHC plan
col_p <- "pnpupsen"

out[, col_n] <- out$npupsenelse + out$npupsenelk # fixed above
out[, col_p] <- out[, col_n] / out[, col_tot] * 100
apply(out, 2, FUN = function(x){sum(is.na(x))})

# write file #

# copy df
data <- out[]

# deleta all columns not necessary
data[, grepl("npupfsm_t", names(data))] <- NULL
data[, grepl("spt", names(data))] <- NULL
data[, grepl("sena", names(data))] <- NULL
data[, grepl("senst", names(data))] <- NULL
data[, grepl("ehc", names(data))] <- NULL
data[, grepl("20", names(data))] <- NULL

apply(data, 2, FUN = function(x){sum(is.na(x))})

# save file
data <- data[with(data, order(urn, time_period)),]
write.csv(data, file = file.path(dir_data, "data_pupils.csv"), row.names = F)


# # code to debug!
# col_n <- "npupfsm_e"
# col_p <- "pnpupfsm_e"
# out <- merge(out, df[, c(id_cols, col_n, col_p)], by = id_cols)
# col_p <- "test"
# out[, col_p] <- out[, col_n] / out[, col_tot] * 100
# 
# psych::describe(round(out$test, 1) - out[, col_p])
