# Schools, pupils and their characteristics #

# This release contains the latest statistics on school and pupil numbers and their characteristics, 
# including age, gender, free school meals (FSM) eligibility, English as an additional language, ethnicity, school characteristics, class sizes.
# The publication combines information from the school census, school level annual school census, 
# general hospital school census and alternative provision census.

# The most recently published data is from the school census which took place on 18th January 2024 (Spring census)

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
dir_in <- file.path(dir_data, "school-pupils-and-their-characteristics")

# derive URNs
school_list <- read.csv(file = file.path(dir_misc, "schools_list.csv"))
urn_list <- school_list$urn

# determine year list (akin to other data sources)
years_list <- paste0(20, 10:23, 11:24)

# rename folders that currently only have one year included (data collected in Jan)
rename_folders <- F
if (rename_folders) {
  # save df with old and new folder names
  start <- 2010
  finish <- 2019
  # data collection takes place in Jan of the year
  # e.g., data from 2014 is from 2013/14
  tmp <- data.frame(old = c(start:finish)) 
  tmp$new <- paste0(paste0(tmp$old-1,"-", gsub(20, "",tmp$old)))
  # add dirs
  tmp$from <- file.path(dir_in, tmp$old)
  tmp$to <- file.path(dir_in, tmp$new)
  # rename
  file.rename(from = c(tmp$from), to = c(tmp$to))
}


# determine years of interest
start <- 2010
finish <- 2023

# create scaffold to safe data
scaffold <- merge(data.frame(time_period = years_list),
                  data.frame(urn = urn_list))
id_cols <- names(scaffold)


# pupil on roll #

files_pupils <- list.files(path = dir_in,
                           pattern = "school_level|School_level_school|Schools_Pupils_UD|pupil_characteristics_UD",
                           recursive = T,
                           full.names = T)
files_pupils <- files_pupils[!grepl("Meta|meta|ncyear|class|census|TEMPLATE", files_pupils)]
files_pupils

# class size #

files_classes <- list.files(path = dir_in,
                            pattern = "class|Class",
                            recursive = T,
                            full.names = T)
files_classes <- files_classes[!grepl(".pdf|.xls|/data/class|/data/spc", files_classes)]
files_classes

#### combine data ####

# loop over years #
for (i in seq_along(start:finish)) {
  
  year = c(start:finish)[i]
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  
  # pupils on roll #
  
  # read in pupil on roll data
  tmp_p <- read.csv(file = files_pupils[i])
  names(tmp_p) <- gsub("X...", "", names(tmp_p), fixed = T)
  names(tmp_p) <- gsub("X..", "perc.", names(tmp_p), fixed = T)
  
  # subset data to only include relevant schools
  names(tmp_p) <- tolower(names(tmp_p))
  tmp_p <- tmp_p %>% filter(urn %in% urn_list)
  
  # filter to remove columns
  tmp_p <- tmp_p[, !grepl(".time.|unclassified|key.stage|early.years|nursery|reception", names(tmp_p))]
  
  # replace spaces and %
  tmp_p <- apply(tmp_p, 2, function(x) {ifelse(grepl(" |%", x), gsub(" |%", "", x), x)}) %>% 
    as.data.frame()
  
  # Figures are suppressed (“supp”) where they concern fewer than 10 pupils.
  tmp_p <- apply(tmp_p, 2, function(x) {ifelse(x == "x" | x == ">" | x == "" | x == " ", NA, as.numeric(x))}) %>%
    as.data.frame()
  
  # remove columns where all rows contain NA values
  tmp_p <- tmp_p[,colSums(is.na(tmp_p))<nrow(tmp_p)]
  
  # add year
  tmp_p$time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # class size #
  
  
  # read in class size data
  tmp_c <- read.csv(file = files_classes[i])
  names(tmp_c) <- gsub("X...", "", names(tmp_c), fixed = T)
  names(tmp_c) <- gsub("_", ".", names(tmp_c), fixed = T) # try to make column years less variable across years
  
  # subset data to only include relevant schools
  names(tmp_c) <- tolower(names(tmp_c))
  tmp_c <- tmp_c %>% filter(urn %in% urn_list)
  
  # filter to remove columns
  tmp_c <- tmp_c[, !grepl("key.stage|classes.of.size|exc|lawful|large", names(tmp_c))]
  
  # replace spaces and %
  tmp_c <- apply(tmp_c, 2, function(x) {ifelse(grepl(" |%", x), gsub(" |%", "", x), x)}) %>% 
    as.data.frame()
  
  # Figures are suppressed (“supp”) where they concern fewer than 10 pupils.
  tmp_c <- apply(tmp_c, 2, function(x) {ifelse(x == "x" | x == ">" | x == "" | x == " ", NA, as.numeric(x))}) %>%
    as.data.frame()
  
  # remove columns where all rows contain NA values
  tmp_c <- tmp_c[,colSums(is.na(tmp_c))<nrow(tmp_c)]
  
  # add year
  tmp_c$time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # combine with data on pupils on roll
  tmp <- merge(tmp_p, tmp_c, by = id_cols)
  
  
  # combine across years
  if (year == start) {
    spc <- tmp
    # pupils <- tmp_p
    # classes <- tmp_c
  } else {
    spc <- rbind.all.columns(spc, tmp)
    # pupils <- rbind.all.columns(pupils, tmp_p)
    # classes <- rbind.all.columns(classes, tmp_c)
  }
  
}

#### extract relevant data ####

# headcount
# Headcount of pupils	= Full-time + part-time pupils
cols_to_merge <- c("headcount.of.pupils..unrounded.", "headcount.of.pupils")
new_col <- "npuptot__spc"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = scaffold)

# fte pupils
# Part-time pupils divided by 2 + full-time pupils
cols_to_merge <- c("fte.pupils..unrounded.", "fte.pupils")
new_col <- "fte_pup__spc"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# number of pupils of compulsory school age and above
# Pupils aged 5 and above

cols_to_merge <- c("number.of.pupils.of.compulsory.school.age.and.above..rounded.", 
                   "number.of.pupils.of.compulsory.school.age.and.above")
new_col <- "npupcaa__spc"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# number of pupils known to be eligible for free school meals
cols_to_merge <- c("number.of.pupils.known.to.be.eligible.for.free.school.meals", 
                   "number.of.pupils.known.to.be.eligible.for.and.claiming.free.school.meals")
new_col <- "npupfsm_e"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# % of pupils known to be eligible for free school meals	
# Number of pupils know to be eligible for FSM expressed as a percentage of the total number of pupils

cols_to_merge <- c("perc.of.pupils.known.to.be.eligible.for.free.school.meals", 
                   "perc.of.pupils.known.to.be.eligible.for.and.claiming.free.school.meals")
new_col <- "pnpupfsm_e"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)


# new_col_p <- "pnpupfsm_e"
# df[, new_col_p] <- df[, new_col] / df$npuptot__spc * 100

# number of pupils taking a free school meal on census day	
cols_to_merge <- c("number.of.pupils.taking.free.school.meals", 
                   "number.of.pupils.taking.a.free.school.meal.on.census.day",
                   "number.of.fsm.eligible.pupils.taking.a.free.school.meal.on.census.day")
new_col <- "npupfsm_t"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# percentage of pupils taking a free school meal on census day	
# new_col_p <- "pnpupfsm_t"
#df[, new_col_p] <- df[, new_col] / df$npuptot__spc * 100

cols_to_merge <- c("perc.of.pupils.taking.free.school.meals",
                   "perc.of.fsm.eligible.pupils.taking.free.school.meals",
                   "perc.of.pupils.taking.free.school.meals.on.census.day")
new_col <- "pnpupfsm_t"
df <- merge_timelines_across_columns(data_in = spc,
                                     identifier_columns = id_cols,
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)


# Number of pupils (used for FSM calculation in Performance Tables)	
new_col <- "npup_calcspt"
spc[, new_col] <- spc$number.of.pupils..used.for.fsm.calculation.in.performance.tables.
df <- merge(df, spc[, c(id_cols, new_col)], by = id_cols, all = T)

# number of pupils known to be eligible for free school meals (School Performance Tables)	
new_col <- "npupfsm_e_spt"
spc[, new_col] <- spc$number.of.pupils.known.to.be.eligible.for.free.school.meals..performance.tables.
df <- merge(df, spc[, c(id_cols, new_col)], by = id_cols, all = T)

# percentage of pupils known to be eligible for free school meals (School Performance Tables)	
# Number of pupils know to be eligible for FSM (School Performance Tables) expressed as a percentage of the total number of pupils (used for FSM calculation in Performance Tables)
new_col <- "pnpupfsm_e_spt"
spc[, new_col] <- spc$perc.of.pupils.known.to.be.eligible.for.free.school.meals..performance.tables.
df <- merge(df, spc[, c(id_cols, new_col)], by = id_cols, all = T)

# number of pupils whose first language is known or believed to be other than English	
new_col <- "npupeal"
spc[, new_col] <- spc$number.of.pupils.whose.first.language.is.known.or.believed.to.be.other.than.english
df <- merge(df, spc[, c(id_cols, new_col)], by = id_cols, all = T)

# % of pupils whose first language is known or believed to be other than English
# First language category expressed as a percentage of the total number of pupils of compulsory school age and above
# new_col_p <- "pnpupeal"
# df[, new_col_p] <- df[, new_col] / df[, "npupcaa__spc"] * 100

new_col <- "pnpupeal"
spc[, new_col] <- spc$perc.of.pupils.whose.first.language.is.known.or.believed.to.be.other.than.english
df <- merge(df, spc[, c(id_cols, new_col)], by = id_cols)

# ethnic origin #
# Number of pupils by ethnic group	Includes pupils of compulsory school age and above only

# white British ethnic origin
new_col <- "numeowb" 
spc[, new_col] <- spc$number.of.pupils.classified.as.white.british.ethnic.origin
df <- merge(df, spc[, c(id_cols, new_col)], by = id_cols, all = T)

new_col <- "pnumeowb" 
spc[, new_col] <- spc$perc.of.pupils.classified.as.white.british.ethnic.origin
df <- merge(df, spc[, c(id_cols, new_col)], by = id_cols, all = T)

# new_col_p <- "pnumeowb" 
# df[, new_col_p] <- df[, new_col] / df[, "npupcaa__spc"] * 100

# Black ethnic origin
new_col <- "numeobl" 
tmp <- spc[, grepl("urn|time_period|as.caribbean|as.african|other.black", names(spc))]
tmp[, new_col] <- rowSums(tmp[, grepl("num", names(tmp))], na.rm = T)
df <- merge(df, tmp[, c(id_cols, new_col)], by = id_cols, all = T)

# new_col_p <- "pnumeobl" 
# df[, new_col_p] <- df[, new_col] / df[, "npupcaa__spc"] * 100

# Asian ethnic origin
new_col <- "numeoas" 
tmp <- spc[, grepl("urn|time_period|indian|paki|bangl|chin|other.asian", names(spc))]
tmp[, new_col] <- rowSums(tmp[, grepl("num", names(tmp))], na.rm = T)
df <- merge(df, tmp[, c(id_cols, new_col)], by = id_cols, all = T)

# new_col_p <- "pnumeoas" 
# df[, new_col_p] <- df[, new_col] / df[, "npupcaa__spc"] * 100

# total number of classes taught by one teacher
# one teacher classes as taught during a single selected period in each school on the day of the census
# replace all zeros with NAs
spc[, grepl("classes.taught", names(spc))] <- 
  apply(spc[, grepl("classes.taught", names(spc))], 2, function(x) {ifelse(x == 0, NA, x)}) %>% 
  as.data.frame()

cols_to_merge <- c("total.number.of.classes.taught.by.one.teacher",
                   "total.number.of.primary.classes.taught.by.one.teacher",
                   "total.number.of.classes.in.primary.schools.taught.by.one.teacher",
                   "total.number.of.secondary.classes.taught.by.one.teacher",
                   "total.number.of.classes.in.secondary.schools.taught.by.one.teacher")
new_col <- "nclt1t"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# total number of pupils in classes taught by one teacher
# already replaced all zeros with NAs with call above
cols_to_merge <- c("total.number.of.pupils.in.classes.taught.by.one.teacher",
                   "total.number.of.pupils.in.primary.classes.taught.by.one.teacher",
                   "total.number.of.pupils.in.classes.in.primary.schools.taught.by.one.teacher",
                   "total.number.of.pupils.in.secondary.classes.taught.by.one.teacher",
                   "total.number.of.pupils.in.classes.in.secondary.schools.taught.by.one.teacher")
new_col <- "npupclt1t"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# average size of one teacher classes = Number of pupils divided by number of classes
# replace all zeros with NAs
spc[, grepl("average", names(spc))] <- apply(spc[, grepl("average", names(spc))], 2, function(x) {ifelse(x == 0, NA, x)}) %>% 
  as.data.frame()

cols_to_merge <- c("average.size.of.one.teacher.classes", 
                   "average.size.of.one.teacher.primary.classes",
                   "average.size.of.one.teacher.classes.in.primary.schools",
                   "average.size.of.one.teacher.secondary.classes",
                   "average.size.of.one.teacher.classes.in.secondary.schools")
new_col <- "avgclsize"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

#### save data ####
df <- df[with(df, order(urn, time_period)),]
write.csv(df, file = file.path(dir_data, "data_spc.csv"), row.names = F)

#### create var dict ####
dict <- data.frame(variable = names(df)[!grepl("_tag", names(df))])
dict$explanation <- c("academic year",
                      "unique reference number",
                      "headcount pupils",
                      "FTE pupils",
                      "number of pupils of compulsary age and above",
                      "number of pupils eligible for FSM",
                      "perc of pupils eligible for FSM",
                      "number of pupils taking FSM",
                      "perc of pupils taking FSM",
                      "number of pupils (SPT)",
                      "number of pupils eligible for FSM (SPT)",
                      "perc of pupils eligible for FSM (SPT)",
                      "number of EAL pupils",
                      "perc of EAL pupils",
                      "number of pupils classified as white British ethnic origin",
                      "perc of pupils classified as white British ethnic origin",
                      "number of pupils classified as Black ethnic origin",
                      #"perc of pupils classified as Black ethnic origin",
                      "number of pupils classified as Asian ethnic origin",
                      #"perc of pupils classified as Asian ethnic origin",
                      "number of classes taught by one teacher",
                      "number of pupils in classes taught by one teacher",
                      "average class size")
# save file
write.csv(dict, file = file.path(dir_misc, "meta_spc.csv"), row.names = F)
