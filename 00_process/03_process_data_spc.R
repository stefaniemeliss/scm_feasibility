# Schools, pupils and their characteristics #

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

# determine years of interest
start <- 2010
finish <- 2023

# create scaffold to safe data
scaffold <- merge(data.frame(time_period = years_list),
                  data.frame(urn = urn_list))
id_cols <- names(scaffold)


# pupil on roll #

# 2010 -> "school_level_census.csv"
# 2011 -> "school_level_schools_pupils_2011.csv"
# 2012 -> "school_level_schools_pupils_2012.csv"
# 2012 -> "school_level_schools_pupil_2013.csv"
# 2014 -> "SFR15_2014_school_level_pupils_UD.csv"
# 2015 -> "SFR16_2015_Schools_Pupils_UD.csv"
# 2016 -> "SFR20_2016_Schools_Pupils_UD.csv"
# 2017 -> "SFR28_2017_Schools_Pupils_UD.csv"
# 2018 -> "Schools_Pupils_and_their_Characteristics_2018_Schools_Pupils_UD.csv"
# 2019-20 -> "spc_school_level_underlying_data.csv"
# 2020-21 -> "spc_school_level_underlying_data220216.csv"
# 2021-22 -> "spc_school_level_underlying_data20230302.csv"
# 2022-23 -> "spc_school_level_underlying_data23112023.csv"
# 2023-24 -> "spc_school_level_underlying_data.csv"

files_pupils <- list.files(path = dir_in,
                           pattern = "school_level|School_level_school|Schools_Pupils_UD",
                           recursive = T,
                           full.names = T)
files_pupils <- files_pupils[!grepl("Meta|meta|ncyear|class", files_pupils)]
files_pupils

# class size #

# 2023-24 -> "spc_school_level_class_size_underlying_data.csv"
# 2022-23 -> "spc_school_level_class_size_underlying_data.csv"
# 2021-22 -> "spc_school_level_class_size_underlying_data.csv"
# 2020-21 -> "spc_school_level_class_size_underlying_data_220216.csv"
# 2019-20 -> "spc_class_size_school_level.csv"
# 2018 -> "Schools_Pupils_and_their_Characteristics_2018 Schools_Classes_UD.csv"
# 2017 -> "SFR28_2017_Schools_Classes_UD.csv"
# 2016 -> "SFR20_2016_Schools_Classes_UD.csv"
# 2015 -> "SFR16_2015_Schools_Classes_UD.csv"
# 2014 -> "SFR15_2014_school_level_classes_UD.csv"
# 2013 -> "School_level_classes_2013.csv"
# 2012 -> "School_level_classes_2012.csv"
# 2011 -> "School_level_Classes_2011.csv"

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
  
  if (i > 1) {# no class size data available for the first year
    # read in class size data
    tmp_c <- read.csv(file = files_classes[i-1])
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
  }
  
  # combine across years
  if (year == start) {
    spc <- tmp_p
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
cols_to_merge <- c("headcount.of.pupils..rounded.", "headcount.of.pupils..unrounded.", "headcount.of.pupils")
new_col <- "hc_pup"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = scaffold)

# fte pupils
# Part-time pupils divided by 2 + full-time pupils
cols_to_merge <- c("fte.pupils..rounded.", "fte.pupils..unrounded.", "fte.pupils")
new_col <- "fte_pup"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# number of pupils of compulsory school age and above
# Pupils aged 5 and above

cols_to_merge <- c("number.of.pupils.of.compulsory.school.age.and.above..rounded.", 
                   "number.of.pupils.of.compulsory.school.age.and.above")
new_col <- "numcompage"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# number of pupils known to be eligible for free school meals
new_col <- "numfsm_e"
spc[, new_col] <- spc$number.of.pupils.known.to.be.eligible.for.free.school.meals
df <- merge(df, spc[, c(id_cols, new_col)], by = id_cols, all = T)

# % of pupils known to be eligible for free school meals	
# Number of pupils know to be eligible for FSM expressed as a percentage of the total number of pupils
new_col_p <- "pnumfsm_e"
# spc[, new_col] <- spc$perc.of.pupils.known.to.be.eligible.for.free.school.meals
# df <- merge(df, spc[, c(id_cols, new_col)], by = id_cols)
df[, new_col_p] <- df[, new_col] / df$hc_pup * 100

# number of pupils taking a free school meal on census day	
cols_to_merge <- c("number.of.pupils.taking.free.school.meals", 
                   "number.of.pupils.taking.a.free.school.meal.on.census.day",
                   "number.of.fsm.eligible.pupils.taking.a.free.school.meal.on.census.day")
new_col <- "numfsm_t"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# percentage of pupils taking a free school meal on census day	
new_col_p <- "pnumfsm_t"
df[, new_col_p] <- df[, new_col] / df$hc_pup * 100

# cols_to_merge <- c("perc.of.pupils.taking.free.school.meals", 
#                    "perc.of.fsm.eligible.pupils.taking.free.school.meals")
# new_col <- "pnumfsm_t"
# df <- merge_timelines_across_columns(data_in = spc, 
#                                      identifier_columns = id_cols, 
#                                      column_vector = cols_to_merge,
#                                      stem = new_col,
#                                      data_out = df)


# Number of pupils (used for FSM calculation in Performance Tables)	
cols_to_merge <- c("number.of.pupils..used.for.fsm.calculation.in.aats.", 
                   "number.of.pupils..used.for.fsm.calculation.in.performance.tables.")
new_col <- "numpup_spt"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# number of pupils known to be eligible for free school meals (School Performance Tables)	
cols_to_merge <- c("number.of.pupils.known.to.be.eligible.for.free.school.meals..aats.", 
                   "number.of.pupils.known.to.be.eligible.for.free.school.meals..performance.tables.")
new_col <- "numfsm_e_spt"

df <- merge_timelines_across_columns(data_in = spc, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)



# percentage of pupils known to be eligible for free school meals (School Performance Tables)	
# Number of pupils know to be eligible for FSM (School Performance Tables) expressed as a percentage of the total number of pupils (used for FSM calculation in Performance Tables)
new_col_p <- "pnumfsm_e_spt"
df[, new_col_p] <- df[, new_col] / df[, "numpup_spt"] * 100

# cols_to_merge <- c("perc.of.pupils.known.to.be.eligible.for.free.school.meals..aats.", 
#                    "perc.of.pupils.known.to.be.eligible.for.free.school.meals..performance.tables.")
# new_col <- "pnumfsm_e_spt"
# 
# df <- merge_timelines_across_columns(data_in = spc, 
#                                      identifier_columns = id_cols, 
#                                      column_vector = cols_to_merge,
#                                      stem = new_col,
#                                      data_out = df)

# number of pupils whose first language is known or believed to be other than English	
new_col <- "numeal"
spc[, new_col] <- spc$number.of.pupils.whose.first.language.is.known.or.believed.to.be.other.than.english
df <- merge(df, spc[, c(id_cols, new_col)], by = id_cols, all = T)

# % of pupils whose first language is known or believed to be other than English
# First language category expressed as a percentage of the total number of pupils of compulsory school age and above
new_col_p <- "pnumeal"
df[, new_col_p] <- df[, new_col] / df[, "numcompage"] * 100

# new_col <- "pnumeal"
# spc[, new_col] <- spc$perc.of.pupils.whose.first.language.is.known.or.believed.to.be.other.than.english
# df <- merge(df, spc[, c(id_cols, new_col)], by = id_cols)

# ethnic origin #
# Number of pupils by ethnic group	Includes pupils of compulsory school age and above only

# white British ethnic origin
new_col <- "numeowb" 
spc[, new_col] <- spc$number.of.pupils.classified.as.white.british.ethnic.origin
df <- merge(df, spc[, c(id_cols, new_col)], by = id_cols, all = T)

new_col_p <- "pnumeowb" 
df[, new_col_p] <- df[, new_col] / df[, "numcompage"] * 100

# Black ethnic origin
new_col <- "numeobl" 
tmp <- spc[, grepl("urn|time_period|as.caribbean|as.african|other.black", names(spc))]
tmp[, new_col] <- rowSums(tmp[, grepl("num", names(tmp))], na.rm = T)
df <- merge(df, tmp[, c(id_cols, new_col)], by = id_cols, all = T)

new_col_p <- "pnumeobl" 
df[, new_col_p] <- df[, new_col] / df[, "numcompage"] * 100

# Asian ethnic origin
new_col <- "numeoas" 
tmp <- spc[, grepl("urn|time_period|indian|paki|bangl|chin|other.asian", names(spc))]
tmp[, new_col] <- rowSums(tmp[, grepl("num", names(tmp))], na.rm = T)
df <- merge(df, tmp[, c(id_cols, new_col)], by = id_cols, all = T)

new_col_p <- "pnumeoas" 
df[, new_col_p] <- df[, new_col] / df[, "numcompage"] * 100

# total number of classes taught by one teacher
# one teacher classes as taught during a single selected period in each school on the day of the census
# replace all zeros with NAs
spc[, grepl("classes.taught", names(spc))] <- 
  apply(spc[, grepl("classes.taught", names(spc))], 2, function(x) {ifelse(x == 0, NA, x)}) %>% 
  as.data.frame()

cols_to_merge <- c("total.number.of.classes.taught.by.one.teacher",
                   "total.number.of.primary.classes.taught.by.one.teacher",
                   "total.number.of.secondary.classes.taught.by.one.teacher")
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
                   "total.number.of.pupils.in.secondary.classes.taught.by.one.teacher")
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
                   "average.size.of.one.teacher.secondary.classes")
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
                      "perc of pupils classified as Black ethnic origin",
                      "number of pupils classified as Asian ethnic origin",
                      "perc of pupils classified as Asian ethnic origin",
                      "number of classes taught by one teacher",
                      "number of pupils in classes taught by one teacher",
                      "average class size")
# save file
write.csv(dict, file = file.path(dir_misc, "meta_spc.csv"), row.names = F)
