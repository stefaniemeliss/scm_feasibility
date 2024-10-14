options(scipen = 999)
# empty work space
rm(list = ls())

# define directory
dir <- getwd()
# dir <- gsub("/00_explore", "", dir)

# load libraries
library(kableExtra)
library(dplyr)

# read in school_list info about OAT schools
school_list <- read.csv(file = file.path(dir, "misc", "OAT Linked establishments.csv"))

# fix input data
names(school_list) <- tolower(names(school_list))
names(school_list)[names(school_list) == "local.authority..name."] <- "laname"
school_list$laestab <- gsub("/", "", school_list$laestab)

# schools must have been part of OAT for 4 or more years
cut_off_date <- as.Date("2021-09-01")
school_list$joined.date <- as.Date(school_list$joined.date, format =  "%d/%m/%Y")
school_list <- school_list[school_list$joined.date <= cut_off_date, ]

# derive URNs
urn_list <- school_list$urn

# available information of schools
gias <- read.csv(file = file.path(dir, "data", "performance-tables", "2022-2023", "2022-2023_england_school_information.csv"))
spc <- read.csv(file = file.path(dir, "data", "school-pupils-and-their-characteristics_2023-24", "supporting-files", "spc_school_level_underlying_data.csv"))
spc_c <- read.csv(file = file.path(dir, "data", "school-pupils-and-their-characteristics_2023-24", "supporting-files", "spc_school_level_class_size_underlying_data.csv"))

# process gias
names(gias) <- tolower(names(gias))
names(gias)[grepl("date", names(gias))] <- paste0(names(gias)[grepl("date", names(gias))], "_gias")

# process spc_c
spc_c$admissions_policy <- ifelse(spc_c$admissions_policy == "NULL", "Unknown", spc_c$admissions_policy)

# subset data to only include OAT schools
gias <- gias %>% filter(urn %in% urn_list)
spc <- spc %>% filter(urn %in% urn_list)
spc_c <- spc_c %>% filter(urn %in% urn_list)

# combine spc data
spc <- merge(spc, spc_c, by = intersect(names(spc), names(spc_c)), all = T)
names(spc) <- gsub("X.", "percentage", names(spc))
spc$opendate <- as.Date(spc$opendate, format =  "%d/%m/%Y")

# combine school list and gias
df <- merge(school_list, gias, by = intersect(names(gias), names(school_list)), all = T)
df <- merge(df, spc, by = c("urn", "laestab"), all = T)

# save data
write.csv(df, "government_data.csv", row.names = F)

# df[, c("typeofestablishment_name", "joined.date", "opendate")]
# 
# df$joined.date == df$opendate
