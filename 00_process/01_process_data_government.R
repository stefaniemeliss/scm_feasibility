options(scipen = 999)
# empty work space
rm(list = ls())

# define directories
dir <- getwd()
dir_data <- file.path(dir, "data")
dir_misc <- file.path(dir, "misc")

# load libraries
library(kableExtra)
library(dplyr)

#### ORMISTON ACADEMIES TRUST ####

# read in info about OAT schools
oat_schools <- read.csv(file = file.path(dir_misc, "OAT Linked establishments.csv"))

# fix input data
names(oat_schools) <- tolower(names(oat_schools))
names(oat_schools)[names(oat_schools) == "local.authority..name."] <- "laname"
oat_schools$laestab <- gsub("/", "", oat_schools$laestab)
oat_schools$linked.establishment <- "ORMISTON ACADEMIES TRUST"

# schools must have been part of OAT for 4 or more years
cut_off_date <- as.Date("2021-09-01")
oat_schools$joined.date <- as.Date(oat_schools$joined.date, format =  "%d/%m/%Y")
oat_schools <- oat_schools[oat_schools$joined.date <= cut_off_date, ]

# read in info about Dixons schools
dixons_schools <- read.csv(file = file.path(dir_misc, "Dixons Linked establishments.csv"))

# fix input data
names(dixons_schools) <- tolower(names(dixons_schools))
names(dixons_schools)[names(dixons_schools) == "local.authority..name."] <- "laname"
dixons_schools$laestab <- gsub("/", "", dixons_schools$laestab)
dixons_schools$joined.date <- as.Date(dixons_schools$joined.date, format =  "%d/%m/%Y")
dixons_schools$linked.establishment <- "DIXONS ACADEMIES TRUST"

# read in info about Lift schools
lift_schools <- read.csv(file = file.path(dir_misc, "Lift Linked establishments.csv"))

# fix input data
names(lift_schools) <- tolower(names(lift_schools))
names(lift_schools)[names(lift_schools) == "local.authority..name."] <- "laname"
lift_schools$laestab <- gsub("/", "", lift_schools$laestab)
lift_schools$joined.date <- as.Date(lift_schools$joined.date, format =  "%d/%m/%Y")
lift_schools$linked.establishment <- "LIFT SCHOOLS"

# save school list
school_list <- rbind(oat_schools, dixons_schools, lift_schools)
write.csv(school_list, file = file.path(dir_misc, "schools_list.csv"), row.names = F)

# derive URNs
urn_list <- school_list$urn

# available information of schools
gias <- read.csv(file = file.path(dir_data, "performance-tables", "2022-2023", "2022-2023_england_school_information.csv"))
spc <- read.csv(file = file.path(dir_data, "school-pupils-and-their-characteristics", "2023-24", "supporting-files", "spc_school_level_underlying_data.csv"))
spc_c <- read.csv(file = file.path(dir_data,"school-pupils-and-their-characteristics", "2023-24", "supporting-files", "spc_school_level_class_size_underlying_data.csv"))
imd <- read.csv(file = file.path(dir_data, "2019-deprivation-by-postcode.csv"))

# process gias
names(gias) <- tolower(names(gias))
names(gias) <- gsub("x...", "", names(gias), fixed = T)
names(gias)[grepl("date|^estab", names(gias))] <- paste0(names(gias)[grepl("date|^estab", names(gias))], "_gias")

# process imd
names(imd) <- gsub("X...", "", names(imd), fixed = T)
imd <- imd[, c("Postcode", "IDACI.Decile")]
names(imd) <- c("school_postcode", "idaci.decile")
imd <- imd[!duplicated(imd), ]

# process spc: replace with NAs
spc <- spc %>%
  mutate(across(where(is.character), ~na_if(., "x"))) %>% # x = not available - information has not been collected or there are no estimates available at this level of aggregation.
  mutate(across(where(is.character), ~na_if(., "z"))) %>% # z = not applicable - statistic cannot be produced. For example where a denominator is not available to produce a percentage.
  mutate(across(where(is.character), ~na_if(., "c"))) %>% # c = confidential - where presentation of data would disclose confidential information
  mutate(across(where(is.character), ~na_if(., "u"))) # u = low reliability - values of the potentially low quality, for example where values of statistical significance have been calculated.
names(spc) <- tolower(names(spc))

# process spc_c
spc_c$admissions_policy <- ifelse(spc_c$admissions_policy == "NULL", "Unknown", spc_c$admissions_policy)
names(spc_c) <- tolower(names(spc_c))

# subset data to only include OAT schools
gias <- gias %>% filter(urn %in% urn_list)
spc <- spc %>% filter(urn %in% urn_list)
spc_c <- spc_c %>% filter(urn %in% urn_list)

# thin down spc columns
spc <- spc[, 1:which(names(spc) == "school_size")]
spc_c <- spc_c[, 1:which(names(spc_c) == "school_postcode")]

# add ICADI data
spc <- merge(spc, imd, by = "school_postcode", all.x = T)

# combine spc data
spc <- merge(spc, spc_c, by = intersect(names(spc), names(spc_c)), all = T)
spc$opendate <- as.Date(spc$opendate, format =  "%d/%m/%Y")

# combine school list and gias
df <- merge(school_list, gias, by = intersect(names(gias), names(school_list)), all = T)
df <- merge(df, spc, by = c("urn", "laestab"), all = T)

# extract postcodes
tmp <- as.data.frame(unique(df$school_postcode))
write.table(tmp, file.path(dir_misc, "school_postcodes.csv"), 
            row.names = F, col.names = F, sep = ",", quote = F)


# split data and save
df_oat <- subset(df, linked.establishment == "ORMISTON ACADEMIES TRUST")
df_dixons <- subset(df, linked.establishment == "DIXONS ACADEMIES TRUST")
df_aet <- subset(df, linked.establishment == "LIFT SCHOOLS")

# save data
xlsx::write.xlsx(df_oat, file = file.path(dir_data, "data_government.xlsx"), sheetName = "data_OAT", row.names = F)
xlsx::write.xlsx(df_dixons, file = file.path(dir_data, "data_government.xlsx"), sheetName = "data_Dixons", append = T, row.names = F)
xlsx::write.xlsx(df_aet, file = file.path(dir_data, "data_government.xlsx"), sheetName = "data_Lift", append = T, row.names = F)


#### data dict ####

# write data dictionary
dict <- data.frame(variable = names(df))
dict$explanation <- c(
  "urn = School unique reference number",
  "laestab = DfE number",
  "laname = Local authority name",
  "Name of school - Linked establishments extract",
  "Address - Linked establishments extract",
  "Phase of school - Linked establishments extract",
  "Type of school - Linked establishments extract",
  "School open or closed - Linked establishments extract",
  "When did school join academy group - Linked establishments extract",
  "Name of linked establishment",
  "la = Local authority - GIAS 2022/23 extract",
  "estab = Establishment number - GIAS 2022/23 extract",
  "School name",
  "School address (1)",
  "School address (2)",
  "School address (3)",
  "School town",
  "School postcode",
  "School open / closed status",
  "Open date of school (if opened on or after 1st September 2022)",
  "Date the school closed",
  "Type of school / college eg maintained school",
  "School Type eg Voluntary Aided school",
  "Does the school provide primary education? ( 0 = No, 1 = Yes)",
  "Does the school provide secondary education? ( 0 = No, 1 = Yes)",
  "Does the school provide post 16 education? ( 0 = No, 1 = Yes)",
  "Lowest age of entry",
  "Highest age of entry",
  "Indicates whether it’s a mixed or single sex school",
  "RELCHAR = Religious character",
  "Admissions Policy",
  "Ofsted rating",
  "Ofsted last inspection date",
  "School postcode - SPC 2023-24",
  "Time period - SPC 2023-24",
  "Time identifier - SPC 2023-24",
  "Country code - SPC 2023-24",
  "Country name - SPC 2023-24",
  "Region name - SPC 2023-24",
  "Region code - SPC 2023-24",
  "Old LA code - SPC 2023-24",
  "LA name - SPC 2023-24",
  "New LA code - SPC 2023-24",
  "estab = Establishment number - SPC 2023-24",
  "Name of school - SPC 2023-24",
  "Phase type grouping - SPC 2023-24",
  "Open date of school - SPC 2023-24",
  "Mixed or single sex - SPC 2023-24",
  "Type of estabishment - SPC 2023-24",
  "Middle school",
  "All through school",
  "Hospital school",
  "Denomination - SPC 2023-24",
  "School admissions policy - Selective or otherwise - SPC 2023-24",
  "district_administrative_code",
  "district_administrative_name",
  "parl_con_code",
  "parl_con_name",
  "ward_code",
  "ward_name",
  "Urban or rural indicator",
  "geographic_level",
  "Which trust",
  "Academy indicator - Academy or otherwise",
  "School size grouped - SPC 2023-24",
  "IDACI Decile for postcode"
)

xlsx::write.xlsx(dict, file = file.path(dir_data, "data_government.xlsx"), sheetName = "dict", append = T, row.names = F)
