# School Workforce Census (SWC) #

# Data source: SWC 2023 (November).

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
dir_in <- file.path(dir_data, "school-workforce-in-england")

# derive URNs
school_list <- read.csv(file = file.path(dir_misc, "schools_list.csv"))
urn_list <- school_list$urn

# determine year list (akin to other data sources)
years_list <- paste0(20, 10:22, 11:23)

# create scaffold to safe data
scaffold <- merge(data.frame(time_period = as.numeric(years_list)),
                  data.frame(urn = urn_list))
id_cols <- names(scaffold)


# Pupil to teacher ratios - school level #

# read in data
ptrs <- read.csv(file.path(dir_in, "2023", "data", "workforce_ptrs_2010_2023_sch.csv"))
names(ptrs) <- tolower(gsub("X...", "", names(ptrs), fixed = T))
names(ptrs)[names(ptrs) == "school_urn"] <- "urn"

# select columns
ptrs <- ptrs[, grepl("time_period|urn|fte|ratio", names(ptrs))]

# subset data to only include relevant schools
ptrs <- ptrs %>% filter(urn %in% urn_list)

# Teacher absences - school level #

# read in data
abs <- read.csv(file.path(dir_in, "2023", "data", "sickness_absence_teachers_sch.csv"))
names(abs) <- tolower(gsub("X...", "", names(abs), fixed = T))
names(abs)[names(abs) == "school_urn"] <- "urn"

# select columns
abs <- abs[, grepl("time_period|urn|abs|day", names(abs))]

# subset data to only include relevant schools
abs <- abs %>% filter(urn %in% urn_list)

# Teacher pay - school level #

# read in data
pay <- read.csv(file.path(dir_in, "2023", "data", "workforce_teacher_pay_2010_2023_school.csv"))
names(pay) <- tolower(gsub("X...", "", names(pay), fixed = T))
names(pay)[names(pay) == "school_urn"] <- "urn"

# select columns
pay <- pay[, grepl("time_period|urn|mean|headcount|pay", names(pay))]

# subset data to only include relevant schools
pay <- pay %>% filter(urn %in% urn_list)

# Teacher vacancies - school level #

# read in data
vac <- read.csv(file.path(dir_in, "2023", "data", "vacancies_number_rate_sch_2010_2023.csv"))
names(vac) <- tolower(gsub("X...", "", names(vac), fixed = T))
names(vac)[names(vac) == "school_urn"] <- "urn"

# select columns
vac <- vac[, grepl("time_period|urn|vac|rate|tmp", names(vac))]

# subset data to only include relevant schools
vac <- vac %>% filter(urn %in% urn_list)

# Size of the school workforce - school level #

# read in data
swf <- read.csv(file.path(dir_in, "2023", "data", "workforce_2010_2023_fte_hc_nat_reg_la_sch.csv"))
names(swf) <- tolower(gsub("X...", "", names(swf), fixed = T))
names(swf)[names(swf) == "school_urn"] <- "urn"

# select columns
swf <- swf[, grepl("time_period|urn|teach", names(swf))]
swf <- swf[, !grepl("fte_ft|fte_pt|hc_pt|hc_ft|leader|head", names(swf))]
# swf <- swf[, grepl("time_period|urn|fte|hc|ratio|percent", names(swf))]
# swf <- swf[, !grepl("fte_ft|fte_pt|hc_pt|hc_ft", names(swf))]
# swf <- swf[, grepl("time_period|urn|teach", names(swf))]

# subset data to only include relevant schools
swf <- swf %>% filter(urn %in% urn_list)

# Workforce teacher characteristics - school level #

dir_tmp <- file.path(dir_in, "2023", "supporting-files", "workforce_teacher_characteristics_school_2010_2023")

files <- list.files(path = dir_tmp, full.names = T)
files <- files[!grepl("meta", files)]

# determine cols to keep
cols_to_keep <- c(id_cols,
                  "gender", "age_group", "ethnicity_major",
                  "grade", "working_pattern", "qts_status", "on_route",
                  "full_time_equivalent", "headcount", "fte_school_percent", "headcount_school_percent")

for (f in 1:length(files)) {
  
  # read in data
  tmp <- read.csv(file = files[f])
  names(tmp) <- tolower(gsub("X...", "", names(tmp), fixed = T))
  
  # subset data to only include relevant schools
  names(tmp)[names(tmp) == "school_urn"] <- "urn"
  tmp <- tmp %>% filter(urn %in% urn_list)
  
  # select relevant columns
  tmp <- tmp[, names(tmp) %in% cols_to_keep]
  
  # combine across years
  if (f == 1) {
    wtc <- tmp
  } else {
    wtc <- rbind.all.columns(wtc, tmp)
  }
  
}

# rename columns
names(wtc) <- gsub("headcount", "hc", names(wtc))
names(wtc) <- gsub("school_percent", "perc", names(wtc))
names(wtc) <- gsub("full_time_equivalent", "fte", names(wtc))

# replace values 
wtc<- wtc %>%
  mutate(across(matches("fte|hc"), ~na_if(., "x"))) %>% # x = not available - information has not been collected or there are no estimates available at this level of aggregation.
  mutate(across(matches("fte|hc"), ~na_if(., "z"))) %>% # z = not applicable - statistic cannot be produced. For example where a denominator is not available to produce a percentage.
  mutate(across(matches("fte|hc"), ~na_if(., "c"))) %>% # c = confidential - where presentation of data would disclose confidential information
  mutate(across(matches("fte|hc"), ~na_if(., "u"))) %>% # u = low reliability - values of the potentially low quality, for example where values of statistical significance have been calculated.
  # remove the comma and then convert the resulting string to a numeric type
  mutate(across(matches("fte|hc"), ~as.numeric(gsub(",", "", .)))) %>%
  # replace spaces
  mutate(across(where(is.character), ~gsub(" ", "_", .))) %>%
  as.data.frame()

# determine value variables
values <- c("hc", "fte", "hc_perc", "fte_perc")
# values <- c("hc")

# make into wide format
tmp <- wtc %>% 
  # TOTALS
  filter_at(vars(!matches("time|urn|fte|hc")), all_vars(. == "Total")) %>%
  select(all_of(c(id_cols, "hc", "fte"))) %>%
  right_join( # GENDER
    wtc %>%
      # filter(gender != "Total", gender != "Gender_Unclassified") %>%
      filter(gender != "Total") %>%
      tidyr::pivot_wider(id_cols = {id_cols},
                         names_from = gender,
                         names_glue = "{.value}_gender_{gender}",
                         values_from = {values}),
    by = id_cols) %>%
  right_join( # AGE
    wtc %>% 
      # filter(age_group != "Total", age_group != "Age_unclassified") %>%
      filter(age_group != "Total") %>%
      tidyr::pivot_wider(id_cols = {id_cols},
                         names_from = age_group,
                         names_glue = "{.value}_age_{age_group}",
                         values_from = {values}),
    by = id_cols) %>%
  right_join( # ETHNICITY
    wtc %>% 
      # filter(ethnicity_major != "Total", ethnicity_major != "Information_not_yet_obtained", ethnicity_major != "Refused") %>%
      filter(ethnicity_major != "Total") %>%
      tidyr::pivot_wider(id_cols = {id_cols},
                         names_from = ethnicity_major,
                         names_glue = "{.value}_ethnicity_{ethnicity_major}",
                         values_from = {values}),
    by = id_cols) %>%
  right_join( # GRADE
    wtc %>% 
      filter(grade != "Total") %>%
      tidyr::pivot_wider(id_cols = {id_cols},
                         names_from = grade,
                         names_glue = "{.value}_grade_{grade}",
                         values_from = {values}),
    by = id_cols) %>%
  right_join( # WORKING PATTERN
    wtc %>% 
      filter(working_pattern != "Total") %>%
      tidyr::pivot_wider(id_cols = {id_cols},
                         names_from = working_pattern,
                         names_glue = "{.value}_pattern_{working_pattern}",
                         values_from = {values}),
    by = id_cols) %>%
  right_join( # QTS STATUS
    wtc %>% 
      filter(qts_status != "Total") %>%
      tidyr::pivot_wider(id_cols = {id_cols},
                         names_from = qts_status,
                         names_glue = "{.value}_qts_{qts_status}",
                         values_from = {values}),
    by = id_cols) %>% 
  mutate(
    # fill gaps in total data
    tmp = rowSums(across(matches("hc_grade")), na.rm = T),
    hc = ifelse(is.na(hc), tmp, hc),
    tmp = rowSums(across(matches("fte_grade")), na.rm = T),
    fte = ifelse(is.na(fte), tmp, fte),
    
    # fill NAs with zeros where possible
    tmp = rowSums(across(matches("hc_gender_")), na.rm = T),
    across(matches("hc_gender_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_gender_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("hc_perc_gender_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_perc_gender_"), ~ifelse(tmp == hc & is.na(.), 0, .)),

    tmp = rowSums(across(matches("hc_age_")), na.rm = T),
    across(matches("hc_age_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_age_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("hc_perc_age_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_perc_age_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    
    tmp = rowSums(across(matches("hc_ethnicity_")), na.rm = T),
    across(matches("hc_ethnicity_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_ethnicity_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("hc_perc_ethnicity_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_perc_ethnicity_"), ~ifelse(tmp == hc & is.na(.), 0, .)),

    tmp = rowSums(across(matches("hc_grade_")), na.rm = T),
    across(matches("hc_grade_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_grade_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("hc_perc_grade_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_perc_grade_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    
    tmp = rowSums(across(matches("hc_pattern_")), na.rm = T),
    across(matches("hc_pattern_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_pattern_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("hc_perc_pattern_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_perc_pattern_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    
    tmp = rowSums(across(matches("hc_qts_")), na.rm = T),
    across(matches("hc_qts_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_qts_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("hc_perc_qts_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_perc_qts_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    tmp = NULL, 
    
    # compute aggregates
    hc_age_Under_30 = rowSums(select(., "hc_age_Under_25", "hc_age_25_to_29"), na.rm = T),
    hc_age_perc_Under_30 = hc_age_Under_30/hc * 100,
    fte_age_Under_30 = rowSums(select(., "fte_age_Under_25", "fte_age_25_to_29"), na.rm = T),
    fte_age_perc_Under_30 = fte_age_Under_30/fte * 100,
    
    hc_age_30_to_49 = rowSums(select(., "hc_age_30_to_39", "hc_age_40_to_49"), na.rm = T),
    hc_age_perc_30_to_49 = hc_age_30_to_49/hc * 100,
    fte_age_30_to_49 = rowSums(select(., "fte_age_30_to_39", "fte_age_40_to_49"), na.rm = T),
    fte_age_perc_30_to_49 = fte_age_30_to_49/fte * 100,
    
    hc_age_50_and_over = rowSums(select(., "hc_age_50_to_59", "hc_age_60_and_over"), na.rm = T),
    hc_age_perc_50_and_over = hc_age_50_and_over/hc * 100,
    fte_age_50_and_over = rowSums(select(., "fte_age_50_to_59", "fte_age_60_and_over"), na.rm = T),
    fte_age_perc_50_and_over = fte_age_50_and_over/fte * 100
  ) %>% #as.data.frame()
  select(matches("time|urn|Female|White|British|Classroom|Under_30|30_to_49|50_and_over")) %>%
  select(matches("time_period|urn|fte_perc")) %>%
  as.data.frame()

apply(tmp, 2, function(x) {sum(is.na(x))})

# combine all df #

# merge dfs
df <- merge(scaffold, abs, by = id_cols, all = T)
df <- merge(df, pay, by = id_cols, all = T)
df <- merge(df, ptrs, by = id_cols, all = T)
df <- merge(df, vac, by = id_cols, all = T)
df <- merge(df, swf, by = id_cols, all = T)
df <- merge(df, tmp, by = id_cols, all = T)


# replace with NAs
df <- df %>%
  mutate(across(where(is.character), ~na_if(., "x"))) %>% # x = not available - information has not been collected or there are no estimates available at this level of aggregation.
  mutate(across(where(is.character), ~na_if(., "z"))) %>% # z = not applicable - statistic cannot be produced. For example where a denominator is not available to produce a percentage.
  mutate(across(where(is.character), ~na_if(., "c"))) %>% # c = confidential - where presentation of data would disclose confidential information
  mutate(across(where(is.character), ~na_if(., "u"))) %>% # u = low reliability - values of the potentially low quality, for example where values of statistical significance have been calculated.
  # remove the comma and then convert the resulting string to a numeric type
  mutate(across(where(is.character), \(x) as.numeric(gsub(",", "", x)))) %>%
  as.data.frame()


# save data
df <- df[with(df, order(urn, time_period)),]
write.csv(df, file = file.path(dir_data, "data_swf.csv"), row.names = F)


