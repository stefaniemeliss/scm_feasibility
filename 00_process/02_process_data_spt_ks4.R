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
ks4_method <- c("standard", "standard", "standard", # (201011 - 201213)
                "ptq", # (201314) DfE restricted the inclusion of vocational qualifications in performance tables, reducing the number of qualifications that can be considered equivalent to GCSEs.
                "ptq_ee", "ptq_ee", # (201415 - 201516) DfE changed the policy on early entry to GCSEs, stating that only a student's first attempt at a GCSE would count in the performance tables. 
                "grades_gcse", "grades_gcse", "grades_gcse", "grades_gcse", "grades_gcse", "grades_gcse", "grades_gcse" # (201617 - 202223)
)
timings = data.frame(time_period = years_list,
                     ks4_method = ks4_method,
                     reform_ptq = c(0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                     reform_ee = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                     reform_gcse = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1))

##### performance tables #####

# determine years of interest
start <- 2010
finish <- 2022

# create scaffold to safe data
scaffold <- merge(data.frame(time_period = years_list),
                  data.frame(urn = urn_list))
id_cols <- names(scaffold)
scaffold <- merge(scaffold, timings, by = "time_period")

for (year in start:finish) {
  
  # skip covid years
  if(year == 2019 | year == 2020) next
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  
  # determine folder for academic year
  dir_year <- file.path(in_dir, academic_year)
  
  # read in performance data
  file_data <- list.files(path = dir_year, pattern = "england_ks4", full.names = T)
  if (file.exists(file_data)) {
    #print(file_data)
    tmp <- read.csv(file = file_data)
    names(tmp) <- gsub("X...", "", names(tmp), fixed = T)
    
    # subset data to only include relevant schools
    names(tmp) <- tolower(names(tmp))
    tmp <- tmp %>% filter(urn %in% urn_list)
    
    # replace spaces and %
    tmp <- apply(tmp, 2, function(x) {ifelse(grepl(" |%", x), gsub(" |%", "", x), x)}) %>% 
      as.data.frame() 
    
    # Figures are suppressed (“supp”) where they concern fewer than 10 pupils.
    tmp <- apply(tmp, 2, function(x) {ifelse(x == "SUPP" | x == "NE" | x == "" | x == " ", NA, as.numeric(x))}) %>% 
      as.data.frame()
    
    # add year
    tmp$time_period <- as.numeric(gsub("-20", "", academic_year))
    
    # combine across years
    if (year == start) {
      ks4 <- tmp
    } else {
      ks4 <- rbind.all.columns(ks4, tmp)
    }
    
  }
  
  # read in performance data
  file_meta <- list.files(path = dir_year, pattern = "ks4_meta", full.names = T)
  if (file.exists(file_meta[1])) {
    print(file_meta[1])
    
    if (grepl(".csv", file_meta[1])) {
      print("READ CSV")
      tmp <- read.csv(file_meta[1])
    } else if (grepl(".xlsx", file_meta[1])) {
      print("READ xlsx")
      tmp <- xlsx::read.xlsx(file_meta, sheetIndex = 1)
    } 
    
    # thin down columns
    tmp <- tmp[, 2:3]
    tmp$time_period <- as.numeric(gsub("-20", "", academic_year))
    
    # combine across years
    if (year == start) {
      meta <- tmp
    } else {
      meta <- rbind.all.columns(meta, tmp)
    }
  }
}

# process whole meta data
names(meta) <- tolower(names(meta))
meta$metafile.heading <- tolower(meta$metafile.heading)
meta <- meta[with(meta, order(metafile.heading, time_period)), ]

write.csv(meta, file = file.path(dir_misc, "meta_spt_ks4.csv"), row.names = F)

################

# The main outcome measure in Key Stage 4 performance tables 
# is the attainment of pupils in their General Certificate of Secondary Education (GCSE) exams 
# and equivalent qualifications. 

# Most if not all outcomes are also computed separately for 
# - subject elements (e.g., English, maths)
# - pupil gender
# - pupil disadvantage, EAL status, non-mobile students
# - prior attainment (high, middle, low)


#### Attainment 8 and Progress 8 ####

# These measures were introduced in 2016 to provide a more comprehensive assessment of student performance across a range of subjects. 
# They replaced the previous focus on the proportion of students achieving 5 A*-C grades, including English and mathematics. 
# Since their introduction, Attainment 8 and Progress 8 have been consistently used to track performance, providing a stable basis for comparison.

# Attainment 8 #
# measures a pupil's average achievement across eight qualifications. 
# These qualifications are divided into specific categories to ensure a broad and balanced curriculum:
# English and Mathematics: These are double-weighted, meaning they count twice in the calculation.
# EBacc Subjects: Three slots are reserved for EBacc subjects, which include sciences, computer science, geography, history, and languages.
# Open Group: The remaining three slots can be filled with any approved GCSEs or other high-value qualifications.
# The scores from these eight qualifications are added together to give the Attainment 8 score. 
# This score provides an overview of a pupil's academic performance across a range of subjects.

# in performance tables: att8scr - average attainment 8 score per pupil

# Progress 8 #
# Progress 8 measures the progress pupils make from the end of primary school (Key Stage 2) to the end of secondary school (Key Stage 4). 
# It compares pupils' Attainment 8 scores to the national average scores of pupils who had similar Key Stage 2 results.
# Baseline: Each pupil's Key Stage 2 results are used as a baseline.
# Expected Progress: The average Attainment 8 score for pupils with similar Key Stage 2 results is calculated.
# Actual Progress: The difference between a pupil's actual Attainment 8 score and the expected score is determined.
# A positive Progress 8 score indicates that a pupil has made more progress than expected, while a negative score suggests less progress than expected. 
# This measure helps to assess the effectiveness of a school in improving its pupils' academic outcomes, regardless of their starting points.

# in performance tables (2014 onwards): p8mea - Progress 8 measure [after adjustment for extreme scores 2017 onwards]
# unadjusted values are in p8mea_orig	- Progress 8 measure based on unadjusted pupil scores

# Progress 8 capping of extreme scores
# From 2018, we now limit how negative a pupil's progress score can be when calculating the school average. 

# other vars that are relevant for Progress 8: 
# p8meacov - percentage of pupils included in progress 8 measure
# p8pup	- number of pupils included in progress 8 measure
# tp8adj - number of pupils who have had p8 score adjusted in average
# p8cilow[_orig] - progress 8 lower 95% confidence interval for [un]adjusted average
# p8ciupp[_orig] - progress 8 upper 95% confidence interval for [un]adjusted average


df <- ks4[, c(id_cols, "att8scr", "p8mea", "p8meacov", "p8pup", "p8cilow", "p8ciupp", 
        "p8mea_orig", "p8cilow_orig", "p8ciupp_orig")]

#### ks2 ####

# ks2aps - Key Stage 2 Average Points Score of Key Stage 4 cohort (201011 - 201819) 
# ks2ass - KS4 cohort average KS2 Scaled Score (average of English reading and maths) (202122- 202223) 

# see https://www.gov.uk/guidance/understanding-scaled-scores-at-key-stage-2
# ks2ss may be converted to ks2aps using conversion tables
# 2022: https://assets.publishing.service.gov.uk/media/62c30b5ae90e07748b592966/2022_key_stage_2_scaled_score_tables.pdf
# 2023: https://assets.publishing.service.gov.uk/media/64ad30d1c933c10012f9e0de/2023_key_stage_2_scaled_score_tables.pdf

cols_to_merge <- c("ks2aps", "ks2ass")
new_col <- "ks2a"

df <- merge_timelines_across_columns(data_in = ks4, 
                               identifier_columns = id_cols, 
                               column_vector = cols_to_merge,
                               stem = new_col,
                               data_out = df)

#### Percentage of pupils achieving a pass or above in GCSEs in English and mathematics ####

# This indicates the proportion of pupils who have achieved strong passes in these core subjects.

# The GCSE grading system in England underwent a significant change starting in 2017. 
# The traditional A*-G grades were replaced with a numerical system ranging from 9 to 1. 
# The 9-1 grading scale is designed to provide more precise information about students' performance. 
# - Grade 9: Higher than an A* and represents the very top tier of achievement.
# - Grade 8: Between an A* and an A.
# - Grade 7: Equivalent to a solid A.
# - Grade 6: Between a B and an A.
# - Grade 5: A strong pass, between a B and a C. The government considers a grade 5 as a good pass.
# - Grade 4: A standard pass, equivalent to a low C.
# - Grade 3: Between a D and an E.
# - Grade 2: Between an E and an F.
# - Grade 1: Between an F and a G.

# Percentage of pupils achieving a strong pass (grades 9-5) in both English and mathematics - "the basics" methodology change
# The threshold for the basics measure has changed in 2017 with the introduction of reformed GCSEs in English and mathematics, graded 9-1. 
# In 2017, pupils must achieve a strong pass (grades 9-5) in either English Language or English Literature and a strong pass (grades 9-5) in mathematics to meet the criteria of the basics measure.
# For transparency, and to help schools show progress, the proportion of pupils achieving a standard pass (grades 9-4) in English and mathematics is also published.
# Prior to 2017 and based on the unreformed GCSE grade scale, pupils had to achieve a grade C or above.
# In 2016, a pupil would have to achieve a C grade or above in either English Language or English Literature. There is no requirement to sit both. Alternatively, a pupil could achieve a C grade or above in Combined English.
# In 2015 and previous years, a pupil must take exams in both English Language and English Literature, and achieve a C grade or better in English Language, or a C or better in Combined English.

# ptl2basics - Percentage of KS4 pupils achieving grades A*-C in both English and mathematics GCSEs (201011 - 201213)
# ptl2basics_ptq - Percentage of Key Stage 4 pupils achieving grades A*-C in both English and mathematics GCSEs (201314)
# ptl2basics_ptq_ee - Percentage of key stage 4 pupils achieving grades A*-C in both English and mathematics GCSEs (201415) 
# ptl2basics_ll_ptq_ee - Percentage of pupils achieving grades A*-C in both English and mathematics GCSEs (in 201516)

cols_to_merge <- c("ptl2basics", "ptl2basics_ptq", "ptl2basics_ptq_ee", "ptl2basics_ll_ptq_ee")
new_col <- "ptl2basics"

df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# ptl2basics_94 - % of pupils achieving standard 9-4 passes in both English and Maths GCSEs 
# ptl2basics_95 - % of pupils achieving strong 9-5 passes in both English and Maths GCSEs

cols_to_merge <- c("ptl2basics_94")

tmp <- ks4[, c(id_cols, cols_to_merge)]
df <- merge(df, tmp, by = id_cols, all = T)

cols_to_merge <- c("ptl2basics", "ptl2basics_ptq", "ptl2basics_ptq_ee", "ptl2basics_ll_ptq_ee", "ptl2basics_94")
new_col <- "ptl2basics_94_ts" # to investigate! 

df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

cols_to_merge <- c("ptl2basics_95")

tmp <- ks4[, c(id_cols, cols_to_merge)]
df <- merge(df, tmp, by = id_cols, all = T)

cols_to_merge <- c("ptl2basics", "ptl2basics_ptq", "ptl2basics_ptq_ee", "ptl2basics_ll_ptq_ee", "ptl2basics_95")
new_col <- "ptl2basics_95_ts" # to investigate! 


df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)


# ptac5em - Percentage of pupils achieving 5+ A*-C or equivalents including A*-C in both English and mathematics GCSEs (201011 - 201213)
# ptac5em_ptq - Percentage of pupils achieving 5+ A*-C or equivalents including A*-C in both English and mathematics GCSEs (201314)
# ptac5em_ptq_ee - Percentage of pupils achieving 5+ A*-C or equivalents including A*-C in both English and mathematics GCSEs (201415 - 201516)
# SAME AS ac5em[08:13][14_ptq][15:16_ptq_ee]

# In Key Stage 4 performance tables, the measurement of attainment at Level 2 includes: 
# 5 A-C including English and Maths*: This was a widely recognised benchmark.

cols_to_merge <- c("ptac5em", "ptac5em_ptq", "ptac5em_ptq_ee")
new_col <- "ptac5em"

df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# ac5em[08:13][14_ptq][15:16_ptq_ee]
# Percentage of pupils achieving 5+ A*-C or equivalents including A*-C in both English and mathematics GCSEs [2008 - 2016]
# same as ptac5em[_ptq[_ee]]

cols_to_merge <- names(ks4)[grepl("ac5em1", names(ks4))]
cols_to_merge <- cols_to_merge[cols_to_merge != "ac5em10"] # ac5em relates to 200910
new_col <- "ac5em"

df <- merge_staggered_timelines_across_columns(data_in = ks4, 
                                               identifier_columns = "urn", 
                                               column_vector = cols_to_merge,
                                               stem = new_col,
                                               variable_levels = years_list[1:6],
                                               data_out = df)

# pt5em_94 - Percentage of pupils achieving 5+ A*-C/9-4 or equivalents including 9-4 in both English and mathematics GCSEs
# Attainment of a full level 2 equates to achievement of 5 or more GCSEs at grades 4 and above, or a Level 2 vocational qualification of equivalent size.

cols_to_merge <- c("pt5em_94")

tmp <- ks4[, c(id_cols, cols_to_merge)]
df <- merge(df, tmp, by = id_cols, all = T)

cols_to_merge <- c("ptac5em", "ptac5em_ptq", "ptac5em_ptq_ee", "pt5em_94")
new_col <- "pt5em_94_ts" # to investigate! 

df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)


# ptgac5em - Percentage of pupils achieving 5+ A*-C (excluding equivalents) including A*-C in both English and mathematics GCSEs (201011 - 201213)
# ptgac5em_ptq - Percentage of pupils achieving 5+ A*-C GCSEs (excluding equivalents) including A*-C in both English and mathematics GCSE (201314)
# ptgac5em_ptq_ee Percentage of pupils achieving 5+ A*-C GCSEs (excluding equivalents) including A*-C in both English and mathematics GCSE (201415)
# no data for 201516!

cols_to_merge <- c("ptgac5em", "ptgac5em_ptq", "ptgac5em_ptq_ee")
new_col <- "ptgac5em"

test <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# tgac5em - Number of pupils achieving 5+ A*-C GCSEs (excluding equivalents) including A*-C in both English and mathematics GCSE (201011 - 201213)
# tgac5em_ptq - Number of pupils achieving 5+ A*-C GCSEs (excluding equivalents) including A*-C in both English and mathematics GCSE (201314)
# tgac5em_ptq_ee - Number of pupils achieving 5+ A*-C GCSEs (excluding equivalents) including A*-C in both English and mathematics GCSE (201415)
# no data for 201516!

cols_to_merge <- c("tgac5em", "tgac5em_ptq", "tgac5em_ptq_ee")
new_col <- "tgac5em"

df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)


#### EBacc entry and achievement ####

# This measures the percentage of pupils who enter and achieve the EBacc, which requires passing grades in English, mathematics, sciences, a language, and either history or geography. 

# The English Baccalaureate (EBacc) is a performance measure for schools in England.
# EBacc subjects are 
# - English
# - Mathematics
# - Sciences (including combined science, physics, chemistry, and biology)
# - A language (ancient or modern)
# - Humanities (history or geography)
# The EBacc measure focuses on two main aspects: EBacc entry and EBacc achievement.

# EBacc Entry #

# For a student to be considered as having entered the EBacc, they must be enrolled in and take exams in all these subjects. 
# The EBacc entry rate is an important metric because it shows how many students are being given the opportunity to study this broad range of subjects.

# ptebacc_e - Percentage of Key Stage 4 pupils with entries in all English Baccalaureate subject areas (201011 - 201213)
# ptebacc_e_ptq - Percentage of Key Stage 4 pupils with entries in all English Baccalaureate subject areas (201314)
# ptebacc_e_ptq_ee - Percentage of key stage 4 pupils with entries in all English Baccalaureate subject areas (201415 - 202223)

cols_to_merge <- c("ptebacc_e", "ptebacc_e_ptq", "ptebacc_e_ptq_ee")
new_col <- "ptebacc_e" # to investigate!

df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)


# tebacc_e - Number of Key Stage 4 pupils with entries in all English Baccalaureate subject areas (201011 - 201213)
# tebacc_e_ptq - Number of Key Stage 4 pupils with entries in all English Baccalaureate subject areas (201314)
# tebacc_e_ptq_ee - Number of key stage 4 pupils with entries in all English Baccalaureate subject areas (201415 - 202223)

cols_to_merge <- c("tebacc_e", "tebacc_e_ptq", "tebacc_e_ptq_ee")
new_col <- "tebacc_e"

df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# EBacc Achievement #

# EBacc achievement refers to the percentage of students who achieve a grade 5 or above in all the EBacc subjects they have entered. 
# This measure indicates how many students not only take the EBacc subjects but also achieve strong passes in them. 
# It provides a clear picture of academic performance across a broad spectrum of important subjects.

# Percentage of pupils achieving the English Baccalaureate
# The threshold for the English Baccalaureate achievement measure has changed in 2017 with the introduction of reformed GCSEs in English and mathematics, graded 9-1.
# In 2017, pupils must achieve strong passes (grades 9-5) in both English and mathematics and A*-C grades in the remaining elements.
# For transparency, the proportion of pupils achieving the English Baccalaureate with a standard pass (grades 9-4) in English and mathematics and A*-C grades in the remaining elements is also published.
# Prior to 2017 a pupil must achieve all elements of the English Baccalaureate with a C grade or better.

# ptebacc - Percentage of Key Stage 4 pupils achieving the English Baccalaureate (201011 - 201213)
# ptebacc_ptq - Percentage of Key Stage 4 pupils achieving the English Baccalaureate (201314)
# ptebacc_ptq_ee - Percentage of key stage 4 pupils achieving the English Baccalaureate (201415 - 201516) 

cols_to_merge <- c("ptebacc", "ptebacc_ptq", "ptebacc_ptq_ee")
new_col <- "ptebacc"

df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)


# ptebacc_94 - Percentage of pupils achieving the English Baccalaureate with standard passes (grades 9-4) in both English and maths and A*-C grades in the remaining elements (201617 - 202223) 
# ptebacc_95 - Percentage of pupils achieving the English Baccalaureate with strong passes (grades 9-5) in both English and maths and A*-C grades in the remaining elements (201617 - 202223) 

cols_to_merge <- c("ptebacc_94")

tmp <- ks4[, c(id_cols, cols_to_merge)]
df <- merge(df, tmp, by = id_cols, all = T)

cols_to_merge <- c("ptebacc", "ptebacc_ptq", "ptebacc_ptq_ee", "ptebacc_94")
new_col <- "ptebacc_94_ts" # to investigate! 

df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

cols_to_merge <- c("ptebacc_95")

tmp <- ks4[, c(id_cols, cols_to_merge)]
df <- merge(df, tmp, by = id_cols, all = T)

cols_to_merge <- c("ptebacc", "ptebacc_ptq", "ptebacc_ptq_ee", "ptebacc_95")
new_col <- "ptebacc_95_ts" # to investigate! 

df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)


# ptebaccag - Percentage of pupils achieving the English Baccalaureate at grades A*-G (201011 - 201213) 
# ptebaccag_ptq - Percentage of pupils achieving the English Baccalaureate at grades A*-G (201314)
# ptebaccag_ptq_ee - Percentage of pupils achieving the English Baccalaureate at grades A*-G (201415 201516) 

cols_to_merge <- c("ptebaccag", "ptebaccag_ptq", "ptebaccag_ptq_ee")
new_col <- "ptebaccag"

df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)


# ptebacc91 - Percentage of pupils achieving the English Baccalaureate at grades 9-1 in both English and maths and A*-G grades in the remaining elements (201617 - 202223) 

cols_to_merge <- c("ptebacc91")

tmp <- ks4[, c(id_cols, cols_to_merge)]
df <- merge(df, tmp, by = id_cols, all = T)

cols_to_merge <- c("ptebacc", "ptebacc_ptq", "ptebacc_ptq_ee", "ptebacc91")
new_col <- "ptebacc91_ts" # to investigate! 

df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# tebaccag - Number of pupils achieving the English Baccalaureate at grades A*-G (201011 - 201213) 
# tebaccag_ptq - Number of pupils achieving the English Baccalaureate at grades A*-G (201314)
# tebaccag_ptq_ee Number of pupils achieving the English Baccalaureate at grades A*-G (201415 - 201516) 

cols_to_merge <- c("tebaccag", "tebaccag_ptq", "tebaccag_ptq_ee")
new_col <- "tebaccag"

df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# tebacc91 - Number of pupils achieving the English Baccalaureate at grades 9-1 in both English and maths and A*-G grades in the remaining elements (201617 - 202223) 

cols_to_merge <- c("tebacc91")

tmp <- ks4[, c(id_cols, cols_to_merge)]
df <- merge(df, tmp, by = id_cols, all = T)

cols_to_merge <- c("tebaccag", "tebaccag_ptq", "tebaccag_ptq_ee", "tebacc91")
new_col <- "tebacc91_ts"

df <- merge_timelines_across_columns(data_in = ks4, 
                                     identifier_columns = id_cols, 
                                     column_vector = cols_to_merge,
                                     stem = new_col,
                                     data_out = df)

# totebaccaps  - Total EBacc APS score per pupil (201718 - 202223)
# ebaccaps - Average EBacc APS score per pupil Average (201718 - 202223)

cols_to_merge <- c("totebaccaps", "ebaccaps")

tmp <- ks4[, c(id_cols, cols_to_merge)]
df <- merge(df, tmp, by = id_cols, all = T)

# save data
df <- merge(df, scaffold, by = id_cols, all = T)
df <- df[with(df, order(urn, time_period)),]
write.csv(df, file = file.path(dir_data, "data_spt_ks4.csv"), row.names = F)



#################

# # understand data #
# 
# tmp <- apply(meta, 2, tolower) %>% as.data.frame()
# tmp <- tmp[!grepl("disadvantaged|boy|girl|prior|confidence|element|code|special edu|20|subject area|science|humani|language|school", 
#                   tmp$metafile.description), ]
# tmp <- tmp[!grepl("eal|nmob|3y", tmp$metafile.heading), ]
# write.csv(tmp, file = file.path(dir_misc, "tmp_meta_ks4.csv"), row.names = F)
# 
# # candidate outcomes 
# c("att8scr" )
# 
# string = "ptl2basics_94"
# 
# string = "ptl2basics_94"
# 
# strings <- names(ks4)[grepl("ptl2basics", names(ks4))]
# 
# strings <- strings[!grepl("elo|ehi|eav|nmob|fsm|eal|girls|boys|hi|lo|mi|av|pgebacc|pbebacc", strings)]
# 
# for (string in strings) {
# 
#   cat(meta$metafile.heading[meta$metafile.heading == string], "\n")
#   cat(meta$metafile.description[meta$metafile.heading == string], "\n")
#   cat(meta$time_period[meta$metafile.heading == string], "\n")
#   cat("\n")
# }
# 
