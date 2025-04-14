#### SETUPS ####

knitr::opts_chunk$set(echo = TRUE)

# Clear the workspace and run garbage collection
rm(list = ls())
gc()

# Set seed
set.seed(202324)

# load libraries
library(kableExtra)
library(dplyr)
library(data.table)
library(lme4)

# create function to source code
source_code <- function(root_dir_name = "code", target_repo = "helper_functions", branch = "main", file_name = "file.R") {
  
  # construct URL
  git_url <- paste0("https://raw.githubusercontent.com/stefaniemeliss/", target_repo, "/", branch, "/", file_name)
  
  # attempt to download from github
  tempp_file <- tempfile(fileext = ".R")
  
  # run processing with the parameters
  message <- tryCatch({
    curl::curl_download(git_url, tempp_file, quiet = F)
  }, error = function(e) {
    return(error = paste("Error in curl::curl_download:", e$message))
  })
  
  if(!grepl("Error", message)) {
    
    # if successful, source file
    source(tempp_file)
    remove(tempp_file)
    
  } else { # load local copy of file
    
    # Get the current working directory
    current_dir <- getwd()
    
    # Split the current directory into its components
    dir_components <- strsplit(current_dir, "/")[[1]]
    
    # Identify the root directory dynamically based on the provided root directory name
    root_index <- which(dir_components == root_dir_name)
    if (length(root_index) == 0) {
      stop(paste("Root directory", root_dir_name, "not found in the current path"))
    }
    root_dir <- do.call(file.path, as.list(dir_components[1:root_index]))
    
    # Identify the subdirectory one level below the root and construct its absolute path
    project_repo <- dir_components[root_index + 1]
    dir <- file.path(root_dir, project_repo)
    
    if (target_repo != project_repo) {
      dir <- gsub(project_repo, target_repo, dir) 
    }
    
    # Construct the full file path
    file_path <- file.path(dir, file_name)
    
    # Print the directory and file path for debugging
    print(paste("Directory:", dir))
    print(paste("File path:", file_path))
    
    # Source the file into the parent frame
    source(file_path, local = parent.frame())
  }
}

# source functions
source_code(target_repo = "scm_feasibility", file_name = "functions.R")

# Define the base directory
dir <- get_directory()
dir_data <- file.path(dir, "data")
dir_misc <- file.path(dir, "misc")

# get file stem name
file_stem <- get_file_stem()

# copy data #
file.copy(
  file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_swf.csv"), dir_data, overwrite = T)
file.copy(
  file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_pupils.csv"), dir_data, overwrite = T)
file.copy(
  file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_establishments_search.csv"), dir_data, overwrite = T)
file.copy(
  file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_establishments_groups.csv"), dir_data, overwrite = T)

# load data #

swf <- fread(file.path(dir_data, "data_swf.csv"))
pup <- fread(file.path(dir_data, "data_pupils.csv"))
est <- fread(file.path(dir_data, "data_establishments_search.csv"), na.strings = "")
groups <- fread(file.path(dir_data, "data_establishments_groups.csv"), na.strings = "")

# get info on treated group
uid_treated <- 2939

# define phases to loop through
phases <- c("mixed", "Secondary", "Primary")
phases <- c("mixed")
p = 1 # debug

# 197 - excl. phase

# 187 - incl. phase

# 217 - excl. phase
# 277 - excl. phase

# 207 - incl. phase
# 267 - incl. phase

# 237 - excl. phase
# 297 - excl. phase

# 227 - incl. phase
# 287 - incl. phase

# default options
target_regions <- c("Yorkshire and the Humber", "North West")
min.years.obs <- 8

simulate_data <- T

#### SIMULATE DATA IN LOOP ####

for (p in 1:length(phases)) {
  
  phase = phases[p]
  
  # declare file name
  file_name <- file.path(getwd(), "03_scm_mat", "interim", paste0("03_treated_mat_scpi_cv_", tolower(phase), "_results.csv"))
  
  # read in results
  results <- read.csv(file_name)
  
  # define different filter option for each phase #
  
  if (phase == "mixed") {
    filter_phase = c("Not applicable", "16 plus")
    it <- c(197, 217, 277, 237, 297)
  } else if (phase == "Secondary") {
    filter_phase = unique(groups$phaseofeducation_name)[!grepl("econdary", unique(groups$phaseofeducation_name))]
  } else if (phase == "Primary") {
    filter_phase = unique(groups$phaseofeducation_name)[!grepl("imary", unique(groups$phaseofeducation_name))]
  }
  
  ii = it[1] # debug
  
  for (ii in it) {
    
    # select params
    params <- results[results$it == ii & !results$cross.val, ]
    
    # process data at MAT level
    process_data_scm_mat(uid_treated = uid_treated, target_regions = target_regions, filter_phase = filter_phase,
                         swf_filter = params$swf.filter, 
                         min_years_obs = min.years.obs, 
                         min_schools_per_timeperiod = params$min.schools.per.timeperiod, min_schools_per_mat = params$min.schools.per.mat)
    
    # Apply more filtering
    if (params$exclude.single.phase) {
      # remove MATs from MATs
      MATs$multiple_phases <- grepl(" | ", MATs$phase, fixed = T)
      MATs <- MATs[MATs$multiple_phases, ]
      # remove average MAT ts
      df_avg <- df_avg[df_avg$group_uid %in% MATs$group_uid, ]
      # remove school TS
      df <- df[df$group_uid %in% MATs$group_uid, ]
    }
    
    if(simulate_data){
      
      # simulate data using linear projection for the next three years #
      
      # transform data structure
      df$laestab_f <- factor(df$laestab)
      df$time_centered <- df$time_period - min(df$time_period)
      
      # add LA to data
      df$la <- as.factor(substr(df$laestab, 1, 3))
      
      # Fit model
      # Different baseline levels for each laestab (school)
      # Different rates of change over time for each la (local authority)
      m1 <- lmer(get(dv) ~ 
                   time_centered + # fixed effect for time_period
                   (1 | laestab_f) + # random intercept for laestab with (1 | laestab)
                   (0 + time_centered | la), # random slope for time_period grouped by la
                 data = df[df$time_period %in% c(2021, 2022, 2023), ])
      
      # Generate the prediction data frame for the next three academic years
      simulated_data <- expand.grid(
        laestab = unique(df$laestab),
        time_period = c(2024, 2025, 2026)  # Representing 2024/25, 2025/26, 2026/27
      )
      
      simulated_data$laestab_f <- factor(simulated_data$laestab)
      simulated_data$la <- as.factor(substr(simulated_data$laestab, 1, 3))
      simulated_data$time_centered <- simulated_data$time_period - min(df$time_period)
      simulated_data$time_period_str <- case_match(simulated_data$time_period,
                                                   2024 ~ "2024/25",
                                                   2025 ~ "2025/26",
                                                   2026 ~ "2026/27")
      simulated_data[, dv] <- numeric()
      
      # Generate *simulations* conditioned on all random effects with noise
      simulations <- simulate(m1, nsim = 100, seed = 202324,
                              newdata = simulated_data, 
                              re.form = NULL, allow.new.levels = FALSE)
      
      # Add simulations to the data frame
      simulated_data <- cbind(simulated_data, simulations)
      
      # Combine data
      df_sim <- bind_rows(df, simulated_data) %>%
        # group by schools
        group_by(laestab) %>%
        arrange(time_period) %>%
        mutate(
          # fill missing values: observations to be carried forward
          across(c(establishmentname, group_uid, group_name, gor_name, phaseofeducation_name, status),
                 ~zoo::na.locf(., na.rm = FALSE, fromLast = FALSE)))  %>%
        ungroup() %>%
        relocate(laestab, laestab_f, time_period, time_period_str, time_centered, establishmentname, group_uid, group_name, 
                 la, gor_name, phaseofeducation_name) %>%
        arrange(laestab, time_period) %>%
        as.data.frame()
      
      # determine output filename
      file_name <- file.path(dir, "03_scm_mat", "interim", paste0(file_stem, "_", tolower(phase), "_it_", sprintf("%03d", ii), ".csv"))
      # Save results
      write.csv(df_sim, file = file_name, row.names = F)
      
      
      # check simulated timeseries #
      
      idx <- sample(1:100, 1)
      
      # Add predictions to the data frame
      # simulated_data$pred <- predictions
      df_sim[, dv] <- ifelse(is.na(df_sim[, dv]), simulations[, paste0("sim_", idx)], df_sim[, dv])
      
      # compute MAT level average
      df_avg <- df_sim %>% 
        group_by(group_uid, time_period)  %>%
        summarise(
          !!sym(dv) := mean(!!sym(dv)),
          !!sym(var_teach) := mean(!!sym(var_teach)),
          !!sym(var_pup) := mean(!!sym(var_pup)),
          n = n(), .groups = "drop"
        ) %>% 
        ungroup() %>%
        left_join(unique(df_sim[, c("time_period", "time_period_str")]), .) %>%
        mutate(status = ifelse(group_uid == uid_treated, id_group, "Donor MATs")) %>%
        arrange(group_uid, time_period) %>%
        as.data.frame()
      
      # create data in long format
      df_long <- df_avg %>%
        tidyr::pivot_longer(
          cols = c(pupil_to_qual_teacher_ratio, fte_avg_age, pnpupfsm_e),
          names_to = "variable") %>%
        mutate(category = case_match(variable, 
                                     "pupil_to_qual_teacher_ratio" ~ "Outcome",
                                     "fte_avg_age" ~ "Teacher age",
                                     "pnpupfsm_e" ~ "% pupils FSM")) %>%
        as.data.frame()
      
      df_long$category <- factor(df_long$category, levels = c("Outcome", "Teacher age", "% pupils FSM"))
      
      # plot timeseries average for each school
      cols <- c(navy40, coral)
      names(cols) <- c("Donor MATs", id_group)
      
      print(ggplot(data = df_long, aes(x = time_period, y = value, col = status, group = group_uid)) +
              geom_line(data = df_long[df_long$status == "Donor MATs", ], aes(col = paste("Donor MATs"))) + 
              geom_line(data = df_long[df_long$status == paste(id_group), ], aes(col = paste(id_group)), linewidth =.8) +
              geom_point(data = df_long[df_long$status == paste(id_group), ], aes(col = paste(id_group), size = 2)) +
              facet_wrap(~ category, ncol = 1, strip.position = "top", scales = "free_y") +
              ambition_theme +
              scale_color_manual(
                breaks=c(id_group, "Donor MATs"),
                values=cols) +
              ylab("Reported value") + xlab("Academic year") +
              scale_x_continuous(breaks = df_long$time_period, labels = df_long$time_period_str) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1),
                    legend.title = element_blank()) +
              guides(size = "none"))
      cat("\n\n")  # Add some space between plots
      
      
    }
  }
  
}
