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
library(scpi)
library(lme4)

# create function to source code
source_code <- function(root_dir_name = "code", target_repo = "helper_functions", branch = "main", file_name = "file.R") {
  
  # construct URL
  git_url <- paste0("https://raw.githubusercontent.com/stefaniemeliss/", target_repo, "/", branch, "/", file_name)
  
  # attempt to download from github
  tempp_file <- tempfile(fileext = ".R")
  message <- curl::curl_download(git_url, tempp_file, quiet = F)
  
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
get_directory()

# get file stem name
file_stem <- get_file_stem()

# process data establishments #

# load in file with timeseries desc
summary <- read.csv(file.path(dir, "02_scm", "interim", "02_treated_schools_filter_donor_pool_out.csv"))

# only select schools with sufficient donor pool
summary <- subset(summary, n_pool >= 50)

# create df_region as reference
df_region <- unique(summary[, c("laestab", "school", "same", "neighbouring")])

#### Define best parameter settings from grid search - per school ####
info <- list(
  # St Peters
  list(school = "St Peter's Catholic School",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = list(c("constant")),
       region.filter = "same",
       sd.range = NULL),
  # Dixon Music primary - EXCLUDED
  # Marchbank Primary
  list(school = "Dixons Marchbank Primary",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = list(c("constant")),
       region.filter = "same",
       sd.range = 50),
  # Manningham Academy
  list(school = "Dixons Manningham Academy",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = NULL,
       region.filter = "same",
       sd.range = 100),
  # Kings Academy
  list(school = "Dixons Kings Academy",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = list(c("constant", "trend")),
       region.filter = "same",
       sd.range = NULL),
  # Dixons Trinity Academy - EXCLUDED
  # McMillan Academy
  list(school = "Dixons McMillan Academy",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = list(c("constant")),
       region.filter = "same",
       sd.range = NULL),
  # Cottingley Academy
  list(school = "Dixons Cottingley Academy",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = list(c("constant")),
       region.filter = "same",
       sd.range = NULL),
  # City Academy
  list(school = "Dixons City Academy",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = NULL,
       region.filter = c("same","neighbouring"),
       sd.range = NULL),
  # Dixons Unity Academy - EXCLUDE
  # Highcrest Academy
  list(school = "The Highcrest Academy",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = list(c("constant")),
       region.filter = "same",
       sd.range = NULL)
)

### Process data ###

# Set options for data preparation
id.var <- "laestab" # ID variable
time.var <- "time_period" # Time variable
outcome.var <- "pupil_to_qual_teacher_ratio" # Dependent variable

w.constr <- list(name = "simplex") # use canonical SC


#### RUN PERMUTATION IN LOOP ####

# specify increments for decrease
increments <- seq(.00, .1, 0.01)
run_placebo <- TRUE

for (i in 1:length(info)) {
  
  # get best parameter for given school
  # i = 3 # debug
  params <- info[[i]]
  
  # define id_treated
  id_name <- params$school
  id_treated <- df_region$laestab[df_region$school == id_name]
  
  # process data
  df <- process_data_scm(id_treated = id_treated, read_files = T, export_data.tables = T)
  
  
  cat("# ", id_name, "\n\n")
  
  # print some information about the school
  cat("Type of establishment:", est_treated$typeofestablishment_name, "\n\n")
  cat("Phase:", est_treated$phaseofeducation_name, "\n\n")
  cat("Gender of pupils:", est_treated$gender_name, "\n\n")
  cat("Religious character:", est_treated$religiouscharacter_name, "\n\n")
  cat("Trust flag:", est_treated$trustschoolflag_name, "\n\n")
  cat("Local authority:", est_treated$la_name, "\n\n")
  cat("Region:", est_treated$gor_name, "\n\n")
  
  ## PROCESS DATA AND SIMULATE OUTCOME TIMESERIES ##
  
  
  # load in df
  if ("region.filter" %in% names(params)) {
    
    # define correct regions to use
    if ("same" %in% params$region.filter) (regions <- unlist(c(df_region[df_region$laestab == id_treated, c("same")])))
    if ("neighbouring" %in% params$region.filter) (regions <- unlist(c(df_region[df_region$laestab == id_treated, c("same", "neighbouring")])))
    
    # process data
    df <- process_data_scm(id_treated = id_treated, regions = regions)
    
  }
  
  if ("sd.range" %in% names(params) & !is.null(params$sd.range)) {
    
    # filter by SD crit
    df <- sd_filtering(data = df, perc = params$sd.range, var = outcome.var)
    df[df$laestab == id_treated, paste0("crit_sd_", outcome.var)] <- T
    df <- subset(df, df[, paste0("crit_sd_", outcome.var)] == T)
    
  }
  
  
  # simulate data using linear projection for the next three years #
  
  # transform data structure
  df$laestab_f <- factor(df$laestab)
  df$time_centered <- df$time_period - min(df$time_period)

  # add LA to data
  df$la <- as.factor(substr(df$laestab, 1, 3))
  
  # Fit model
  # Different baseline levels for each laestab (school)
  # Different rates of change over time for each la (local authority)
  m1 <- lmer(pupil_to_qual_teacher_ratio ~ 
               time_centered + # fixed effect for time_period
               (1 | laestab_f) + # random intercept for laestab with (1 | laestab)
               (0 + time_centered | la), # random slope for time_period grouped by la
             data = df[df$time_period %in% c(2021, 2022, 2023), ])
  
  # Check model summary
  summary(m1)
  
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
  
  # Generate *predictions* conditioned on all random effects
  predictions <- predict(m1, 
                         newdata = simulated_data, 
                         re.form = NULL)
  
  # Generate *simulations* conditioned on all random effects with noise
  simulations <- simulate(m1, nsim = 1,
                          newdata = simulated_data, 
                          re.form = NULL, allow.new.levels = FALSE)
  
  # Add predictions to the data frame
  # simulated_data$pred <- predictions
  simulated_data$sim <- simulations$sim_1
  
  # simulate effects (i.e., decrease in pupil-to-teacher ratio)
  for (decrease in increments) {
    
    # decrease = increments[5] # debug
    multiplier <- 1 - decrease
    
    df_perm <- simulated_data %>%
      # Apply decrease
      mutate(pupil_to_qual_teacher_ratio = ifelse(laestab == id_treated, sim*multiplier, sim)) %>%
      # drop column
      select(-sim) %>%
      # Combine data
      bind_rows(df, .) %>%
      # group by schools
      group_by(laestab) %>%
      arrange(time_period) %>%
      mutate(
        # fill missing values: observations to be carried forward
        across(c(urn, school),
               ~zoo::na.locf(., na.rm = FALSE, fromLast = FALSE)))  %>%
      ungroup() %>%
      arrange(laestab, time_period) %>%
      as.data.frame()
    
    if (decrease == 0) {
      
      # check simulated timeseries #
      
      # select columns
      tmp <- df_perm[, c("laestab", "time_period", "time_period_str", "school", "pupil_to_qual_teacher_ratio")]
      
      # create data
      tmp <- tmp %>%
        mutate(status = ifelse(laestab == paste(id_treated), id_name, "Donor schools")) %>%
        tidyr::pivot_longer(
          cols = -c(laestab, school, time_period, time_period_str, status),
          names_to = "variable") %>%
        mutate(variable = case_match(variable, 
                                     "pupil_to_qual_teacher_ratio" ~ "Outcome",
                                     "fte_avg_age" ~ "Teacher age",
                                     "pnpupfsm_e" ~ "% pupils FSM"
        ))
      tmp$variable <- factor(tmp$variable, levels = c("Outcome", "% pupils FSM", "Teacher age"))
      
      # plot timeseries average for each school
      cols <- c(navy40, coral)
      names(cols) <- c("Donor schools", id_name)
      
      print(ggplot(data = tmp, aes(x = time_period_str, y = value, col = status, group = laestab)) +
              geom_line(data = tmp[tmp$status == "Donor schools", ], aes(col = paste("Donor schools"))) + 
              geom_line(data = tmp[tmp$status == paste(id_name), ], aes(col = paste(id_name)), linewidth =.8) +
              facet_wrap(~ variable, ncol = 1, strip.position = "top", scales = "free_y") +
              ambition_theme +
              scale_color_manual(
                breaks=c(id_name, "Donor schools"),
                values=cols) +
              ylab("Reported value") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1),
                    axis.title.x = element_blank(), 
                    legend.title = element_blank()))
      
      # determine output filename
      file_name <- file.path(dir, "02_scm", "interim", paste0(file_stem, "_" , gsub(" ", "_", id_name), "_simdata.csv"))
      # Save results
      write.csv(df_perm, file = file_name, row.names = F)
    }
    
    
    # Define timeseries
    period.post <- c(2024:2026) # Simulated post-treatment period
    period.avail <- sort(unique(df_perm$time_period))
    period.pre <- setdiff(period.avail, period.post) # Pre-treatment period
    
    # determine features and covariate adjustment settings
    features <- params$features
    cov.adj<- params$cov.adj
    
    if (run_placebo) {
      
      # determine output filename
      file_name <- file.path(dir, "02_scm", "interim", paste0(file_stem, "_" , gsub(" ", "_", id_name), "_decrease_", sprintf("%.2f", decrease), ".csv"))
      
      # store all schools in a vector
      units <- unique(df_perm$laestab)
      
      # create struture to hold results
      storegaps <- 
        matrix(NA,
               length(period.avail), # rows
               length(units) # columns
        )
      rownames(storegaps) <- period.avail
      colnames(storegaps) <- units
      
      # loop over all units in the donor pool (incl. treated schools)
      for(i in 1:length(units)){
        
        unit <- units[i]
        message(i)
        unit.tr <- unit # looping through all units in donor pool
        unit.co <- unique(df$laestab[df$laestab != unit])
        
        ### Data preparation
        scdata.out <- scdata(df = df_perm, 
                             id.var = id.var, 
                             time.var = time.var,
                             outcome.var = outcome.var,
                             period.pre = period.pre,
                             period.post = period.post,
                             unit.tr = unit.tr,
                             unit.co = unit.co,
                             features = features,
                             cov.adj = cov.adj)
        
        
        ### SC - point estimation with simplex
        scest.out <- scest(data = scdata.out, 
                           w.constr = w.constr
        )
        
        ### SC - extract gap
        
        # Extract the actual and synthetic control outcomes for all years - PRE
        actual_pre <- scest.out$data$Y.pre
        synthetic_pre <- scest.out$est.results$Y.pre.fit
        gap_pre <- actual_pre - synthetic_pre # compute gap as difference between both
        
        # Extract the actual and synthetic control outcomes for all years - POST
        actual_post <- scest.out$data$Y.post
        synthetic_post <- scest.out$est.results$Y.post.fit
        gap_post <- actual_post - synthetic_post # compute gap as difference between both
        
        # store in matrix
        storegaps[,i] <- c(gap_pre, gap_post)
        
      } # close loop over control units
      
      # Save results
      write.csv(storegaps, file = file_name)

    } 
    
  }
  
}

