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
library(foreach)
library(doParallel)

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

### Process data ###

# Set options for data preparation
id.var <- "group_uid" # ID variable
time.var <- "time_period" # Time variable
outcome.var <- "pupil_to_qual_teacher_ratio" # Dependent variable

w.constr <- list(name = "simplex") # use canonical SC

# default options
target_regions <- c("Yorkshire and the Humber", "North West")
min.years.obs <- 8

#### RUN PERMUTATION IN LOOP ####

# specify increments for decrease
increments <- seq(.00, .1, 0.02)

run_placebo <- TRUE

sim = 1
n_sim = 50

s = 1
k = 1
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
    it <- c(197)
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
    
    # determine features and covariate adjustment settings
    features <- unlist(strsplit(params$features, ", "))
    if(!is.na(params$cov.adj)){
      cov.adj <- case_match(params$cov.adj, "constant" ~ list(c("constant")), "trend" ~ list(c("trend")), "constant, trend" ~ list(c("constant", "trend")))
    } else { 
      cov.adj <- NULL
    }
    
    
    # load in simulated data # 
    
    # determine input filename
    file_name <- file.path(dir, "03_scm_mat", "interim", paste0("06_treated_mat_simulate_data_", tolower(phase), "_it_", sprintf("%03d", ii), ".csv"))
    
    # read in results
    df_sim_raw <- read.csv(file_name)
    lookup <- unique(df_sim_raw[, c("time_period", "time_period_str")])
    
    # Define timeseries
    period.simulated <- unique(df_sim_raw$time_period[is.na(df_sim_raw$pupil_to_qual_teacher_ratio)]) # identify simulated years
    period.post <- period.simulated # Simulated post-treatment period
    period.avail <- sort(unique(df_sim_raw$time_period))
    period.pre <- setdiff(period.avail, period.post) # Pre-treatment period
    
    # get names of treated schools
    list_laestab_treated <- unique(df_sim_raw$laestab[df_sim_raw$group_uid %in% uid_treated])
    
    # for each data simulation
    for (s in sim:n_sim) {
      
      # simulate effects (i.e., decrease in pupil-to-teacher ratio)
      for (k in 1:length(increments)) {
        
        cols_to_remove <- names(df_sim_raw)[grepl("sim_", names(df_sim_raw))]
        sim_col = paste0("sim_", s)
        
        # create copy to simulate interventions
        df_sim_raw <- df_sim_raw %>%
          # only focus on one simulation
          mutate(sim := !!sym(sim_col)) %>%
          #select(-c(any_of(cols_to_remove))) %>%
          #mutate(!!sym(dv) := ifelse(is.na(!!sym(dv)), sim, !!sym(dv))) %>%
          arrange(laestab, time_period) %>%
          as.data.frame()
        
        # determine multiplier
        decrease = increments[k]      
        multiplier <- 1 - decrease
        
        # # APPLY EFFECTS AT SCHOOL LEVEL
        # df_sim_int <- df_sim_raw %>%
        #   # Apply decrease
        #   mutate(sim_eff = ifelse(laestab == id_treated, sim*multiplier, sim)) %>%
        #   # replace NAs with simulated values
        #   mutate(!!sym(dv) := ifelse(is.na(!!sym(dv)), sim_eff, !!sym(dv))) %>%
        #   arrange(laestab, time_period) %>%
        #   as.data.frame()
        
        # compute MAT level average
        # df_avg <- df_sim_int %>% # APPLY EFFECTS AT SCHOOL LEVEL
        df_avg_raw <- df_sim_raw %>% # APPLY EFFECTS AT MAT LEVEL
          group_by(group_uid, time_period)  %>%
          summarise(
            !!sym(dv) := mean(!!sym(dv)),
            !!sym(var_teach) := mean(!!sym(var_teach)),
            !!sym(var_pup) := mean(!!sym(var_pup)),
            sim = mean(sim),
            n = n(), .groups = "drop"
          ) %>% 
          ungroup() %>%
          left_join(lookup, .) %>%
          mutate(status = ifelse(group_uid == uid_treated, id_group, "Donor MATs")) %>%
          arrange(group_uid, time_period) %>%
          as.data.frame()
        
        # APPLY EFFECTS AT SCHOOL LEVEL
        df_avg_int <- df_avg_raw %>%
          # Apply decrease
          mutate(sim_eff = ifelse(group_uid == uid_treated, sim*multiplier, sim)) %>%
          # replace NAs with simulated values
          mutate(!!sym(dv) := ifelse(is.na(!!sym(dv)), sim_eff, !!sym(dv))) %>%
          arrange(group_uid, time_period) %>%
          as.data.frame()
        
        
        if (run_placebo) {
          
          start.time <- Sys.time()
          
          # determine output filename
          file_name <- file.path(dir, "03_scm_mat", "interim", paste0(file_stem, "_" , tolower(phase), "_it_", sprintf("%03d", ii), "_sim_", sprintf("%03d", s), "_decrease_", sprintf("%.2f", decrease), ".csv"))
          
          # store all schools in a vector
          units <- unique(df_avg$group_uid)
          
          # Determine number of cores to use
          totalCores <- detectCores(logical = FALSE)
          # Use 75% of available cores to avoid overloading the system
          cl <- makeCluster(floor(0.75 * totalCores))
          registerDoParallel(cl)
          
          # Parallelize the loop over all units in the donor pool
          storegaps <- foreach(iii = 1:length(units), .combine = 'cbind', .packages = 'scpi') %dopar% {
            
            
            unit <- units[iii]
            message(unit, "\n")
            
            unit.tr <- unit # looping through all units in donor pool
            unit.co <- unique(df_avg$group_uid[df_avg$group_uid != unit])
            
            ### Data preparation
            scdata.out <- scdata(df = df_avg_int, 
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
            
            # # store in matrix
            # storegaps[,ii] <- c(gap_pre, gap_post)
            
            # Return a vector of gaps
            c(gap_pre, gap_post)
            
          } # close loop over control units
          
          # Set row and column names for the resulting matrix
          rownames(storegaps) <- period.avail
          colnames(storegaps) <- units
          
          # Stop the cluster when done
          stopCluster(cl)
          
          # Save results
          write.csv(storegaps, file = file_name)
          
          end.time <- Sys.time()
          time.taken <- end.time - start.time
          print(time.taken)
          
        } 
        
        
      }
      
    }
    
  }
  
}