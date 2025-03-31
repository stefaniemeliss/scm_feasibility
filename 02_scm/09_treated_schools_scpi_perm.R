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
summary <- read.csv(file.path(dir, "02_scm",  "02_treated_schools_filter_donor_pool_out.csv"))

# only select schools with sufficient donor pool
summary <- subset(summary, n_pool >= 50)

# save laestab numbers
list_laestab_treated <- unique(summary$laestab)
list_laestab_treated <- list_laestab_treated[1] # focus on St. Peters for now

# create df_region as reference
df_region <- unique(summary[, c("laestab", "school", "same", "neighbouring")])

# make directory for output
dir.create(file.path(dir, "02_scm",  "interim"))

#### Define best parameter settings from grid search - per school ####
info <- list(
  # St Peters
  list(school = "St Peter's Catholic School",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"), 
       cov.adj = list(c("constant")),
       region.filter = "same",
       sd.range = NULL),
  # Dixon Music primary
  list(school = "Dixons Music Primary",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = list(c("constant")),
       region.filter = "same",
       sd.range = 50),
  # Marchbank Primary
  list(school = "Dixons Marchbank Primary",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = list(c("constant")),
       region.filter = "same",
       sd.range = 50),
  # Manningham Academy
  list(school = "Dixons Manningham Academy",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = list(c("trend")),
       region.filter = "same",
       sd.range = 50),
  # Kings Academy
  list(school = "Dixons Kings Academy",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = list(c("constant")),
       region.filter = "same",
       sd.range = NULL),
  # Dixons Trinity Academy
  list(school = "Dixons Trinity Academy",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = list(c("constant")),
       region.filter = "same",
       sd.range = NULL),
  # McMillan Academy
  list(school = "Dixons McMillan Academy",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = NULL,
       region.filter = "same",
       sd.range = NULL),
  # Cottingley Academy
  list(school = "Dixons Cottingley Academy",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = list(c("trend")),
       region.filter = "same",
       sd.range = NULL),
  # City Academy
  list(school = "Dixons City Academy",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = list(c("constant")),
       region.filter = c("same","neighbouring"),
       sd.range = 100),
  # Dixons Unity Academy
  list(school = "Dixons City Academy",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"),
       cov.adj = list(c("constant")),
       region.filter = "same",
       sd.range = NULL),
  # Highcrest Academy
  list(school = "The Highcrest Academy",
       features = c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"), 
       cov.adj = list(c("constant")),
       region.filter = c("same","neighbouring"),
       sd.range = 100)
)
info <- info[1] # focus on St. Peters for now

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

sim = 1
n_sim = 100

# Set options for data preparation
id.var <- "laestab" # ID variable
time.var <- "time_period" # Time variable
outcome.var <- "pupil_to_qual_teacher_ratio" # Dependent variable

w.constr <- list(name = "simplex") # use canonical SC

i = 1 # debug
ii = 1 # debug
k = 2 # debug
s = 1 # debug


for (i in 1:length(info)) {
  
  # get best parameter for given school
  # i = 3 # debug
  params <- info[[i]]
  # determine features and covariate adjustment settings
  features <- params$features
  cov.adj<- params$cov.adj
  
  
  # define id_treated
  id_name <- params$school
  id_treated <- df_region$laestab[df_region$school == id_name]
  
  # determine control units based on outputs of cross validation #
  
  # load in df
  if ("region.filter" %in% names(params)) {
    
    # define correct regions to use
    if ("same" %in% params$region.filter) (regions <- unlist(c(df_region[df_region$laestab == id_treated, c("same")])))
    if ("neighbouring" %in% params$region.filter) (regions <- unlist(c(df_region[df_region$laestab == id_treated, c("same", "neighbouring")])))
    
    # process data
    if (i == 1) { df <- process_data_scm(id_treated = id_treated, regions = regions, read_files = T, export_data.tables = T)
    } else { df <- process_data_scm(id_treated = id_treated, regions = regions) }
    
  }
  
  if ("sd.range" %in% names(params) & !is.null(params$sd.range)) {
    
    # filter by SD crit
    df <- sd_filtering(data = df, perc = params$sd.range, var = outcome.var)
    df[df$laestab == id_treated, paste0("crit_sd_", outcome.var)] <- T
    df <- subset(df, df[, paste0("crit_sd_", outcome.var)] == T)
    
  }
  
  
  # load in simulated data # 
  
  # determine input filename
  file_name <- file.path(dir, "02_scm", "interim", paste0("08_treated_schools_simulate_data_", gsub(" ", "_", id_name), ".csv"))
  
  # read in results
  df_sim_raw <- read.csv(file_name)
  lookup <- unique(df_sim_raw[, c("time_period", "time_period_str")])
  
  # Define timeseries
  period.simulated <- unique(df_sim_raw$time_period[is.na(df_sim_raw$pupil_to_qual_teacher_ratio)]) # identify simulated years
  period.post <- period.simulated # Simulated post-treatment period
  period.avail <- sort(unique(df_sim_raw$time_period))
  period.pre <- setdiff(period.avail, period.post) # Pre-treatment period
  
  # for each data simulation
  for (s in sim:n_sim) {
    
    # simulate effects (i.e., decrease in pupil-to-teacher ratio)
    for (k in 1:length(increments)) {
    
    # create copy to simulate interventions
    df_sim_int <- df_sim_raw %>%
      # only focus on one simulation
      mutate(sim = get(paste0("sim_", s))) %>%
      select(c(laestab, time_period, time_period_str, school, 
               pupil_to_qual_teacher_ratio, fte_avg_age, pnpupfsm_e,
               sim)) %>%
      arrange(laestab, time_period) %>%
      as.data.frame()
    
    # determine multiplier
    decrease = increments[k]      
    multiplier <- 1 - decrease
    
    df_sim_int <- df_sim_int %>%
      # Apply decrease
      mutate(sim_eff = ifelse(laestab == id_treated, sim*multiplier, sim)) %>%
      # replace NAs with simulated values
      mutate(pupil_to_qual_teacher_ratio = ifelse(is.na(pupil_to_qual_teacher_ratio), sim_eff, pupil_to_qual_teacher_ratio)) %>%
      arrange(laestab, time_period) %>%
      as.data.frame()
    
      if (run_placebo) {
        
        start.time <- Sys.time()
        
        # determine output filename
        file_name <- file.path(dir, "02_scm", "interim", paste0(file_stem, "_" , gsub(" ", "_", id_name), "_decrease_", sprintf("%.2f", decrease), "_sim_", sprintf("%03d", s), ".csv"))

        # store all schools in a vector
        units <- unique(df$laestab)
        
        # # create struture to hold results
        # storegaps <- 
        #   matrix(NA,
        #          length(period.avail), # rows
        #          length(units) # columns
        #   )
        # rownames(storegaps) <- period.avail
        # colnames(storegaps) <- units
        
        # Determine number of cores to use
        totalCores <- detectCores(logical = FALSE)
        # Use 75% of available cores to avoid overloading the system
        cl <- makeCluster(floor(0.75 * totalCores))
        registerDoParallel(cl)
        
        # # loop over all units in the donor pool (incl. treated schools)
        # for(ii in 1:length(units)){
        
        # Parallelize the loop over all units in the donor pool
        storegaps <- foreach(ii = 1:length(units), .combine = 'cbind', .packages = 'scpi') %dopar% {
          
          
          unit <- units[ii]
          
          unit.tr <- unit # looping through all units in donor pool
          unit.co <- unique(df$laestab[df$laestab != unit])
          
          ### Data preparation
          scdata.out <- scdata(df = df_sim_int, 
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
  
