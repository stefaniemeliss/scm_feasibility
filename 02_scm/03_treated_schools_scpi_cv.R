#### SETUPS ####

knitr::opts_chunk$set(echo = TRUE)

# Clear the workspace and run garbage collection
rm(list = ls())
gc()

# load libraries
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
# list_laestab_treated <- list_laestab_treated[1:3] # debug
# list_laestab_treated <- list_laestab_treated[-1:-2]
# list_laestab_treated <- list_laestab_treated[1]

# create df_region as reference
df_region <- unique(summary[, c("laestab", "same", "neighbouring")])

#### RUN GRIDSEARCH IN LOOP ####

for (i in 1:length(list_laestab_treated)) {
  
  id_treated <- list_laestab_treated[i]
  
  # process data
  df <- process_data_scm(id_treated = id_treated, read_files = T, export_data.tables = T)
  regions <- est_treated$gor_name
  
  # determine ids of control schools
  id_cont <- unique(df$laestab[df$laestab != id_treated])
  length(id_cont)
  
  cat("# ", id_name, "\n\n")
  
  # print some information about the school
  cat("Type of establishment:", est_treated$typeofestablishment_name, "\n\n")
  cat("Phase:", est_treated$phaseofeducation_name, "\n\n")
  cat("Gender of pupils:", est_treated$gender_name, "\n\n")
  cat("Religious character:", est_treated$religiouscharacter_name, "\n\n")
  cat("Trust flag:", est_treated$trustschoolflag_name, "\n\n")
  cat("Local authority:", est_treated$la_name, "\n\n")
  cat("Region:", est_treated$gor_name, "\n\n")
  
  # run grid searcg
  cat("## Grid search \n\n")
  
  ####################################
  ### define grid
  
  # Define timeseries
  years_avail <- sort(unique(df$time_period))
  period.post <- c(2022:2023)
  
  years_avail <- setdiff(years_avail, period.post)
  
  # Define option for pre-timeseries
  period.pre.options <- list(c(sort(years_avail)), # all years
                             c(sort(setdiff(years_avail, 2020))), # omit 2020/21
                             c(sort(setdiff(years_avail, c(2020, 2021))))) # omit 2021/22
  
  # Define options for outcome variable
  outcome.var.options <- c("pupil_to_qual_teacher_ratio"#, 
                           #"pupil_to_qual_unqual_teacher_ratio"
  ) # Outcome variable
  
  # Define options for features
  features.options <- list(
    # no DV
    c("pnpupfsm_e", "fte_avg_age"), 
    # DV included
    ## raw
    c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age")#,
    #c("pupil_to_qual_unqual_teacher_ratio", "pnpupfsm_e", "fte_avg_age")
  )
  
  # Define possible covariate adjustments
  cov.adj.options <- list(
    NULL,
    list(c("constant")),
    list(c("trend")),
    list(c("constant", "trend"))
  )
  
  # Define possible options for including a constant
  constant.options <- c(TRUE, FALSE)
  
  # Define cointegrated data options
  cointegrated.data.options <- c(TRUE, FALSE)
  
  # Define options on how to constrain the weight matrix
  w.constr.options = list(
    list(name = "simplex", p = "L1", lb = 0, Q = 1, dir = "=="), # default SCM
    list(name = "lasso", p = "L1", lb = -Inf, Q = 1, dir = "<="), # default lasso
    list(                p = "L1", lb = 0, Q = 1, dir = "<="), # lasso, lower bound changed to 0
    list(name = "L1-L2", p = "L1-L2", lb = 0, Q = 1, Q2 = 1, dir = "==/<=") # default L1-l2
  )
  
  # data processing options
  rolling.window.options <- c(1:2) # how many years to calculate rolling average with
  region.filter.options <- list(c("same"), c("same", "neighbouring"))

  if(summary$phase[summary$laestab == id_treated & summary$region.filter == "same"] == "Primary"){
    # apply some SD filtering to donor school to deal with large number of primary schools available in donor pool
    sd.range.options <- list(c(100), c(50))
  } else {
    sd.range.options <- list(NULL, c(100))
  }
  
  
  # Create initial parameter grid
  param_grid <- expand.grid(
    outcome.var = outcome.var.options,
    features = I(features.options),
    cov.adj = I(cov.adj.options),
    period.pre = I(period.pre.options),
    period.post = I(list(period.post)),
    # cointegrated.data = I(cointegrated.data.options),
    # constant = I(constant.options),
    w.constr = I(w.constr.options),
    region.filter = I(region.filter.options),
    sd.range = I(sd.range.options),
    rolling.window = rolling.window.options,
    stringsAsFactors = FALSE
  )
  
  # tidy up grid #
  
  # check if features include any ratios
  param_grid$ratio <- sapply(1:nrow(param_grid), function(i){ grepl("ratio", param_grid$features[i]) })
  # check if outcome variable is included as feature
  param_grid$included <- sapply(1:nrow(param_grid), function(i){ grepl(param_grid$outcome.var[i], param_grid$features[i]) })
  # remove if ratio is in features but the features do not have outcome variable included
  param_grid$keep1 <- ifelse(param_grid$ratio & !param_grid$included, F, T)
  param_grid <- param_grid[param_grid$keep1 == T, ]
  # remove if covariate adjustment is not NULL but outcome variable is not
  param_grid$keep2 <- ifelse(grepl("t", param_grid$cov.adj) & !param_grid$included, F, T)
  param_grid <- param_grid[param_grid$keep2 == T, ]
  
  # remove cols
  param_grid$keep1 <- NULL
  param_grid$keep2 <- NULL
  param_grid$ratio <- NULL
  param_grid$included <- NULL
  
  # determine output filename
  file_name <- file.path(dir, "02_scm", paste0(file_stem, "_gridsearch_", gsub(" ", "_", id_name), ".csv"))
  
  run_gridsearch <- T
  
  # execute gridsearch
  if (run_gridsearch) {
    
    start <- Sys.time()
    
    results <- grid_search_scpi(df = df,
                                param_grid = param_grid,
                                cv = T)
    
    print( Sys.time() - start )
    
    # Save results
    write.csv(results, file = file_name, row.names = FALSE)
    
  } 
}



