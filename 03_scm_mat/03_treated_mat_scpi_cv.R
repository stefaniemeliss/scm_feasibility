#### SETUPS ####

# Clear the workspace and run garbage collection
rm(list = ls())
gc()

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

for (phase in phases) {
  
  # define different filter option for each phase #
  
  if (phase == "mixed") {
    # define options for filtering
    filter.options <- list(
      "NULL",  # This will be interpreted as NULL later
      "! time_period %in% c(201011, 201112)"
    )
    
    # define options for donor pool
    min.years.obs.options = c(4, 6, 8)
    min.schools.per.mat.options = c(2, 3, 4)
    min.schools.per.timeperiod.options = c(2, 3, 4)
    
    regions.options = list(c("Yorkshire and the Humber", "North West"), c("Yorkshire and the Humber"))
    
    exclude.single.phase.options = c(TRUE, FALSE)
    exclude.northwest.options = c(TRUE, FALSE)
    
    filter_phase = "Not applicable"
    
  } else if (phase == "Secondary") {
    
    # define options for filtering
    filter.options <- list(
      "! time_period %in% c(201011, 201112)",
      "! time_period %in% c(201011, 201112, 201213, 201314)"
    )
    
    # define options for donor pool
    min.years.obs.options = c(4, 6, 8)
    min.schools.per.mat.options = c(2, 3, 4)
    min.schools.per.timeperiod.options = c(2, 3, 4)
    
    regions.options = list(c("Yorkshire and the Humber", "North West"))
    
    exclude.single.phase.options = c(FALSE)
    exclude.northwest.options = c(TRUE, FALSE)
    
    filter_phase = unique(groups$phaseofeducation_name)[!grepl("econdary", unique(groups$phaseofeducation_name))]
  } else if (phase == "Primary") {
    
    # define options for filtering
    filter.options <- list(
      "! time_period %in% c(201011, 201112, 201213, 201314)",
      "! time_period %in% c(201011, 201112, 201213, 201314, 201415)"
    )
    
    # define options for donor pool
    min.years.obs.options = c(4, 6, 8)
    min.schools.per.mat.options = c(2, 3, 4)
    min.schools.per.timeperiod.options = c(2, 3, 4)
    
    regions.options = list(c("Yorkshire and the Humber", "North West"), c("Yorkshire and the Humber"))
    
    exclude.single.phase.options = c(FALSE)
    exclude.northwest.options = c(TRUE, FALSE)
    
    filter_phase = unique(groups$phaseofeducation_name)[!grepl("imary", unique(groups$phaseofeducation_name))]
  }
  
  ####################################
  ### define grid
  
  
  # Define timeseries
  years.avail <- c(2010:2023)
  period.post <- c(2022:2023)
  
  years.avail <- setdiff(years.avail, period.post)
  
  # Define option for pre-timeseries
  period.pre.options <- list(c(sort(years.avail)))
  
  # Define options for outcome variable
  outcome.var.options <- c("pupil_to_qual_teacher_ratio") # Outcome variable
  
  # Define options for features
  features.options <- list(
    # no DV
    c("pnpupfsm_e", "fte_avg_age"), 
    # DV included
    ## raw
    c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age"))
  
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
    # list(name = "lasso", p = "L1", lb = -Inf, Q = 1, dir = "<="), # default lasso
    # list(                p = "L1", lb = 0, Q = 1, dir = "<="), # lasso, lower bound changed to 0
    list(name = "L1-L2", p = "L1-L2", lb = 0, Q = 1, Q2 = 1, dir = "==/<=") # default L1-l2
  )
  
  
  # Create initial parameter grid
  param_grid <- expand.grid(
    min.years.obs = min.years.obs.options,
    min.schools.per.mat = min.schools.per.mat.options, 
    min.schools.per.timeperiod = min.schools.per.timeperiod.options,
    swf.filter.idx = 1:length(filter.options),
    regions = I(regions.options),
    exclude.single.phase = exclude.single.phase.options,
    exclude.northwest = exclude.northwest.options,
    filter_phase = I(list(filter_phase)),
    
    outcome.var = outcome.var.options,
    features = I(features.options),
    cov.adj = I(cov.adj.options),
    # period.pre = I(period.pre.options), # define using ifelse!!
    period.pre = NA, # define using ifelse!!
    period.post = I(list(period.post)),
    # cointegrated.data = I(cointegrated.data.options),
    # constant = I(constant.options),
    w.constr = w.constr.options,
    stringsAsFactors = FALSE
  )
  
  
  # remove the options that don't include North West to begin with and then exclude north west later (redundant)
  param_grid <- param_grid[! (!grepl("North West", param_grid$regions)& param_grid$exclude.northwest == T), ]
  
  # remove options where all years are used but min_schools_per_timeperiod is larger than 2
  param_grid <- param_grid[ !(param_grid$swf.filter.idx == 1 & param_grid$min.schools.per.timeperiod > 2), ]
  
  # Add the actual filter expressions
  param_grid$swf.filter <- filter.options[param_grid$swf.filter.idx]
  
  # Remove the index column
  param_grid$swf.filter.idx <- NULL
  
  # manually define period.pre based on filters
  if (phase == "mixed") {
    param_grid$period.pre <- ifelse(param_grid$swf.filter == "NULL", I(list(c(sort(years.avail)))), I(list(c(sort(setdiff(years.avail, c(2010:2011)))))))
  } else if (phase == "Secondary") {
    param_grid$period.pre <- ifelse(grepl("201314", param_grid$swf.filter), I(list(c(sort(setdiff(years.avail, c(2010:2013)))))), I(list(c(sort(setdiff(years.avail, c(2010:2011)))))))
  } else if (phase == "Primary") {
    param_grid$period.pre <- ifelse(grepl("201415", param_grid$swf.filter), I(list(c(sort(setdiff(years.avail, c(2010:2014)))))), I(list(c(sort(setdiff(years.avail, c(2010:2013)))))))
  }
  
  
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
  file_name <- file.path(dir, "03_scm_mat", paste0(file_stem, "_", tolower(phase), ".csv"))
  
  run_gridsearch <- T
  
  # execute gridsearch
  if (run_gridsearch) {
    
    start <- Sys.time()
    
    results <- grid_search_scpi_mat(param_grid = param_grid[, ],
                                    cv = T)
    
    print( Sys.time() - start )
    
    # Save results
    write.csv(results, file = file_name, row.names = FALSE)
    
  } 
}
