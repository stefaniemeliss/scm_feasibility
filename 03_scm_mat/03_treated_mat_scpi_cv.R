#### SETUPS ####

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
p = 1 # debug


### define grid options, applicable to all phases ###

# define options for donor pool
min.years.obs.options = c(4, 6, 8)
min.years.obs.options = c(8)
min.schools.per.mat.options = c(2, 3, 4)
min.schools.per.timeperiod.options = c(2, 3, 4)

regions.options = list(c("Yorkshire and the Humber", "North West"), c("Yorkshire and the Humber"))
regions.options = list(c("Yorkshire and the Humber", "North West"))

exclude.northwest.options = c(TRUE, FALSE)

# Define timeseries

period.avail <- c(2010:2023)
period.post <- c(2024:2026)

period.avail.cv <- c(2010:2023)
period.post.cv <- c(2022:2023)

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

# define options for cross validation
cross.val.options <- c(TRUE, FALSE)

idx = c(1:30, 271:300, 541:570, 811:840) # debug
for (p in 1:length(phases)) {
  
  phase = phases[p]
  
  # define different filter option for each phase #
  
  if (phase == "mixed") {
    
    # define options for filtering
    filter.options <- list(
      "! laestab %in% c(3802008)",  # This will be interpreted as NULL later
      "! laestab %in% c(3802008) & ! time_period %in% c(201011, 201112)",
      "! laestab %in% c(3802008) & ! time_period %in% c(201011, 201112, 201213, 201314)"
    )
    
    exclude.single.phase.options = c(TRUE, FALSE)
    
    filter_phase = c("Not applicable", "16 plus")
    
  } else if (phase == "Secondary") {
    
    # define options for filtering
    filter.options <- list(
      "! time_period %in% c(201011, 201112)",
      "! time_period %in% c(201011, 201112, 201213, 201314)"
    )
    
    exclude.single.phase.options = c(FALSE)
    
    filter_phase = unique(groups$phaseofeducation_name)[!grepl("econdary", unique(groups$phaseofeducation_name))]
    
  } else if (phase == "Primary") {
    
    # define options for filtering
    filter.options <- list(
      "! laestab %in% c(3802008) & ! time_period %in% c(201011, 201112, 201213, 201314)",
      "! laestab %in% c(3802008) & ! time_period %in% c(201011, 201112, 201213, 201314, 201415)"
    )
    
    exclude.single.phase.options = c(FALSE)
    
    filter_phase = unique(groups$phaseofeducation_name)[!grepl("imary", unique(groups$phaseofeducation_name))]
  }
  
  
  # Create initial parameter grid
  param_grid <- expand.grid(
    min.years.obs = min.years.obs.options,
    min.schools.per.mat = min.schools.per.mat.options, 
    min.schools.per.timeperiod = min.schools.per.timeperiod.options,
    swf.filter.idx = 1:length(filter.options),
    regions = I(regions.options),
    exclude.single.phase = exclude.single.phase.options,
    # exclude.northwest = exclude.northwest.options,
    filter.phase = I(list(filter_phase)),
    
    outcome.var = outcome.var.options,
    features = I(features.options),
    cov.adj = I(cov.adj.options),
    # period.pre = I(period.pre.options), # define using ifelse!!
    # period.post = I(list(period.post)), # define using ifelse!!
    period.avail = NA, # define using ifelse!!
    period.pre = NA, # define using ifelse!!
    period.post = NA, # define using ifelse!!
    # cointegrated.data = I(cointegrated.data.options),
    # constant = I(constant.options),
    w.constr = w.constr.options,
    cross.val = cross.val.options,
    stringsAsFactors = FALSE
  )
  
  # translate SWF filter #
  
  # Add the actual filter expressions
  param_grid$swf.filter <- filter.options[param_grid$swf.filter.idx]
  
  # Remove the index column
  param_grid$swf.filter.idx <- NULL
  
  # Add if any schools should be exclude
  param_grid$excl.outlier <- grepl("laestab", param_grid$swf.filter)
  
  # extract any years to be excluded
  param_grid$period.excl <- I(apply(param_grid, 1, function(row) {
    extract_years_from_filter(row["swf.filter"])
  }))
  
  # define period.post and period.avail based on CV filter
  param_grid$period.post <- ifelse(param_grid$cross.val, I(list(period.post.cv)), I(list(period.post)))
  param_grid$period.avail <- ifelse(param_grid$cross.val, I(list(period.avail.cv)), I(list(period.avail)))
  
  # Calculate period.pre based on period.avail, period.post and period.excl.
  param_grid$period.pre <- mapply(calculate_period_pre, param_grid$period.avail, param_grid$period.post, param_grid$period.excl, SIMPLIFY = FALSE)

  # tidy up grid #
  
  # check if outcome variable is included as feature
  param_grid$included <- sapply(1:nrow(param_grid), function(i){ grepl(param_grid$outcome.var[i], param_grid$features[i]) })
  # remove if covariate adjustment is not NULL but outcome variable is not
  param_grid$keep <- ifelse(grepl("t", param_grid$cov.adj) & !param_grid$included, F, T)
  param_grid <- param_grid[param_grid$keep == T, ]
  
  # remove cols
  param_grid$keep <- NULL
  param_grid$included <- NULL
  
  # ADD A COLUMN FOR EACH ITERATION #
  # comb <- param_grid %>%
  param_grid <- param_grid %>%
    # only those not impacted by CV (hence not using period.pre)
    group_by(min.years.obs, min.schools.per.mat, min.schools.per.timeperiod, 
             swf.filter, exclude.single.phase,
             features, cov.adj, w.constr
             ) %>%
    # check that there are two each (CV == T and CV == F)
    summarise(n = n()) %>%
    ungroup() %>%
    # create column indexing the iteration
    mutate(it = as.character(1:nrow(.))) %>%
    # move it to first position
    relocate(it) %>%
    # drop n
    select(-n) %>%
    # combine with other columns
    full_join(., param_grid) %>%
    # make df
    as.data.frame()
  
  # TRANSFORM DATA FOR SAVING #
  out <- param_grid %>%
    select(it,
           min.years.obs, min.schools.per.mat, min.schools.per.timeperiod, 
           swf.filter, exclude.single.phase,
           features, cov.adj, w.constr,
           period.pre, period.post, cross.val, excl.outlier
    ) %>%
    as.data.frame()
  
  # Create a vector to store the concatenated strings
  # Ensure the result is unlisted properly
  # Add the character column to the existing dataframe
  
  # period pre
  list_data <- param_grid$period.pre
  string <- sapply(list_data, function(x) paste(x[1], x[length(x)], sep = ":"))
  out$period.pre <- string
  # period post
  list_data <- param_grid$period.post
  string <- sapply(list_data, function(x) paste(x[1], x[length(x)], sep = ":"))
  out$period.post <- string
  # swf filter
  out$swf.filter <- unlist(out$swf.filter)
  # features
  list_data <- param_grid$features
  string <- sapply(list_data, function(x) paste(x, collapse = ", "))
  string <- unlist(string)
  out$features <- string
  # cov.adj
  list_data <- param_grid$cov.adj
  string <- sapply(list_data, function(x) paste(x, collapse = ", "))
  string <- unlist(string)
  out$cov.adj <- string
  # weight constraints
  list_data <- param_grid$w.constr
  string <- sapply(list_data, function(x) paste(x, collapse = ", "))
  string <- unlist(string)
  out$w.constr <- string
  
  # determine output filename
  file_name_grid <- file.path(dir, "03_scm_mat", "interim", paste0(file_stem, "_", tolower(phase), "_grid.csv"))
  file_name_results <- file.path(dir, "03_scm_mat", "interim", paste0(file_stem, "_", tolower(phase), "_results.csv"))
  file_name_ts <- file.path(dir, "03_scm_mat", "interim", paste0(file_stem, "_", tolower(phase), "_timeseries.csv"))
  
  run_gridsearch <- T
  
  # execute gridsearch
  if (run_gridsearch) {
    
    # save grid
    write.csv(out, file = file_name_grid, row.names = FALSE)
    
    
    start <- Sys.time()

    results <- grid_search_scpi_mat(param_grid = param_grid[, ],
                                    sim = T)
    
    print( Sys.time() - start )
    
    # Save results
    write.csv(results$results, file = file_name_results, row.names = FALSE)
    write.csv(results$timeseries, file = file_name_ts, row.names = FALSE)
    
  } 
}
