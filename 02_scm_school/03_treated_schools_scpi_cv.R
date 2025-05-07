#### SETUPS ####

# Clear the workspace and run garbage collection
rm(list = ls())
gc()

# load libraries
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
dir <- get_directory()
dir_data <- file.path(dir, "data")
dir_misc <- file.path(dir, "misc")

# get file stem name
file_stem <- get_file_stem()

# process data establishments #

# copy data #
file.copy(
  file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_swf.csv"), dir_data, overwrite = T)
file.copy(
  file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_pupils.csv"), dir_data, overwrite = T)
file.copy(
  file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_establishments_download.csv"), dir_data, overwrite = T)

# load data #

swf <- fread(file.path(dir_data, "data_swf.csv"))
pup <- fread(file.path(dir_data, "data_pupils.csv"))
est <- fread(file.path(dir_data, "data_establishments_download.csv"), na.strings = "")


# load in file with timeseries desc
summary <- read.csv(file.path(dir, "02_scm_school", "interim", "02_treated_schools_filter_donor_pool_out.csv"))

# only select schools with sufficient donor pool
summary <- subset(summary, n_pool >= 50)

# save laestab numbers
list_laestab_treated <- unique(summary$laestab)
# list_laestab_treated <- list_laestab_treated[1:3] # debug
# list_laestab_treated <- list_laestab_treated[-1:-5]
list_laestab_treated <- list_laestab_treated[-1]

# create df_region as reference
df_region <- unique(summary[, c("laestab", "same", "neighbouring")])


#### RUN GRIDSEARCH IN LOOP ####

i = 1 # debug

for (i in 1:length(list_laestab_treated)) {
  
  id_treated <- list_laestab_treated[i]
  
  # define options for filtering for each school
  
  if (id_treated == 3804004) { # Dixons Kings Academy
    swf_filter = "! time_period %in% c(201112, 201213, 201314)" # gap in data for 2013/14
  } else {
    swf_filter = NULL
  }
  
  # process data
  data <- process_data_scm(id_treated = id_treated,
                           var_teach = c("fte_avg_age_known"),
                           var_pup = c("pnpupfsm_e", "pnpupfsm_ever"),
                           swf_filter = swf_filter)
  df = data$df
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
  
  period.avail <- sort(unique(df$time_period))
  period.post <- c(2024:2026)
  
  period.avail.cv <- sort(unique(df$time_period))
  period.post.cv <- c(2022:2023)
  
  # Define options for outcome variable
  outcome.var.options <- c("pupil_to_qual_teacher_ratio") # Outcome variable
  
  # Define options for features
  features.options <- list(
    # no DV
    c("pnpupfsm_e", "fte_avg_age_known"), 
    c("pnpupfsm_ever", "fte_avg_age_known"), 
    # DV included
    ## raw
    c("pupil_to_qual_teacher_ratio", "pnpupfsm_e", "fte_avg_age_known"),
    c("pupil_to_qual_teacher_ratio", "pnpupfsm_ever", "fte_avg_age_known")
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
    #list(name = "lasso", p = "L1", lb = -Inf, Q = 1, dir = "<="), # default lasso
    #list(                p = "L1", lb = 0, Q = 1, dir = "<="), # lasso, lower bound changed to 0
    list(name = "L1-L2", p = "L1-L2", lb = 0, Q = 1, Q2 = 1, dir = "==/<=") # default L1-l2
  )
  
  # data processing options
  rolling.window.options <- c(1:2) # how many years to calculate rolling average with
  roll.outcome.options <- c(TRUE, FALSE) # calculate rolling average for outcome as well as for features
  region.filter.options <- list(c("same"), c("same", "neighbouring")) # which regions to include into donor pool
  
  if(summary$phase[summary$laestab == id_treated & summary$region.filter == "same"] == "Primary"){
    # apply some SD filtering to donor school to deal with large number of primary schools available in donor pool
    sd.range.options <- list(c(100), c(50))
  } else {
    sd.range.options <- list(NULL, c(100))
  }
  
  # define options for cross validation
  cross.val.options <- c(TRUE, FALSE)
  
  # define options for filtering for each school
  if (id_name == "Dixons Kings Academy") {
    filter.options <- list(
      "! time_period %in% c(201112, 201213, 201314)", # gap in data for 2013/14
      "! time_period %in% c(201112, 201213, 201314, 201415, 201516)"
    )
  } else if (id_name == "St Peter's Catholic School") {
    filter.options <- list(
      "NULL",
      "! time_period %in% c(201011, 201112, 201213)", 
      "! time_period %in% c(201011, 201112, 201213, 201314)" 
    )
  } else if (id_name == "Dixons Music Primary") {
    filter.options <- list(
      "NULL",
      "! time_period %in% c(201213, 201314)"
    )
  } else if (id_name == "Dixons Marchbank Primary") {
    filter.options <- list(
      "NULL",
      "! time_period %in% c(201415, 201516)"
    )
  } else if (id_name == "Dixons Trinity Academy") {
    filter.options <- list(
      "NULL",
      "! time_period %in% c(201213, 201314)"
    )
  } else if (id_name == "Dixons City Academy") {
    filter.options <- list(
      "NULL",
      "! time_period %in% c(201112)"
    )
  } else if (id_name == "The Highcrest Academy") {
    filter.options <- list(
      "NULL",
      "! time_period %in% c(201011, 201112)"
    )
  } else {
    filter.options <- list(
      "NULL"  # This will be interpreted as NULL later
    )
  }

  # Create initial parameter grid
  param_grid <- expand.grid(
    region.filter = I(region.filter.options),
    sd.range = I(sd.range.options),
    rolling.window = rolling.window.options,
    roll.outcome = roll.outcome.options,
    swf.filter.idx = 1:length(filter.options),
    
    outcome.var = outcome.var.options,
    features = I(features.options),
    cov.adj = I(cov.adj.options),
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
  
  # Extract any years to be excluded and add them as a list column
  param_grid$period.excl <- apply(param_grid, 1, function(row) {
    extract_years_from_filter(row["swf.filter"])
  })
  
  # define period.post and period.avail based on CV filter
  param_grid$period.post <- ifelse(param_grid$cross.val, I(list(period.post.cv)), I(list(period.post)))
  param_grid$period.avail <- ifelse(param_grid$cross.val, I(list(period.avail.cv)), I(list(period.avail)))
  
  # Calculate period.pre based on period.avail, period.post and period.excl.
  param_grid$period.pre <- mapply(calculate_period_pre, param_grid$period.avail, param_grid$period.post, param_grid$period.excl, SIMPLIFY = FALSE)
  
  
  # tidy up grid #
  
  # check if outcome variable is included as feature
  param_grid$included <- sapply(1:nrow(param_grid), function(i){ grepl(param_grid$outcome.var[i], param_grid$features[i]) })
  # remove if covariate adjustment is not NULL but outcome variable is not included in features
  param_grid$keep <- ifelse(grepl("t", param_grid$cov.adj) & !param_grid$included, F, T)
  param_grid <- param_grid[param_grid$keep == T, ]
  
  # remove if rolling.window == 1 & roll.outcome == TRUE (this is the same same as rolling.window == 1 & roll.outcome == FALSE)
  param_grid$discard <- param_grid$rolling.window == 1 & param_grid$roll.outcome == T
  param_grid <- param_grid[param_grid$discard == F, ]
  
  # remove cols
  param_grid$keep <- NULL
  param_grid$included <- NULL
  param_grid$discard <- NULL
  
  # ADD A COLUMN FOR EACH ITERATION #
  param_grid <- param_grid %>%
    # group by param grid vars
    # only those not impacted by CV (hence not using period.pre etc)
    group_by_at(setdiff(names(.), 
                        names(.)[grepl("period.|cross.val", names(.))])
                )%>%
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
           region.filter, sd.range, 
           rolling.window, roll.outcome,
           swf.filter, 
           features, cov.adj, w.constr,
           period.pre, period.post, cross.val
    ) %>%
    as.data.frame()
  
  # Create a vector to store the concatenated strings
  # Ensure the result is unlisted properly
  # Add the character column to the existing dataframe
  
  # period pre
  list_data <- out$period.pre
  string <- sapply(list_data, function(x) paste(x[1], x[length(x)], sep = ":"))
  out$period.pre <- string
  # period post
  list_data <- out$period.post
  string <- sapply(list_data, function(x) paste(x[1], x[length(x)], sep = ":"))
  out$period.post <- string
  # swf filter
  out$swf.filter <- unlist(out$swf.filter)
  # region filter
  list_data <- out$region.filter
  string <- sapply(list_data, function(x) paste(x, collapse = ", "))
  out$region.filter <- string
  # sd filter
  out$sd.range <- unlist(out$sd.range)
  
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
  file_name_grid <- file.path(dir, "02_scm_school", "interim", paste0(file_stem, "_", gsub(" ", "_", id_name), "_grid.csv"))
  file_name_results <- file.path(dir, "02_scm_school", "interim", paste0(file_stem, "_", gsub(" ", "_", id_name), "_results.csv"))
  file_name_ts_synth <- file.path(dir, "02_scm_school", "interim", paste0(file_stem, "_", gsub(" ", "_", id_name), "_ts_synth.csv"))
  file_name_ts_donor <- file.path(dir, "02_scm_school", "interim", paste0(file_stem, "_", gsub(" ", "_", id_name), "_ts_donor.csv"))
  
  run_gridsearch <- T
  
  # execute gridsearch
  if (run_gridsearch) {
    
    # save grid
    write.csv(out, file = file_name_grid, row.names = FALSE)
    
    
    start <- Sys.time()
    
    # results <- grid_search_scpi(param_grid = param_grid[seq(1, nrow(param_grid), 120), ],
    #                                 sim = T)
    results <- grid_search_scpi(param_grid = param_grid[, ],
                                    sim = T)
    
    print( Sys.time() - start )
    
    # Save results
    write.csv(results$results, file = file_name_results, row.names = FALSE)
    write.csv(results$ts_synth, file = file_name_ts_synth, row.names = FALSE)
    write.csv(results$ts_donor, file = file_name_ts_donor, row.names = FALSE)
   
    # run garbage collection
    gc()
  }
}



