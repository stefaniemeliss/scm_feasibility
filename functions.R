#### ALL-PURPOSE HELPER FUNCTIONS ####

# source code
source_code(target_repo = "helper_functions", file_name = "functions.R")

#### PROJECT-SPECIFIC FUNCTIONS ####

# functions for data processing #

# Function to insert "/" in the format "YYYYYY" to "YYYY/YY"
insert_slash <- function(number) {
  sub("(\\d{4})(\\d{2})", "\\1/\\2", number)
}

process_data_scm <- function(id_treated = "id_treated", 
                             dv = "pupil_to_qual_teacher_ratio",
                             var1 = "fte_avg_age",
                             var2 = "pnpupfsm_e",
                             regions = NULL,
                             copy_files = F,
                             read_files = F,
                             export_data.tables = F
                             ){
  
  # This is a data pre-processing function for later synthetic control method (SCM) analysis using [id_treated] as the treated school.
  # The script copies and loads data files, processes data for treated and control schools
  # to create outcome and predictor time series.
  
  
  #### SETUP ####
  
  # Load necessary libraries
  library(kableExtra)
  library(dplyr)
  library(data.table)
  
  # Define directories based on the current working directory
  dir <- normalizePath(dir, winslash = "/")
  dir_data <- file.path(dir, "data")
  dir_misc <- file.path(dir, "misc")
  
  # Export the root and subdirectory to the global environment
  assign("dir_data", dir_data, envir = .GlobalEnv)
  assign("dir_misc", dir_misc, envir = .GlobalEnv)
  
  if(copy_files){
    
    # Copy data files from the source directory to the target directory
    file.copy(
      file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_swf.csv"),
      dir_data,
      overwrite = TRUE
    )
    file.copy(
      file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_pupils.csv"),
      dir_data,
      overwrite = TRUE
    )
    file.copy(
      file.path(gsub("scm_feasibility", "edu_stats", dir_data), "data_establishments_search.csv"),
      dir_data,
      overwrite = TRUE
    )
  }
  
  if (read_files) {
    # Load data from CSV files into data tables
    swf <- fread(file = file.path(dir_data, "data_swf.csv"))
    pup <- fread(file = file.path(dir_data, "data_pupils.csv"))
    est <- fread(file = file.path(dir_data, "data_establishments_search.csv"), na.strings = "")
  }
  
  
  #### DETERMINE AVAILABLE TIMESERIES DATA ####
  
  # Identify years for which there are observations for treated school
  data_avail_dv <- swf %>%
    filter(!is.na(get(dv))) %>%
    filter(laestab == id_treated) %>%
    select(time_period, laestab, paste(dv)) %>%
    distinct(time_period) %>%
    pull(time_period)
  
  # Identify years for which there are observations for treated school
  data_avail_age <- swf %>%
    filter(!is.na(get(var1))) %>%
    filter(laestab == id_treated) %>%
    select(time_period, laestab, paste(var1)) %>%
    distinct(time_period) %>%
    pull(time_period)
  
  # Identify years for which there are observations for treated school
  data_avail_fsm <- pup %>%
    filter(!is.na(get(var2))) %>%
    filter(laestab == id_treated) %>%
    select(time_period, laestab, paste(var2)) %>%
    distinct(time_period) %>%
    pull(time_period)
  
  # combine, excluding duplicates
  data_avail <- intersect(data_avail_dv, intersect(data_avail_age, data_avail_fsm))
  
  
  #### PROCESS DATA FOR ESTABLISHMENTS ####
  
  # Get data for the treated school based on the ID
  est_treated <- est %>%
    filter(laestab == id_treated) %>%
    mutate(status = "treated") %>%
    as.data.frame()
  
  # overwrite region with default if not previously specified
  if (is.null(regions)) {
    regions <- unique(c(est_treated$gor_name))
  }
  
  # Get donor pool data excluding the treated school
  if(exists("list_laestab_exclude")) {
    est_cont <- est %>%
      filter(
        laestab != list_laestab_exclude,
        phaseofeducation_name %in% unique(c(est_treated$phaseofeducation_name)),
        gor_name %in% regions,
        ! parliamentaryconstituency_name %in% unique(c(est_treated$parliamentaryconstituency_name)),
        ! grepl("Boarding school", boarders_name),
        admissionspolicy_name != "Selective"
      ) %>%
      mutate(status = "untreated") %>%
      as.data.frame()
  } else {
    est_cont <- est %>%
      filter(
        laestab != id_treated,
        phaseofeducation_name %in% unique(c(est_treated$phaseofeducation_name)),
        gor_name %in% regions,
        ! parliamentaryconstituency_name %in% unique(c(est_treated$parliamentaryconstituency_name)),
        ! grepl("Boarding school", boarders_name),
        admissionspolicy_name != "Selective"
      ) %>%
      mutate(status = "untreated") %>%
      as.data.frame()
  }
  
  # Save unique lists of laestab and urn
  list_laestab <- c(unique(est_cont[, "laestab"]), unique(est_treated[, "laestab"]))
  
  #### CREATE OUTCOME DATASET ####
  
  # Filter SWF data to create outcome dataset
  z <- swf[laestab %in% list_laestab & time_period %in% data_avail]
  
  # Remove rows for years for which the treated school has no data, 
  # rows with NA for the dependent variable and age predictor 
  # and add observation count
  z <- z %>%
    filter(!is.na(get(dv))) %>%
    group_by(laestab) %>%
    mutate(
      obs_count_dv = sum(!is.na(get(dv))),
      obs_count_var1 = sum(!is.na(get(var1)))
    ) %>%
    ungroup()
  
  # Filter for rows where observation count matches the treated ID and select columns
  z <- z %>%
    filter(obs_count_dv == unique(z$obs_count_dv[z$laestab == id_treated])) %>%
    filter(obs_count_var1 == unique(z$obs_count_var1[z$laestab == id_treated])) %>%
    select(time_period, laestab, school, pupil_to_qual_teacher_ratio, pupil_to_qual_unqual_teacher_ratio, fte_avg_age) %>%
    group_by(laestab) %>%
    arrange(laestab, desc(time_period)) %>%
    mutate(school = first(school)) %>%
    ungroup() %>%
    as.data.frame()
  
  # Update list of laestab numbers based on the dependent dviable
  check <- z %>% group_by(laestab) %>% summarise(n = n()) # identify estabs with more than 1 idaci decile (school has moved locations)
  id_remove <- check$laestab[check$n != check$n[check$laestab == id_treated]] # identify estab numbers
  list_laestab_dv <- unique(z$laestab)
  list_laestab_dv <- setdiff(list_laestab_dv, id_remove)# update list_laestab_dv to exclude the ids that should be removed
  rm(check)
  
  #### CREATE PREDICTOR DATASET ####
  
  # Filter SWF data to create outcome dataset
  x <- pup[laestab %in% list_laestab_dv & time_period %in% data_avail]
  
  # Remove rows for years for which the treated school has no data, 
  # rows with NA for the dependent variable and age predictor 
  # and add observation count
  x <- x %>%
    filter(!is.na(get(var2))) %>%
    group_by(laestab) %>%
    mutate(
      obs_count_var2 = sum(!is.na(get(var2)))
    ) %>%
    ungroup()
  
  # Filter for rows where observation count matches the treated ID and select columns
  x <- x %>%
    filter(obs_count_var2 == unique(x$obs_count_var2[x$laestab == id_treated])) %>%
    select(time_period, laestab, urn, pnpupfsm_e) %>%
    arrange(laestab, desc(time_period)) %>%
    as.data.frame()
  
  # combine outcome and predictor
  df <- merge(z, x, by = c("laestab", "time_period"))
  
  # arrange data
  df <- df %>%
    relocate(urn, .after = laestab) %>%
    arrange(laestab, time_period) %>%
    mutate(
      # # change name to include laestab to navigate duplicates
      # school = paste(laestab, school),
      # add slash and use as string
      time_period_str = insert_slash(time_period),
      # remove the last two digits
      time_period = as.numeric(substr(time_period, 0, 4))
    )
  
  # export school name
  id_name <- unique(df$school[df$laestab == id_treated])
  assign("id_name", id_name, envir = .GlobalEnv)
  
  # save information about school to environment
  if (nrow(est_treated) > 1) {
    # if school has more than 1 URN linked to the same LAESTAB
    est_treated <- subset(est_treated, est_treated$establishmentstatus_name == "Open")
  }
  assign("est_treated", est_treated, envir = .GlobalEnv)
  
  # clean up a little
  gc()
  
  if(export_data.tables){
    assign("swf", swf, envir = .GlobalEnv)
    assign("pup", pup, envir = .GlobalEnv)
    assign("est", est, envir = .GlobalEnv)
    
  }
  
  return(df)
}

# Function to determine if average pre-treatment timeseries is within [X] SDs of the average for treated school 
sd_filtering <- function(data = df, var = "var", perc = NULL, show_kbl = F){
  
  # pre-treatment outcome average and sd for treated school
  mean_treated <- mean(data[data$laestab == id_treated, paste(var)])
  sd_treated <- sd(data[data$laestab == id_treated, var])
  
  # pre-treatment outcome average and sd for control schools
  school_ave <- data %>% 
    filter(laestab != id_treated) %>%
    group_by(laestab) %>%
    summarise(
      mean_dv = mean(get(var), na.rm = T),
      sd_dv = sd(get(var), na.rm = T)
    ) %>%
    mutate(
      # check if average pre-treatment outcome is within [X] SDs of the average for treated school 
      crit_sd_100 = ifelse(mean_dv > (mean_treated + 1.0 * sd_treated) | mean_dv < (mean_treated - 1.0 * sd_treated), FALSE, TRUE),
      crit_sd_90 = ifelse(mean_dv > (mean_treated + .90 * sd_treated) | mean_dv < (mean_treated - .90 * sd_treated), FALSE, TRUE),
      crit_sd_80 = ifelse(mean_dv > (mean_treated + .80 * sd_treated) | mean_dv < (mean_treated - .80 * sd_treated), FALSE, TRUE),
      crit_sd_75 = ifelse(mean_dv > (mean_treated + .75 * sd_treated) | mean_dv < (mean_treated - .75 * sd_treated), FALSE, TRUE),
      crit_sd_70 = ifelse(mean_dv > (mean_treated + .70 * sd_treated) | mean_dv < (mean_treated - .70 * sd_treated), FALSE, TRUE),
      crit_sd_60 = ifelse(mean_dv > (mean_treated + .60 * sd_treated) | mean_dv < (mean_treated - .60 * sd_treated), FALSE, TRUE),
      crit_sd_50 = ifelse(mean_dv > (mean_treated + .50 * sd_treated) | mean_dv < (mean_treated - .50 * sd_treated), FALSE, TRUE),
      crit_sd_40 = ifelse(mean_dv > (mean_treated + .40 * sd_treated) | mean_dv < (mean_treated - .40 * sd_treated), FALSE, TRUE),
      crit_sd_30 = ifelse(mean_dv > (mean_treated + .30 * sd_treated) | mean_dv < (mean_treated - .30 * sd_treated), FALSE, TRUE),
      crit_sd_25 = ifelse(mean_dv > (mean_treated + .25 * sd_treated) | mean_dv < (mean_treated - .25 * sd_treated), FALSE, TRUE),
      crit_sd_20 = ifelse(mean_dv > (mean_treated + .20 * sd_treated) | mean_dv < (mean_treated - .20 * sd_treated), FALSE, TRUE),
      crit_sd_10 = ifelse(mean_dv > (mean_treated + .10 * sd_treated) | mean_dv < (mean_treated - .10 * sd_treated), FALSE, TRUE),
    )
  
  
  # numbers show number of schools passing the critical threshold
  out <- merge(data.frame(laestab = id_treated, var = var), data.frame(as.list(colSums(school_ave[, c(-1:-3)]))), by = 0)
  out$Row.names <- NULL
  
  if(show_kbl){
    # summarise results: numbers show count kept
    out[, c(-1:-2)] %>% 
      kbl(caption = paste(var), row.names = F) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T) %>% 
      add_footnote("Data shows the number of schools with average within crit_sd_[percentage].") %>%
      print()
  }
  
  if (!is.null(perc)) {
    # if percentage is assigned, add the check to the input df
    tmp <- school_ave[, c("laestab", paste0("crit_sd_", perc))]
    names(tmp)[2] <- paste0("crit_sd_", var)
    data <- merge(data, tmp, by = "laestab", all.x = T)
    
    # update the original df in the parent environment
    assign("df", data, envir = .GlobalEnv)
    
  } else {
    # export output
    return(out)
  }
  
}


# Function to determine the status of an establishment in a given academic year
get_establishment_status <- function(data, laestab, academic_year_start) {
  # Define the start and end dates of the academic year
  academic_start <- as.Date(paste0(academic_year_start, "-09-01"))
  academic_end <- as.Date(paste0(academic_year_start + 1, "-08-31"))
  
  # Filter the data for the given establishment
  est_data <- data[data$laestab == laestab, ]
  
  # Check each row for the status during the academic year
  for (i in 1:nrow(est_data)) {
    row <- est_data[i, ]
    open_date <- as.Date(row$opendate, format = "%Y-%m-%d")
    close_date <- as.Date(row$closedate, format = "%Y-%m-%d")
    
    if ((is.na(open_date) || open_date <= academic_end) && (is.na(close_date) || close_date >= academic_start)) {
      return(row$trustschoolflag_name)
    }
  }
  
  return("Closed")
}

# Create a new data frame to store the status of each establishment for each academic year
create_status_df <- function(data, start_year, end_year) {
  # Get a unique list of establishments
  establishments <- unique(data$laestab)
  
  # Create an empty data frame to store the results
  status_df <- data.frame(laestab = integer(), trustschoolflag_name = character(), academic_year = integer(), stringsAsFactors = FALSE)
  
  # Loop through each academic year and each establishment
  for (year in start_year:end_year) {
    for (est in establishments) {
      status <- get_establishment_status(data, est, year)
      status_df <- rbind(status_df, data.frame(laestab = est, trustschoolflag_name = status, time_period = year, stringsAsFactors = FALSE))
    }
  }
  
  return(status_df)
}

# Define the modify_special_predictors function
modify_special_predictors <- function(special_predictors, op) {
  lapply(special_predictors, function(predictor_list) {
    if (is.null(predictor_list)) {
      return(NULL)
    }
    lapply(predictor_list, function(predictor) {
      c(predictor, op)
    })
  })
}

# Create function to run grid search
grid_search_synth <- function(df, param_grid, treatment_identifier, dependent_var, 
                              unit_var, time_var, unit_names_var, use_parallel = TRUE) {
  
  # Define default values for parameters
  default_values <- data.frame(
    predictors = I(list(c("pnpupfsm_e", "pnpupeal"))),
    predictors_op = "mean",
    special_predictors = I(list(NULL)),
    time_predictors_prior = I(list(2014:2023)),
    optimxmethod = "All",
    Margin.ipop = 0.0005,
    Sigf.ipop = 5,
    Bound.ipop = 10,
    stringsAsFactors = FALSE
  )
  
  run_scm <- function(df, params) {
    
    tmp <- default_values[, setdiff(names(default_values), names(params))]
    params <- merge(params, tmp, by = 0)
    
    dataprep.out <- tryCatch({
      dataprep(
        foo = df,
        predictors = params$predictors[[1]],
        predictors.op = params$predictors_op,
        special.predictors = params$special_predictors[[1]],
        dependent = dependent_var,
        unit.variable = unit_var,
        time.variable = time_var,
        treatment.identifier = treatment_identifier,
        controls.identifier = unique(df[[unit_var]][df[[unit_var]] != treatment_identifier]),
        time.predictors.prior = params$time_predictors_prior[[1]],
        time.optimize.ssr = params$time_predictors_prior[[1]],
        unit.names.variable = unit_names_var,
        time.plot = 2010:2023
      )
    }, error = function(e) {
      message("Error in dataprep: ", e$message)
      return(NULL)
    })
    
    if (is.null(dataprep.out)) return(list(sd_treated = NA, m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
                                           rmspe = "dataprep() failed", mspe = NA, mae = NA, loss_v= NA, loss_w = NA,
                                           params = params))
    
    synth.out <- tryCatch({
      synth(
        data.prep.obj = dataprep.out,
        optimxmethod = params$optimxmethod,
        Margin.ipop = params$Margin.ipop,
        Sigf.ipop = params$Sigf.ipop,
        Bound.ipop = params$Bound.ipop
      )
    }, error = function(e) {
      message("Error in synth: ", e$message)
      return(NULL)
    })
    
    if (is.null(synth.out)) return(list(sd_treated = NA, m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
                                        rmspe = "synth() failed", mspe = NA, mae = NA, loss_v= NA, loss_w = NA,
                                        params = params))
    
    # Extract the actual and synthetic control outcomes for all years
    actual <- dataprep.out$Y1plot
    synthetic <- dataprep.out$Y0plot %*% synth.out$solution.w
    gap <- actual - synthetic # compute gap as difference between both
    
    # compute performance parameters
    sd_treated <- sd(actual, na.rm = TRUE)
    m_gap <- mean(gap, na.rm = TRUE)
    sd_gap <- sd(gap, na.rm = TRUE)
    min_gap <- min(gap, na.rm = TRUE)
    max_gap <- max(gap, na.rm = TRUE)
    cor <- cor(actual, synthetic)[1]
    rmspe <- sqrt(mean((gap)^2, na.rm = TRUE))
    mspe <- mean((gap)^2, na.rm = TRUE)
    mae <- mean(abs(gap), na.rm = TRUE)
    loss_v <- synth.out$loss.v[1]
    loss_w <- synth.out$loss.w[1]
    
    # overwrite optimxmethod from "All" to best one
    params$optimxmethod <- row.names(synth.out$rgV.optim$par)
    
    return(list(sd_treated = sd_treated, m_gap = m_gap, sd_gap = sd_gap, min_gap = min_gap, max_gap = max_gap, cor = cor,
                rmspe = rmspe, mspe = mspe, mae = mae, loss_v= loss_v, loss_w = loss_w,
                params = params))
  }
  
  if (use_parallel) {
    # Register parallel backend
    num_cores <- detectCores() - 1
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)
    
    # Perform grid search with parallel processing
    results <- foreach(i = 1:nrow(param_grid), .combine = rbind, .packages = c("Synth", "dplyr")) %dopar% {
      params <- param_grid[i, ]
      row.names(params) <- 1
      
      result <- tryCatch({
        run_scm(df, params)
      }, error = function(e) {
        
        list(sd_treated = NA, m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
             rmspe = "run_scm() failed", mspe = NA, mae = NA, loss_v= NA, loss_w = NA,
             params = params)
      })
      
      # Create a list to store the results
      result_list <- list(
        predictors = ifelse(!is.null(result$params$predictors[[1]]), paste(result$params$predictors[[1]], collapse = ", "), NA),
        predictors_op = ifelse(!is.null(result$params$predictors_op), result$params$predictors_op, NA),
        special_predictors = ifelse(!is.null(result$params$special_predictors[[1]]), 
                                    paste(sapply(result$params$special_predictors[[1]], function(x) paste(x, collapse = ", ")), collapse = "; \n"), 
                                    NA),
        time_predictors_prior = ifelse(!is.null(result$params$time_predictors_prior[[1]]), 
                                       paste(result$params$time_predictors_prior[[1]], collapse = ", "), 
                                       NA),
        optimxmethod = ifelse(!is.null(result$params$optimxmethod), result$params$optimxmethod, NA),
        Margin.ipop = ifelse(!is.null(result$params$Margin.ipop), result$params$Margin.ipop, NA),
        Sigf.ipop = ifelse(!is.null(result$params$Sigf.ipop), result$params$Sigf.ipop, NA),
        Bound.ipop = ifelse(!is.null(result$params$Bound.ipop), result$params$Bound.ipop, NA),
        sd_treated = result$sd_treated,
        m_gap = result$m_gap,
        sd_gap = result$sd_gap,
        min_gap = result$min_gap,
        max_gap = result$max_gap,
        cor = result$cor,
        rmspe = result$rmspe,
        mspe = result$mspe,
        mae = result$mae,
        loss_v = result$loss_v,
        loss_w = result$loss_w
      )
      
      # Convert the list to a data frame
      result_df <- as.data.frame(result_list, stringsAsFactors = FALSE)
      
      return(result_df)
    }
    
    # Stop the cluster
    stopCluster(cl)
    
  } else {
    # Perform grid search without parallel processing
    results <- do.call(rbind, lapply(1:nrow(param_grid), function(i) {
      params <- param_grid[i, ]
      row.names(params) <- 1
      
      result <- tryCatch({
        run_scm(df, params)
      }, error = function(e) {
        list(sd_treated = NA, m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
             rmspe = "run_scm() failed", mspe = NA, mae = NA, loss_v= NA, loss_w = NA,
             params = params)
      })
      
      # Create a list to store the results
      result_list <- list(
        predictors = ifelse(!is.null(result$params$predictors[[1]]), paste(result$params$predictors[[1]], collapse = ", "), NA),
        predictors_op = ifelse(!is.null(result$params$predictors_op), result$params$predictors_op, NA),
        special_predictors = ifelse(!is.null(result$params$special_predictors[[1]]), 
                                    paste(sapply(result$params$special_predictors[[1]], function(x) paste(x, collapse = ", ")), collapse = "; \n"), 
                                    NA),
        time_predictors_prior = ifelse(!is.null(result$params$time_predictors_prior[[1]]), 
                                       paste(result$params$time_predictors_prior[[1]], collapse = ", "), 
                                       NA),
        optimxmethod = ifelse(!is.null(result$params$optimxmethod), result$params$optimxmethod, NA),
        Margin.ipop = ifelse(!is.null(result$params$Margin.ipop), result$params$Margin.ipop, NA),
        Sigf.ipop = ifelse(!is.null(result$params$Sigf.ipop), result$params$Sigf.ipop, NA),
        Bound.ipop = ifelse(!is.null(result$params$Bound.ipop), result$params$Bound.ipop, NA),
        sd_treated = result$sd_treated,
        m_gap = result$m_gap,
        sd_gap = result$sd_gap,
        min_gap = result$min_gap,
        max_gap = result$max_gap,
        cor = result$cor,
        rmspe = result$rmspe,
        mspe = result$mspe,
        mae = result$mae,
        loss_v = result$loss_v,
        loss_w = result$loss_w
      )
      
      # Convert the list to a data frame
      result_df <- as.data.frame(result_list, stringsAsFactors = FALSE)
      
      return(result_df)
    }))
  }
  
  return(results)
}


# Create function to run grid search
grid_search_scpi <- function(df, param_grid, use_parallel = FALSE, cv = FALSE) {
  
  # Define default values for parameters
  default_values <- data.frame(
    id.var = "laestab", # ID variable
    time.var = "time_period", # Time variable
    period.pre = I(list(2014:2023)), # Pre-treatment period
    period.post = I(list(2024)), # Post-treatment period
    outcome.var = "pupil_to_qual_teacher_ratio", # Outcome variable
    unit.tr = id_treated, # Treated unit (in terms of id.var)
    unit.co = NA, # Donors pool
    features = I(list(NULL)), # No features other than outcome
    cov.adj = I(list(NULL)), # Covariates for adjustment
    cointegrated.data = FALSE, # don't belief that the data are cointegrated
    anticipation = 0, # No anticipation
    constant = FALSE, # No constant term
    stringsAsFactors = FALSE
  )
  
  
  run_scm <- function(df, params) {
    
    # debug
    # params <- param_grid[2158 , ]
    # row.names(params) <- 1
    
    # Merge with parameters in grid
    tmp <- default_values[, setdiff(names(default_values), names(params))]
    params <- merge(params, tmp, by = 0)
    
    
    if ("region.filter" %in% names(params)) {
      
      # define correct regions to use
      if ("same" %in% params$region.filter[[1]]) (regions <- unlist(c(df_region[df_region$laestab == id_treated, c("same")])))
      if ("neighbouring" %in% params$region.filter[[1]]) (regions <- unlist(c(df_region[df_region$laestab == id_treated, c("same", "neighbouring")])))
      
      # process data
      df <- process_data_scm(id_treated = id_treated, regions = regions)
      
    }
    
    if ("sd.range" %in% names(params) & !is.null(params$sd.range[[1]])) {
      
      # filter by SD crit
      df <- sd_filtering(data = df, perc = params$sd.range[[1]], var = params$outcome.var)
      df[df$laestab == id_treated, paste0("crit_sd_", params$outcome.var)] <- T
      df <- subset(df, df[, paste0("crit_sd_", params$outcome.var)] == T)
      
    }
    
    if ("rolling.window" %in% names(params)) {
      
      features <- setdiff(params$features[[1]], params$outcome.var)
      
      pre <- df %>%
        filter(time_period %in% params$period.pre[[1]]) %>%
        group_by(laestab) %>%
        arrange(laestab, desc(time_period)) %>%
        mutate(
          across(all_of(features), ~ zoo::rollapply(.x, width = params$rolling.window, mean, align = "left", partial = T)),
          dv_roll = zoo::rollapply(get(params$outcome.var), width = params$rolling.window, mean, align = "left", partial = T)
        )
      
      post <- df %>%
        filter(time_period %in% params$period.post[[1]]) %>%
        group_by(laestab) %>%
        arrange(laestab, desc(time_period)) %>%
        mutate(
          across(all_of(features), ~ zoo::rollapply(.x, width = params$rolling.window, mean, align = "left", partial = T)),
          dv_roll = zoo::rollapply(get(params$outcome.var), width = params$rolling.window, mean, align = "left", partial = T)
        )
      
      df <- bind_rows(pre, post) %>%
        arrange(laestab, desc(time_period)) %>%
        as.data.frame()
    }
    
    # determine ids of control schools
    id_cont <- unique(df$laestab[df$laestab != id_treated])
    params$unit.co = I(list(id_cont)) # update Donors pool
    
    # make sure that weight contraints are defined
    if(! "w.constr" %in% names(params)) {
      params$w.constr <- I(list(list(name = "simplex")))
      params$w.constr.str <- NA
    }
    
    scdata.out <- tryCatch({
      # data preparation
      scdata(df = df, 
             id.var = params$id.var, 
             time.var = params$time.var, 
             outcome.var = params$outcome.var, 
             period.pre = params$period.pre[[1]], 
             period.post = params$period.post[[1]], 
             unit.tr = params$unit.tr[[1]], 
             unit.co = params$unit.co[[1]], 
             features = params$features[[1]], 
             cov.adj = params$cov.adj[[1]], 
             cointegrated.data = params$cointegrated.data[[1]], 
             anticipation = params$anticipation[[1]], 
             constant = params$constant[[1]], 
             verbose = T)
    }, error = function(e) {
      message("Error in scdata: ", e$message)
      return(NULL)
    })
    
    if (is.null(scdata.out)) {
      return(list(status = "scdata() failed",
                  n_pool = length(id_cont),
                  n_active = NA, 
                  sd_treated = sd(df[df$laestab == id_treated & df$time_period %in% params$period.pre[[1]], params$outcome.var], na.rm = T), 
                  m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
                  rmspe_pre = NA, mspe_pre = NA, mae_pre = NA, 
                  rmspe_post = NA, mspe_post = NA, mae_post = NA, 
                  params = params))
    } else { 
      n_pool <- length(scdata.out$specs$donors.units)
    }
    
    scest.out <- tryCatch({
      # estimate synthetic control
      scest(data = scdata.out, 
            w.constr = params$w.constr[[1]]
      )
      
    }, error = function(e) {
      message("Error in scest: ", e$message)
      return(NULL)
    })
    
    if(is.null(scest.out)) {
      return(list(status = "scest() failed",
                  n_pool = length(id_cont),
                  n_active = NA, 
                  sd_treated = sd(df[df$laestab == id_treated & df$time_period %in% params$period.pre[[1]], params$outcome.var], na.rm = T), 
                  m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
                  rmspe_pre = NA, mspe_pre = NA, mae_pre = NA, 
                  rmspe_post = NA, mspe_post = NA, mae_post = NA, 
                  params = params))
    } else {
      # save info on weight constraints
      w.constr <- scest.out$est.results$w.constr
      w.constr <- w.constr[c("name", "p", "lb", "Q", "dir")]
      w.constr <- paste(names(w.constr), w.constr, sep = " = ",collapse = "; " )
      params$w.constr.str <- w.constr
      # save information on number of active donors
      Weights    <- round(scest.out$est.results$w, digits = 3)
      n_active  <- sum(abs(Weights) > 0)
    }
    
    
    if (cv) {
      # Extract the actual and synthetic control outcomes for all years - PRE
      actual_pre <- scest.out$data$Y.pre
      synthetic_pre <- scest.out$est.results$Y.pre.fit
      gap_pre <- actual_pre - synthetic_pre # compute gap as difference between both
      
      # Compute fit - PRE
      rmspe_pre <- sqrt(mean((gap_pre)^2, na.rm = TRUE))
      mspe_pre <- mean((gap_pre)^2, na.rm = TRUE)
      mae_pre <- mean(abs(gap_pre), na.rm = TRUE)
      
      # Extract the actual and synthetic control outcomes for all years - POST
      actual_post <- scest.out$data$Y.post
      synthetic_post <- scest.out$est.results$Y.post.fit
      gap_post <- actual_post - synthetic_post # compute gap as difference between both
      
      # Compute fit - POST
      rmspe_post <- sqrt(mean((gap_post)^2, na.rm = TRUE))
      mspe_post <- mean((gap_post)^2, na.rm = TRUE)
      mae_post <- mean(abs(gap_post), na.rm = TRUE)
      
      rm(actual_post, synthetic_post, gap_post)
      
    } else {
      # Extract the actual and synthetic control outcomes for pre-treatment years only
      actual_pre <- scest.out$data$Y.pre
      synthetic_pre <- scest.out$est.results$Y.pre.fit
      gap_pre <- actual_pre - synthetic_pre # compute gap as difference between both
      
      # Compute fit - PRE
      rmspe_pre <- sqrt(mean((gap_pre)^2, na.rm = TRUE))
      mspe_pre <- mean((gap_pre)^2, na.rm = TRUE)
      mae_pre <- mean(abs(gap_pre), na.rm = TRUE)
      
      # set NA for fit - POST
      rmspe_post <- NA
      mspe_post <- NA
      mae_post <- NA 
    }
    
    
    # compute performance parameters
    sd_treated <- sd(actual_pre, na.rm = TRUE)
    m_gap <- mean(gap_pre, na.rm = TRUE)
    sd_gap <- sd(gap_pre, na.rm = TRUE)
    min_gap <- min(gap_pre, na.rm = TRUE)
    max_gap <- max(gap_pre, na.rm = TRUE)
    cor <- cor(actual_pre, synthetic_pre)[1]
    
    rm(actual_pre, synthetic_pre, gap_pre)
    
    return(list(status = "scest() completed",
                n_pool = n_pool,
                n_active = n_active, 
                sd_treated = sd_treated, 
                m_gap = m_gap, sd_gap = sd_gap, min_gap = min_gap, max_gap = max_gap, cor = cor,
                rmspe_pre = rmspe_pre, mspe_pre = mspe_pre, mae_pre = mae_pre, 
                rmspe_post = rmspe_post, mspe_post = mspe_post, mae_post = mae_post, 
                params = params))
    
  }
  
  if (use_parallel) {
    # Register parallel backend
    num_cores <- detectCores() - 3
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)
    
    # Perform grid search with parallel processing
    results <- foreach(i = 1:nrow(param_grid), .combine = rbind, .packages = c("scpi", "dplyr")) %dopar% {
      params <- param_grid[i, ]
      row.names(params) <- 1
      
      result <- tryCatch({
        run_scm(df, params)
      }, error = function(e) {
        list(status = "run_scm() failed",
             n_pool = length(id_cont),
             n_active = NA, 
             sd_treated = sd(df[df$laestab == id_treated & df$time_period %in% params$period.pre[[1]], params$outcome.var], na.rm = T), 
             m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
             rmspe_pre = NA, mspe_pre = NA, mae_pre = NA,
             rmspe_post = NA, mspe_post = NA, mae_post = NA,
             params = params)
      })
      
      # Create a list to store the results
      result_list <- list(
        outcome.var = ifelse(!is.null(result$params$outcome.var[[1]]), paste(result$params$outcome.var[[1]], collapse = ", "), NA),
        features = ifelse(!is.null(result$params$features[[1]]), paste(result$params$features[[1]], collapse = ", "), NA),
        cov.adj = ifelse(!is.null(result$params$cov.adj[[1]]), 
                         paste(sapply(result$params$cov.adj[[1]], function(x) paste(x, collapse = ", ")), collapse = "; \n"), 
                         NA),
        region.filter = ifelse(!is.null(result$params$region.filter[[1]]), result$params$region.filter[[1]], NA),
        sd.range = ifelse(!is.null(result$params$sd.range[[1]]), result$params$sd.range[[1]], NA),
        rolling.window = ifelse(!is.null(result$params$rolling.window), result$params$rolling.window, NA),
        period.pre = ifelse(!is.null(result$params$period.pre[[1]]), 
                            paste(result$params$period.pre[[1]], collapse = ", "), 
                            NA),
        period.post = ifelse(!is.null(result$params$period.post[[1]]), 
                             paste(result$params$period.post[[1]], collapse = ", "), 
                             NA),
        w.constr = ifelse(!is.null(result$params$w.constr[[1]]), result$params$w.constr.str, NA),
        cointegrated.data = ifelse(!is.null(result$params$cointegrated.data), result$params$cointegrated.data, NA),
        anticipation = ifelse(!is.null(result$params$anticipation), result$params$anticipation, NA),
        constant = ifelse(!is.null(result$params$constant), result$params$constant, NA),
        status = ifelse(is.null(result$status), "run_scm() completed", result$status),
        n_pool = result$n_pool,
        n_active = result$n_active,
        sd_treated = result$sd_treated,
        m_gap = result$m_gap,
        sd_gap = result$sd_gap,
        min_gap = result$min_gap,
        max_gap = result$max_gap,
        cor = result$cor,
        rmspe_pre = result$rmspe_pre,
        mspe_pre = result$mspe_pre,
        mae_pre = result$mae_pre,
        rmspe_post = result$rmspe_post,
        mspe_post = result$mspe_post,
        mae_post = result$mae_post
      )
      
      # Convert the list to a data frame
      result_df <- as.data.frame(result_list, stringsAsFactors = FALSE)
      
      return(result_df)
    }
    
    # Stop the cluster
    stopCluster(cl)
    
  } else {
    # Perform grid search without parallel processing
    results <- do.call(rbind, lapply(1:nrow(param_grid), function(i) {
      message(i)
      params <- param_grid[i, ]
      row.names(params) <- 1
      
      result <- tryCatch({
        run_scm(df, params)
      }, error = function(e) {
        list(status = "run_scm() failed",
             n_pool = length(id_cont),
             n_active = NA, 
             sd_treated = sd(df[df$laestab == id_treated & df$time_period %in% params$period.pre[[1]], params$outcome.var], na.rm = T), 
             m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
             rmspe_pre = NA, mspe_pre = NA, mae_pre = NA,
             rmspe_post = NA, mspe_post = NA, mae_post = NA,
             params = params)
      })
      
      # Create a list to store the results
      result_list <- list(
        outcome.var = ifelse(!is.null(result$params$outcome.var[[1]]), paste(result$params$outcome.var[[1]], collapse = ", "), NA),
        features = ifelse(!is.null(result$params$features[[1]]), paste(result$params$features[[1]], collapse = ", "), NA),
        cov.adj = ifelse(!is.null(result$params$cov.adj[[1]]), 
                         paste(sapply(result$params$cov.adj[[1]], function(x) paste(x, collapse = ", ")), collapse = "; \n"), 
                         NA),
        region.filter = ifelse(!is.null(result$params$region.filter[[1]]), result$params$region.filter[[1]], NA),
        sd.range = ifelse(!is.null(result$params$sd.range[[1]]), result$params$sd.range[[1]], NA),
        rolling.window = ifelse(!is.null(result$params$rolling.window), result$params$rolling.window, NA),
        period.pre = ifelse(!is.null(result$params$period.pre[[1]]), 
                            paste(result$params$period.pre[[1]], collapse = ", "), 
                            NA),
        period.post = ifelse(!is.null(result$params$period.post[[1]]), 
                             paste(result$params$period.post[[1]], collapse = ", "), 
                             NA),
        w.constr = ifelse(!is.null(result$params$w.constr[[1]]), result$params$w.constr.str, NA),
        cointegrated.data = ifelse(!is.null(result$params$cointegrated.data), result$params$cointegrated.data, NA),
        anticipation = ifelse(!is.null(result$params$anticipation), result$params$anticipation, NA),
        constant = ifelse(!is.null(result$params$constant), result$params$constant, NA),
        status = ifelse(is.null(result$status), "run_scm() completed", result$status),
        n_pool = result$n_pool,
        n_active = result$n_active,
        sd_treated = result$sd_treated,
        m_gap = result$m_gap,
        sd_gap = result$sd_gap,
        min_gap = result$min_gap,
        max_gap = result$max_gap,
        cor = result$cor,
        rmspe_pre = result$rmspe_pre,
        mspe_pre = result$mspe_pre,
        mae_pre = result$mae_pre,
        rmspe_post = result$rmspe_post,
        mspe_post = result$mspe_post,
        mae_post = result$mae_post
      )
      
      # Convert the list to a data frame
      result_df <- as.data.frame(result_list, stringsAsFactors = FALSE)
      
      return(result_df)
    }))
  }
  
  return(results)
}

# get scpi summary of estimation 
summarise_scest <- function(object, ...) {
  
  J       <- object$data$specs$J
  M       <- object$data$specs$M
  K       <- object$data$specs$K
  KM      <- object$data$specs$KM
  T0      <- object$data$specs$T0.features
  tr.unit <- colnames(object$data$A)
  pt.in   <- strsplit(rownames(object$data$Y.pre)[1], "\\.")[[1]][2]
  pt.fi   <- strsplit(rownames(object$data$Y.pre)[length(object$data$Y.pre)], "\\.")[[1]][2]  
  pt   <- paste(object$data$specs$period.pre, collapse = ", ")  
  names   <- paste(object$data$specs$features, collapse = ", ")  
  w.cons  <- object$est.results$w.constr[["name"]]
  if (is.null(object$est.results$w.constr[["Q"]])) {
    w.size <- "-"
  } else {
    w.size  <- round(object$est.results$w.constr[["Q"]], 3)
  }
  cat("\n\n")
  cat(paste0("#### Synthetic Control Prediction - Setup \n"))
  cat("\n\n")
  
  cat(paste("Constraint Type:                           ", w.cons, "\n\n"))
  cat(paste("Constraint Size (Q):                       ", w.size, "\n\n"))
  cat(paste("Treated Unit:                              ", tr.unit,"\n\n"))
  cat(paste("Size of the donor pool:                    ", J,"\n\n"))
  cat(paste("Outcome variable:                          ", object$data$specs$outcome.var,"\n\n"))
  cat(paste("Number of features included:               ", M,"\n\n"))
  cat(paste("Name(s) of features:                       ", names,"\n\n"))
  cat(paste("Pre-treatment period:                      ", pt.in,"-",pt.fi,"\n\n"))
  cat(paste("Years included:                            ", pt,"\n\n"))
  
  if (M == 1) {
    cat(paste("Pre-treatment periods used in prediction:  ",T0,"\n\n"))
    cat(paste("Covariates used for adjustment:            ",KM,"\n\n"))
    
  } else {
    cat("Pre-treatment periods used in prediction per feature:\n\n")
    kbl(T0) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T) %>% print()
    cat("\n\n")
    cat("Covariates used for adjustment per feature:\n\n")
    kbl(K) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T) %>% print()
    cat("\n\n")
  }
  cat(paste("Cointegrated data:                         ", object$data$specs$cointegrated.data,"\n\n"))
  cat(paste("Constant:                                  ", object$data$specs$constant,"\n\n"))
  cat(paste("Outcome in features:                       ", object$data$specs$out.in.features,"\n\n"))
  
  Weights    <- round(object$est.results$w, digits = 3)
  
  if (length(object$est.results$r) > 0) {
    Covariates <- round(object$est.results$r, digits = 3)
  }
  active.w  <- sum(abs(Weights) > 0)
  
  cat("\n\n")
  cat("#### Synthetic Control Prediction - Results \n")
  cat("\n\n")
  cat(paste("Active donors:", active.w,"\n\n"))
  cat("\n\n")
  if (length(object$est.results$r) > 0) {
    cat("Coefficients:\n")
    cbind(Covariates) %>% kbl() %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T) %>% print()
  }
  
  print(coef(object))
  cat("\n\n")
  
  
  cat("\n\n")
  cat("#### Synthetic Control Prediction - Fit \n")
  cat("\n\n")
  
  
  # Extract the actual and synthetic control outcomes for all years
  actual <- object$data$Y.pre
  synthetic <- object$est.results$Y.pre.fit
  gap <- actual - synthetic # compute gap as difference between both
  
  
  # compute descriptive stats
  rbind(psych::describe(actual, fast = T), psych::describe(synthetic, fast = T), psych::describe(gap, fast = T)) %>%
    as.data.frame(row.names = c("Treated unit", "Synthetic unit", "Gap")) %>%
    select(-vars) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 2))) %>%
    kbl() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T) %>%
    print()
  cat("\n\n")
  
  # Calculate the RMSPE for all years
  # RMSPE is in the same units as the dependent variable
  rmspe <- sqrt(mean((gap)^2))
  cat(paste("\nRMSPE (Root Mean Squared Prediction Error; in unit of DV):", round(rmspe, 3), "\n\n"))
  cat("\n\n")
  
  # Calculate the MSPE for all years
  # MSPE is in the squared units of the dependent variable
  mspe <- mean((gap)^2)
  cat(paste("\nMSPE (Mean Squared Prediction Error; in squared units of DV):", round(mspe, 3), "\n\n"))
  cat("\n\n")
  
  # Calculate the MAE for all years
  # MAE is in the squared units of the dependent variable
  mae <- mean(abs(gap))
  cat(paste("\nMAE (Mean Absolute Error; in units of DV):", round(mae, 3), "\n\n"))
  cat("\n\n")
  
  ## path plot
  years <- as.numeric(gsub(paste0(id_treated, "."), "", row.names(actual)))
  
  plot(years, actual, 
       t = "l", col = "black", lwd = 2, 
       xaxs = "i", yaxs = "i",
       main = "Outcome trajectories for treated school and its Synthetic Control Unit",
       ylab = "Ratio of pupils to qualified teachers",
       xlab = "Start of academic year",
       ylim = c((min(c(actual, synthetic)) - 0.3 * min(c(actual, synthetic))), (0.3 * max(c(actual, synthetic)) + max(c(actual, synthetic)))))
  
  lines(years, synthetic, col = "black", 
        lty = "dashed", lwd = 2, cex = 4/5)
  
  legend("bottomright", legend=c(id_name, "Synthetic school"),
         col=c("black", "black"), lty=1:2)
  
  ## gaps plot
  plot(years, gap, t = "l", 
       col = "black", lwd = 2, 
       main = "Gap in outcome trajectories",
       ylab = "Gap in ratio of pupils to qualified teachers",
       xlab = "Start of academic year",
       ylim = c(-5, 5), xaxs = "i", yaxs = "i")
  abline(h = 0, col = "black", lty = "dashed", 
         lwd = 2)
  
  # calculate correlation 
  cat("\n\n")
  cat(paste("\n##### Intercorrelation matrix (entries above the diagonal adjusted for multiple tests) \n"))
  tmp <- psych::corr.test(data.frame(actual = actual[,1], synthetic = synthetic[,1], gap = gap[,1]))
  corrplot::corrplot(tmp$r,
                     p.mat = tmp$p,
                     method = "number", tl.pos = "d")
  cat("\n\n")
  
}

# Create a function to compute pairwise correlations for each school
compute_pairwise_correlations <- function(data, vars, new_names) {
  cor_values <- combn(vars, 2, function(x) {
    test_result <- cor.test(data[[x[1]]], data[[x[2]]], use = "complete.obs")
    cor_val <- test_result$estimate
    ci_lower <- test_result$conf.int[1]
    ci_upper <- test_result$conf.int[2]
    return(c(cor_val, ci_lower, ci_upper))
  })
  
  # change output structure to have length(vars) * 3 columns
  cor_df <- as.data.frame(t(as.vector(t(cor_values))))
  # Create column names
  cor_names <- combn(new_names, 2, function(x) paste(x, collapse = " ~ "))
  colnames(cor_df) <- c(paste0(cor_names, " - COR"), paste0(cor_names, " - CI L"), paste0(cor_names, " - CI U"))
  
  return(cor_df)
}

