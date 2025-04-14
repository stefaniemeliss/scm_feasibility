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
      tidyr::replace_na(list(admissionspolicy_name = "unknown", boarders_name = "unknown")) %>% 
      filter(
        !laestab %in% list_laestab_exclude,
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
      tidyr::replace_na(list(admissionspolicy_name = "unknown", boarders_name = "unknown")) %>% 
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
  # and add observation count and count of outliers per school
  z <- z %>%
    select(time_period, laestab, school, pupil_to_qual_teacher_ratio, pupil_to_qual_unqual_teacher_ratio, fte_avg_age) %>%
    filter(!is.na(get(dv))) %>%
    group_by(laestab) %>%
    mutate(
      obs_count_dv = sum(!is.na(get(dv))),
      obs_count_var1 = sum(!is.na(get(var1))),
      count_outliers_dv = sum(is_outlier_3sd(get(dv))),
      count_outliers_var1 = sum(is_outlier_3sd(get(var1)))
    ) %>%
    ungroup() %>%
    as.data.frame()
  
  # Filter for rows where observation count matches the treated ID and select columns
  z <- z %>%
    # Filter for rows where observation count matches the treated ID and select columns
    filter(obs_count_dv == unique(z$obs_count_dv[z$laestab == id_treated])) %>%
    filter(obs_count_var1 == unique(z$obs_count_var1[z$laestab == id_treated])) %>%
    # Remove any schools that have an outlier within their timeseries
    filter(count_outliers_dv == 0) %>%
    filter(count_outliers_var1 == 0) %>%
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
      obs_count_var2 = sum(!is.na(get(var2))),
      count_outliers_var2 = sum(is_outlier_3sd(get(var2)))
    ) %>%
    ungroup()
  
  # Filter for rows where observation count matches the treated ID and select columns
  x <- x %>%
    filter(obs_count_var2 == unique(x$obs_count_var2[x$laestab == id_treated])) %>%
    filter(count_outliers_var2 == 0) %>%
    select(time_period, laestab, urn, pnpupfsm_e) %>%
    arrange(laestab, desc(time_period)) %>%
    as.data.frame()
  
  #### COMBINE OUTCOME AND PREDICTOR ####
  
  df <- merge(z, x, by = c("laestab", "time_period"))
  
  df_treat <- df %>%
    filter(laestab == id_treated)
  
  # Remove outliers from donor pool
  df_donor <- df %>%
    filter(laestab != id_treated) %>%
    mutate(
      outlier_dv = is_outlier_3sd(get(dv)),
      outlier_var1 = is_outlier_3sd(get(var1)),
      outlier_var2 = is_outlier_3sd(get(var2))
    ) %>%
    group_by(laestab) %>%
    mutate(
      count_outliers_dv = sum(outlier_dv),
      count_outliers_var1 = sum(outlier_var1),
      count_outliers_var2 = sum(outlier_var2)
    ) %>%
    ungroup() %>%
    # Remove any schools that have an outlier within their timeseries
    filter(count_outliers_dv == 0) %>%
    filter(count_outliers_var1 == 0) %>%
    filter(count_outliers_var2 == 0) %>%
    select(-outlier_dv, -outlier_var1, -outlier_var2, -count_outliers_dv, -count_outliers_var1, -count_outliers_var2) %>%
    as.data.frame()
  
  # arrange data
  df <- df_treat %>%
    bind_rows(df_donor) %>%
    relocate(urn, .after = laestab) %>%
    arrange(laestab, time_period) %>%
    mutate(
      # # change name to include laestab to navigate duplicates
      # school = paste(laestab, school),
      # add slash and use as string
      time_period_str = insert_slash(time_period),
      # remove the last two digits
      time_period = as.numeric(substr(time_period, 0, 4))
    ) %>%
    as.data.frame()
  
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
    # params <- param_grid[i , ]
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
      
      if ("roll.outcome" %in% names(params) & params$roll.outcome == T) {
        # make sure that outcome is included
        roll.vars <- union(params$features[[1]], params$outcome.var)
      } else {
        # make sure that outcome is NOT included
        roll.vars <- setdiff(params$features[[1]], params$outcome.var)
      }
      
      # add to params
      params$roll.vars <- paste(roll.vars, collapse = ", ")
      
      # apply rolling window average to pre treatment period
      pre <- df %>%
        filter(time_period %in% params$period.pre[[1]]) %>%
        group_by(laestab) %>%
        arrange(laestab, desc(time_period)) %>%
        mutate(
          across(all_of(roll.vars), ~ zoo::rollapply(.x, width = params$rolling.window, mean, align = "left", partial = T))
        )
      
      # apply rolling window average to pre treatment period
      post <- df %>%
        filter(time_period %in% params$period.post[[1]]) %>%
        group_by(laestab) %>%
        arrange(laestab, desc(time_period)) %>%
        mutate(
          across(all_of(roll.vars), ~ zoo::rollapply(.x, width = params$rolling.window, mean, align = "left", partial = T))
        )
      
      # combine rolled data from time time periods
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
      return(list(error = paste("Error in scdata:", e$message)))
    })
    
    if (is.list(scdata.out) && "error" %in% names(scdata.out)) {
      return(list(status = scdata.out$error,
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
      return(list(error = paste("Error in scest:", e$message)))
    })
    
    if (is.list(scest.out) && !is.null(scest.out$error)) {
      # save info on weight constraints
      w.constr <- params$w.constr[[1]]
      
      if (!"name" %in% names(w.constr)) {
        w.constr[["name"]] <- "user provided"
      }
      
      # format weight constraints as string
      w.constr <- w.constr[c("name", "p", "lb", "Q", "dir")]
      w.constr <- paste(names(w.constr), w.constr, sep = " = ", collapse = "; " )
      # add to params
      params$w.constr.str <- w.constr
      
      return(list(status = scest.out$error,
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
      # format weight constraints as string
      w.constr <- w.constr[c("name", "p", "lb", "Q", "dir")]
      w.constr <- paste(names(w.constr), w.constr, sep = " = ", collapse = "; " )
      # add to params
      params$w.constr.str <- w.constr
      
      # save information on number of active donors
      Weights    <- round(scest.out$est.results$w, digits = 3)
      n_active  <- sum(abs(Weights) > 0)
    }
    
    # Extract the actual and synthetic control outcomes for all years - PRE
    actual_pre <- scest.out$data$Y.pre
    synthetic_pre <- scest.out$est.results$Y.pre.fit
    gap_pre <- actual_pre - synthetic_pre # compute gap as difference between both
    
    # Compute fit - PRE
    rmspe_pre <- sqrt(mean((gap_pre)^2, na.rm = TRUE))
    mspe_pre <- mean((gap_pre)^2, na.rm = TRUE)
    mae_pre <- mean(abs(gap_pre), na.rm = TRUE)
    
    if (cv) {
      
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
        return(list(status=paste("Error in run_scm:", e$message),
                    n_pool = length(id_cont),
                    n_active = NA, 
                    sd_treated = sd(df[df$laestab == id_treated & df$time_period %in% params$period.pre[[1]], params$outcome.var], na.rm = T), 
                    m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
                    rmspe_pre = NA, mspe_pre = NA, mae_pre = NA,
                    rmspe_post = NA, mspe_post = NA, mae_post = NA,
                    params = params))
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
        roll.outcome = ifelse(!is.null(result$params$roll.outcome), result$params$roll.outcome, NA),
        roll.vars = ifelse(!is.null(result$params$roll.vars), result$params$roll.vars, NA),
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
        return(list(status=paste("Error in run_scm:", e$message),
                    n_pool = length(id_cont),
                    n_active = NA, 
                    sd_treated = sd(df[df$laestab == id_treated & df$time_period %in% params$period.pre[[1]], params$outcome.var], na.rm = T), 
                    m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
                    rmspe_pre = NA, mspe_pre = NA, mae_pre = NA,
                    rmspe_post = NA, mspe_post = NA, mae_post = NA,
                    params = params))
      })
      
      # Create a list to store the results
      result_list <- list(
        outcome.var = ifelse(!is.null(result$params$outcome.var[[1]]), paste(result$params$outcome.var[[1]], collapse = ", "), NA),
        features = ifelse(!is.null(result$params$features[[1]]), paste(result$params$features[[1]], collapse = ", "), NA),
        cov.adj = ifelse(!is.null(result$params$cov.adj[[1]]),
                         paste(sapply(result$params$cov.adj[[1]], function(x) paste(x, collapse = ", ")), collapse = "; \n"),
                         NA),
        region.filter = ifelse(!is.null(result$params$region.filter[[1]]), paste(result$params$region.filter[[1]], collapse = ", "), NA),
        sd.range = ifelse(!is.null(result$params$sd.range[[1]]), result$params$sd.range[[1]], NA),
        rolling.window = ifelse(!is.null(result$params$rolling.window), result$params$rolling.window, NA),
        roll.outcome = ifelse(!is.null(result$params$roll.outcome), result$params$roll.outcome, NA),
        roll.vars = ifelse(!is.null(result$params$roll.vars), result$params$roll.vars, NA),
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
summarise_scest <- function(object, id_treated, id_name, cv = F) {
  
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
  cat(paste0("###### Synthetic Control Prediction - Setup \n"))
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
  cat("###### Synthetic Control Prediction - Results \n")
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
  cat("###### Synthetic Control Prediction - Fit \n")
  cat("\n\n")
  
  # Extract the actual and synthetic control outcomes for all years - PRE
  actual_pre <- object$data$Y.pre
  synthetic_pre <- object$est.results$Y.pre.fit
  gap_pre <- actual_pre - synthetic_pre # compute gap as difference between both
  years_pre <- as.numeric(gsub(paste0(id_treated, "."), "", row.names(actual_pre)))
  
  # Compute fit - PRE
  rmspe_pre <- sqrt(mean((gap_pre)^2, na.rm = TRUE))
  mspe_pre <- mean((gap_pre)^2, na.rm = TRUE)
  mae_pre <- mean(abs(gap_pre), na.rm = TRUE)
  
  # compute descriptive stats
  rbind(psych::describe(actual_pre, fast = T), psych::describe(synthetic_pre, fast = T), psych::describe(gap_pre, fast = T)) %>%
    as.data.frame(row.names = c("Treated unit", "Synthetic unit", "Gap")) %>%
    select(-vars) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 2))) %>%
    kbl(caption = "Pre-treatment timeseries") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T) %>%
    print()
  cat("\n\n")
  
  # Calculate the RMSPE for all years
  # RMSPE is in the same units as the dependent variable
  cat(paste("\nRMSPE (Root Mean Squared Prediction Error; in unit of DV):", round(rmspe_pre, 3), "\n\n"))
  cat("\n\n")
  
  # Calculate the MSPE for all years
  # MSPE is in the squared units of the dependent variable
  cat(paste("\nMSPE (Mean Squared Prediction Error; in squared units of DV):", round(mspe_pre, 3), "\n\n"))
  cat("\n\n")
  
  # Calculate the MAE for all years
  # MAE is in the squared units of the dependent variable
  cat(paste("\nMAE (Mean Absolute Error; in units of DV):", round(mae_pre, 3), "\n\n"))
  cat("\n\n")
  
  if (cv) {
    
    # Extract the actual and synthetic control outcomes for all years - POST
    actual_post <- object$data$Y.post
    synthetic_post <- object$est.results$Y.post.fit
    gap_post <- actual_post - synthetic_post # compute gap as difference between both
    years_post <- as.numeric(gsub(paste0(id_treated, "."), "", row.names(actual_post)))
    
    # Compute fit - POST
    rmspe_post <- sqrt(mean((gap_post)^2, na.rm = TRUE))
    mspe_post <- mean((gap_post)^2, na.rm = TRUE)
    mae_post <- mean(abs(gap_post), na.rm = TRUE)
    
    # compute descriptive stats
    rbind(psych::describe(actual_post, fast = T), psych::describe(synthetic_post, fast = T), psych::describe(gap_post, fast = T)) %>%
      as.data.frame(row.names = c("Treated unit", "Synthetic unit", "Gap")) %>%
      select(-vars) %>%
      mutate(across(where(is.numeric), ~ round(., digits = 2))) %>%
      kbl(caption = "Post-treatment timeseries") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T) %>%
      print()
    cat("\n\n")
    
    # Calculate the RMSPE for all years
    # RMSPE is in the same units as the dependent variable
    cat(paste("\nRMSPE (Root Mean Squared Prediction Error; in unit of DV):", round(rmspe_post, 3), "\n\n"))
    cat("\n\n")
    
    # Calculate the MSPE for all years
    # MSPE is in the squared units of the dependent variable
    cat(paste("\nMSPE (Mean Squared Prediction Error; in squared units of DV):", round(mspe_post, 3), "\n\n"))
    cat("\n\n")
    
    # Calculate the MAE for all years
    # MAE is in the squared units of the dependent variable
    cat(paste("\nMAE (Mean Absolute Error; in units of DV):", round(mae_post, 3), "\n\n"))
    cat("\n\n")
    
    # combine years
    years <- c(years_pre, years_post)
    actual <- c(actual_pre, actual_post)
    synthetic <- c(synthetic_pre, synthetic_post)
    gap <- c(gap_pre, gap_post)
    
  } else {
    
    # declare pre to be final
    years <- years_pre
    actual <- actual_pre[,1]
    synthetic <- synthetic_pre[,1]
    gap <- gap_pre[,1]
  }
  
  
  ## path plot
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
  cat(paste("\n###### Intercorrelation matrix (entries above the diagonal adjusted for multiple tests) \n"))
  tmp <- psych::corr.test(data.frame(actual = actual, synthetic = synthetic, gap = gap))
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

process_data_scm_mat <- function(uid_treated, target_regions, filter_phase = c("Not applicable"), 
                                 dv = "pupil_to_qual_teacher_ratio", var_teach = "fte_avg_age", var_pup = "pnpupfsm_e",
                                 min_years_obs = 4, min_schools_per_mat = 2, min_schools_per_timeperiod = 2, 
                                 swf_filter = NULL, pup_filter = NULL) {
  # ---- Get info on treated group ----
  id_group <- groups %>% 
    filter(group_uid == uid_treated) %>% 
    pull(group_name) %>% 
    unique()
  
  # ---- Region definition ----
  # Define target regions for filtering the donor pool
  # Create combinations of region names for later filtering
  # This includes individual regions and combined strings with both regions in different orders
  if(length(target_regions) == 1) region_pairing <- c(target_regions, "North West") else region_pairing <- target_regions
  combinations <- c(region_pairing, 
                    paste(region_pairing, collapse = " | "), 
                    paste(rev(region_pairing), collapse = " | "))
  
  # ---- Initial data cleaning ----
  # Remove establishments that have left a group
  groups <- groups %>% 
    filter(is.na(date_left_group))
  
  # Remove special provision schools (where phase is "Not applicable")
  groups <- groups %>%
    filter(!phaseofeducation_name %in% filter_phase)
  
  # ---- MAT and school identification ----
  # Get unique MAT UIDs with schools in the target regions
  list_uid <- groups %>% 
    filter(gor_name %in% target_regions) %>%
    pull(group_uid) %>% 
    unique()
  
  # Get unique LAESTABs (school identifiers) associated with MATs in target regions
  list_laestab <- groups %>% 
    filter(group_uid %in% list_uid) %>%
    pull(laestab) %>% 
    unique()
  
  # Make sure that the schools are not boarding schools or grammar schools
  list_laestab <- est %>%
    tidyr::replace_na(list(admissionspolicy_name = "unknown", boarders_name = "unknown")) %>% 
    filter(laestab %in% list_laestab) %>%
    filter(admissionspolicy_name != "Selective") %>%
    filter(! grepl("Boarding school", boarders_name)) %>%
    pull(laestab) %>%
    unique()
  
  # ---- Dataset creation ----
  # Filter School Workforce (SWF) data to create outcome dataset with selected variables
  z <- swf %>%
    filter(laestab %in% list_laestab)
  
  # Apply additional filter to SWF data if provided
  if (!is.null(swf_filter)) {
    z <- z %>% filter(!!rlang::parse_expr(swf_filter))
  }
  
  z <- z %>% select(time_period, laestab, !!sym(dv), !!sym(var_teach))
  
  # Filter pupil data to create predictor dataset with selected variables
  x <- pup %>% 
    filter(laestab %in% list_laestab)
  
  # Apply additional filter to pupil data if provided
  if (!is.null(pup_filter)) {
    x <- x %>% filter(!!rlang::parse_expr(pup_filter))
  }
  
  x <- x %>% select(time_period, laestab, !!sym(var_pup))
  
  # Combine outcome and predictor datasets
  df <- merge(z, x, all = T, by = c("laestab", "time_period"))
  
  # Remove any rows with missing values
  # This creates complete obs for all vars
  df <- na.omit(df)
  
  # merge with a scaffold so that timeseries is complete again
  df <- merge(expand.grid(laestab = unique(df$laestab),
                          time_period = unique(df$time_period)), 
              df, all = T)
  
  # Add MAT information to the dataset
  # Create lookup table with relevant group information (avoiding duplicates)
  lookup <- groups[laestab %in% list_laestab, c("laestab", "establishmentname", "group_uid", "group_name", "gor_name", "phaseofeducation_name")]
  lookup <- lookup[!duplicated(lookup), ]
  df <- merge(df, lookup, by = "laestab", all.x = T)
  
  # ---- Longitudinal data filtering ----
  
  # Remove any NA years at the beginning of the timeseries
  # Create a cumulative sum of non-NA values starting from the first non-NA value encountered.
  # cum_non_na remain zero as long as the values are NAs
  df <- df %>%
    group_by(laestab) %>%
    # sort ascending by timeseries - BEGINNING
    arrange(time_period) %>%
    mutate(cum_non_na_start = cumsum(!is.na(!!sym(dv)))) %>%
    filter(cum_non_na_start > 0) %>%
    select(-cum_non_na_start) %>%
    ungroup() %>%
    arrange(laestab, time_period)
  
  # Identify schools that do not have gaps at the end of the timeseries
  list_laestab <- df %>%
    group_by(laestab) %>%
    # sort ascending by timeseries - END
    arrange(desc(time_period)) %>%
    mutate(cum_non_na_end = cumsum(!is.na(!!sym(dv)))) %>%
    # check if there are any per laestab
    summarise(
      n = n(),
      cum_non_na_end = sum(cum_non_na_end > 0)
    ) %>%
    # remove if so
    filter(n == cum_non_na_end) %>%
    pull(laestab) %>%
    unique()
  
  # Remove any schools with gaps in their timeseries (i.e., NA in the middle of their data)
  df <- df %>%
    filter(laestab %in% laestab) %>%
    group_by(laestab) %>%
    mutate(na = sum(is.na(!!sym(dv)))) %>%
    ungroup() %>%
    filter(na == 0) %>%
    select(-na)
  
  # Only keep schools with min_years_obs+ years of observations
  df <- df %>% 
    group_by(laestab) %>%
    mutate(n_obs = sum(!is.na(get(dv)))) %>%
    ungroup() %>%
    filter(n_obs >= min_years_obs) %>%
    select(-n_obs)
  
  # Update list of schools to include only those that don't have any missing values in the middle or at the end
  list_laestab <- unique(df$laestab)
  
  # ---- MAT-level filtering ----
  # Identify MATs that meet specific criteria:
  # - Have multiple schools with min_years_obs+ years of data
  # - Schools are only in the specified regions
  # - MATs with schools in the target region combinations
  tmp <- groups %>%
    filter(laestab %in% list_laestab) %>% 
    group_by(group_uid) %>% 
    summarise(
      n_linked = n(),                                # Count schools per MAT
      n_gor = length(unique(gor_name)),              # Count unique regions per MAT
      gor = paste(unique(gor_name), collapse = " | ")) %>%
    ungroup() %>%
    filter(n_linked >= min_schools_per_mat) %>%      # Keep MATs with at least min_schools_per_mat schools
    filter(n_gor <= length(region_pairing)) %>%      # Keep MATs with no more than specified regions
    filter(gor %in% combinations)                    # Keep MATs in target region combinations
  
  # Update list of MAT UIDs based on filtering criteria
  list_uid <- tmp %>%
    pull(group_uid) %>% 
    unique()
  
  # Update dataset to include only schools from filtered MATs
  df <- df %>%
    filter(group_uid %in% list_uid)
  
  # ---- Data aggregation ----
  # Compute MAT-level averages for each time period
  df_avg <- df %>% 
    group_by(group_uid, time_period) %>% 
    summarise(
      !!sym(dv) := mean(!!sym(dv)),
      !!sym(var_teach) := mean(!!sym(var_teach)),
      !!sym(var_pup) := mean(!!sym(var_pup)),
      n = n(), .groups = "drop"
    ) %>% 
    ungroup()
  
  # Keep only MAT-time periods with data from at least min_schools_per_timeperiod schools
  df_avg <- df_avg %>% 
    filter(n >= min_schools_per_timeperiod)
  
  # Determine the number of time periods that make up a complete time series
  # by counting unique time periods in the dataset
  n_complete_timeseries <- length(unique(df_avg$time_period))
  
  # Keep only MATs with complete time series
  list_uid <- df_avg %>% 
    group_by(group_uid) %>% 
    summarise(n = n()) %>%
    ungroup() %>%
    filter(n == n_complete_timeseries) %>% 
    pull(group_uid)
  
  df_avg <- df_avg %>% 
    filter(group_uid %in% list_uid)
  
  # ---- Outlier detection and removal ----
  # Check for within-MAT time series outliers and remove MATs with any outliers
  df_avg <- df_avg %>%
    group_by(group_uid) %>%
    mutate(
      count_outliers_dv = sum(is_outlier_3sd(!!sym(dv))),
      count_outliers_var1 = sum(is_outlier_3sd(!!sym(var_teach))),
      count_outliers_var2 = sum(is_outlier_3sd(!!sym(var_pup)))
    ) %>%
    ungroup() %>%
    mutate(
      status = ifelse(group_uid == uid_treated, id_group, "Donor MATs"),
      # Convert time_period to string with slash (e.g., "2018/19")
      time_period_str = insert_slash(time_period),
      # Extract year only from time_period (e.g., "2018" from "201819")
      time_period = as.numeric(substr(time_period, 0, 4))
    ) %>%
    # Remove any MATs that have an outlier within their timeseries
    filter(count_outliers_dv == 0, count_outliers_var1 == 0, count_outliers_var2 == 0) %>%
    select(-count_outliers_dv, -count_outliers_var1, -count_outliers_var2) %>%
    as.data.frame()
  
  # Extract data for the treated MAT
  df_treat <- df_avg %>%
    filter(group_uid == uid_treated) %>%
    as.data.frame()
  
  # Create donor pool by removing the treated MAT and any MATs with outliers
  df_donor <- df_avg %>%
    filter(group_uid != uid_treated) %>%
    mutate(
      # Here outliers are calculated across all MATs in the donor pool
      outlier_dv = is_outlier_3sd(!!sym(dv)),
      outlier_var1 = is_outlier_3sd(!!sym(var_teach)),
      outlier_var2 = is_outlier_3sd(!!sym(var_pup))
    ) %>%
    group_by(group_uid) %>%
    mutate(
      count_outliers_dv = sum(outlier_dv),
      count_outliers_var1 = sum(outlier_var1),
      count_outliers_var2 = sum(outlier_var2)
    ) %>%
    ungroup() %>%
    # Remove any MATs that have an outlier compared to other MATs
    filter(count_outliers_dv == 0, count_outliers_var1 == 0, count_outliers_var2 == 0) %>%
    select(-outlier_dv, -outlier_var1, -outlier_var2, -count_outliers_dv, -count_outliers_var1, -count_outliers_var2) %>%
    as.data.frame()
  
  # ---- Final dataset preparation ----
  # Combine treated and donor data, format time periods
  df_avg <- df_treat %>%
    bind_rows(df_donor) %>%
    arrange(group_uid, time_period) %>%
    as.data.frame()
  
  # ---- School-level dataset synchronization ----
  # Update the school-level dataset (df) to only include schools from MATs in the final df_avg
  # This ensures consistency between school-level and MAT-level datasets
  df <- df %>%
    filter(group_uid %in% unique(df_avg$group_uid)) %>%
    mutate(
      status = ifelse(group_uid == uid_treated, id_group, "Donor MATs"),
      # Convert time_period to string with slash (e.g., "2018/19")
      time_period_str = insert_slash(time_period),
      # Extract year only from time_period (e.g., "2018" from "201819")
      time_period = as.numeric(substr(time_period, 0, 4))
    ) %>%
    as.data.frame()
  
  # ---- Summary information ----
  # Create summary information about MATs in the pool
  MATs <- df %>% 
    filter(group_uid %in% list_uid) %>%
    group_by(group_uid) %>%
    summarise(
      name = paste(unique(group_name), collapse = " | "),
      schools = length(unique(laestab)),
      phase = paste(unique(phaseofeducation_name), collapse = " | "),
      gor = paste(unique(gor_name), collapse = " | ")) %>%
    ungroup() %>%
    mutate(name = iconv(name, from = "ASCII", to = "UTF-8")) %>%
    as.data.frame()
  
  # Assign dataframes to the global environment
  assign("df", df, envir = .GlobalEnv)
  assign("df_avg", df_avg, envir = .GlobalEnv)
  assign("df_treat", df_treat, envir = .GlobalEnv)
  assign("df_donor", df_donor, envir = .GlobalEnv)
  assign("MATs", MATs, envir = .GlobalEnv)  
  
  # export other values
  assign("id_group", id_group, envir = .GlobalEnv)  
  assign("dv", dv, envir = .GlobalEnv)  
  assign("var_teach", var_teach, envir = .GlobalEnv)  
  assign("var_pup", var_pup, envir = .GlobalEnv)  

  # Return invisible to suppress output but still allow assignment if desired
  invisible(list(df = df, df_avg = df_avg, df_treat = df_treat, df_donor = df_donor, MATs = MATs))
}

# Create function to run grid search
grid_search_scpi_mat <- function(param_grid, sim = F) {
  
  # Define default values for parameters
  default_values <- data.frame(
    id.var = "group_uid", # ID variable
    time.var = "time_period", # Time variable
    period.pre = I(list(2014:2023)), # Pre-treatment period
    period.post = I(list(2024)), # Post-treatment period
    outcome.var = "pupil_to_qual_teacher_ratio", # Outcome variable
    unit.tr = uid_treated, # Treated unit (in terms of id.var)
    unit.co = NA, # Donors pool
    features = I(list(NULL)), # No features other than outcome
    cov.adj = I(list(NULL)), # Covariates for adjustment
    cointegrated.data = FALSE, # don't belief that the data are cointegrated
    anticipation = 0, # No anticipation
    constant = FALSE, # No constant term
    filter.phase = "Not applicable",
    swf.filter = "NULL",
    cross.val = FALSE,
    stringsAsFactors = FALSE
  )
  
  # create empty df - for timeseries export
  df_empty = data.frame(time_period = numeric(1),
                        period = character(1),
                        actual = numeric(1),
                        synthetic = numeric(1),
                        gap = numeric(1)
  )
  
  # debug
  # i = 1
  # params <- param_grid[i , ]
  # row.names(params) <- 1
  
  run_scm <- function(df, params) {
    
    # Merge with parameters in grid
    tmp <- default_values[, setdiff(names(default_values), names(params))]
    params <- merge(params, tmp, by = 0)
    
    # Convert "NULL" string to actual NULL
    swf.filter.param <- if(params$swf.filter == "NULL") NULL else unlist(params$swf.filter)
    params$swf.filter <- unlist(params$swf.filter)
    
    # run processing with the parameters
    data <- tryCatch({
      process_data_scm_mat(uid_treated = uid_treated, 
                           target_regions = unlist(params$regions), 
                           filter_phase = unlist(params$filter.phase),
                           min_years_obs = params$min.years.obs,
                           min_schools_per_mat = params$min.schools.per.mat,
                           min_schools_per_timeperiod = params$min.schools.per.timeperiod,
                           swf_filter = swf.filter.param)
    }, error = function(e) {
      return(list(error = paste("Error in process_data_scm_mat:", e$message)))
    })
    
    
    # Apply more filtering
    tmp <- data$MATs
    tmp$multiple_phases <- grepl(" | ", tmp$phase, fixed = T)
    tmp$multiple_gor <- grepl(" | ", tmp$gor, fixed = T)
    tmp <- create_element_columns(tmp, "gor")
    tmp <- tmp[! tmp$group_uid %in% uid_treated, ]
    if (params$exclude.single.phase) tmp <- tmp[tmp$multiple_phases, ]
    #if (params$exclude.northwest) tmp <- tmp[! (tmp$multiple_gor == F & tmp$gor_north_west == T), ]
    
    # if(sim){
    #   # Simulate data using the timeseries mean #
    #   if(max(params$period.post[[1]]) > max(data$df_avg$time_period)){
    # 
    #     # Repeat the process for each period and combine the results
    #     ave_list <- lapply(params$period.post[[1]], function(period) {
    #       
    #       # determine which columns to do this for
    #       if (params$outcome.var %in% params$features[[1]]) {
    #         cols_to_compute <- params$features[[1]]
    #       } else { cols_to_compute <- c(params$features[[1]], params$outcome.var) }
    #       
    #       data$df_avg %>%
    #         group_by(group_uid) %>%
    #         summarise(across(all_of(cols_to_compute), mean, .names = "{.col}")) %>%
    #         mutate(time_period = period)
    #     })
    #     
    #     # Combine all data frames in the list
    #     ave <- bind_rows(ave_list)
    #     
    #     # Combine the result
    #     data$df_avg <- bind_rows(data$df_avg, ave)
    #     
    #   }
    #   
    # }
    
    if(sim){
      
      df <- data$df
      
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
      
      # Generate *simulations* conditioned on all random effects with noise
      simulations <- simulate(m1, nsim = 1, seed = 202324,
                              newdata = simulated_data, 
                              re.form = NULL, allow.new.levels = FALSE)
      
      # Add predictions to the data frame
      # simulated_data$pred <- predictions
      simulated_data[, dv] <- simulations$sim_1
      
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
        arrange(laestab, time_period) %>%
        as.data.frame()
      
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
        arrange(group_uid, time_period)
      
      # overwrite data
      data$df_avg = df_avg
      
    }
    
    
    # determine ids of control schools
    id_cont <- unique(tmp$group_uid[tmp$group_uid != uid_treated])
    params$unit.co = I(list(id_cont)) # update Donors pool
    
    # make sure that weight contraints are defined
    if(! "w.constr" %in% names(params)) {
      params$w.constr <- I(list(list(name = "simplex")))
      params$w.constr.str <- NA
    }
    
    scdata.out <- tryCatch({
      # data preparation
      scdata(df = data$df_avg, 
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
      return(list(error = paste("Error in scdata:", e$message)))
    })
    
    if (is.list(scdata.out) && "error" %in% names(scdata.out)) {
      params$w.constr.str <- NA
      
      # save info on weight constraints
      w.constr <- params$w.constr[[1]]
      
      if (!"name" %in% names(w.constr)) {
        w.constr[["name"]] <- "user provided"
      }
      
      # format weight constraints as string
      w.constr <- w.constr[c("name", "p", "lb", "Q", "dir")]
      w.constr <- paste(names(w.constr), w.constr, sep = " = ", collapse = "; " )
      # add to params
      params$w.constr.str <- w.constr
      
      return(list(status = scdata.out$error,
                  n_pool = NA,
                  n_active = NA, 
                  sd_treated = NA, 
                  m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
                  rmspe_pre = NA, mspe_pre = NA, mae_pre = NA, 
                  rmspe_post = NA, mspe_post = NA, mae_post = NA,
                  params = params,
                  time_series = df_empty))
    } else { 
      n_pool <- length(scdata.out$specs$donors.units)
    }
    
    scest.out <- tryCatch({
      # estimate synthetic control
      scest(data = scdata.out, 
            w.constr = params$w.constr[[1]]
      )
      
    }, error = function(e) {
      return(list(error = paste("Error in scest:", e$message)))
    })
    
    if (is.list(scest.out) && !is.null(scest.out$error)) {
      # save info on weight constraints
      w.constr <- params$w.constr[[1]]
      
      if (!"name" %in% names(w.constr)) {
        w.constr[["name"]] <- "user provided"
      }
      
      # format weight constraints as string
      w.constr <- w.constr[c("name", "p", "lb", "Q", "dir")]
      w.constr <- paste(names(w.constr), w.constr, sep = " = ", collapse = "; " )
      # add to params
      params$w.constr.str <- w.constr
      
      return(list(status = scest.out$error,
                  n_pool = NA,
                  n_active = NA, 
                  sd_treated = NA, 
                  m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
                  rmspe_pre = NA, mspe_pre = NA, mae_pre = NA, 
                  rmspe_post = NA, mspe_post = NA, mae_post = NA,
                  params = params,
                  time_series = df_empty))
    } else {
      # save info on weight constraints
      w.constr <- scest.out$est.results$w.constr
      # format weight constraints as string
      w.constr <- w.constr[c("name", "p", "lb", "Q", "dir")]
      w.constr <- paste(names(w.constr), w.constr, sep = " = ", collapse = "; " )
      # add to params
      params$w.constr.str <- w.constr
      
      # save information on number of active donors
      Weights    <- round(scest.out$est.results$w, digits = 3)
      n_active  <- sum(abs(Weights) > 0)
    }
    
    # Extract the actual and synthetic control outcomes for all years - PRE
    actual_pre <- scest.out$data$Y.pre
    synthetic_pre <- scest.out$est.results$Y.pre.fit
    gap_pre <- actual_pre - synthetic_pre # compute gap as difference between both
    years_pre <- as.numeric(gsub(paste0(params$unit.tr[[1]], "."), "", row.names(scest.out$data$Y.pre)))
    
    # Compute fit - PRE
    rmspe_pre <- sqrt(mean((gap_pre)^2, na.rm = TRUE))
    mspe_pre <- mean((gap_pre)^2, na.rm = TRUE)
    mae_pre <- mean(abs(gap_pre), na.rm = TRUE)
    
    # Extract the actual and synthetic control outcomes for all years - POST
    actual_post <- scest.out$data$Y.post
    synthetic_post <- scest.out$est.results$Y.post.fit
    gap_post <- actual_post - synthetic_post # compute gap as difference between both... # Compute fit - POST
    years_post <- as.numeric(gsub(paste0(params$unit.tr[[1]], "."), "", row.names(scest.out$data$Y.post)))
    
    # Store time series data
    df_ts <- data.frame(
      time_period = c(years_pre, years_post),
      period = c(rep("pre", length(years_pre)), rep("post", length(years_post))),
      actual = c(actual_pre, actual_post),
      synthetic = c(synthetic_pre, synthetic_post),
      gap = c(gap_pre, gap_post)
    )
    
    if (params$cross.val) {
      
      rmspe_post <- sqrt(mean((gap_post)^2, na.rm = TRUE))
      mspe_post <- mean((gap_post)^2, na.rm = TRUE)
      mae_post <- mean(abs(gap_post), na.rm = TRUE)
      
    } else {
      
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
    
    return(list(status = "scest() completed",
                n_pool = n_pool,
                n_active = n_active, 
                sd_treated = sd_treated, 
                m_gap = m_gap, sd_gap = sd_gap, min_gap = min_gap, max_gap = max_gap, cor = cor,
                rmspe_pre = rmspe_pre, mspe_pre = mspe_pre, mae_pre = mae_pre, 
                rmspe_post = rmspe_post, mspe_post = mspe_post, mae_post = mae_post, 
                params = params,
                time_series = df_ts))
    
  }
  
  # Perform grid search without parallel processing
  results <- lapply(1:nrow(param_grid), function(i) {
    message(i)
    params <- param_grid[i, ]
    row.names(params) <- 1
    
    result <- tryCatch({
      run_scm(df, params)
    }, error = function(e) {
      return(list(error = paste("Error in run_scm:", e$message)))
    })
    
    if (is.list(result) && !is.null(result$error)) {
      
      return(list(status = result$error,
                  n_pool = NA,
                  n_active = NA, 
                  sd_treated = NA, 
                  m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
                  rmspe_pre = NA, mspe_pre = NA, mae_pre = NA,
                  rmspe_post = NA, mspe_post = NA, mae_post = NA, 
                  params = params,
                  time_series = df_empty))
    }
    
    # Create a list to store the results
    result_list <- list(
      it = ifelse(!is.null(result$params$it), result$params$it, NA),
      outcome.var = ifelse(!is.null(result$params$outcome.var[[1]]), paste(result$params$outcome.var[[1]], collapse = ", "), NA),
      features = ifelse(!is.null(result$params$features[[1]]), paste(result$params$features[[1]], collapse = ", "), NA),
      cov.adj = ifelse(!is.null(result$params$cov.adj[[1]]),
                       paste(sapply(result$params$cov.adj[[1]], function(x) paste(x, collapse = ", ")), collapse = "; \n"),
                       NA),
      
      regions = ifelse(!is.null(result$params$regions[[1]]), paste(result$params$regions[[1]], collapse = ", "), NA),
      filter.phase = ifelse(!is.null(result$params$filter.phase[[1]]), paste(result$params$filter.phase[[1]], collapse = ", "), NA),
      swf.filter = ifelse(!is.null(result$params$swf.filter), result$params$swf.filter, NA),
      exclude.single.phase = ifelse(!is.null(result$params$exclude.single.phase), result$params$exclude.single.phase, NA),
      exclude.northwest = ifelse(!is.null(result$params$exclude.northwest), result$params$exclude.northwest, NA),
      excl.outlier = ifelse(!is.null(result$params$excl.outlier), result$params$excl.outlier, NA),
      
      min.years.obs = ifelse(!is.null(result$params$min.years.obs), result$params$min.years.obs, NA),
      min.schools.per.mat = ifelse(!is.null(result$params$min.schools.per.mat), result$params$min.schools.per.mat, NA),
      min.schools.per.timeperiod = ifelse(!is.null(result$params$min.schools.per.timeperiod), result$params$min.schools.per.timeperiod, NA),
      
      period.pre = ifelse(!is.null(result$params$period.pre[[1]]),
                          paste(result$params$period.pre[[1]], collapse = ", "),
                          NA),
      period.post = ifelse(!is.null(result$params$period.post[[1]]),
                           paste(result$params$period.post[[1]], collapse = ", "),
                           NA),
      cross.val = ifelse(!is.null(result$params$cross.val), result$params$cross.val, NA),
      
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
    df_result <- as.data.frame(result_list, stringsAsFactors = FALSE)
    
    # Add run ID to data frame for identification
    df_result$run_id <- i
    
    # Combine results and timeseries
    df_ts <- merge(df_result, result$time_series) # this is either df_ts or df_empty

    return(list(results = df_result, timeseries = df_ts))
  })
  
  # Extract and combine all results into two separate data frames
  all_results <- do.call(rbind, lapply(results, function(x) x$results))
  all_timeseries <- do.call(rbind, lapply(results, function(x) x$timeseries))
  
  # Final output structure
  results <- list(
    results = all_results,
    timeseries = all_timeseries
  )
  
  return(results)
}

extract_years_from_filter <- function(swf_filter) {
  # Split the unique values by "&" and unlist the result
  filters <- unlist(strsplit(as.character(swf_filter), " & ", fixed = TRUE))
  
  # Filter unique values that contain "time_period"
  filters <- unique(filters[grepl("time_period", filters)])
  
  # Extract years using stringr
  years_excl <- unlist(stringr::str_extract_all(filters, "\\d+"))
  years_excl <- as.numeric(substr(years_excl, 0, 4))
  
  return(years_excl)
}

# Function to calculate period.pre
calculate_period_pre <- function(period_avail, period_post, period_excl) {
  setdiff(period_avail, union(period_post, period_excl))
}

analyse_missing_values <- function(data, group_col = "laestab", time_col = "time", value_col = "value") {
  results <- list()
  
  # Process each group
  for (id in unique(data[[group_col]])) {
    group <- data[data[[group_col]] == id, ]
    
    # Sort by time column to ensure correct sequence
    group <- group[order(group[[time_col]]), ]
    
    total_observations <- nrow(group)
    missing_count <- sum(is.na(group[[value_col]]))
    has_missing <- missing_count > 0
    
    # Find indices
    non_na_indices <- which(!is.na(group[[value_col]]))
    na_indices <- which(is.na(group[[value_col]]))
    
    first_non_na_idx <- if(length(non_na_indices) > 0) min(non_na_indices) else NA
    last_non_na_idx <- if(length(non_na_indices) > 0) max(non_na_indices) else NA
    
    first_na_idx <- if(length(na_indices) > 0) min(na_indices) else NA
    last_na_idx <- if(length(na_indices) > 0) max(na_indices) else NA
    
    first_non_na_after_first_na <- if(has_missing && !is.na(first_na_idx)) {
      candidates <- non_na_indices[non_na_indices > first_na_idx]
      if(length(candidates) > 0) min(candidates) else NA
    } else NA
    
    # Determine missing regions
    missing_at_beginning <- !is.na(first_na_idx) && first_na_idx == 1
    missing_at_end <- !is.na(last_na_idx) && last_na_idx == total_observations
    
    # Check for missing in middle
    missing_in_middle <- FALSE
    if (has_missing) {
      # If there are NAs not at the beginning and not at the end, they must be in the middle
      middle_nas <- na_indices[na_indices > 1 & na_indices < total_observations]
      if (length(middle_nas) > 0) {
        # Check if these middle NAs are after the first non-NA and before the last non-NA
        if (!is.na(first_non_na_idx) && !is.na(last_non_na_idx)) {
          middle_nas_between_non_nas <- middle_nas[middle_nas > first_non_na_idx & middle_nas < last_non_na_idx]
          missing_in_middle <- length(middle_nas_between_non_nas) > 0
        } else {
          missing_in_middle <- TRUE
        }
      }
    }
    
    # Categorize pattern - exactly matching the 8 scenarios
    if (missing_count == 0) {
      pattern <- "No missing values"
    } else if (missing_at_beginning && missing_in_middle && missing_at_end) {
      pattern <- "Missing values at beginning, middle, and end"
    } else if (missing_in_middle && missing_at_end && !missing_at_beginning) {
      pattern <- "Missing values in middle and end"
    } else if (missing_at_beginning && missing_at_end && !missing_in_middle) {
      pattern <- "Missing values at beginning and end"
    } else if (missing_at_end && !missing_at_beginning && !missing_in_middle) {
      pattern <- "Missing values at end"
    } else if (missing_at_beginning && missing_in_middle && !missing_at_end) {
      pattern <- "Missing values at beginning and middle"
    } else if (missing_in_middle && !missing_at_beginning && !missing_at_end) {
      pattern <- "Missing values in middle"
    } else if (missing_at_beginning && !missing_in_middle && !missing_at_end) {
      pattern <- "Missing values at beginning"
    } else {
      pattern <- "Complex pattern not matching predefined scenarios"
    }
    
    # Store results
    results[[length(results) + 1]] <- list(
      group_id = id,
      total_observations = total_observations,
      missing_count = missing_count,
      has_missing = has_missing,
      first_non_na_idx = first_non_na_idx,
      last_non_na_idx = last_non_na_idx,
      first_na_idx = first_na_idx,
      last_na_idx = last_na_idx,
      first_non_na_after_first_na = first_non_na_after_first_na,
      missing_pattern = pattern
    )
  }
  
  # Convert list to data frame
  result_df <- do.call(rbind, lapply(results, as.data.frame))
  
  # Rename the group_id column to match the input group_col name
  names(result_df)[names(result_df) == "group_id"] <- group_col
  
  return(result_df)
}

# Function to extract min and max year from a string
extract_min_max_years <- function(period_string) {
  years <- as.numeric(unlist(strsplit(period_string, ", ")))
  min_year <- min(years)
  max_year <- max(years)
  return(c(min_year, max_year))
}