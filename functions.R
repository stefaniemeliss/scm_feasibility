# source ambition theme
devtools::source_url("https://github.com/stefaniemeliss/ambition_theme/blob/main/ambition_theme.R?raw=TRUE")

# combine to palette
ambition_palette_bright <- c(cyan, coral, teal, purple, orange) # bright palette
ambition_palette_accent <- c(yellow, blue, red)
ambition_palette <- c(coral, teal, purple, orange, blue, red, cyan, yellow) # de-prioritise cyan and yellow

# declare dominant and non-dominant colour in plots
dominant_col <- coral
nondominant_col <- navy

# functions for data display #

table_desc <- function(data = df, group_var = "group", dep_var = "variable"){
  
  out <- rbind(
    psych::describe(data[, dep_var]), # get descriptives whole sample
    do.call("rbind",psych::describeBy(data[, dep_var], group = data[, group_var])) # get descriptives per group
  )
  # edit output
  out$vars <- NULL
  rownames(out)[1] <- "all"
  out <- round(out, 3)
  # print output
  kbl(out, caption = paste0("Descriptives of variable '", dep_var,"' for whole sample and within each group")) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T) %>% 
    print()
  cat("\n")
}

# functions for data processing #

# function to determine outliers
is_outlier_iqr <- function(x) {
  # +/- 1.5*IQR
  return(x < quantile(x, 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T) | x > quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T))
}

# Function to insert "/" in the format "YYYYYY" to "YYYY/YY"
insert_slash <- function(number) {
  sub("(\\d{4})(\\d{2})", "\\1/\\2", number)
}

# rbind all columns
rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
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
grid_search_scm <- function(df, param_grid, treatment_identifier, dependent_var, 
                            unit_var, time_var, unit_names_var, use_parallel = TRUE) {
  
  # Define default values for parameters
  default_values <- data.frame(
    predictors = I(list(c("pnpupfsm_e", "pnpupeal"))),
    predictors_op = "median",
    special_predictors = I(list(NULL)),
    time_predictors_prior = I(list(2014:2023)),
    optimxmethod = "BFGS",
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
    
    if (is.null(dataprep.out)) return(list(rmspe = "dataprep() failed", sd_treated = NA, mspe = NA, params = params))
    
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
    
    if (is.null(synth.out)) return(list(rmspe = "synth() failed", sd_treated = NA, mspe = NA, params = params))
    
    actual <- dataprep.out$Y1plot
    synthetic <- dataprep.out$Y0plot %*% synth.out$solution.w
    
    rmspe <- sqrt(mean((actual - synthetic)^2, na.rm = TRUE))
    sd_treated <- sd(actual, na.rm = TRUE)
    mspe <- mean((actual - synthetic)^2, na.rm = TRUE)
    
    return(list(rmspe = round(rmspe, 3), sd_treated = round(sd_treated, 3), mspe = round(mspe, 3), params = params))
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
        list(rmspe = "run_scm() failed", sd_treated = NA, mspe = NA, params = params)
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
        rmspe = result$rmspe,
        sd_treated = result$sd_treated,
        mspe = result$mspe
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
        list(rmspe = "run_scm() failed", sd_treated = NA, mspe = NA, params = params)
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
        rmspe = result$rmspe,
        sd_treated = result$sd_treated,
        mspe = result$mspe
      )
      
      # Convert the list to a data frame
      result_df <- as.data.frame(result_list, stringsAsFactors = FALSE)
      
      return(result_df)
    }))
  }
  
  return(results)
}
