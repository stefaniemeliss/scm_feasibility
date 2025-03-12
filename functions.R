#### ALL-PURPOSE HELPER FUNCTIONS ####

# source code
source_code(target_repo = "helper_functions", file_name = "functions.R")

#### PROJECT-SPECIFIC FUNCTIONS ####

# functions for data processing #

# Function to insert "/" in the format "YYYYYY" to "YYYY/YY"
insert_slash <- function(number) {
  sub("(\\d{4})(\\d{2})", "\\1/\\2", number)
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
    unit.co = I(list(id_cont)), # Donors pool
    features = I(list(NULL)), # No features other than outcome
    cov.adj = I(list(NULL)), # Covariates for adjustment
    cointegrated.data = FALSE, # don't belief that the data are cointegrated
    anticipation = 0, # No anticipation
    constant = FALSE, # No constant term
    stringsAsFactors = FALSE
  )
  
  run_scm <- function(df, params) {
    
    # debug
    # params <- param_grid[1, ]
    # row.names(params) <- 1
    
    tmp <- default_values[, setdiff(names(default_values), names(params))]
    params <- merge(params, tmp, by = 0)
    
    if(! "w.constr" %in% names(params)) (params$w.constr <- I(list(list(name = "simplex"))))
    
    
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
    
    if (is.null(scdata.out)) return(list(sd_treated = NA, m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
                                         rmspe_pre = "scdata() failed", mspe_pre = NA, mae_pre = NA, 
                                         rmspe_post = NA, mspe_post = NA, mae_post = NA, 
                                         params = params))
    
    scest.out <- tryCatch({
      # estimate synthetic control
      scest(data = scdata.out, 
            w.constr = params$w.constr[[1]]
      )
      # save info on weight constraints
      w.constr <- scest.out$est.results$w.constr
      w.constr <- w.constr[c("name", "p", "lb", "Q", "dir")]
      w.constr <- paste(names(w.constr), w.constr, sep = " = ",collapse = "; " )
      
    }, error = function(e) {
      message("Error in scest: ", e$message)
      return(NULL)
    })
    
    if (is.null(scest.out)) return(list(sd_treated = NA, m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
                                        rmspe_pre = "scest() failed", mspe_pre = NA, mae_pre = NA, 
                                        rmspe_post = NA, mspe_post = NA, mae_post = NA, 
                                        params = params))
    
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
    
    return(list(sd_treated = sd_treated, m_gap = m_gap, sd_gap = sd_gap, min_gap = min_gap, max_gap = max_gap, cor = cor,
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
        
        list(sd_treated = NA, m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
             rmspe_pre = "run_scm() failed", mspe_pre = NA, mae_pre = NA, 
             rmspe_post = NA, mspe_post = NA, mae_post = NA, 
             params = params)
      })
      
      # Create a list to store the results
      result_list <- list(
        features = ifelse(!is.null(result$params$features[[1]]), paste(result$params$features[[1]], collapse = ", "), NA),
        cov.adj = ifelse(!is.null(result$params$cov.adj[[1]]), 
                         paste(sapply(result$params$cov.adj[[1]], function(x) paste(x, collapse = ", ")), collapse = "; \n"), 
                         NA),
        cointegrated.data = ifelse(!is.null(result$params$cointegrated.data), result$params$cointegrated.data, NA),
        period.pre = ifelse(!is.null(result$params$period.pre[[1]]), 
                            paste(result$params$period.pre[[1]], collapse = ", "), 
                            NA),
        period.post = ifelse(!is.null(result$params$period.post[[1]]), 
                             paste(result$params$period.post[[1]], collapse = ", "), 
                             NA),
        w.constr = ifelse(!is.null(result$params$w.constr[[1]]), w.constr, NA),
        anticipation = ifelse(!is.null(result$params$anticipation), result$params$anticipation, NA),
        constant = ifelse(!is.null(result$params$constant), result$params$constant, NA),
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
      params <- param_grid[i, ]
      row.names(params) <- 1
      
      result <- tryCatch({
        run_scm(df, params)
      }, error = function(e) {
        list(sd_treated = NA, m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
             rmspe_pre = "run_scm() failed", mspe_pre = NA, mae_pre = NA, 
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
        cointegrated.data = ifelse(!is.null(result$params$cointegrated.data), result$params$cointegrated.data, NA),
        period.pre = ifelse(!is.null(result$params$period.pre[[1]]), 
                            paste(result$params$period.pre[[1]], collapse = ", "), 
                            NA),
        period.post = ifelse(!is.null(result$params$period.post[[1]]), 
                             paste(result$params$period.post[[1]], collapse = ", "), 
                             NA),
        w.constr = ifelse(!is.null(result$params$w.constr[[1]]), w.constr, NA),
        anticipation = ifelse(!is.null(result$params$anticipation), result$params$anticipation, NA),
        constant = ifelse(!is.null(result$params$constant), result$params$constant, NA),
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

