# source ambition theme
tempp_file <- tempfile(fileext = ".R")
message <- curl::curl_download("https://raw.githubusercontent.com/stefaniemeliss/ambition_theme/main/ambition_theme.R", tempp_file, quiet = F)

if(!grepl("Error", message)) {
  source(tempp_file)
  remove(tempp_file)
} else {
  source(file.path(gsub("scm_feasibility", "ambition_theme", dir), "ambition_theme.R"), local = T) 
}


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
    
    tmp <- default_values[, setdiff(names(default_values), names(params))]
    params <- merge(params, tmp, by = 0)
    
    if(! "w.constr" %in% names(params)) (params$w.constr <- I(list(name = "simplex")))
    
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
                                         rmspe = "scdata() failed", mspe = NA, mae = NA, #loss_v= NA, loss_w = NA,
                                         params = params))
    
    scest.out <- tryCatch({
      # estimate synthetic control
      scest(data = scdata.out, 
            w.constr = params$w.constr[[1]]
      )
    }, error = function(e) {
      message("Error in scest: ", e$message)
      return(NULL)
    })
    
    if (is.null(scest.out)) return(list(sd_treated = NA, m_gap = NA, sd_gap = NA, min_gap = NA, max_gap = NA, cor = NA,
                                        rmspe = "scest() failed", mspe = NA, mae = NA, #loss_v= NA, loss_w = NA,
                                        params = params))
    
    if (cv) {
      # Extract the actual and synthetic control outcomes for all years
      actual <- scdata.out$Y.post
      synthetic <- scest.out$est.results$Y.post.fit
    } else {
      # Extract the actual and synthetic control outcomes for all years
      actual <- scdata.out$Y.pre
      synthetic <- scest.out$est.results$Y.pre.fit
    }
    
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
    # loss_v <- synth.out$loss.v[1]
    # loss_w <- synth.out$loss.w[1]
    
    rm(actual, synthetic, gap)
    
    return(list(sd_treated = sd_treated, m_gap = m_gap, sd_gap = sd_gap, min_gap = min_gap, max_gap = max_gap, cor = cor,
                rmspe = rmspe, mspe = mspe, mae = mae, #loss_v= loss_v, loss_w = loss_w,
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
             rmspe = "run_scm() failed", mspe = NA, mae = NA, #loss_v= NA, loss_w = NA,
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
        w.constr = ifelse(!is.null(result$params$w.constr[[1]][[1]]), result$params$w.constr[[1]][[1]], NA),
        anticipation = ifelse(!is.null(result$params$anticipation), result$params$anticipation, NA),
        constant = ifelse(!is.null(result$params$constant), result$params$constant, NA),
        sd_treated = result$sd_treated,
        m_gap = result$m_gap,
        sd_gap = result$sd_gap,
        min_gap = result$min_gap,
        max_gap = result$max_gap,
        cor = result$cor,
        rmspe = result$rmspe,
        mspe = result$mspe,
        mae = result$mae#,
        # loss_v = result$loss_v,
        # loss_w = result$loss_w
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
             rmspe = "run_scm() failed", mspe = NA, mae = NA, #loss_v= NA, loss_w = NA,
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
        w.constr = ifelse(!is.null(result$params$w.constr[[1]][[1]]), result$params$w.constr[[1]][[1]], NA),
        anticipation = ifelse(!is.null(result$params$anticipation), result$params$anticipation, NA),
        constant = ifelse(!is.null(result$params$constant), result$params$constant, NA),
        sd_treated = result$sd_treated,
        m_gap = result$m_gap,
        sd_gap = result$sd_gap,
        min_gap = result$min_gap,
        max_gap = result$max_gap,
        cor = result$cor,
        rmspe = result$rmspe,
        mspe = result$mspe,
        mae = result$mae#,
        # loss_v = result$loss_v,
        # loss_w = result$loss_w
      )
      
      # Convert the list to a data frame
      result_df <- as.data.frame(result_list, stringsAsFactors = FALSE)
      
      return(result_df)
    }))
  }
  
  return(results)
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
  w.cons  <- object$est.results$w.constr[["name"]]
  if (is.null(object$est.results$w.constr[["Q"]])) {
    w.size <- "-"
  } else {
    w.size  <- round(object$est.results$w.constr[["Q"]], 3)
  }
  cat("\n")
  cat(paste0("Synthetic Control Prediction - Setup\n"))
  cat("\n")
  
  cat(paste("Constraint Type:                           ", w.cons, "\n", sep = ""))
  cat(paste("Constraint Size (Q):                       ", w.size, "\n", sep = ""))
  cat(paste("Treated Unit:                              ", tr.unit,"\n", sep = ""))
  cat(paste("Size of the donor pool:                    ", J,"\n", sep = ""))
  cat(paste("Outcome variable:                          ", object$data$specs$outcome.var,"\n", sep = ""))
  cat(paste("Number of features included:               ", M,"\n", sep = ""))
  cat(paste("Name(s) of features:                       ", object$data$specs$features,"\n", sep = ""))
  cat(paste("Pre-treatment period:                      ", pt.in,"-",pt.fi,"\n", sep = ""))
  cat(paste("Years included:                            ", pt,"\n", sep = ""))
  
  if (M == 1) {
    cat(paste("Pre-treatment periods used in prediction:  ",T0,"\n", sep = ""))
    cat(paste("Covariates used for adjustment:            ",KM,"\n", sep = ""))
    
  } else {
    cat("Pre-treatment periods used in prediction per feature:\n")
    print(T0)
    cat("Covariates used for adjustment per feature:\n")
    print(K)
  }
  cat(paste("Cointegrated data:                         ", object$data$specs$cointegrated.data,"\n", sep = ""))
  cat(paste("Constant:                                  ", object$data$specs$constant,"\n", sep = ""))
  cat(paste("Outcome in features:                       ", object$data$specs$out.in.features,"\n", sep = ""))
  
  Weights    <- round(object$est.results$w, digits = 3)
  
  if (length(object$est.results$r) > 0) {
    Covariates <- round(object$est.results$r, digits = 3)
  }
  active.w  <- sum(abs(Weights) > 0)
  
  cat("\n")
  cat("Synthetic Control Prediction - Results\n")
  cat("\n")
  cat(paste("Active donors:", active.w,"\n"))
  cat("\n")
  if (length(object$est.results$r) > 0) {
    cat("Coefficients:\n")
    print(cbind(Covariates), col.names = FALSE)
  }
}
