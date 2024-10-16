# source ambition theme
devtools::source_url("https://github.com/stefaniemeliss/ambition_theme/blob/main/ambition_theme.R?raw=TRUE")

# combine to palette
ambition_palette_bright <- c(cyan, coral, teal, purple, orange) # bright palette
ambition_palette_accent <- c(yellow, blue, red)
ambition_palette <- c(coral, teal, purple, orange, blue, red, cyan, yellow) # de-prioritise cyan and yellow

# declare dominant and non-dominant colour in plots
dominant_col <- coral
nondominant_col <- navy

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

# function to determine outliers
is_outlier_iqr <- function(x) {
  # +/- 1.5*IQR
  return(x < quantile(x, 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T) | x > quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T))
}


# these functions have been found online here https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/

# The basics steps
# 1. Specify the input dataframes
# 2. Calculate which dataframe has the greatest number of columns
# 3. Identify which columns in the smaller dataframe match the columns in the larger dataframe
# 4. Create a vector of the column names that occur in both dataframes
# 5. Combine the data from both dataframes matching the listed column names using rbind
# 6. Return the combined data

# rbind matching columns
rbind.match.columns <- function(input1, input2) {
  n.input1 <- ncol(input1)
  n.input2 <- ncol(input2)
  
  if (n.input2 < n.input1) {
    TF.names <- which(names(input2) %in% names(input1))
    column.names <- names(input2[, TF.names])
  } else {
    TF.names <- which(names(input1) %in% names(input2))
    column.names <- names(input1[, TF.names])
  }
  
  return(rbind(input1[, column.names], input2[, column.names]))
}

# rbind all columns
rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}



merge_timelines_across_columns <- function(data_in = df_in,
                                           column_vector = "cols_to_merge",
                                           stem = "new_var", 
                                           identifier_columns = "id_cols",
                                           data_out = df_out) {
  
  # select columns
  tmp <- data_in[, c(identifier_columns, column_vector)]
  #tmp <- data_in[, c(get(identifier_columns), get(column_vector))]
  
  # replace any NAs with ""
  tmp[, column_vector] <- apply(tmp[, column_vector], 2, function(x) {ifelse(is.na(x), "", x)})
  
  # merge information across cols using paste
  tmp[, "tmp"] = apply(tmp[, column_vector, drop = F], MARGIN = 1, FUN = function(i) paste(i, collapse = ""))
  
  # create column that contains tag with information about the column data retained
  tmp[, column_vector] <- apply(tmp[, column_vector], 2, function(x) {ifelse(x != "", "true", "")}) # replace values with "true"
  w <- which(tmp=="true",arr.ind=TRUE) # get indices of "true"
  tmp[w] <- names(tmp)[w[,"col"]] # replace "true" with column name
  tmp[, "tag"] = apply(tmp[, column_vector, drop = F], MARGIN = 1, FUN = function(i) paste(i, collapse = "")) # merge across
  
  # drop columns that are now merged
  tmp[, column_vector] <- NULL
  
  # replace "" with NA
  tmp[, c(-1, -2)] <- apply(tmp[, c(-1, -2)], 2, function(x) {ifelse(x == "", NA, x)})
  
  # change names
  names(tmp) <- c(identifier_columns, stem, paste0(stem, "_tag"))
  
  # merge with data_out
  data_out <- merge(data_out, tmp, by = identifier_columns, all = T)
  
  return(data_out)
}
