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