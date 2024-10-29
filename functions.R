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
  tmp[, "tmp"] <- apply(tmp[, column_vector, drop = F], MARGIN = 1, FUN = function(i) paste(i, collapse = ""))

  # create column that contains tag with information about the column data retained
  tmp[, column_vector] <- apply(tmp[, column_vector], 2, function(x) {ifelse(x != "", "true", "")}) # replace values with "true"
  w <- which(tmp=="true",arr.ind=TRUE) # get indices of "true"
  tmp[w] <- names(tmp)[w[,"col"]] # replace "true" with column name
  tmp[, "tag"] = apply(tmp[, column_vector, drop = F], MARGIN = 1, FUN = function(i) paste(i, collapse = "")) # merge across
  
  # drop columns that are now merged
  tmp[, column_vector] <- NULL
  
  # replace "" with NA
  tmp[, c(-1, -2)] <- apply(tmp[, c(-1, -2)], 2, function(x) {ifelse(x == "", NA, x)})
  
  # make new variable numeric
  tmp[, "tmp"] <- as.numeric(tmp[, "tmp"])
  
  # change names
  names(tmp) <- c(identifier_columns, stem, paste0(stem, "_tag"))
  
  # merge with data_out
  data_out <- merge(data_out, tmp, by = identifier_columns, all = T)
  
  return(data_out)
}


merge_staggered_timelines_across_columns <- function(data_in = df_in,
                                                     column_vector = "cols_to_merge",
                                                     stem = "new_var", 
                                                     variable_levels = "new_levels",
                                                     identifier_columns = "id_cols",
                                                     data_out = df_out) {
  
  # select columns
  tmp <- data_in[, c(identifier_columns, column_vector)]
  
  # determine mapping
  mapping <- data.frame(old = column_vector,
                        new = variable_levels)
  cat("Applied mapping from column_vector to variable_levels:\n\n")
  print(mapping)
  
  tag = paste0(stem, "_tag")
  
  # use dplyr
  tmp <- tmp %>%
    # apply grouping by identifier variable
    group_by(.data[[identifier_columns]]) %>%
    # replace every NA with the unique value observed for each group
    mutate_at(column_vector, function(x) {ifelse(is.na(x), unique(x[!is.na(x)]), x)}) %>%
    # remove all duplicated columns
    distinct(., .keep_all = TRUE) %>%
    
    # transform into long format
    reshape2::melt(id = identifier_columns, variable.name = tag, value.name = stem) %>%
    # change variable levels
    mutate(time_period = plyr::mapvalues(get(tag), column_vector, variable_levels, warn_missing = TRUE))
  
  
  # merge with data_out
  data_out <- merge(data_out, tmp, by = id_cols, all = T)
  rm(tmp)
  
  return(data_out)
}


### web scraping ###

# Function to identify year of release
get_year <- function(input_url){

  # Split the URLs into parts
  parts <- unlist(strsplit(input_url, "[[:punct:]]"))
  
  # Find the year indices
  idx <- grep("(^20[0-2][0-9]$|^2[0-9]$)", parts)
  
  # check if it contains a year
  if (identical(idx, integer(0)) == F) {
    # if so, return year
    year <- paste(parts[idx], collapse = "-")
  } else {
    year <- character(0)
  }
  
  return(year)
}

assign_dir_year <- function(x, input_url = "url") assign(x, file.path(dir_out, get_year(input_url)),envir=globalenv())

# functions
is.sequential <- function(x){
  all(abs(diff(x)) == 1)
} 

# Function to handle overlapping parts and convert relative URLs to absolute URLs
resolve_url <- function(base_url, relative_url) {
  if (!grepl("^http", relative_url)) {  # Check if the link is not absolute
    # Remove the trailing slash from the base URL if it exists
    base_url <- sub("/$", "", base_url)
    
    # Remove the leading slash from the relative URL if it exists
    relative_url <- sub("^/", "", relative_url)
    
    # Split the URLs into parts
    base_parts <- unlist(strsplit(base_url, "/"))
    relative_parts <- unlist(strsplit(relative_url, "/"))
    
    # Find the index where the overlapping part starts
    overlap_index <- which(base_parts %in% relative_parts)
    
    if (is.sequential(overlap_index)) {
      # Find the first non-overlapping parts in the base URL
      pre_overlap_base <- min(overlap_index) - 1
      base_unique <- base_parts[1:pre_overlap_base]
      
      # Find the overlapping parts in the base URL
      base_overlap <- base_parts[overlap_index]
      
      # Find the last non-overlapping parts in the relative URL
      relative_unique <- relative_parts[! relative_parts %in% base_parts]
      
      # Combine the base URL with the overlapping and non-overlapping part of the relative URL 
      absolute_url <- paste0(paste(base_unique, collapse = "/"), "/", paste(base_overlap, collapse = "/"), "/", paste(relative_unique, collapse = "/"))
    } else {
      absolute_url <- paste0(base_url, "/", relative_url)
    }
    return(absolute_url)
  } else {
    return(relative_url)
  }
}

# function to download data from an URL that directly links to a file
download_data_from_url <- function(url){
  
  # determine header information
  headers = c(
    `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
  )
  
  # retrieve information from URL 
  request <- httr::GET(url = url, httr::add_headers(.headers=headers))
  # request <- httr::GET(url = url_meta, httr::add_headers(.headers=headers))
  # request <- httr::GET(url = url_data, httr::add_headers(.headers=headers))
  
  # check for file name information
  input = request$headers$`content-disposition` # e.g., "attachment; filename=Performancetables_114742.zip; filename*=UTF-8''Performancetables_114742.zip"
  if (grepl("'", input, perl = T)) {
    tmp <- sub(".*'", "", input) # remove everything before '
    tmp <- sub("%2F", "_", tmp)
   } else { # if (grepl('[^\"]', input, perl = T)) { # [^\"] = \
     tmp <- sub('[^\"]+\"([^\"]+).*', '\\1', input)
   }
  tmp <- ifelse(nchar(tmp) > 100, gsub("_20", "", tmp), tmp) # replace if filename is too long
  
  # check if higher level variable dir_year exists in environment
  if (!exists("dir_year")){
    
    # get year from url
    assign_dir_year("dir_year_data", url)
    
    # if url does not contain a year
    if (identical(dir_year_data, character(0)) == T) {
      
      # check headers$`content-disposition`
      assign_dir_year("dir_year_data", tmp)
    }
    
  }
  
  # information on dir_year would come from a higher level in the script
  if (!exists("dir_year") & exists("dir_year_data")){ # dir_year does not exist, but dir_year_data was created
    
    if (identical(dir_year_data, character(0)) == T) { # check it's not empty
      
      # if empty, ABORT & print to console
      cat("\nNo information on year available, character is empty\n")
      cat("\nSkipped file", tmp, "\n")
      next
      
    } else {
      
      # if not empty, create directory to save data
      if (!dir.exists(dir_year_data)) {
        dir.create(dir_year_data)
      }
      # declare dir_year_data to be the higher level dir_year variable
      dir_year <- dir_year_data
      
    }
    
  }
  
  # determine file name
  # if (grepl('[^/]', tmp, perl = T)) {
  #   dir_ex <- sub("/[^/]+$", "", dir_year)
  # } else {
  #   dir_ex <- dir_year
  # }
  # 
  # file_name <- file.path(dir_ex, tmp)
  file_name <- file.path(dir_year, tmp)
  
  # retrieve raw content from request
  cat("\nDownloading file from url...\n")
  cat("\t", url, "\n")
  bin <- content(request, "raw")
  
  # write binary data to file
  writeBin(bin, file_name)
  cat("\t...done\n")
  
  
  # unzip folder
  if (grepl("zip", file_name)) {
    
    cat("Unzipping...\n")
    cat("\t...done\n")
    
    if (grepl("performance-tables", dir_year)) {
      # Remove everything after the last / from directory
      dir_ex <- sub("/[^/]+$", "", dir_year)
    } else {
      dir_ex <- dir_year
    }
    
    unzipped <- unzip(file_name, exdir = dir_ex)
    file.remove(file_name)
  }
  
}

# function to scrape a website for file download links that also downloads all linked files
webscrape_government_data <- function(dir_out = "path_to_directory",
                                      parent_url = "url",
                                      pattern_to_match = "pattern"){
  
  # create output dir
  if (!dir.exists(dir_out)) {
    dir.create(dir_out)
  }
  
  assign("dir_out", dir_out, envir=globalenv())
  
  # Read the webpage content
  webpage <- read_html(parent_url)
  
  # Extract all the links from the webpage
  links <- webpage %>%
    html_nodes("a") %>%  # Select all <a> tags
    html_attr("href")    # Extract the href attribute
  
  # check if there are any application/octet-stream links
  download_links <-  unique(links[grepl("/files$", links)])
  
  if (identical(download_links, character(0)) == F) {
    cat("\nFound download links on parent URL...\n")
    cat("\t", download_links, sep = "\n\t")
    cat("\n")
    # if so, download
    sapply(download_links, download_data_from_url)
  }
  
  # Filter the links using the specified pattern
  release_links <- unique(links[grepl(pattern_to_match, links)])
  
  if (grepl("school-pupils-and-their-characteristics", parent_url)) {
    # data not linked there
    release_links <- sort(c(release_links, "https://www.gov.uk/government/statistics/schools-pupils-and-their-characteristics-january-2018", "https://www.gov.uk/government/statistics/schools-pupils-and-their-characteristics-january-2019"))
  }
  
  
  # check if there are any matching links
  if (identical(release_links, character(0)) == T) {
    cat("NO MATCHES FOUND")
    cat(release_links)
    cat(pattern_to_match)
    
  } else {
    
    # Apply the function to deal with relative urls to all release links
    release_links <- sapply(release_links, function(link) {
      resolve_url(parent_url, link)
    })
    
    # Output the release links to the console
    cat("\nLooping over these release links\n")
    cat("\t", release_links, sep = "\n\t")
    cat("\n")
    
    # loop over all releases
    for (release_url in release_links) {
      
      #release_url <- release_links[1]
      
      # create folder for year of release
      assign_dir_year("dir_year", file.path(dir_out, get_year(release_url)))
      
      if (!dir.exists(dir_year)) {
        dir.create(dir_year)
      }
      
      cat("\nReading content of release landing page", release_url, "\n")
      
      # Read the webpage content
      webpage <- read_html(release_url)
      
      # Extract all the links from the webpage
      links <- webpage %>%
        html_nodes("a") %>%  # Select all <a> tags
        html_attr("href")    # Extract the href attribute
      
      # Filter the download links (e.g., links ending with .pdf)
      download_links <- links[grepl("\\.[a-zA-Z]+$|/files$", links)]
      download_links <- download_links[!grepl(".uk$", download_links)]
      
      # Remove duplicates
      download_links <- unique(download_links)
      
      if (identical(download_links, character(0)) == F) {
        cat("\nFound download links on release URL...\n")
        cat("\t", download_links, sep = "\n\t")
        cat("\n")
        # if so, download
        sapply(download_links, download_data_from_url)
      }
      
    }
    
  }
  
}


