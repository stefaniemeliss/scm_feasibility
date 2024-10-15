##### WEB SCRAPING SCRIPT #####

options(scipen = 999)
# empty work space
rm(list = ls())

# define directory
dir <- getwd()

# create sub folders
dir_data <- file.path(dir, "data")
if (!dir.exists(dir_data)) {
  dir.create(dir_data)
}

dir_misc <- file.path(dir, "misc")
if (!dir.exists(dir_misc)) {
  dir.create(dir_misc)
}

# load libraries
library(httr)

# determine header information
headers = c(
  `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
)

##### Get information about Dixons schools #####

# specify URL
url <- "https://get-information-schools.service.gov.uk/Downloads/Download/Group/2939?state=W3siUm91dGVOYW1lIjoiU2VhcmNoIiwiUm91dGVWYWx1ZXMiOnsiU2VsZWN0ZWRUYWIiOiJHcm91cHMifSwiTGFiZWwiOiJTZWFyY2gifSx7IlJvdXRlTmFtZSI6Ikdyb3VwRGV0YWlscyIsIlJvdXRlVmFsdWVzIjp7ImlkIjoyOTM5fSwiTGFiZWwiOiJESVhPTlMgQUNBREVNSUVTIFRSVVNUIn1d0&downloadtype=csv&start=True"

# retrieve information from URL 
request <- GET(url = url, httr::add_headers(.headers=headers))
request$headers$`content-type`
request$headers$`content-disposition`
request$headers$`content-encoding`

# retrieve raw content from request
bin <- content(request, "raw")

# write binary data to file
file_name <- file.path(dir_misc, "Dixons.zip")
writeBin(bin, file_name)

# unzip folder
unzipped <- unzip(file_name, exdir = dir_misc)
file.rename(unzipped, gsub("misc/", "misc/Dixons ", unzipped))

##### Get information about OAT schools #####

# specify URL
url <- "https://get-information-schools.service.gov.uk/Downloads/Download/Group/4106?state=W3siUm91dGVOYW1lIjoiU2VhcmNoIiwiUm91dGVWYWx1ZXMiOnsiU2VsZWN0ZWRUYWIiOiJHcm91cHMifSwiTGFiZWwiOiJTZWFyY2gifSx7IlJvdXRlTmFtZSI6Ikdyb3VwRGV0YWlscyIsIlJvdXRlVmFsdWVzIjp7ImlkIjo0MTA2fSwiTGFiZWwiOiJPUk1JU1RPTiBBQ0FERU1JRVMgVFJVU1QifV01&downloadtype=csv&start=True"

# retrieve information from URL 
request <- GET(url = url, httr::add_headers(.headers=headers))
request$headers$`content-type`
request$headers$`content-disposition`
request$headers$`content-encoding`

# retrieve raw content from request
bin <- content(request, "raw")

# write binary data to file
file_name <- file.path(dir_misc, "OAT.zip")
writeBin(bin, file_name)

# unzip folder
unzipped <- unzip(file_name, exdir = dir_misc)
file.rename(unzipped, gsub("misc/", "misc/OAT ", unzipped))

##### Get information about Lift schools #####

# specify URL
url <- "https://get-information-schools.service.gov.uk/Downloads/Download/Group/2053?state=W3siUm91dGVOYW1lIjoiU2VhcmNoIiwiUm91dGVWYWx1ZXMiOnsiU2VsZWN0ZWRUYWIiOiJHcm91cHMifSwiTGFiZWwiOiJTZWFyY2gifSx7IlJvdXRlTmFtZSI6Ikdyb3VwRGV0YWlscyIsIlJvdXRlVmFsdWVzIjp7ImlkIjoyMDUzfSwiTGFiZWwiOiJMSUZUIFNDSE9PTFMifV01&downloadtype=csv&start=True"

# retrieve information from URL 
request <- GET(url = url, httr::add_headers(.headers=headers))
request$headers$`content-type`
request$headers$`content-disposition`
request$headers$`content-encoding`

# retrieve raw content from request
bin <- content(request, "raw")

# write binary data to file
file_name <- file.path(dir_misc, "Lift.zip")
writeBin(bin, file_name)

# unzip folder
unzipped <- unzip(file_name, exdir = dir_misc)
file.rename(unzipped, gsub("misc/", "misc/Lift ", unzipped))

##### performance tables #####

# determine output directory
out_dir <- file.path(dir_data, "performance-tables")
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

# determine years of interest
start <- 2010
finish <- 2022


for (year in start:finish) {
  
  # skip covid year
  if(year == 2019) next
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  
  # create folder for academic year
  dir_year <- file.path(out_dir, academic_year)
  
  if (!dir.exists(dir_year)) {
    dir.create(dir_year)
  }
  
  # determine datasets of interest based on year
  if (year < 2018) {
    datasets = c("KS2", "KS4", "KS5", "PUPILABSENCE", "CENSUS", "SPINE")
  } else if (year == 2020) {
    datasets = c("CENSUS", "GIAS")
  } else if (year == 2021) {
    datasets = c("KS4", "KS5", "PUPILABSENCE", "CENSUS", "GIAS")
  } else {
    datasets = c("KS2", "KS4", "KS5", "PUPILABSENCE", "CENSUS", "GIAS")
  }
  
  
  for (d in 1:length(datasets)) {
    
    # get data files #
    
    # specify URL
    url <- paste0("https://www.compare-school-performance.service.gov.uk/download-data?download=true&regions=0&filters=", datasets[d], "&fileformat=csv&year=", academic_year,"&meta=false")
    # print(url)
    
    # retrieve information from URL 
    request <- GET(url = url, httr::add_headers(.headers=headers))
    # request$headers$`content-type`
    # request$headers$`content-disposition`
    
    # specify file name
    tmp <- sub('[^\"]+\"([^\"]+).*', '\\1', request$headers$`content-disposition`) # extract relevant info from response header
    tmp <- gsub("/", "_", tmp)
    file_name <- file.path(dir_year, tmp)
    
    # retrieve raw content from request
    bin <- content(request, "raw")
    
    # write binary data to file
    writeBin(bin, file_name)
    
    # get meta data #
    
    # specify URL
    url <- paste0("https://www.compare-school-performance.service.gov.uk/download-data?download=true&regions=", datasets[d], "&filters=meta&fileformat=csv&year=", academic_year,"&meta=true")
    # print(url)
    
    # retrieve information from URL 
    request <- GET(url = url, httr::add_headers(.headers=headers))
    # request$headers$`content-type`
    # request$headers$`content-disposition`
    
    # specify file name
    if (request$headers$`content-type` == "text/csv") {
      tmp <- sub('[^\"]+\"([^\"]+).*', '\\1', request$headers$`content-disposition`) # extract relevant info from response header
      tmp <- gsub("/", "_", tmp)
      file_name <- file.path(dir_year, tmp)
    } else if (request$headers$`content-type` == "application/zip") {
      file_name <- file.path(dir_year, paste0(tolower(datasets[d]), "_meta.zip"))
    }
    
    # retrieve raw content from request
    bin <- content(request, "raw")
    
    # write binary data to file
    writeBin(bin, file_name)
    
    # unzip folder
    if (request$headers$`content-type` == "application/zip") {
      unzipped <- unzip(file_name, exdir = out_dir)
    }
    
  }
  
}

##### School pupils and their characteristics (2023-24) ##### 

url <- "https://content.explore-education-statistics.service.gov.uk/api/releases/60f096b8-6ed9-4e9e-97ee-2ca83867d51e/files"

# retrieve information from URL 
request <- GET(url = url, httr::add_headers(.headers=headers))
request$headers$`content-type`
request$headers$`content-disposition`

# specify file name
tmp <- sub('[^\"]+\"([^\"]+).*', '\\1', request$headers$`content-disposition`) # extract relevant info from response header
tmp <- gsub("/", "_", tmp)
file_name <- file.path(dir_data, tmp)

# retrieve raw content from request
bin <- content(request, "raw")

# write binary data to file
writeBin(bin, file_name)

# unzip folder and delete
unzipped <- unzip(file_name, exdir = file.path(dir_data, gsub(".zip", "", tmp)))
file.remove(file_name)

##### School workforce in England (reporting year 2023) ##### 

url <- "https://content.explore-education-statistics.service.gov.uk/api/releases/6c48cc4c-5daa-4fdf-bd3c-ba8ccb146b5f/files"

# retrieve information from URL 
request <- GET(url = url, httr::add_headers(.headers=headers))
request$headers$`content-type`
request$headers$`content-disposition`

# specify file name
tmp <- sub('[^\"]+\"([^\"]+).*', '\\1', request$headers$`content-disposition`) # extract relevant info from response header
tmp <- gsub("/", "_", tmp)
file_name <- file.path(dir_data, tmp)

# retrieve raw content from request
bin <- content(request, "raw")

# write binary data to file
writeBin(bin, file_name)

# unzip folder and delete
unzipped <- unzip(file_name, exdir = file.path(dir_data, gsub(".zip", "", tmp)))
file.remove(file_name)

##### School capacity (2022-2023) ##### 

url <- "https://content.explore-education-statistics.service.gov.uk/api/releases/cfb27ea6-5078-4329-b59d-38ac104806dc/files"

# retrieve information from URL 
request <- GET(url = url, httr::add_headers(.headers=headers))
request$headers$`content-type`
request$headers$`content-disposition`

# specify file name
tmp <- sub('[^\"]+\"([^\"]+).*', '\\1', request$headers$`content-disposition`) # extract relevant info from response header
tmp <- gsub("/", "_", tmp)
file_name <- file.path(dir_data, tmp)

# retrieve raw content from request
bin <- content(request, "raw")

# write binary data to file
writeBin(bin, file_name)

# unzip folder and delete
unzipped <- unzip(file_name, exdir = file.path(dir_data, gsub(".zip", "", tmp)))
file.remove(file_name)

# extract postcodes
tmp <- read.csv(file = file.path(dir_data, "school-pupils-and-their-characteristics_2023-24", "supporting-files", "spc_school_level_underlying_data.csv"))
tmp <- as.data.frame(unique(tmp$school_postcode))

write.table(tmp, file.path(dir_misc, "school_postcodes_spc_2023-24.csv"), 
            row.names = F, col.names = F, sep = ",", quote = F)
