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
      file.remove(file_name)
    }
    
  }
  
}

##### School pupils and their characteristics ##### 

urls <- c(
  "https://content.explore-education-statistics.service.gov.uk/api/releases/60f096b8-6ed9-4e9e-97ee-2ca83867d51e/files",
  "https://content.explore-education-statistics.service.gov.uk/api/releases/5597d8c4-ae2c-43cd-ba21-c6cbe80c5e4a/files",
  "https://content.explore-education-statistics.service.gov.uk/api/releases/b71871a9-3207-4e83-8c5f-15eefdd2f458/files",
  "https://content.explore-education-statistics.service.gov.uk/api/releases/18afd52c-ef75-406c-993e-e6459781dbcc/files",
  "https://assets.publishing.service.gov.uk/media/5a824241ed915d74e340292a/SFR28_2017_Underlying_Data.zip",
  "https://assets.publishing.service.gov.uk/media/5a7f3f4ced915d74e62294a7/SFR20_2016_Underlying_Data.zip",
  "https://assets.publishing.service.gov.uk/media/5a7f22f6ed915d74e62289c9/SFR16_2015_Underlying_Data.zip",
  "https://assets.publishing.service.gov.uk/media/5a80260de5274a2e8ab4e77a/SFR15_2014_Underlying_data_v102.zip",
  "https://assets.publishing.service.gov.uk/media/5a7c1750e5274a1f5cc75bb1/SFR21-2013_UD.zip",
  "https://assets.publishing.service.gov.uk/media/5a7a100ee5274a319e777952/sfr10-2012ud.zip",
  "https://assets.publishing.service.gov.uk/media/5a7af937ed915d71db8b3d79/sfr12-2011udv4.zip",
  "https://assets.publishing.service.gov.uk/media/5a7a2d68e5274a319e77865a/underlying_20data_20sfr092010.zip"
)

# determine output directory
out_dir <- file.path(dir_data, "spc")
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

for (url in urls) {
  
  # retrieve information from URL 
  request <- GET(url = url, httr::add_headers(.headers=headers))
  request$headers$`content-type`
  request$headers$`content-disposition`
  
  # specify file name
  tmp <- sub('[^\"]+\"([^\"]+).*', '\\1', request$headers$`content-disposition`) # extract relevant info from response header
  tmp <- gsub("/", "_", tmp)
  file_name <- file.path(out_dir, tmp)
  
  # retrieve raw content from request
  bin <- content(request, "raw")
  
  # write binary data to file
  writeBin(bin, file_name)
  
  # unzip folder and delete
  unzipped <- unzip(file_name, exdir = file.path(out_dir, gsub(".zip", "", tmp)))
  file.remove(file_name)
}

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

##### School capacity ##### 

academic_years <- c(
  "Academic year 2022-23",
  "Academic year 2021-22",
  "Academic year 2020-21",
  "Academic year 2018-19",
  "Academic Year 2017-18",
  "Academic Year 2016-17",
  "Academic Year 2015-16",
  "Academic Year 2014-15",
  
  "Academic Year 2013-14",
  "Academic Year 2013-14",
  
  "Academic Year 2012-13",
  "Academic Year 2012-13",
  
  "Academic Year 2011-12",
  
  "Academic Year 2010-11",
  "Academic Year 2010-11",
  
  "Academic Year 2009-10"
)

urls <- c(
  "https://content.explore-education-statistics.service.gov.uk/api/releases/cfb27ea6-5078-4329-b59d-38ac104806dc/files", #2022-23
  "https://content.explore-education-statistics.service.gov.uk/api/releases/35560c95-6ee1-4b3f-9c63-0e1afbcb62e2/files", #2021-22
  "https://content.explore-education-statistics.service.gov.uk/api/releases/8f353ba3-5d7c-4d86-ae05-08d9ebca404d/files", #2020-21
  "https://content.explore-education-statistics.service.gov.uk/api/releases/d57205e5-6517-44e1-9ebc-d42ac99f03dc/files", #2018-19
  "https://assets.publishing.service.gov.uk/media/5ca33f25ed915d0c57e97e7c/School_Capacity_2018_Main_tables_underlying_data_.zip",
  "https://assets.publishing.service.gov.uk/media/5aa85d0ded915d4f563b737e/SFR07_2018_SCAP_underlying_data.zip",
  "https://assets.publishing.service.gov.uk/media/5a82e22e40f0b6230269d349/SCAP_2016_Underlying_data.zip",
  "https://assets.publishing.service.gov.uk/media/5a80be33e5274a2e8ab51e83/2015_capacity_and_forecast_underlying_data.ods",

  "https://assets.publishing.service.gov.uk/media/5a818c1de5274a2e8ab54927/Primary_Underlying_data.xlsx",
  "https://assets.publishing.service.gov.uk/media/5a7f6657ed915d74e622a397/Secondary_Underlying_data.xlsx",
  
  "https://assets.publishing.service.gov.uk/media/5a749839ed915d0e8e399783/Primary_underlying_data.xlsx",
  "https://assets.publishing.service.gov.uk/media/5a7d8f5fed915d3fb95947cd/Secondary_underlying_data.xlsx",
  
  "https://assets.publishing.service.gov.uk/media/5a7a1b8040f0b66eab999ca6/sfr01-2013ud.zip",
  
  "https://assets.publishing.service.gov.uk/media/5a7b548340f0b6425d5926e9/primary_20school_20underlying_20data_20update.csv",
  "https://assets.publishing.service.gov.uk/media/5a7a13f8ed915d6eaf15396f/secondary_20school_20underlying_20data_20update.csv",
  
  "https://assets.publishing.service.gov.uk/media/5a7aea0040f0b66eab99d8a5/osr33-2010ud.zip"
)

# determine output directory
out_dir <- file.path(dir_data, "school_capacity")
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

for (u in 1:length(urls)) {
  
  # create folder for academic year
  dir_year <- file.path(out_dir, academic_years[u])
  
  if (!dir.exists(dir_year)) {
    dir.create(dir_year)
  }
  
  
  # retrieve information from URL 
  request <- GET(url = urls[u], httr::add_headers(.headers=headers))
  request$headers$`content-type`
  request$headers$`content-disposition`
  
  # specify file name
  tmp <- sub('[^\"]+\"([^\"]+).*', '\\1', request$headers$`content-disposition`) # extract relevant info from response header
  tmp <- gsub("/", "_", tmp)
  file_name <- file.path(dir_year, tmp)
  
  # retrieve raw content from request
  bin <- content(request, "raw")
  
  # write binary data to file
  writeBin(bin, file_name)
  
  # unzip folder
  if (grepl("application", request$headers$`content-type`)) {
    unzipped <- unzip(file_name, exdir = dir_year)
    file.remove(file_name)
  }
  
}
