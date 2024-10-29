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
library(rvest)
library(xml2)
library(httr)

devtools::source_url("https://github.com/stefaniemeliss/scm_feasibility/blob/main/functions.R?raw=TRUE")


##### performance tables #####

# determine output directory
dir_out <- file.path(dir_data, "performance-tables")
if (!dir.exists(dir_out)) {
  dir.create(dir_out)
}

# determine years of interest
start <- 2010
finish <- 2022

for (year in start:finish) {
  
  # skip covid year
  if(year == 2019) next
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  cat("\n\n#############", academic_year, "#############\n\n")
  
  # create folder for academic year
  dir_year <- file.path(dir_out, academic_year)
  
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
    url_data <- paste0("https://www.compare-school-performance.service.gov.uk/download-data?download=true&regions=0&filters=", datasets[d], "&fileformat=csv&year=", academic_year,"&meta=false")
    # download data
    download_data_from_url(url = url_data)
    
    # get meta data #
    
    # specify URL
    url_meta <- paste0("https://www.compare-school-performance.service.gov.uk/download-data?download=true&regions=", datasets[d], "&filters=meta&fileformat=csv&year=", academic_year,"&meta=true")
    if (year == 2014 & datasets[d] == "PUPILABSENCE") {
      url_meta <- paste0("https://www.compare-school-performance.service.gov.uk/download-data?download=true&regions=GUIDANCE&filters=meta&fileformat=zip&year=", academic_year,"&meta=true")
    }
    # download data
    download_data_from_url(url = url_meta)
    
    if (year == 2014 & datasets[d] == "PUPILABSENCE") {
      # move guidance data in separate folder, currently saved in dir_out
      tmp_dir <- file.path(dir_out, academic_year, "guidance_meta")
      dir.create(tmp_dir)
      # get all files
      tmp_files <- list.files(dir_out, pattern = ".pdf|.xls", full.names = T)
      # determine new names
      tmp_files_new <- gsub("performance-tables", paste0("performance-tables/", academic_year, "/guidance_meta"), tmp_files)
      # move
      file.rename(tmp_files, tmp_files_new)
    }
  }
  
}


##### Schools, pupils and their characteristics ##### 

webscrape_government_data(dir_out =  file.path(dir_data, "school-pupils-and-their-characteristics"),
                          parent_url = "https://explore-education-statistics.service.gov.uk/find-statistics/school-pupils-and-their-characteristics",
                          pattern_to_match = glob2rx("*school-pupils-and-their-characteristics/20*|*schools-pupils*20*"))

##### School workforce in England ##### 

webscrape_government_data(dir_out =  file.path(dir_data, "school-workforce-in-england"),
                          parent_url = "https://explore-education-statistics.service.gov.uk/find-statistics/school-workforce-in-england",
                          pattern_to_match = glob2rx("*school-workforce*england/20*|*school-workforce*november-20*"))

##### School capacity ##### 

webscrape_government_data(dir_out =  file.path(dir_data, "school-capacity"),
                          parent_url = "https://explore-education-statistics.service.gov.uk/find-statistics/school-capacity",
                          pattern_to_match = glob2rx("*school-capacity/20*|*school-capacity*20**"))

##### Special educational needs in England ##### 

webscrape_government_data(dir_out =  file.path(dir_data, "special-educational-needs-in-england"),
                          parent_url = "https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england",
                          pattern_to_match = glob2rx("*special-educational-needs-in-england/20*|*special-educational-needs-in-england-january-20**"))


##### Get information about Dixons schools #####

# determine header information
headers = c(
  `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
)

# specify URL
url <- "https://get-information-schools.service.gov.uk/Downloads/Download/Group/2939?state=W3siUm91dGVOYW1lIjoiU2VhcmNoIiwiUm91dGVWYWx1ZXMiOnsiU2VsZWN0ZWRUYWIiOiJHcm91cHMifSwiTGFiZWwiOiJTZWFyY2gifSx7IlJvdXRlTmFtZSI6Ikdyb3VwRGV0YWlscyIsIlJvdXRlVmFsdWVzIjp7ImlkIjoyOTM5fSwiTGFiZWwiOiJESVhPTlMgQUNBREVNSUVTIFRSVVNUIn1d0&downloadtype=csv&start=True"

# retrieve information url = # retrieve information from URL 
request <- GET(url = url, httr::add_headers(.headers=headers))

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

# retrieve raw content from request
bin <- content(request, "raw")

# write binary data to file
file_name <- file.path(dir_misc, "Lift.zip")
writeBin(bin, file_name)

# unzip folder
unzipped <- unzip(file_name, exdir = dir_misc)
file.rename(unzipped, gsub("misc/", "misc/Lift ", unzipped))

