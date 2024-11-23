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

# determine header information
headers = c(
  `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
)

##### Get information about Dixons schools #####

# specify URL
url <- "https://get-information-schools.service.gov.uk/Downloads/Download/Group/2939?state=W3siUm91dGVOYW1lIjoiU2VhcmNoIiwiUm91dGVWYWx1ZXMiOnsiU2VsZWN0ZWRUYWIiOiJHcm91cHMifSwiTGFiZWwiOiJTZWFyY2gifSx7IlJvdXRlTmFtZSI6Ikdyb3VwRGV0YWlscyIsIlJvdXRlVmFsdWVzIjp7ImlkIjoyOTM5fSwiTGFiZWwiOiJESVhPTlMgQUNBREVNSUVTIFRSVVNUIn1d0&downloadtype=csv&start=True"

# retrieve information url = # retrieve information from URL 
request <- GET(url = url, httr::add_headers(.headers=headers))

# retrieve raw content from request
bin <- content(request, "raw")

# write binary data to file
file_name <- file.path(dir_misc, "Dixons.zip")
writeBin(bin, file_name)

# unzip folder and remove
unzipped <- unzip(file_name, exdir = dir_misc)
file.rename(unzipped, gsub("misc/", "misc/Dixons ", unzipped))
file.remove(file_name)

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
file.remove(file_name)

##### Get information about Our Lady and All Saints Catholic Multi Academy Company ("the MAC") #####

# specify URL
url <- "https://get-information-schools.service.gov.uk/Downloads/Download/Group/17473?state=W3siUm91dGVOYW1lIjoiU2VhcmNoIiwiUm91dGVWYWx1ZXMiOnsiU2VsZWN0ZWRUYWIiOiJHcm91cHMifSwiTGFiZWwiOiJTZWFyY2gifSx7IlJvdXRlTmFtZSI6Ikdyb3VwRGV0YWlscyIsIlJvdXRlVmFsdWVzIjp7ImlkIjoxNzQ3M30sIkxhYmVsIjoiT1VSIExBRFkgQU5EIEFMTCBTQUlOVFMgQ0FUSE9MSUMgTVVMVEkgQUNBREVNWSBDT01QQU5ZIn1d0&downloadtype=csv&start=True"

# retrieve information from URL 
request <- GET(url = url, httr::add_headers(.headers=headers))

# retrieve raw content from request
bin <- content(request, "raw")

# write binary data to file
file_name <- file.path(dir_misc, "MAC.zip")
writeBin(bin, file_name)

# unzip folder
unzipped <- unzip(file_name, exdir = dir_misc)
file.rename(unzipped, gsub("misc/", "misc/MAC ", unzipped))
file.remove(file_name)

