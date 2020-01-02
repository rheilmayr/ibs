# load required libraries --------------------------------------------------

library(dplyr)
library(readxl)
library(sf)
library(rgdal)
library(foreign)
library(readstata13)
library(skimr)
library(jsonlite)
library(aws.s3)
library(tidyverse)
library(stringr)
library(openxlsx)

# credentials -------------------------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-indonesia"

# set dropbox directory intput --------------------------------------------

file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
if (length(file_name)==0){
  file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}

file_content <- fromJSON(txt=file_name)$personal
dropbox_dir <- file_content$path
wdir <- paste0(dropbox_dir,"\\kraus\\data\\ucsb\\")
setwd(wdir)

# read data ---------------------------------------------------------------

# read trase mills
trase_mills_raw <- s3read_using(read_excel, object = "3_LOGISTICS/OUT/MILLS/MILLS_13102019.xlsx", bucket = bucket)
# read companies
companies <- as_tibble(fromJSON(rawToChar(get_object("4_COMPANIES/idn_company_dictionary_v1.json", bucket))))
# read mill est date & capacities
est_yr <- read_csv(paste0(wdir,"traseMills_estyear.csv"))

# merge mills and establishment year
mills <- trase_mills_raw %>% 
  left_join(select(est_yr,trase_code,cap,est_year), by = "trase_code") %>%
  filter(active == 1) %>%
  select(-cap) %>%
  select(uml_id,mill_id=trase_code,parent_co,mill_name,latitude,longitude,est_year)

skim(mills)

# export to xlsx
write.xlsx(mills,file=paste0(wdir,"mills_estyear_clean.xlsx"))  
