## ---------------------------------------------------------
##
## Purpose of script: Create clean trase refinery dataset for RSPO mill impact analysis
##
## Author: Jason Benedict
##
## Date Created: 2022-10-04
## 
## ---------------------------------------------------------
##
## Notes: 
##   
##
## ---------------------------------------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

# load libraries -------------------------------------------
library(readxl)
library(sf)
library(rgdal)
library(jsonlite)
library(aws.s3)
library(tidyverse)
library(leaflet)
library(htmltools)
library(googledrive)
library(htmlwidgets)
library(testthat)
library(stringdist)
library(strex)
library(lubridate)
library(DT)
library(formattable)
library(zoo)
library(d3.format) ## devtools::install_github("dreamRs/d3.format")

## set working directory ------------------------

file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
if (length(file_name)==0){
  file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}

file_content <- fromJSON(txt=file_name)$personal
dropbox_dir <- file_content$path
wdir <- paste0(dropbox_dir,"\\kraus\\data\\ucsb\\")

## credentials -----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")
setwd(wdir)

## read data ----------------------------------

# refineries
refs_raw <- s3read_using(read_excel, object = "indonesia/logistics/out/refineries/REFS_14112021.xlsx", bucket = bucket)

# companies
companies <- as_tibble(fromJSON(rawToChar(get_object("indonesia/companies/idn_company_dictionary_v2.json", bucket))))

## clean data ---------------------------------

# join refineries with company and group names
refs <- companies %>%
  unnest_legacy(other_names) %>%
  filter(source == "refineries") %>%
  distinct(company, id, group, company_og) %>%
  right_join(refs_raw, by = c("company_og" = "comp_name")) %>%
  select(company, company_id = id, group, ref_id = trase_id, ref_name, latitude, longitude, cap_mt = cap_final_mtperyr) %>%
  distinct() %>%
  mutate(cap_mt = as.numeric(as.character(cap_mt))) %>%
  mutate(cap_mt = ifelse(is.na(cap_mt), 100000, cap_mt)) %>%
  # Replace NA's with dummy value
  mutate_if(is.character, str_to_upper) %>%
  drop_na(ref_id) %>%
  filter(ref_id != "R-0001") # MNS Paya Pasir only supplies PK

## export to csv --------------------------
write_csv(refs,paste0(wdir,"trase_refineries.csv"))
