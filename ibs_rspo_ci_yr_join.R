# load required libraries --------------------------------------------------

library(dplyr)
library(readxl)
library(sf)
library(rgdal)
library(skimr)
library(jsonlite)
library(aws.s3)
library(readxl)
library(tidyverse)
library(stringr)
library(openxlsx)

# credentials -------------------------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-indonesia"

# set links to dropbox directories ---------------------------------------

file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
if (length(file_name)==0){
  file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}

file_content <- fromJSON(txt=file_name)$personal
dropbox_dir <- file_content$path
ibs_dir <- paste0(dropbox_dir,"\\kraus\\data\\ibs\\")
rspo_dir <- paste0(dropbox_dir,"\\Trase\\Indonesia\\mill_lists\\")


# read and clean data ----------------------------------------------------

rspo_mills <- read_xlsx(paste0(rspo_dir,"rspoCert.xlsx"))

# reformat data 

cert_mills <- rspo_mills %>%
  select(uh_code=code,trase_code,ICDate,model) %>%
  mutate(ci_year = format(as.Date(ICDate, format="%Y-%M-%d %H:%M:%S"),"%Y")) %>%
  select(-ICDate,-uh_code)

# read ibs matched mills 

ibs_mills <- read_excel(paste0(ibs_dir,"IBS_UML_panel.xlsx"))

# merge ibs mills with rspo CI year
ibs_rspo_merge <- ibs_mills %>%
  left_join(cert_mills,by="trase_code")


# exploring data ---------------------------------------------------------

# check number of unique mills matched
ibs_rspo_merge %>%
  distinct(trase_code) %>%
  drop_na() %>%
  count()

# check number of unique mills matched that are rspo certified
ibs_rspo_merge %>%
  distinct(trase_code,ci_year) %>%
  drop_na() %>%
  count()

# export data ------------------------------------------------------------

write.csv(ibs_rspo_merge,file=paste0(ibs_dir,"ibs_matched_rspo_ci_year_feb2020.csv"))  

