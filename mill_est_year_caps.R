## ---------------------------
##
## Purpose of script: Merging mills with capacities and est year
##
## Author: Jason Benedict
##
## Date Created: 2020-04-21
## 
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory


## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load packages

library(tidyverse)
library(data.table)
library(janitor)
library(jsonlite)
library(lubridate)
library(aws.s3)
library(sf)
library(readxl)
library(sjmisc)

# set dropbox directory -------

file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
if (length(file_name)==0){
  file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}

file_content <- fromJSON(txt=file_name)$personal
dropbox_dir <- file_content$path
wdir <- paste0(dropbox_dir,"\\Trase\\Indonesia\\mill_lists\\")

# read files -----------------
mills <- read_excel(paste0(wdir,"MILLS_20200421.xlsx"))
mill_est_yr <- read_excel(paste0(wdir,"tracker/mill_est_year_tracker.xlsx"))
mill_cap <- read_excel(paste0(wdir,"tracker/mill_capacity_tracker.xlsx"))

# get caps of most recent yr 
cap_cols <- mill_cap %>% select(starts_with("cap_2")) %>% names()

# mill_cap_final <- mill_cap %>%
#   select(starts_with("cap_2")) %>%
#   mutate(cap = invoke(pmax, na_if(., 0), na.rm = TRUE)) %>%
#   bind_cols(select(mill_cap,trase_code,group),.) %>%
#   select(group,trase_code,cap)

df = mill_cap %>%
  select(starts_with("cap_2"))
  
mill_cap_final <- df %>% mutate(cap = coalesce(!!!df[ncol(df):1])) %>%
  bind_cols(select(mill_cap,trase_code,group),.) %>%
  select(group,trase_code,cap)

# create final tbl
mill_tbl <- mills %>%
  left_join(mill_cap_final,by="trase_code") %>%
  left_join(select(mill_est_yr,trase_code,est_year,est_year_source=source),by="trase_code") %>%
  move_columns(group, .before = 3) %>%
  mutate(group = ifelse(is.na(group),"UNKNOWN",group))

# export to excel
date = format(Sys.Date(),"%Y%m%d")
write.xlsx(mill_tbl,file=paste0(wdir,"MILLS_CAP_ESTYEAR_",date,".xlsx"))  
