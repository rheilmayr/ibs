## ---------------------------
##
## Purpose of script: Find mills that overlap KH releases
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
library(myutil)
library(readxl)
library(sjmisc)
library(skimr)
library(RecordLinkage)

# set working directory -------

dropbox_dir <- dropbox_folder()
wdir <- paste0(dropbox_dir,"Trase\\Indonesia\\palm\\mill_lists\\tracker\\")

# read files -----------------

# mill data
mills <- read_excel(paste0(wdir,"mills_master_list.xlsx"))
mill_est_yr <- read_excel(paste0(wdir,"mill_est_year_tracker.xlsx"))
mill_cap <- read_excel(paste0(wdir,"mill_capacity_tracker.xlsx"))

# KH releases
pkh <- read_sf(paste0(dropbox_dir,"KH\\Data\\Indonesia\\klhk\\PKH\\Pelepasan_Kawasan_Hutan.shp"))

# get caps of most recent yr 
cap_cols <- mill_cap %>% select(starts_with("cap_2")) %>% names()

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

## intersect with KH releases
pkh_mills <- mill_tbl %>%
  filter(active == 1) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  select(-cap,-est_year,-est_year_source,-active) %>%
  st_join(pkh) %>%
  drop_na(NPLS) %>%
  select(-OBJECTID,-Shape_Area,-Shape_Leng) %>%
  st_drop_geometry() %>%
  mutate(NPLS = str_replace(NPLS,"PT","")) %>%
  mutate(NPLS = trimws(str_replace(NPLS, "\\(.*?\\)", ""))) %>%
  mutate(str_comp = levenshteinSim(parent_co, NPLS))
  

# write data -----------------
write_excel_csv(pkh_mills,"D:/pkh_mills.csv")      
