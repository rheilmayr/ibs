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
library(summarytools)

# set dropbox directory intput --------------------------------------------

file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
if (length(file_name)==0){
  file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}

file_content <- fromJSON(txt=file_name)$personal
dropbox_dir <- file_content$path
wdir <- paste0(dropbox_dir,"\\Trase\\Indonesia\\production\\ibs\\")
setwd(wdir)

# read data ---------------------------------------------------------------

# read IBS full cleaned dataset
ibs_full <- read_excel(paste0(wdir,"IBS_match_attempt.xls"))

# extract ownership columns
ibs_own <- ibs_full %>%
  filter(!is.na(firm_id)) %>%
  select(firm_id,year,pct_own_cent_gov,pct_own_for,pct_own_loc_gov,pct_own_nat_priv) %>%
  unite(own_pc,pct_own_cent_gov,pct_own_for,pct_own_loc_gov,pct_own_nat_priv,sep = "_", remove = FALSE) %>%
  group_by(firm_id) %>%
  mutate(Consistent = n_distinct(own_pc) == 1)

count_change_own <- ibs_own %>%
  group_by(firm_id) %>%
  summarize(Consistent = n_distinct(own_pc) == 1) %>%
  group_by(Consistent) %>%
  tally()

count_change_own

ibs_non_na <- ibs_own[complete.cases(ibs_own),]
length(unique(ibs_own$firm_id))
length(unique(ibs_non_na$firm_id))

na_counts <- ibs_own %>%
  group_by(firm_id) %>%
  mutate(Consistent = n_distinct(own_pc) == 1) %>%
  filter(own_pc == "NA_NA_NA_NA") %>%
  group_by(firm_id) %>%
  tally()

# percent ownership changed
pc_own_changed <- count_change_own$n[1]/(count_change_own$n[1]+count_change_own$n[2])*100
pc_own_changed      

# extract export percent column
ibs_exp_pc <- ibs_full %>%
  select(firm_id,year,export_pct_imp)



# count firms that export
count_firm_exp <- ibs_exp_pc %>%
  mutate(exp = ifelse(export_pct_imp > 0 ,"1","0")) %>%
  mutate(exp = ifelse(is.na(export_pct_imp) ,"0",exp)) 

# get count if exporting firms
firm_exp <- count_firm_exp %>%
  filter(exp =="1")
length(unique(firm_exp$firm_id))

# percent of exporting firms
pc_exporting_firms <- length(unique(firm_exp$firm_id))/length(unique(count_firm_exp$firm_id))*100

### 22.8 percent of firms export
### 70.1 percent of firms have no ownership change
