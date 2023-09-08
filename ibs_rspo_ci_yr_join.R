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
library(haven)
library(foreign)
library(janitor)
library(tidylog)

ihsTransform <- function(y) {log(y + (y ^ 2 + 1) ^ 0.5)}


# credentials -------------------------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-indonesia"

# set links to dropbox directories ---------------------------------------

file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
if (length(file_name)==0){
  file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}

file_content <- fromJSON(txt=file_name)$personal
dropbox_dir <- file_content$path
ibs_dir <- paste0(dropbox_dir,"\\collaborations\\indonesia\\indo_mill_spillovers\\ucsb-kraus\\data\\ibs\\")
rspo_dir <- paste0(dropbox_dir,"\\collaborations\\trase\\Trase\\Indonesia\\palm\\mill_lists\\")


# read and clean data ----------------------------------------------------

rspo_mills <- read_xlsx(paste0(rspo_dir,"rspoCert.xlsx"))

# read smallholder shares
small_shr <- read_csv(paste0(dropbox_dir,"\\collaborations\\indonesia\\indo_mill_spillovers\\ucsb-kraus\\data\\ucsb\\mill_smallholder_share.csv")) %>% 
  rename(trase_code = mill_id)

# reformat data 

cert_mills <- rspo_mills %>%
  select(uh_code=code,trase_code,ICDate,model) %>%
  mutate(ci_year = format(as.Date(ICDate, format="%Y-%M-%d %H:%M:%S"),"%Y")) %>%
  select(-ICDate,-uh_code)

# read ibs matched mills 

ibs_mills <- read_excel(paste0(ibs_dir,"IBS_UML_panel_final.xlsx"))

# merge ibs mills with rspo CI year
ibs_rspo_merge <- ibs_mills %>%
  left_join(cert_mills,by="trase_code")


# check number of unique mills matched
ibs_rspo_merge %>%
  distinct(trase_code) %>%
  drop_na() %>%
  count() %>% 
  print()

# check number of unique mills matched that are rspo certified
ibs_rspo_merge %>%
  distinct(trase_code,ci_year) %>%
  drop_na() %>%
  count() %>% 
  print()

# # clean column names
# ibs_rspo_merge <- ibs_rspo_merge %>% 
#   clean_names()

# identify correct column for duplicated variables
ibs_rspo_merge <- ibs_rspo_merge %>% 
  rename(workers_total_imp2 = workers_total_imp2...112)


# merge smallholder data
ibs_rspo_merge <- ibs_rspo_merge %>% 
  left_join(small_shr, by = "trase_code")


# drop data without known certification status
ibs_rspo_merge <- ibs_rspo_merge %>% 
  filter(!is.na(trase_code))

# Creating cleaned variables ---------------------------------------------------------
ibs_rspo_merge <- ibs_rspo_merge %>%
  mutate(cert = year>=ci_year,
         cert = replace_na(cert, 0),
         cert_start = replace_na(ci_year, 0),
         ln_ffb_price = log(ffb_price_imp1),
         ln_ffb_val = log(in_val_ffb_imp1),
         ln_cpo_price = log(cpo_price_imp1),
         ln_pko_price = log(pko_price_imp1),
         ln_cpo_vol = log(out_ton_cpo_imp1),
         ln_pko_vol = log(out_ton_pko_imp1),
         ln_ffb_vol = log(in_ton_ffb_imp1),
         ln_rev = log(revenue_total_imp2),
         ln_workers = log(workers_total_imp2),
         ln_oer = log(in_ton_ffb_imp1 / out_ton_cpo_imp1),
         ln_cpo_export_shr = ihsTransform(prex_cpo_imp1),
         ln_wage = ihsTransform(wage_prod_imp2),
         ln_value_added = log(value_added_self_imp2),
         ln_materials = log(materials_tot_imp2),
         ln_electricity = log(elec_qty_imp2),
         ln_fc_add = log(fc_add_imp),
         ln_fc_est_tot = log(fc_est_tot_imp6co), # Aug 2022 - Valentin and Sebastian confirm that this is the correct fixed capital variable
         ln_labor_cost = log(workers_prod_imp2 * wage_prod_imp2),
         ln_gifts = log(gifts)) 


ibs_rspo_merge

# export r data ------------------------------------------------------------
write.csv(ibs_rspo_merge,file=paste0(ibs_dir,"ibs_matched_rspo_ci_year.csv"))


# export stata data ------------------------------------------------------------
prod_df <- ibs_rspo_merge %>% 
  select(firm_id, year, ln_value_added, ln_workers, ln_fc_est_tot, ln_fc_add, ln_materials, 
         ln_ffb_price, ln_ffb_vol, ln_ffb_val, ln_electricity, ln_labor_cost) %>% 
  drop_na()
write_dta(prod_df, paste0(ibs_dir, "ucsb_ibs.dta"), version = 14)

