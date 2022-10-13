## ---------------------------------------------------------
##
## Purpose of script: Combine palm oil price data
##
## Author: Jason Benedict
##
## Date Created: 2022-10-04
## 
## ---------------------------------------------------------
##
## Notes: Palmtrace data scraped from : https://rspo.org/palmtrace
##        FRED data from : https://fred.stlouisfed.org/series/PPOILUSDM
##        BAPPEBTI data from : https://bappebti.go.id/harga_komoditi_bursa
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
setwd(wdir)

## read data ----------------------------------

# palm trace
palmtrace <- read_csv(paste0(wdir,"rspo_palmtrace.csv"))

# FRED CPO prices
fred_cpo <- read_csv(paste0(wdir,"PPOILUSDM.csv"))

# BAPPETI (Commodity Futures Trading Regulatory Agency)
bappebti_cpo <- read_csv(paste0(wdir,"CPO-bappebti.csv"))

## clean and merge data -----------------------

palmtrace_all_df <- palmtrace %>%
  mutate(DATE = paste0(month,"-",year), DATE=as.Date(zoo::as.yearmon(DATE, "%B-%Y"))) %>%
  select(DATE,TRADE_TYPE=chart_name,UNIT=type,VALUE=value) %>%
  mutate(SOURCE = "PALMTRACE", COMMODITY = if_else(str_detect(TRADE_TYPE, "^CSPO"), "CPO", "PKO"))

palmtrace_prices_df <- palmtrace %>%
  mutate(DATE = paste0(month,"-",year), DATE=as.Date(zoo::as.yearmon(DATE, "%B-%Y"))) %>%
  filter(str_detect(chart_name, "CSPO") & type == "Price (USD)") %>%
  select(-year,-month) %>%
  pivot_wider(names_from = "chart_name",values_from="value") %>%
  select(DATE,PT_CSPO_MARKET_TRADES_PRICE_USD=`CSPO On Market Trades`,PT_IS_CSPO_CREDIT_SALES_PRICE_USD=`IS CSPO Credit Sales`)

bappebti_df <- bappebti_cpo %>%
  select(DATE = Tanggal,LOCATION=Lokasi,TRADE_TYPE=Penyerahan,VALUE=Harga,UNIT=Satuan) %>%
  mutate(SOURCE = "BAPPEBTI",COMMODITY = "CPO", UNIT = ifelse(str_detect(UNIT, "^US"), "USD/MT","RP/KG"))

fred_df <- fred_cpo %>%
  mutate(VALUE = PPOILUSDM,UNIT = "USD/MT",COMMODITY="CPO", SOURCE="FRED") %>%
  select(-PPOILUSDM)
  
#fred_palmtrace <- palmtrace_prices_df %>% 
#  #filter(str_detect(chart_name, "CSPO") & type == "Price (USD)") %>%
#  #select(DATE,VALUE=value) %>%
#  left_join(fred_cpo,by="DATE") %>%
#  rename(FRED_CPO_PRICE_USDPMT=PPOILUSDM) %>%
#  pivot_longer(cols = c("PT_CSPO_MARKET_TRADES_PRICE_USD","PT_IS_CSPO_CREDIT_SALES_PRICE_USD","FRED_CPO_PRICE_USDPMT"), names_to = "TYPE",values_to="PRICE_USD")


## merge datasets
merge_df <- palmtrace_all_df %>%
  bind_rows(bappebti_df) %>%
  bind_rows(fred_df) %>%
  select(DATE,TRADE_TYPE,UNIT,VALUE,LOCATION,COMMODITY,SOURCE) %>%
  arrange(-desc(DATE))

## export to csv --------------------------
write_csv(merge_df,paste0(wdir,"po_prices.csv"))
