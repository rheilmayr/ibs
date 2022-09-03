## ---------------------------------------------------------
##
## Purpose of script: Calculate smallholder share for mills
##
## Author: Jason Benedict & Robert Heilmayr
##
## Date Created: 2022-09-02
## 
## ---------------------------------------------------------
##
## Notes:
##   
##
## ---------------------------------------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------------------------------------

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
library(dlookr)
library(DT)
library(formattable)
library(rgeoboundaries)
library(ggnewscale)
library(patchwork)
library(janitor)
library(Metrics)
library(comtradr)
library(gghighlight)
library(countrycode)
library(d3.format) ## devtools::install_github("dreamRs/d3.format")

## credentials ------------------------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

#drive_find(type = "xlsx") 
# Select no corresponding to your Google Drive Account (default: 1)

## find dropbox folder function -------------------------------------------
dropbox_folder <- function() {
  if (Sys.info()["sysname"]!="Windows") stop("Currently, 'dropbox_folder' works for Windows only. Sorry.")
  if (!require(jsonlite, quietly=TRUE)) stop ("You need to install the jsonlite package.")
  file_name<-list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "info*.json", full.names = TRUE)
  if (length(file_name)==0){
    file_name<-list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), 
                          pattern = "info*.json", full.names = TRUE)}
  file_content <- jsonlite::fromJSON(txt=file_name)$personal
  path <-paste0(file_content$path, "/")
  path
}

## set working directory --------------------------------------------------

dropbox_dir <- dropbox_folder()
wdir <- paste0(dropbox_dir,"/kraus/data/ucsb/")
#wdir <- paste0(dropbox_dir,"collaborations/trase/Trase/Indonesia/palm/documents/")

## read data --------------------------------------------------------------

mycomma <- function(digits = 0) {
  formatter("span", x ~ comma(x, digits = digits)
  )}

'%ni%' <- Negate('%in%') # filter out function

# concession -> mill links
conc_mills <- read_delim(get_object(object = "indonesia/production/out/CONCESSION_MILL_LINKS.csv", bucket), delim = ",")

# combined stitched full results
#results_full <- read_delim(get_object(object = "indonesia/palm_oil/sei_pcs/v1.2.0/ID_PALM_RESULTS_CONCAT.csv", bucket), delim = ",") %>%
#  clean_names()

# load clean results file
source("D:/Dev/trase-po/scripts/06_insights/clean_results_file.R")


## calculate mill level SH share -------------------------------------------

mill_sh <- results_full %>%
  group_by(mill_trase_id) %>%
  summarize(sh_share = sum((smallholder_production_t)/sum(volume_primary_commodity_tons))*100) %>%
  mutate(mill_id = str_replace(mill_trase_id,"ID-PALM-MILL-","M-")) %>%
  select(mill_id,sh_share) %>%
  filter(mill_id != "M-X") %>%
  print()

## export to csv -----------------------------------------------------------
write_csv(mill_sh,paste0(wdir,"mill_smallholder_share.csv"))

