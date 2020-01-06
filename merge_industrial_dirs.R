# Load required libraries ------------------------------------------------
library(pdftools)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(openxlsx)
library(readxl)

# set dropbox directory intput --------------------------------------------

file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
if (length(file_name)==0){
  file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}

file_content <- fromJSON(txt=file_name)$personal
dropbox_dir <- file_content$path
wdir <- paste0(dropbox_dir,"\\kraus\\data\\direktori_industri\\pdf\\")


# Loop through folder and merge excel files

# pre 2008
pre2008 <- paste0(wdir,"pre2008\\clean\\")
setwd(pre2008)
file.list <- list.files(pattern='*.xlsx',recursive = TRUE)
df.list <- lapply(file.list, read_excel)
df1 <- bind_rows(df.list)

# post 2008
post2008 <- paste0(wdir,"post2008\\clean\\")
setwd(post2008)
file.list <- list.files(pattern='*.xlsx',recursive = TRUE)
df.list <- lapply(file.list, read_excel)
df2 <- bind_rows(df.list)

# Merge pre and post 2008 df's
df_merge <- df1 %>%
  bind_rows(df2) %>%
  select(company_name,year,main_product,no_workers,address,head_off_address)

# Export to excel file --------------------------------------------
write.xlsx(df_merge,paste0(wdir,'direktori_industri_merged.xlsx')) 