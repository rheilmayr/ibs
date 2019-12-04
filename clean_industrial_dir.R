
#### Using pdftools to extract text from Direkti Industri Pengolahan Indonesia into tabular format

# Load required libraries ------------------------------------------------
library(pdftools)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(openxlsx)

# set dropbox directory intput --------------------------------------------

file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
if (length(file_name)==0){
  file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}

file_content <- fromJSON(txt=file_name)$personal
dropbox_dir <- file_content$path
wdir <- paste0(dropbox_dir,"\\kraus\\data\\direktori_industri\\")
setwd(wdir)

# Read pdf data -----------------------------------------------------------

pdf_file = paste0(wdir,"pdf\\Direktori_Industri_Pengolahan_Indonesia_2003-pages-43-59.pdf")

# Cleaning up data -------------------------------------------------------

data <- pdf_data(pdf_file) %>% 
  bind_rows() %>%
  filter(height <=8) %>%
  mutate(category = ifelse(text =="^","main_product",
         ifelse(text ==";","no_workers",
         ifelse(text =="`","address",
         ifelse(text == "%","tel_no",
         ifelse(text == "#","fax_no",
         ifelse(text == "$","head_off_tel_no",
         ifelse(text == "@","head_off_fax_no",
         ifelse(text == ">","contact_person",
         ifelse(text == "<","department_occupation",
         ifelse(text == "E","email",NA))))))))))) %>%
         fill(category) %>%
  mutate(text = str_replace(text,","," ")) %>%
  mutate(text = str_replace(text,"-"," ")) %>%
  mutate(text = str_replace(text,'"',"")) %>%
  mutate(text = str_replace(text,"[.]","")) %>%
  mutate(text = str_replace(text,"/"," ")) %>%
  mutate(text = str_replace(text,"[(]"," ")) %>%
  mutate(text = str_replace(text,"[)]"," ")) %>%
  mutate(text = trimws(text,which="right")) %>%
  mutate(text = trimws(text,which="left")) %>%
  mutate(category = ifelse(str_detect(text,"^[:upper:]+$") & category != "main_product","company_name",category)) %>%
  mutate(category = ifelse(str_detect(text,"^[:upper:]+$ ^[:upper:]+$") & category !="company name","company_name",category)) %>%
  mutate(category = ifelse(str_detect(text,"PT"),"company_name",category)) %>%
  #mutate(category = ifelse(str_detect(text,"^[:upper:]+$") & category == "fax_no","company_name",category)) %>%
  mutate(category = ifelse(is.na(category), "company_name",category)) %>%
  mutate(category = ifelse(text =="SH", "contact_person",category)) %>%
  mutate(category = ifelse(text =="SE", "contact_person",category)) %>%
  mutate(category = ifelse(text =="E", "email",category)) %>%
  mutate(category = ifelse(text =="M", "address",category)) %>%
  mutate(category = ifelse(text =="S", "address",category)) %>%
  #mutate(category = ifelse(text =="III", "company_name",category)) %>%
  #mutate(category = ifelse(text =="II", "company_name",category)) %>%
  #mutate(category = ifelse(text =="IV", "company_name",category)) %>%
  mutate(category = ifelse(text =="BUAH DHT", "company_name",category)) %>%
  #mutate(category = ifelse(category == lead(category) & lag(category) == "category","company_name",category)) %>%
  mutate(category = ifelse(lag(category) == "address" & lead(category) == "address","address",category)) %>%
  mutate(category = ifelse(lag(category) == "company_name" & lead(category) == "company_name","company_name",category)) %>%
  mutate(category = ifelse(lag(category) == "department_occupation" & lead(category) == "department_occupation","department_occupation",category)) %>%
  #mutate(category = ifelse(lag(category) == "contact_person" & lead(category) == "contact_person","contact_person",category)) %>%
  mutate(category = ifelse(is.na(category),"company_name",category)) %>%
  filter(text != ">" & text != "^" & text != ";" & text != "<" & text != ">" & text != "#" & text != "@" & text != "`" & text != "E" & text != "%" & text != "$") %>%
  mutate(category = ifelse(str_detect(text,"@"),"email",category)) %>%
  group_by(category, grp = cumsum(category == "company_name" & lag(category, default = "") != "company_name")) %>%
  summarise(desc = paste(text, collapse =  " ")) %>%
  pivot_wider(id_cols = grp, names_from = category, values_from = desc) %>%
  select("company_name","main_product","address","no_workers","tel_no","fax_no","head_off_tel_no","head_off_fax_no","email","contact_person","department_occupation") %>%
  mutate(main_product = str_replace(main_product,"produksi utama main product","")) %>%
  mutate(main_product = str_replace(main_product,"C P O","CPO")) %>%
  mutate(main_product = str_replace(main_product,"CP.O","CPO")) %>%
  mutate(no_workers = str_replace(no_workers,"tenaga kerja person engaged","")) %>%
  mutate(head_off_tel_no = str_replace(head_off_tel_no,"telpon kantor pusat head office phone number","")) %>%
  mutate(tel_no = str_replace(tel_no,"telepon","")) %>%
  mutate(no_workers = sub("\\s+[^ ]+$", "",no_workers)) %>%
  mutate_all(na_if,"") %>%
  drop_na(main_product) %>%
  filter(stringr::str_detect(main_product, 'CPO|SAWIT'))


# Export to excel file
output_filename <- tools::file_path_sans_ext(basename(pdf_file))
write.xlsx(data,file=paste0(wdir,"\\extracted_data\\",output_filename,".xlsx")) 
