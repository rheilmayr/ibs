
#### Using pdftools to extract text from Direkti Industri Pengolahan / Manufaktur Indonesia into tabular format

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

# pre2008 pdf's

# Read pdf file ----------------------------------------------------------
pre2008 <- paste0(wdir,"pdf\\pre2008\\")
setwd(pre2008)

filenames <- list.files(pattern=".*pdf")

for (i in filenames) {
  pdf_file = paste0(pre2008,i)

  # get report year
  year <- str_extract_all(i, "2\\d{3}", simplify = T)
  #year <- yearExtract(pdf_file)

# Cleaning up data -------------------------------------------------------
data <- pdf_data(pdf_file) %>% 
  bind_rows() %>%
  filter(height <=8) %>%
  # Setting delimiters to get different information in pdf file
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
  mutate(text = str_replace(text,","," ")) %>% # remove commas
  mutate(text = str_replace(text,"-"," ")) %>% # remove hyphens
  mutate(text = str_replace(text,'"',"")) %>% # remove quotes
  mutate(text = str_replace(text,"[.]","")) %>% # remove periods
  mutate(text = str_replace(text,"/"," ")) %>% # remove forward slashes
  mutate(text = str_replace(text,"[(]"," ")) %>% # remove left parenthesis
  mutate(text = str_replace(text,"[)]"," ")) %>% # remove right parenthesis
  mutate(text = trimws(text,which="right")) %>% # trim right whitespace
  mutate(text = trimws(text,which="left")) %>% # trim left whitespace
  mutate(category = ifelse(str_detect(text,"^[:upper:]+$") & category != "main_product","company_name",category)) %>% # anything with uppercase to company name
  mutate(category = ifelse(str_detect(text,"^[:upper:]+$ ^[:upper:]+$") & category !="company name","company_name",category)) %>% # anything with two uppercase words to company name
  mutate(category = ifelse(str_detect(text,"PT"),"company_name",category)) %>% # anything with 'PT' to company name
  mutate(category = ifelse(is.na(category), "company_name",category)) %>% # NA categories at top rows to company name (first company)
  mutate(category = ifelse(text =="SH", "contact_person",category)) %>% # SH is part of contact person
  mutate(category = ifelse(text =="SE", "contact_person",category)) %>% # SE is part of contact person
  mutate(category = ifelse(text =="E", "email",category)) %>% # E is part of email
  mutate(category = ifelse(text =="M", "address",category)) %>% # M is part of address
  mutate(category = ifelse(text =="S", "address",category)) %>% # S is part of address
  mutate(category = ifelse(text =="BUAH DHT", "company_name",category)) %>% # BUAH DHT is part of company name
  mutate(category = ifelse(text =="SIRINGO RINGO", "company_name",category)) %>% # SIRINGO RINFO is part of company name
  mutate(category = ifelse(lag(category) == "address" & lead(category) == "address","address",category)) %>% # if before and after value in row is address, change in between value to address
  mutate(category = ifelse(lag(category) == "company_name" & lead(category) == "company_name","company_name",category)) %>% # if before and after value in row is company name, change in between value to company name
  mutate(category = ifelse(lag(category) == "department_occupation" & lead(category) == "department_occupation","department_occupation",category)) %>% # if before and after value in row is department occupation, change in between value to department occupation
  mutate(category = ifelse(is.na(category),"company_name",category)) %>% # any rows with NA is company name  
  filter(text != ">" & text != "^" & text != ";" & text != "<" & text != ">" & text != "#" & text != "@" & text != "`" & text != "E" & text != "%" & text != "$") %>% # remove delimiters
  mutate(category = ifelse(str_detect(text,"@"),"email",category)) %>% # remove emails that are part of company names
  group_by(category, grp = cumsum(category == "company_name" & lag(category, default = "") != "company_name")) %>%  # split into columns at each company name
  summarise(desc = paste(text, collapse =  " ")) %>% 
  pivot_wider(id_cols = grp, names_from = category, values_from = desc) %>%
  select("company_name","main_product","address","no_workers","tel_no","fax_no","head_off_tel_no","head_off_fax_no","email","contact_person","department_occupation") %>% #select required columns
  mutate(main_product = str_replace(main_product,"produksi utama main product","")) %>% # remove erroneous values in column
  mutate(main_product = str_replace(main_product,"C P O","CPO")) %>% # standardize CPO naming in product column
  mutate(main_product = str_replace(main_product,"CP.O","CPO")) %>% # standardize CPO naming in product column
  mutate(no_workers = str_replace(no_workers,"tenaga kerja person engaged","")) %>% # remove erroneous values in column
  mutate(head_off_tel_no = str_replace(head_off_tel_no,"telpon kantor pusat head office phone number","")) %>% # remove erroneous values in column
  mutate(tel_no = str_replace(tel_no,"telepon","")) %>% # remove erroneous values in column
  mutate(address = str_replace(address,"alamat address","")) %>% # remove erroneous values in column
  mutate(no_workers = sub("\\s+[^ ]+$", "",no_workers)) %>% # remove additional numbers in no of workers column
  mutate_all(na_if,"") %>% # remove any rows with no values
  drop_na(main_product) %>% # drop rows with NA's in main product column
  filter(str_detect(main_product, 'CPO|SAWIT|PALM|RBDPO|GORENG')) %>% # filter only palm products (includes CPO,sawit) %>%
  add_column(year=year)

# Export to excel file --------------------------------------------
output_filename <- tools::file_path_sans_ext(basename(pdf_file))
write.xlsx(data,file=paste0(year,".xlsx")) 

}

# post 2008 pdf's

# Read pdf file ----------------------------------------------------------
post2008 <- paste0(wdir,"pdf\\post2008\\")
setwd(post2008)

filenames <- list.files(pattern=".*pdf")

for (i in filenames) {
  pdf_file = paste0(post2008,i)
  
  # get report year
  year <- str_extract_all(i, "2\\d{3}", simplify = T)
  #year <- yearExtract(pdf_file)
  
data <- pdf_data(pdf_file) %>% 
  bind_rows() %>%
  filter(height <=8) %>%
  # Setting delimiters to get different information in pdf file
  mutate(category = ifelse(text =="^","main_product",
                           ifelse(text ==";","no_workers",
                                  ifelse(text =="`","address",
                                         ifelse(text == "%","tel_no",
                                                ifelse(text == "#","fax_no",
                                                       ifelse(text == ":","head_off_address",
                                                       ifelse(text == "E","email",NA)))))))) %>%
  fill(category) %>%
  filter(y != 74) %>% # remove categories
  mutate(text = str_replace(text,","," ")) %>% # remove commas
  mutate(text = str_replace(text,"-"," ")) %>% # remove hyphens
  mutate(text = str_replace(text,'"',"")) %>% # remove quotes
  mutate(text = str_replace(text,"[.]","")) %>% # remove periods
  mutate(text = str_replace(text,"/"," ")) %>% # remove forward slashes
  mutate(text = str_replace(text,"[(]"," ")) %>% # remove left parenthesis
  mutate(text = str_replace(text,"[)]"," ")) %>% # remove right parenthesis
  mutate(text = trimws(text,which="right")) %>% # trim right whitespace
  mutate(text = trimws(text,which="left")) %>% # trim left whitespace
  mutate(category = ifelse(str_detect(text,"^[:upper:]+$") & category != "main_product","company_name",category)) %>% # anything with uppercase to company name
  mutate(category = ifelse(str_detect(text,"^[:upper:]+$ ^[:upper:]+$") & category !="company name","company_name",category)) %>% # anything with two uppercase words to company name
  mutate(category = ifelse(str_detect(text,"PT"),"company_name",category)) %>% # anything with 'PT' to company name
  mutate(category = ifelse(is.na(category), "company_name",category)) %>% # NA categories at top rows to company name (first company)
  mutate(category = ifelse(text =="SH", "contact_person",category)) %>% # SH is part of contact person
  mutate(category = ifelse(text =="SE", "contact_person",category)) %>% # SE is part of contact person
  #mutate(category = ifelse(text =="E", "email",category)) %>% # E is part of email
  #mutate(category = ifelse(text =="M", "address",category)) %>% # M is part of address
  #mutate(category = ifelse(text =="S", "address",category)) %>% # S is part of address
  mutate(category = ifelse(text =="BUAH DHT", "company_name",category)) %>% # BUAH DHT is part of company name
  mutate(category = ifelse(text =="SIRINGO RINGO", "company_name",category)) %>% # BUAH DHT is part of company name
  mutate(category = ifelse(lag(category) == "address" & lead(category) == "address","address",category)) %>% # if before and after value in row is address, change in between value to address
  mutate(category = ifelse(lag(category) == "company_name" & lead(category) == "company_name","company_name",category)) %>% # if before and after value in row is company name, change in between value to company name
  #mutate(category = ifelse(lag(category) == "department_occupation" & lead(category) == "department_occupation","department_occupation",category)) %>% # if before and after value in row is department occupation, change in between value to department occupation
  mutate(category = ifelse(is.na(category),"company_name",category)) %>% # any rows with NA is company name  
  filter(text != ">" & text != "^" & text != ";" & text != "<" & text != ">" & text != "#" & text != "@" & text != "`" & text != "E" & text != "%" & text != "$" & text != ":") %>% # remove delimiters
  mutate(category = ifelse(str_detect(text,"@"),"email",category)) %>% # remove emails that are part of company names
  group_by(category, grp = cumsum(category == "company_name" & lag(category, default = "") != "company_name")) %>%  # split into columns at each company name
  summarise(desc = paste(text, collapse =  " ")) %>% 
  pivot_wider(id_cols = grp, names_from = category, values_from = desc) %>%
  select("company_name","main_product","address","no_workers","tel_no","fax_no","head_off_address","email") %>% #select required columns
  mutate(main_product = str_replace(main_product,"produksi utama main product","")) %>% # remove erroneous values in column
  mutate(main_product = str_replace(main_product,"C P O","CPO")) %>% # standardize CPO naming in product column
  mutate(main_product = str_replace(main_product,"CP.O","CPO")) %>% # standardize CPO naming in product column
  mutate(no_workers = str_replace(no_workers,"tenaga kerja person engaged","")) %>% # remove erroneous values in column
  #mutate(head_off_tel_no = str_replace(head_off_tel_no,"telpon kantor pusat head office phone number","")) %>% # remove erroneous values in column
  mutate(tel_no = str_replace(tel_no,"phone number","")) %>% # remove erroneous values in column
  mutate(tel_no = str_replace(tel_no,"telepon","")) %>% # remove erroneous values in column
  mutate(fax_no = str_replace(fax_no,"fax pabrik fax number of establishment","")) %>% # remove erroneous values in column
  mutate(address = str_replace(address,"alamat pabrik address of establishment","")) %>% # remove erroneous values in column
  mutate(head_off_address = str_replace(head_off_address,"alamat kantor pusat address of head office telpon kantor pusat head office phone number fax kantor pusat head office fax number",""))	%>%
  mutate(no_workers = sub("\\s+[^ ]+$", "",no_workers)) %>% # remove additional numbers in no of workers column
  mutate_all(na_if,"") %>% # remove any rows with no values
  drop_na(main_product) %>% # drop rows with NA's in main product column
  filter(str_detect(main_product, 'CPO|SAWIT|PALM|RBDPO|GORENG')) # filter only palm products (includes CPO,sawit)

# Export to excel file --------------------------------------------
output_filename <- tools::file_path_sans_ext(basename(pdf_file))
write.xlsx(data,file=paste0(year,".xlsx")) 

}
