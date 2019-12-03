library(pdftools)
library(jsonlite)

# set dropbox directory intput --------------------------------------------

file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
if (length(file_name)==0){
  file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}

file_content <- fromJSON(txt=file_name)$personal
dropbox_dir <- file_content$path
wdir <- paste0(dropbox_dir,"\\kraus\\data\\direktori_industri\\")
setwd(wdir)

# Read pdf data -----------------------------------------------------------

pdf_file = paste0(wdir,"Direktori_Industri_Pengolahan_Indonesia_2003-pages-43-59.pdf")

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
  mutate(text = trimws(text,which="right")) %>%
  mutate(category = ifelse(str_detect(text,"^[:upper:]+$") & category != "main_product","company_name",category)) %>%
  mutate(category = ifelse(str_detect(text,"^[:upper:]+$ ^[:upper:]+$") & category !="company name","company_name",category)) %>%
  mutate(category = ifelse(str_detect(text,"PT"),"company_name",category)) %>%
  #mutate(category = ifelse(str_detect(text,"^[:upper:]+$") & category == "fax_no","company_name",category)) %>%
  mutate(category = ifelse(is.na(category), "company_name",category)) %>%
  mutate(category = ifelse(text =="SH", "contact_person",category)) %>%
  mutate(category = ifelse(text =="E", "email",category)) %>%
  mutate(category = ifelse(text =="M", "address",category)) %>%
  mutate(category = ifelse(text =="S", "address",category)) %>%
  #mutate(category = ifelse(text =="III", "address",category)) %>%
  #mutate(category = ifelse(text =="II", "address",category)) %>%
  #mutate(category = ifelse(text =="IV", "address",category)) %>%
  mutate(category = ifelse(text =="BUAH DHT", "company_name",category)) %>%
  #mutate(comp_id = ifelse(category =="company_name",paste0("company","_",y),NA)) %>%
  #fill(comp_id) %>%
  #group_by(comp_id) %>%
  filter(text != ">" & text != "^" & text != ";" & text != "<" & text != ">" & text != "#" & text != "@" & text != "`" & text != "E" & text != "%" & text != "$") %>%
  group_by(category, grp = cumsum(category == "company_name" & lag(category, default = "") != "company_name")) %>%
  summarise(desc = paste(text, collapse =  " ")) %>%
  pivot_wider(id_cols = grp, names_from = category, values_from = desc) %>%
  #group_by(comp_id,category) %>%
  #summarise(comp_info = paste(text, collapse = " ")) %>%
  select("company_name","main_product","address","no_workers","tel_no","fax_no","head_off_tel_no","head_off_fax_no","email","contact_person","department_occupation")
