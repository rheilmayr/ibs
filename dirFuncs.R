library(jsonlite)

# Function to find path of dropbox folder on a random computer
find_dropbox <- function(){
  file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
  if (length(file_name)==0){
    file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}
  file_content <- fromJSON(txt=file_name)$personal
  dropbox_dir <- file_content$path
  dropbox_dir <- paste0(dropbox_dir, "\\")
  return(dropbox_dir)
  }