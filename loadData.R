# Import libraries
source("dirFuncs.R")

# Identify dropbox directory and set up paths for inputs and outputs
dropbox_dir <- find_dropbox()
root_dir <- paste0(dropbox_dir, "collaborations\\ucsb-kraus\\")
data_dir <- paste0(root_dir, "data\\")
out_dir <- paste0(root_dir, "outputs\\")

# Load IBS data
data <- read.csv(paste0(data_dir, "IBS_mill_cleaned.csv"))
