#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Robert Heilmayr, Frances Moore, Joan Dudney
# Project: IBS
# Date: 9/8/23
# Purpose: Creates a symlink from code directory to a directory storing project data
# Note: Should be run with administrator priviledges
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # Build compute environment -----------------------------------------
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# library(renv)
# renv::restore()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Create symbolic link to data directory -----------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(R.utils)

# Define the path to your local code directory
code_dir <- "D:/dev/ibs/"

# Define the root directory where you've unzipped the replication dataset.
# The replication dataset can be downloaded from XX.
# After unzipping, the root directory should contain the "1_input_processed" directory.
data_dir <- "D:/cloud/dropbox/collaborations/indonesia/indo_mill_spillovers/ucsb-kraus/"

createLink(paste0(code_dir, 'remote'), data_dir, overwrite = FALSE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Add output directory structure -----------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
