# this script crops raw data from HOBO files
#
# input: raw data files, ldrtimes file
# output: cropped data files
#
# Important:
#   Before running this script, rename raw HOBO files to have filenames in 
#     this format: sitename_medium_deployseason_deployyear.csv
#     e.g. NolanLower_air_sum_23.csv
# 
#   Make sure ldrtimes has columns for deployment season and year:
#   ldrtimes$deploy_season = "sum"
#   ldrtimes$deploy_year = "23"
#
# we can label the red lines on the plots if you like
#
# before running this script, update the file paths below
# - to toggle a line between commented (#...) and uncommented (...), use 
#   Shift-Control-C (Windows) or Shift-Command-C (Mac)
# - make sure to add a slash "/" at the end of each file path!
#

source("dataQCfunctions.R")

# base path: this is the shared part of the directory to your raw, cropped file, 
# and cropped plot directories
base_loc <- "2022/2022 summer_NEW/"

# directory containing raw data
rawdata_loc <- paste0(base_loc, "2. Cropped files/1. Raw csv/")
# directory where cropped data will be (or currently is) stored
# csv files in this directory are data files to be plotted!
cropped_loc <- paste0(base_loc, "2. Cropped files/2. Cropped csv/")
# directory where cropped vs raw plots will be stored
croppedplots_loc <- paste0(base_loc, "2. Cropped files/3. Cropped plots/")
# name of your LDRTimes file that has the lookup table with deployment/retrieval dates
ldrtimes_fn <- "LDRTimes_summer22.xlsx"

# check that R can find your raw data files
# Get all temperature data filenames
# note: * is called a glob, short for global
# IMPORTANT: filenames must have format sitename_medium_deployseason_deployyear.csv
#            e.g. NolanLower_air_sum_23.csv
csv_files = list.files(path = rawdata_loc, pattern = '*csv')
# this is now a list of all filenames; we haven't read in the data yet, but
# make sure this lists all the raw files you want to crop
csv_files

# read in LDR file and take a look at it
# note: this assumes the LDR file is in the folder indicated by rawdata_loc
ldrtimes = read_xlsx(paste0(rawdata_loc, ldrtimes_fn))

# crop the files!
crop_raw_data(rawdata_loc, cropped_loc, croppedplots_loc,
              ldrtimes_fn = ldrtimes_fn)


