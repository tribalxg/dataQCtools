#' Read in quality-controlled data for all sites and deployments for a given water year
#'
#' @param fall_data_loc
#' @param sum_data_loc
#'
#' @return
#' @export
#'
#' @examples
read_in_qcd_data = function(fall_data_loc, sum_data_loc){
  print("Reading in QC'd data...")
  # assumes all data files are csv format, not xls or xlsx
  # read in fall deployment files
  # fall filename format: "AndersonLower_2022_fall_FINAL.csv"
  data_files_fall = list.files(path = fall_data_loc, pattern = '*csv')
  # read in summer deployment files
  # summer filename format: "AndersonLower_2023_sum_FINAL.csv"
  data_files_sum = list.files(path = sum_data_loc, pattern = '*csv')
  # combine fall and summer filenames into one list so we just need one loop
  data_files = c(data_files_fall, data_files_sum)

  # loop through each file, read it in, process it
  all_data <-NULL # Create new data frame to hold combined data

  file_no = 0
  for(this_file in data_files){ # for each file,
    # update file_no counter
    file_no = file_no + 1
    #this_file = data_files[51] # uncomment to troubleshoot within loop

    # extract metadata (sitename and deployment season) from filename
    filename_parts = strsplit(this_file, '[_.]')[[1]]
    sitename = filename_parts[1]
    deploy_season = paste(filename_parts[3], filename_parts[2])

    # all the fall files are first in the list, so we can use this to tell
    # from file_no whether the current file is a summer or fall deployment
    # and that tells us in which directory we can find this file
    if(file_no > length(data_files_fall)){
      file_dir = sum_data_loc
    }else{
      file_dir = fall_data_loc
    }

    # read in the data (this_file) from the right directory (file_dir)
    this_data = read_csv(file = paste0(file_dir, this_file),
                         skip = 1, # skip header line
                         # the only columns we really need are 2, 3, 10, and 13,
                         # (date, time, water temp, and the QC indicator UseForCalc)
                         # but we can keep them all for reference
                         col_select = c(2, 3, 10, 13), # read only the four columns of data we need
                         col_names = FALSE, # don't try to name columns from a row of the file
                         show_col_types = FALSE) %>% # suppresses print message
      data.frame() %>% # get rid of annoying attribute information
      # the next several lines
      # convert the character-format datetime to an R POSIXct object
      # ymd_hms is the format the character string is in initially; it tells R
      # how to read and interpret the character string
      rename(StartDate = X2,
             StartTime = X3,
             WaterTemp = X10,
             UseForCalc = X13) %>%
      mutate(StartDate = mdy(StartDate),
             StartTime = as.character(StartTime),
             SiteName = sitename,
             DeploySeason = deploy_season) %>%
      mutate(datetime = as.POSIXct(paste(StartDate, StartTime, " "),
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = "America/Los_Angeles")) %>%
      mutate(Date = as.Date(datetime, format= "%Y-%m-%d")) %>%
      # keep only the data with UseForCalc = 1 (data that passed QC)
      filter(UseForCalc == 1)

    # add this file's data to the combined data frame
    all_data <- bind_rows(all_data, this_data)
    # print some basic info to the R Console to update us on what was read in
    print(paste0(" - Site ", sitename, " Deployment ", deploy_season,
                 " Dates ", min(this_data$Date), " to ", max(this_data$Date)))
  } # end for-loop
  print("Done reading in QC'd data.")
  return(all_data)
}
