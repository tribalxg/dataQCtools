# IMPORTANT: filenames must have format sitename_medium_deployseason_deployyear.csv
#            e.g. NolanLower_air_sum_23.csv
# Note:
# if you get warning messages like the following, it's okay; it is just telling
# you how many missing values you have
#1: Removed 189 rows containing missing values or values outside the scale range (`geom_line()`).
#2: Removed 189 rows containing missing values or values outside the scale range (`geom_point()`)

#' Crop raw water/air temperature data from HOBO sensors
#'
#' @param rawdata_loc File path to the directory containing the raw data to be cropped. The names of the raw data files must be in the format sitename_medium_deployseason_deployyear.csv, e.g. NolanLower_air_sum_23.csv.
#' @param ldrtimes_fn Filename for the LDRTimes file, the lookup table of deployment and retrieval times. This file is assumed to be in the same folder as the raw data.
#' @param cropped_loc File path to the directory where cropped data files will be written and stored.
#' @param croppedplots_loc File path to the directory where plots of cropped and raw data will be stored.
#'
#' @return
#' @export
#'
#' @examples
#'
#' library(readxl)
#' library(dplyr)
#' library(ggplot2)
#'
#' base_loc <- "data/2022_summer/"
#' rawdata_loc <- paste0(base_loc, "1_raw_csv/")
#' cropped_loc <- paste0(base_loc, "2_cropped_csv/")
#' croppedplots_loc <- paste0(base_loc, "2_cropped_plots/")
#' ldrtimes_fn <- "LDRTimes_summer22.xlsx"

#' # check that R can find your raw data files
#' # Get a list of the filenames of all the raw data files you will be clipping
#' csv_files = list.files(path = rawdata_loc, pattern = '*csv')
#' # this is now a list of all filenames; we haven't read in the data yet, but
#' # make sure this lists all the raw files you want to crop:
#' csv_files
#'
#' # read in LDRTimes lookup table and take a look at it
#' ldrtimes = readxl::read_xlsx(paste0(rawdata_loc, ldrtimes_fn))
#' ldrtimes # or View(ldrtimes)
#'
#' # then crop the data!
#' crop_raw_data(rawdata_loc, ldrtimes_fn, cropped_loc, croppedplots_loc)
#'
crop_raw_data = function(rawdata_loc, ldrtimes_fn, cropped_loc, croppedplots_loc){
  # get a list of all the temperature data filenames
  csv_files = list.files(path = rawdata_loc, pattern = '*csv')
  # read in LDR file
  # - this assumes the LDR file is an excel file in the folder indicated by rawdata_loc
  ldrtimes = read_xlsx(paste0(rawdata_loc, ldrtimes_fn))

  i = 0
  for(this.file in csv_files){
    i = i + 1
    #this.file = csv_files[1] # uncomment to troubleshoot within loop
    cat(paste0("Reading file ", i, " of ", length(csv_files), ": ", this.file), fill = TRUE)

    # extract metadata from the filename
    filename.parts = strsplit(this.file, '[_.]')[[1]]
    csv.site = filename.parts[1]
    csv.media = filename.parts[2]
    csv.season = filename.parts[3]
    csv.year = filename.parts[4]

    # convert the character-format datetime to an R POSIXct object
    # ymd_hm is the format the character string is in initially; it tells R
    # how to read and interpret the character string
    # sometimes R reads in the datetime format as mdy_hms and sometimes mdy_hm.
    # This tryCatch handles either hh:mm:ss or hh:mm format in csv files
    this.data =  tryCatch(
      {
        readr::read_csv(paste0(rawdata_loc, this.file),
                 skip = 2, # skip the first two lines of the file
                 col_select = 1:3, # read only the first three columns of data
                 col_names = FALSE, # don't try to name columns from a row of the file
                 show_col_types = FALSE) %>% # suppresses print message
          dplyr::rename("row.num" = X1,
                 "datetime" = X2,
                 "temperature" = X3) %>%
          dplyr::mutate(datetime = mdy_hms(datetime)) #for datetime in hh:mm:ss
      },
      warning = function(cond) { #if datetime isn't in hh:mm:ss, will now try hh:mm format
        readr::read_csv(paste0(rawdata_loc, this.file),
                 skip = 2, # skip the first two lines of the file
                 col_select = 1:3, # read only the first three columns of data
                 col_names = FALSE, # don't try to name columns from a row of the file
                 show_col_types = FALSE) %>% # suppresses print message
          dplyr::rename("row.num" = X1,
                 "datetime" = X2,
                 "temperature" = X3) %>%
          dplyr::mutate(datetime = mdy_hm(datetime)) #for datetime in hh:mm
      }
    )

    # crop the data
    deploy.retrieval = ldrtimes %>%
      # select the row(s) of ldrtimes that match this datafile
      # should be exactly one row, but if there are no rows or multiple rows that
      # match, this step will pull that many rows
      dplyr::filter(site == csv.site, deploy_season == csv.season,
             deploy_year == csv.year, media == csv.media) %>%
      # keep just the deploy_time and retrieval_time variables/columns
      dplyr::select(deploy_time, retrieval_time)

    if(nrow(deploy.retrieval) == 0){
      stop("no rows of ldrtimes matched this csv file.")
    }
    if(nrow(deploy.retrieval) > 1){
      stop("multiple rows of ldrtimes matched this csv file.")
    }

    deploy = deploy.retrieval$deploy_time
    retrieval = deploy.retrieval$retrieval_time

    if(retrieval > deploy) {
      cropped.data = dplyr::filter(this.data,
                            datetime > deploy,
                            datetime < retrieval)

    } # if(retrieval > deploy)

    # write cropped csv files to cropped folder
    readr::write_csv(cropped.data,
              file=paste0(cropped_loc, str_split_i(this.file,"[.]",1),"_cropped.csv"))

    #Create a dataframe of the raw and cropped data
    cropvraw <- dplyr::left_join(this.data, cropped.data, by=c("row.num", "datetime")) %>%
      rename(raw.temp=temperature.x, cropped.temp =temperature.y) %>%#rename temperature from each file
      #create new column of data type (raw or cropped for plotting in ggplot)
      tidyr::pivot_longer(cols=raw.temp:cropped.temp, names_to="type", values_to="temp")

    cropvraw.plot <- ggplot(cropvraw, aes(x=datetime, y=temp, color=type)) +
      geom_line() +
      geom_point() +
      labs(title = paste0(" Raw versus Cropped data"),
           x = "Date", y = "Temperature (C)")+
      theme(axis.text = element_text(colour = "black", size = (12)))

    ggsave(paste0(croppedplots_loc, csv.site, "_rawvscroppeddata.png"), cropvraw.plot,
           width = 11, height = 8.5, units = "in")

  }

  cat("Done.", fill = TRUE)
}
