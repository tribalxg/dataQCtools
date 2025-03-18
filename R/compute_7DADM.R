#' Compute 7DADM
#'
#' For each period of seven consecutive days, this function computes the daily
#' max water temperature for each day, then computes the average of those daily
#' max temperatures. This implementation assumes the data has already been
#' quality-controlled to remove or impute data during time periods in which the
#' logger was dewatered, buried, or nonfunctional.
#'
#' @param all_data
#'
#' @return
#' @export
#'
#' @examples
compute_7DADM = function(all_data){
  print("Computing 7DADM...")
  temp_stats = all_data %>%
    # compute daily max water temp by site and date
    group_by(SiteName, Date)  %>%
    # summarize(DailyMax = max(WaterTemp, na.rm = TRUE),
    #           DailyMin = min(WaterTemp, na.rm = TRUE),
    #           DailyMean = mean(WaterTemp, na.rm = TRUE)) %>%
    summarize(DailyMax = max(WaterTemp, na.rm = TRUE)) %>%
    # summarize(DailyMax = max(WaterTemp, na.rm = TRUE), .groups = "drop_last") %>%
    ungroup() %>%
    # add in any missing SiteName-Date combinations so we can tell which data is
    # for consecutive dates
    right_join(data.frame(SiteName = rep(unique(all_data$SiteName),
                                         each = as.integer(max(all_data$Date)-min(all_data$Date))+1),
                          Date = rep(seq(min(all_data$Date), max(all_data$Date), 1),
                                     n_distinct(all_data$SiteName)))) %>%
    # sort by SiteName first, then by Date within SiteName
    arrange(SiteName, Date) %>%
    # compute 7DADM
    group_by(SiteName) %>%
    mutate(sevenDADM = mean_run(x = DailyMax, k = 7, lag = -3, idx = Date,
                                na_pad = TRUE, na_rm = FALSE)) %>%
    ungroup()

  print("Done computing 7DADM.")
  return(temp_stats)
}
