#' Match groups of sites to their corresponding water quality standards
#'
#' Use group-WQS lookup table to determine what the WQS is for each group over
#' the course of the water year.
#'
#' @param wq_data
#' @param group_wqs
#'
#' @return
#' @export
#'
#' @examples
match_group_to_WQS = function(wq_data, group_wqs){
  wq_data = wq_data %>%
    mutate(Day = format(as.Date(Date), "%m/%d")) %>%
    left_join(group_wqs, by = "grp_type", relationship = "many-to-many") %>%
    mutate(dates_match = (Day > start_date & Day < end_date)) %>%
    filter(dates_match) %>%
    select(SiteName, Date, Day, sevenDADM, WQS) %>%
    mutate(Exceedance = (sevenDADM > WQS))
  return(wq_data)
}
