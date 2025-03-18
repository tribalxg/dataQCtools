#' Match individual sites to water quality standards
#'
#' Use a site-group lookup table to determine which WQS group each site is in.
#'
#' @param wq_data
#' @param site_group
#'
#' @return
#' @export
#'
#' @examples
match_sites_to_WQS_groups = function(wq_data, site_group){
  return(left_join(wq_data, site_group, by = join_by(SiteName == site)))
}
