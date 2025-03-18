#' Write group-WQS lookup table to file
#'
#' @param outdir
#'
#' @return
#' @export
#'
#' @examples
write_group_wqs = function(outdir){
  # define group_wqs
  group_wqs = data.frame(
    grp_type = c("core_sal",
                 rep("core_sal_supp1", 3),
                 rep("core_sal_supp2", 3),
                 "char"),
    start_date = c("01/01",
                   "01/01", "07/02", "09/01",
                   "01/01", "02/15", "07/02",
                   "01/01"),
    end_date =   c("12/31",
                   "07/01", "08/31", "12/31",
                   "02/14", "07/01", "12/31",
                   "12/31"),
    WQS = c(16,
            13, 16, 13,
            16, 13, 16,
            12)
  )

  # write to csv file
  write_csv(group_wqs, paste0(outdir, file = "group_wqs.csv"))
}
