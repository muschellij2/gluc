#' @title Flag DEXCOM CGMS first 24 hours
#' @description Flags points of data within the first 24 hours of wear.
#'
#' @param x \code{data.frame} of glucose data with a column "time"
#'
#' @return A \code{data.frame} of values with additional columns for within
#' first 24 hours and time from baseline
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr "%>%"
first_24 = function(x) {
  tmp_id = time = time1 = first24 = NULL
  rm(list = c("time", "time1", "first24", "tmp_id"))
  x$tmp_id = seq(nrow(x))
  x = x %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      time1 = time[1], # first time
      first24 = time1 + as.period(24, unit = "hours"), # get time from first day
      within_first24 = time <= first24, # flag if within
      time_from_baseline = time - time1) %>% # get diff_time
    dplyr::select(-time1, first24) %>%
    arrange(tmp_id)
  x$tmp_id = NULL
  return(x)
}