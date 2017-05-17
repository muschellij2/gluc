#' @title Flag DEXCOM CGMS first 24 hours
#' @description Flags points of data within the first 24 hours of wear.
#'
#' @param glucose \code{data.frame} of glucose data with a column "time"
#'
#' @return A \code{data.frame} of values with additional columns for within
#' first 24 hours and time from baseline
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr "%>%"
#' @importFrom lubridate hours
first_24 = function(glucose) {
  tmp_id = time = time1 = first24 = NULL
  rm(list = c("time", "time1", "first24", "tmp_id"))
  glucose$tmp_id = seq(nrow(glucose))
  glucose = glucose %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      time1 = time[1], # first time
      # get time from first day
      # first24 = time1 + lubridate::as.period(24, unit = "hours"),
      first24 = time1 + hours(24),
      within_first24 = time <= first24, # flag if within
      time_from_baseline = time - time1) %>% # get diff_time
    dplyr::select(-time1, first24) %>%
    arrange(tmp_id)
  glucose$tmp_id = NULL
  return(glucose)
}