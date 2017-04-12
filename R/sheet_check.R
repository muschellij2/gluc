#' @title Check and Complete a Sheet
#' @description Wrapper check for Abbott and Dexcom sheets
#'
#' @param df \code{data.frame} of the sheet with \code{time} as a column
#' @param complete Should all the times be completed for the data?
#' @param ... additional arguments passed to \code{\link{complete_time_df}}
#'
#'
#' @return Either the completed data.frame or NULL
#' @export
#' @importFrom dplyr filter
sheet_check = function(df, complete = TRUE, ...) {
  ##############################
  # Note Workaround
  time = NULL
  rm(list = "time")
  ##############################

  df = dplyr::filter(df, !is.na(time))
  # df = dplyr::filter(df, time >= lubridate::ymd("1950-01-01"))
  df = dplyr::filter(df, time >= as.Date("1950-01-01"))
  if (nrow(df) == 0) {
    return(NULL)
  }

  if (complete) {
    if (nrow(df) == 1) {
      warning("Cannot make a complete df, only one row!")
    } else {
      df = complete_time_df(df, ...)
    }
  }
  return(df)
}