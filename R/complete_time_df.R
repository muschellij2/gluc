#' @title Complete all time intervals for a recording
#' @description Creates all the times for a certain interval and merges
#' with a current data set
#'
#' @param df A \code{data.frame} with a time column to be merged upon
#' @param interval The interval to space out the times
#' @param interval_unit the unit for the intervals
#'
#' @note If either \code{interval} or \code{interval_unit} are specified, the
#' other must be specified.  If both are \code{NULL}, then
#' \code{\link{interval_check}} is run and the minimum interval is used
#'
#' @return A \code{data.frame} of all the times in a sequcne
#' @export
#' @importFrom dplyr arrange full_join
complete_time_df = function(df, interval = NULL, interval_unit = NULL) {
  times = df$time

  num_null = sum(c(is.null(interval), is.null(interval_unit)))
  if (num_null == 1) {
    stop(paste0("Either both or neither of interval or interval_unit ",
         "must be specified if one is "))
  }
  if (is.null(interval) | is.null(interval_unit)) {
    icheck = interval_check(times)
    interval = icheck$interval
    interval_unit = icheck$interval_unit
  }
  time_df = create_all_intervals_df(times = times,
                                    interval = interval,
                                    interval_unit = interval_unit
  )
  df$observed = TRUE
  if (!all(times %in% time_df$time)) {
    warning("Some times are not present in the time_df!")
  }
  df = full_join(time_df, df, by = "time")
  df = arrange(df, time)
  df$observed[ is.na(df$observed) ] = FALSE

  return(df)
}
