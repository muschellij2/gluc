#' @title Approximate Glucose Data
#' @description Creates a \code{data.frame} of all the times interpolated to
#' the interval.   Uses \code{\link{approxfun}} to approximate
#'
#' @param glucose A \code{data.frame} with a time column and glucose column
#' @param interval The interval to space out the times
#' @param interval_unit the unit for the intervals
#' @param round_second Round the seconds in the times
#' @param ... additional arguments passed to \code{\link{approxfun}}
#'
#'
#' @return A \code{data.frame} of all the times in the range of the original data
#' @export
#' @importFrom dplyr full_join
#' @importFrom stats approxfun
#' @importFrom lubridate second
approx_glucose = function(
  glucose,
  interval = 1,
  interval_unit = "mins",
  round_second = TRUE, ...) {

  df = glucose[ !is.na(glucose$glucose), ]
  df$orig_time = df$time
  df$observed = TRUE

  round_seconds = function(x) {
    round(lubridate::second(x)/60)*60
  }
  if (round_second) {
    lubridate::second(df$time) = round_seconds(df$time)
  }

  num_null = sum(c(is.null(interval), is.null(interval_unit)))
  if (num_null == 1) {
    stop(paste0("Either both or neither of interval or interval_unit ",
                "must be specified if one is "))
  }
  time_range = range(df$time, na.rm = TRUE)

  time_df = create_all_intervals_df(
    times = df$time,
    interval = interval,
    interval_unit = interval_unit
  )
  fun = stats::approxfun(x = df$time, y = df$glucose, ...)
  time_df$glucose = fun(time_df$time)

  time_df$observed = time_df$time %in% df$time

  return(time_df)
}
