#' @title Create all time intervals for a recording
#' @description Creates all the times for a certain interval
#'
#' @param times Vector of times, usually POSIXct or other date format
#' @param interval The interval to space out the times
#' @param interval_unit the unit for the intervals
#' @param ... not used.
#'
#' @return A vector of times from minimum to the maximum
#' (including some potential roundup)
#' @note The additional \code{create_all_intervals_df} function is
#' to simply put this vector into a \code{data.frame} in a column called
#' \code{time} and a logical called \code{grid}
#' @export
create_all_intervals = function(times,
                                interval,
                                interval_unit = "mins") {
  time_range = range(times, na.rm = TRUE)
  drange = time_range[2] - time_range[1]
  units(drange) = interval_unit
  max_time = drange / interval
  max_time = ceiling(max_time) * interval
  max_time = time_range[1] + max_time
  interval = as.difftime(interval, units = interval_unit)
  out_times = seq(time_range[1], max_time, by = interval)
  return(out_times)
}

#' @rdname create_all_intervals
#' @export
create_all_intervals_df = function(...) {
  times = create_all_intervals(...)
  data.frame(time = times,
             grid = TRUE,
             stringsAsFactors = FALSE)
}