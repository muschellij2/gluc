#' @title Check the interval timing for glucose monitoring
#'
#' @param times A Date vector of day/times for records
#'
#' @return A list of a logical if all differences are multiples of the minimum
#' and the minimum interval time and the interval unit
#' @export
interval_check = function(times) {
  dtimes = diff(times)
  dtimes = dtimes[ !is.na(dtimes) ]
  interval_unit = units(dtimes)
  dtimes = sort(unique(dtimes))
  min_time = min(dtimes)
  if (min_time == 0) {
    warning("Minimum time is zero!")
  }
  mods = dtimes %% min_time
  L = list(
    all_multiples = all(mods == 0),
    interval = min_time,
    interval_unit = interval_unit
  )
  return(L)
}