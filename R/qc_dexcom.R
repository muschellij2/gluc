#' @title Quality Control Measures for Dexcom CGMS
#' @description Flags points of questionable quality for a Dexcom
#'
#' @param glucose \code{data.frame} of glucose data with columns of
#'
#' @return A \code{data.frame} of values with a qc column
#' @note This is a wrapper for \code{\link{first_24}},...
#' @export
qc_dexcom = function(glucose) {
  max_diff = max(abs(glucose$time - glucose$internal_time))
  glucose = first_24(glucose)

  L = list(
    glucose = glucose,
    max_diff = max_diff
  )
  return(L)
}