#' @title Quality Control Measures for Dexcom CGMS
#' @description Flags points of questionable quality for a Dexcom
#'
#' @param x \code{data.frame} of glucose data with columns of
#'
#' @return A \code{data.frame} of values with a qc column
#' @note This is a wrapper for \code{\link{first_24}},...
#' @export
qc_dexcom = function(x) {
  max_diff = max(abs(x$time - x$internal_time))
  x = first_24(x)

  L = list(
    glucose = x,
    max_diff = max_diff
  )
  return(L)
}