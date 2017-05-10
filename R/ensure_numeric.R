#' @title Ensure a numeric output
#' @description Simple wrapper for character to numeric conversion
#' @param x vector of values
#'
#' @return vector of numeric values
ensure_numeric = function(x) {
  as.numeric(as.character(x))
}