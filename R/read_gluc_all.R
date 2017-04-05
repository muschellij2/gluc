#' @title Read All Glucose Sheets from file
#' @description A global function to read the different types of glucose
#' data from an xlsx
#'
#' @param path paths of xlsx files
#' @param type type of file to read, defaults to all
#' @param ... additional argument to send to the read function related to
#' \code{type}
#'
#' @return A list of \code{tbl} data.frame for each type.
#' @export
read_gluc_all = function(
  path,
  type = c("Abbott", "Dexcom",
           "AbbottRaw", "DexcomRaw"),
  ...){

  type = match.arg(type, several.ok = TRUE)
  res = lapply(type, function(x) {
    read_gluc(path = path, type = x, ...)
  })
  names(res) = type
  return(res)
}