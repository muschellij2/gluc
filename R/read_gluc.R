#' @title Read Glucose XLS Data file
#' @description A global function to read the different types of glucose
#' data
#'
#' @param path path of xlsx file
#' @param type type of file to read
#' @param ... additional argument to send to the read function related to
#' \code{type}
#'
#' @return A \code{tbl} data.frame type
#' @export
read_gluc = function(path, type = c("Abbott", "Dexcom"), ...){
  type = match.arg(type)
  func = switch(type,
         Abbott = "read_abbott",
         Dexcom = "read_dexcom")
  args = list(path = path, ...)
  res = do.call(what = func, args = args)
  return(res)
}