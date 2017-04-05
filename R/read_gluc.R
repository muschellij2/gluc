#' @title Read Glucose XLS Data file
#' @description A global function to read the different types of glucose
#' data
#'
#' @param path path of xlsx file
#' @param type type of file to read
#' @param ... additional argument to send to the read function related to
#' \code{type}
#'
#' @return A \code{tbl} data.frame type.
#' @export
read_gluc = function(
  path,
  type = c("Abbott", "Dexcom",
           "AbbottRaw", "DexcomRaw"),
  ...){

  type = match.arg(type)
  raw_sheet = grepl("raw$", tolower(type))
  type = gsub("Raw$", "", type)
  func = switch(type,
                Abbott = "read_abbott",
                Dexcom = "read_dexcom")

  args = list(raw_sheet = raw_sheet, ...)

  ##############################
  # Ability to run multiple paths
  ##############################
  one_path = length(path) == 1
  if (one_path) {
    args$path = path
    res = do.call(what = func, args = args)
    if (!is.null(res)) {
      res$file = path
    }
  } else {
    res = lapply(path, function(x) {
      args$path = x
      res = do.call(what = func, args = args)
      if (!is.null(res)) {
        res$file = x
      }
      return(res)
    })
    res = data.table::rbindlist(res, fill = TRUE)
    res = as.data.frame(res)
  }

  if (!is.null(res)) {
    attr(res, "gluc_type") = type
  }
  return(res)
}