#' @title Read Glucose XLS Data file
#' @description A global function to read the different types of glucose
#' data
#'
#' @param path path of xlsx file
#' @param type type of file to read
#' @param min_rows Minimum number of rows
#' @param ... additional argument to send to the read function related to
#' \code{type}
#'
#' @return A \code{tbl} data.frame type.
#' @importFrom pbapply pblapply
#' @importFrom dplyr group_by tally left_join filter arrange
#' @export
read_gluc = function(
  path,
  type = c("Abbott", "Dexcom",
           "AbbottRaw", "DexcomRaw"),
  min_rows = 5,
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
    res = pblapply(path, function(x) {
      args$path = x
      res = do.call(what = func, args = args)
      if (!is.null(res)) {
        res$file = x
      }
      return(res)
    })
    res = data.table::rbindlist(res, fill = TRUE)
    res = as.data.frame(res)
    res$RERUN_INDEX = seq(nrow(res))
    res = dplyr::group_by(.data = res, file)
    tal = dplyr::tally(res)
    colnames(tal) = c("file", "NUMBER_OF_TIME_POINTS")
    res = dplyr::left_join(res, tal, by = file)
    res = dplyr::filter(res, NUMBER_OF_TIME_POINTS >= min_rows)
    res$NUMBER_OF_TIME_POINTS = NULL
    res = dplyr::arrange(res, RERUN_INDEX)
    res$RERUN_INDEX = NULL

  }

  if (!is.null(res)) {
    attr(res, "gluc_type") = type
  }
  return(res)
}