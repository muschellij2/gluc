#' @title Read Abbott Output Data
#' @description Reads an excel file with Abbot Raw data
#'
#' @param path Path of .xlsx file.  Must have a sheet named "AbbottRaw" in it
#' @param raw_sheet Should the raw or processed sheet be read in?
#' @param complete Should all the times be completed for the data?
#' @param make_numeric Should glucose be coerced to numeric?  Values such as
#' "\code{Low}" would be missing in the glucose data
#' @param ... additional arguments passed to \code{\link{complete_time_df}}
#'
#' @return If the sheet is not empty, it will return a \code{data.frame} of
#' values.  Otherwise, it will return \code{NULL}
#' @export
#' @importFrom readxl read_excel
#' @importFrom dplyr rename filter
read_abbott = function(path, raw_sheet = TRUE, complete = TRUE,
                       make_numeric = TRUE, ...
                       ) {
  sheet = "Abbott"
  if (raw_sheet) {
    sheet = paste0(sheet, "Raw")
  }
  res = read_excel(path = path,
                   sheet = sheet,
                   col_names = TRUE,
                   skip = ifelse(raw_sheet, 2, 0))

  # cnames = c("ID", "Time", "Record Type",
  #            "Historic Glucose (mg/dL)")
  if (all(is.na(res))) {
    return(NULL)
  }
  ##############################
  # Note Workaround
  Time = `Historic Glucose (mg/dL)` = NULL
  rm(list = c("Time", "Historic Glucose (mg/dL)"))
  ##############################
  res = dplyr::rename(
    res,
    time = Time,
    glucose = `Historic Glucose (mg/dL)`
  )
  if (make_numeric) {
    res$glucose = ensure_numeric(res$glucose)
  }
  res = sheet_check(res, complete = complete, ...)
  return(res)
}