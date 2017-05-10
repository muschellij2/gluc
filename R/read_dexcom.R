#' @title Read Dexcom Output Data
#' @description Reads an excel file with Dexcom Raw data
#'
#' @param path Path of .xlsx file.  Must have a sheet named "DexcomRaw" in it
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
read_dexcom = function(path, raw_sheet = TRUE,
                       complete = TRUE, make_numeric = TRUE, ...) {
  sheet = "Dexcom"

  if (raw_sheet) {
    sheet = paste0(sheet, "Raw")
  }
  res = read_excel(path = path,
                   sheet = sheet,
                   col_names = TRUE)
  # cnames = c("PatientInfoField", "PatientInfoValue", "GlucoseInternalTime",
  #            "GlucoseDisplayTime", "GlucoseValue", "MeterInternalTime",
  #            "MeterDisplayTime",
  #            "MeterValue", "EventLoggedInternalTime",
  #            "EventLoggedDisplayTime",
  #            "EventTime", "EventType", "EventDescription")

  if (all(is.na(res))) {
    return(NULL)
  }
  res$PatientInfoField = NULL
  res$PatientInfoValue = NULL
  ##############################
  # Note Workaround
  GlucoseDisplayTime = GlucoseValue = NULL
  rm(list = c("GlucoseDisplayTime", "GlucoseValue"))
  ##############################

  res = dplyr::rename(
    res,
    time = GlucoseDisplayTime,
    glucose = GlucoseValue
  )
  if (make_numeric) {
    res$glucose = ensure_numeric(res$glucose)
  }
  res = sheet_check(res, complete = complete)
  return(res)
}