#' @title Read Dexcom Output Data
#' @description Reads an excel file with Dexcom Raw data
#'
#' @param path Path of .xlsx file.  Must have a sheet named "DexcomRaw" in it
#' @param raw_sheet Should the raw or processed sheet be read in?
#'
#' @return If the sheet is not empty, it will return a \code{data.frame} of
#' values.  Otherwise, it will return \code{NULL}
#' @export
#' @importFrom readxl read_excel
read_dexcom = function(path, raw_sheet = TRUE) {
  sheet = "Dexcom"
  if (raw_sheet) {
    sheet = paste0(sheet, "Raw")
  }
  res = read_excel(path = path,
                   sheet = sheet,
                   col_names = TRUE)
  cnames = c("PatientInfoField", "PatientInfoValue", "GlucoseInternalTime",
             "GlucoseDisplayTime", "GlucoseValue", "MeterInternalTime",
             "MeterDisplayTime",
             "MeterValue", "EventLoggedInternalTime",
             "EventLoggedDisplayTime",
             "EventTime", "EventType", "EventDescription")

  if (all(is.na(res))) {
    return(NULL)
  }
  res$PatientInfoField = NULL
  res$PatientInfoValue = NULL
  # id_row = as.data.frame(res)[1,1]
  # res = res[-c(1:2),]
  # colnames(res) = as.character(res[1,])
  # res = res[-1,]
  # attr(res, "id_value") = id_row
  # res = as.data.frame(res)
  return(res)
}