#' @title Read Dexcom Export Data
#' @description Reads a raw Dexcom export
#'
#' @param path Path of .txt file
#' @param complete Should all the times be completed for the data?
#' @param make_numeric Should glucose be coerced to numeric?  Values such as
#' "\code{Low}" would be missing in the glucose data
#' @param ... additional arguments passed to \code{\link{complete_time_df}}
#'
#' @return If the sheet is not empty, it will return a \code{data.frame} of
#' values.  Otherwise, it will return \code{NULL}
#' @export
#' @importFrom readr read_tsv
#' @importFrom dplyr select filter everything mutate
#' @importFrom lubridate as.period
read_dexcom_export = function(path,
                              complete = TRUE,
                              make_numeric = TRUE, ...) {

  ##############################
  # Note Workaround
  GlucoseInternalTime = GlucoseDisplayTime = GlucoseValue = NULL
  rm(list = c("GlucoseDisplayTime", "GlucoseValue"))
  PatientInfoField = PatientInfoValue = NULL
  rm(list = c("PatientInfoField", "PatientInfoValue"))
  MeterInternalTime = MeterDisplayTime = MeterValue = NULL
  rm(list = c("MeterInternalTime", "MeterDisplayTime", "MeterValue"))
  time = glucose = internal_time = NULL
  rm(list = c("time", "glucose", "internal_time"))
  ##############################

  res = readr::read_tsv(file = path,
                        col_names = TRUE)
  if (all(is.na(res))) {
    return(NULL)
  }
  full_data = res
  #############################
  # Header information
  #############################
  header = dplyr::select(res,
                         PatientInfoField, PatientInfoValue)
  header = dplyr::filter(header,
                         !is.na(PatientInfoField) | !is.na(PatientInfoValue))
  #############################
  # Meter Data - calibration
  #############################
  res = dplyr::select(res,
                      -PatientInfoField, -PatientInfoValue)
  meter = dplyr::select(res,
                        MeterInternalTime, MeterDisplayTime, MeterValue)
  colnames(meter) = c("internal_time", "time", "glucose")
  if (make_numeric) {
    meter$glucose = ensure_numeric(meter$glucose)
  }
  meter = dplyr::filter(meter,
                         !is.na(internal_time) |
                          !is.na(time) |
                          !is.na(glucose))
  meter = select(meter, time, glucose, dplyr::everything())



  #############################
  # Glucose Data
  #############################
  gluc = dplyr::select(res,
                       GlucoseInternalTime, GlucoseDisplayTime, GlucoseValue)
  colnames(gluc) = c("internal_time", "time", "glucose")
  if (make_numeric) {
    gluc$glucose = ensure_numeric(gluc$glucose)
  }
  gluc = dplyr::filter(gluc,
                        !is.na(internal_time) |
                          !is.na(time) |
                          !is.na(glucose))
  gluc = select(gluc, time, glucose, dplyr::everything())

  gluc = sheet_check(gluc, complete = complete, ...)
  L = list(full_data = full_data,
           glucose = gluc,
           calibration = meter,
           header = header)
  return(L)
}