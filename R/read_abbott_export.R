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
#' @importFrom dplyr select filter
#' @importFrom lubridate ymd_hm
read_abbott_export = function(path,
                              complete = TRUE,
                              make_numeric = TRUE, ...) {

  ##############################
  # Note Workaround
  # GlucoseInternalTime = GlucoseDisplayTime = GlucoseValue = NULL
  # rm(list = c("GlucoseDisplayTime", "GlucoseValue"))
  # PatientInfoField = PatientInfoValue = NULL
  # rm(list = c("PatientInfoField", "PatientInfoValue"))
  # MeterInternalTime = MeterDisplayTime = MeterValue = NULL
  # rm(list = c("MeterInternalTime", "MeterDisplayTime", "MeterValue"))
  time = glucose = internal_time = NULL
  rm(list = c("time", "glucose", "internal_time"))
  ##############################

  ##########################
  # getting rid of the header crap
  ##########################
  res = readLines(path, n = 100)
  start = grep("^ID", trimws(res))[1]
  if (length(start) == 0) {
    stop(paste0("This Abbott format is different!  ",
                "Please report this bug with a file sample"))
  }
  skip = start - 1
  header = res[seq(skip)]

  res = readr::read_tsv(file = path,
                        col_names = TRUE,
                        skip = skip)
  if (all(is.na(res))) {
    return(NULL)
  }
  full_data = res

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

  gluc = select(res, time, glucose, dplyr::everything())
  gluc$time = lubridate::ymd_hm(res$time)

  gluc = sheet_check(gluc, complete = complete, ...)
  L = list(full_data = full_data,
           glucose = gluc,
           header = header)
  return(L)
}