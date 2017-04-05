#' @title Read Abbott Output Data
#' @description Reads an excel file with Abbot Raw data
#'
#' @param path Path of .xlsx file.  Must have a sheet named "AbbottRaw" in it
#'
#' @return If the sheet is not empty, it will return a \code{data.frame} of
#' values.  Otherwise, it will return \code{NULL}
#' @export
#' @importFrom readxl read_excel
read_abbott = function(path) {
  res = read_excel(path = path,
                   sheet = "AbbottRaw",
                   col_names = TRUE,
                   skip = 2)

  cnames = c("ID", "Time", "Record Type",
             "Historic Glucose (mg/dL)")
  if (all(is.na(res))) {
    return(NULL)
  }
  # id_row = as.data.frame(res)[1,1]
  # res = res[-c(1:2),]
  # colnames(res) = as.character(res[1,])
  # res = res[-1,]
  # attr(res, "id_value") = id_row
  # res = as.data.frame(res)
  return(res)
}