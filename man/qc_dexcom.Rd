% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_dexcom.R
\name{qc_dexcom}
\alias{qc_dexcom}
\title{Quality Control Measures for Dexcom CGMS}
\usage{
qc_dexcom(glucose)
}
\arguments{
\item{glucose}{\code{data.frame} of glucose data with columns of}
}
\value{
A \code{data.frame} of values with a qc column
}
\description{
Flags points of questionable quality for a Dexcom
}
\note{
This is a wrapper for \code{\link{first_24}},...
}
