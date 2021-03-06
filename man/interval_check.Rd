% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval_check.R
\name{interval_check}
\alias{interval_check}
\title{Check the interval timing for glucose monitoring}
\usage{
interval_check(times)
}
\arguments{
\item{times}{A Date vector of day/times for records}
}
\value{
A list of a logical if all differences are multiples of the minimum
and the minimum interval time and the interval unit
}
\description{
Takes in a vector fo times, finds the gaps in the intervals and
determines if all gaps are multiples of the smallest gap.
}
