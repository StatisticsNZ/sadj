% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spec.R
\name{X13SpecList}
\alias{X13SpecList}
\title{X13-ARIMA SEATS specification.}
\usage{
X13SpecList(..., sname, fac_name)
}
\arguments{
\item{...}{Variable number of name-value pairs.  The name is the name of the
spec, and the value is a list containing additional name-value pairs where
names are spec parameter names and values are the spec parameter values.}

\item{fac_name}{The name of a factor files as a string.}
}
\value{
A list of class \code{\link{X13SpecList}}, where each element in the
list is of class \code{\link{X13Spec}}.
}
\description{
A list containing a set of specifications to apply when seasonally adjusting
a time series.
}
\examples{
specl <- X13SpecList(
  series = list(
    start = "1949.1", period = "12", title = "AirPassengers",
    file = "AirPassengers.dat", format = "datevalue", save = "(b1)"
  ),
  x11 = list(
    mode = "mult", sigmalim = "(1.8,2.8)",
    save = "(d8 d10 d11 d12 d13 c17)"
  )
)
specl
}
