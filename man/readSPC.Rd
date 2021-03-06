% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{readSPC}
\alias{readSPC}
\title{Read SPC file.}
\usage{
readSPC(
  fname,
  to_lower = TRUE,
  sname = stringr::str_remove(basename(fname), "[.]spc$")
)
}
\arguments{
\item{fname}{file name}
}
\description{
Read SPC file.
}
