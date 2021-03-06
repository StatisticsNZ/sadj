% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spec.R
\name{removeSpec.X13SpecList}
\alias{removeSpec.X13SpecList}
\title{Remove a spec.}
\usage{
\method{removeSpec}{X13SpecList}(x, specname)
}
\arguments{
\item{x}{Object to modify.}

\item{name}{Parameter name.}
}
\value{
A copy of \code{x} with named spec removed.
}
\description{
Remove a spec for objects that support it--currently only for objects of
type \code{\link{X13SpecList}}.
}
\examples{
specl <- X13SpecList(
  series = list(
    start = "1949.1", period = "12", title = "AirPassengers",
    file = "AirPassengers.dat",  format = "datevalue", save = "(b1)"
  ),
  x11 = list(
    mode = "mult", sigmalim = "(1.8,2.8)", save = "(d8 d10 d11 d12 d13 c17)"
  ),
  outlier = NULL
)
specl
specl <- removeSpec(specl, "outlier")
specl
}
