#' Create an object to be adjusted by X13-ARIMA-SEATS.
#'
#' @param x A data frame containing a single time series--should have columns
#' \code{year}, \code{period}, \code{value}.  If \code{x} is of class
#' \code{\link{ts}}, then it will be converted to the required format.
#' @param sname A short name for the series.
#' @param lname A long name for the series.
#' @param type Either of \code{x11} or \code{seats}.  When \code{speclist} is
#' left missing, a default is provided; and this determines whether the
#' provided specification contains an \code{x11} or \code{seats} block.
#' @param speclist A spec list created with \code{\link{X13SpecList}}.
#' @param ... As an alternative to \code{SpecList}, a variable number of
#' name-value pairs.
#'
#' @export
X13SeriesGroup <- function(sname, ..., lname = sname){
   series <- list(...)
   if (length(series) == 0)
     stop("No input provided")
   if (!all(sapply(series, inherits, "X13Series")))
     stop("'...' must all be of class 'X13Series'.")
   structure(series, sname = sname, lname = lname, class = c("X13SeriesGroup", class(series)))
}

#' @export
sname.X13SeriesGroup <- function(x) attr(x, 'sname')

#' @export
lname.X13SeriesGroup <- function(x) attr(x, 'lname')

#' @export
is.X13SeriesGroup <- function(x) inherits(x, "X13SeriesGroup")

#' @keywords internal
specroot.X13SeriesGroup <- function(x){
  if (!inherits(x, "X13SeriesGroup"))
    stop(sprintf("%s must be of class 'X13SeriesGroup'.", deparse(substitute(x))))
  sprintf("%s/%s", workdir(), sname(x))
}

writeMTA.X13SeriesGroup <- function(x, ...){
  if (!inherits(x, "X13SeriesGroup"))
    stop(sprintf("%s must be of class 'X13SeriesGroup'.", deparse(substitute(x))))
  metafile <- sprintf("%s.mta", specroot.X13SeriesGroup(x))
  sink(metafile)
  for (series in x)
    cat(sprintf("%s %s/%s\n", specroot.X13Series(series), outpdir(), sname(series)))
  sink()
}

#' @export
adjust.X13SeriesGroup <- function(x, purge = TRUE, ...){
   for (series in x){
     if (purge)
       clean(series)
     writeSpecList(series)
   }
  if (purge){
    unlink(dir(workdir(), pattern = sprintf("^%s[_].+[.].+$", sname(x)), full.names = TRUE))
    unlink(sprintf("%s.mta", specroot.X13SeriesGroup(x)))
  }
  writeMTA.X13SeriesGroup(x)
  binpath <- sprintf("%s/x13ashtml", paste0(x13binary::x13path()))
  cmd <- sprintf("%s -m %s -s", binpath, specroot.X13SeriesGroup(x))
  system(cmd, intern=TRUE)
  res <- list()
  for(i in 1:length(x)){
    res[[sname(x[[i]])]] <- importOutput(x[[i]])
    if (purge)
      clean(x[[i]])
  }
  if (purge){
    unlink(dir(workdir(), pattern = sprintf("^%s[_].+[.].+$", sname(x)), full.names = TRUE))
    unlink(sprintf("%s.mta", specroot.X13SeriesGroup(x)))
  }

  class(res) <- c("X13SeriesGroupResult")

  res
}

#' @export
print.X13SeriesGroup <- function(x, ...){
  s <- sprintf("X13SeriesGroup: %s (%s)", lname(x), sname(x))
  cat(s)
  cat("\n")
  for(i in 1:nchar(s)) cat("-")
  cat("\n")
  for (series in x){
    cat(sprintf("\n  %s (%s)", lname(series), sname(series)))
  }
}
