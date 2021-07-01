#' Create an object to be adjusted by X13-ARIMA-SEATS.
#'
#' @param sname A short name for the series.
#' @param ... a list of \code{X13Series} objects
#' @param lname A long name for the series.
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
     writeDAT(series)
     writeSpecList(series)
   }
  if (purge){
    unlink(dir(workdir(), pattern = sprintf("^%s[_].+[.].+$", sname(x)), full.names = TRUE))
    unlink(sprintf("%s.mta", specroot.X13SeriesGroup(x)))
  }
  writeMTA.X13SeriesGroup(x)
  binpath <- sprintf("%s/x13ashtml", paste0(x13binary::x13path()))
  # cmd <- sprintf("%s -m %s -s", binpath, specroot.X13SeriesGroup(x))
  cmd <- sprintf("cd %s && %s -m %s -s", workdir(), binpath, specroot.X13SeriesGroup(x))

  x13_messages <- system(cmd, intern=TRUE)

  # Currently, dumping entirety of x13 messages on error
  saved_option_warn_len <- getOption("warning.length")
  options(warning.length = 8000L)
  on.exit(options(warning.length = saved_option_warn_len), add = TRUE)

  if(x13ErrorDetected(x13_messages))
    stop(paste(x13_messages, collapse="\n"))


  # tryCatch(
  #   error = function(cnd){
  #     message(cnd)
  #     message(cat(paste(x13_messages, collapse="\n")))
  #
  #   },
  #   x13_messages <- system(cmd, intern=TRUE)
  #
  # )

  res <- list()
  for(i in 1:length(x)){
    res_ <- importOutput(x[[i]])
    attr(res_, "input") <- x[[i]]
    res[[sname(x[[i]])]] <- res_
    # if (purge)
    #   clean(x[[i]])
  }
  if (purge){
    for(i in 1:length(x)) clean(x[[i]])
    unlink(dir(workdir(), pattern = sprintf("^%s[_].+[.].+$", sname(x)), full.names = TRUE))
    unlink(sprintf("%s.mta", specroot.X13SeriesGroup(x)))
  }

  attr(res,"x13_messages") <- x13_messages
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

X13Messages.X13SeriesGroupResult <- function(x) cat(paste(attr(x,"x13_messages"), collapse="\n"))
