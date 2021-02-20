# How to create composites...
# should comp = TRUE be an arg to simplify this?

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
#' @param facfile A factor file created with \code{\link{new_facFile}}.
#' @param ... As an alternative to \code{SpecList}, a variable number of
#' name-value pairs.
#'
#' @export
X13Series <- function(x,
                      value = "value",
                      sname = deparse(substitute(x)),
                      lname = sname,
                      type = "x11",
                      speclist,
                      facfile,
                      ...){
  if (inherits(x, "ts")) res <- tsdf(x)
  else res <- data.frame(x)[, c("year", "period", value)]
  res <- cbind(date=date(res),res)
  attr(res, "value") <- value

  # Try to handle snames derived from list element
  if(missing(sname) && grepl("\\[\\[\"\\w+\"\\]\\]",sname)) sname <- str_extract_all(sname, "\\w+")[[1]][[2]]

  attr(res, "sname") <- tolower(sname)
  attr(res, "lname") <- lname
  args <- list(...)
  if (length(args) > 0) {
    attr(res, "SpecList") <- do.call('X13SpecList', list(...))
    if (!missing(speclist))
      warn(sprintf("Specs provided as separate arguments.  Ignoring %s.",
                   deparse(substitute(speclist))))
  }
  else {
    if (missing(speclist) & tolower(type) == "x11") speclist <- .x11
    else if (missing(speclist) & tolower(type) == "seats") speclist <- .seats
    attr(res, "SpecList") <- speclist
  }

  if(!missing(facfile)) attr(res, "SpecList") <- facfile

  class(res) <- c("X13Series", class(res))
  res
}

#' @export
sname.X13Series <- function(x) attr(x, 'sname')

#' @export
lname.X13Series <- function(x) attr(x, 'lname')

#' @export
is.X13Series <- function(x) inherits(x, "X13Series")

#' Get a specification list.
#'
#' @param x Input object.
#'
#' @export
getSpecList.X13Series <- function(x){
  attr(x, "SpecList")
}

#' Get a spec.
#'
#' @param x Input object.
#' @param name Parameter name.
#' @param value Parameter value.
#'
#' @export
getSpec.X13Series <- function(x, spec){
  attr(x, "SpecList")[[spec]]
}

#' Get a spec parameter.
#'
#' Get the value of a spec parameter.
#'
#' @param x Input object.
#' @param name Parameter name.
#' @param value Parameter value.
#'
#' @export
getSpecParameter.X13Series <- function(x, spec, parameter){
  attr(x, "SpecList")[[spec]][[parameter]]
}

#' @export
"setSpecParameter<-.X13Series" <- function(x, spec, name, value){
  s <- getSpecList(x)
  setSpecParameter(s, spec, name) <- value
  attr(x, "SpecList") <- s
  x
}

#' @export
"setSpec<-.X13Series" <- function(x, specname, value){
  s <- getSpecList(x)
  setSpec(s, specname) <- value
  attr(x, "SpecList") <- s
  x
}

#' @export
removeSpec.X13Series <- function(x, specname){
  s <- getSpecList(x)
  s <- removeSpec(s)
  attr(x, "SpecList") <- s
  x
}

#' @keywords internal
clean <- function(x){
  if (inherits(x, "X13Series")){
    unlink(c(dir(workdir(), pattern = sprintf("^%s[.].+$", sname(x)), full.names = TRUE),
             dir(workdir(), pattern = sprintf("^%s[_].+[.].+$", sname(x)), full.names = TRUE),
             dir(outpdir(), pattern = sprintf("^%s[.].+$", sname(x)), full.names = TRUE),
             dir(outpdir(), pattern = sprintf("^%s[_].+[.].+$", sname(x)), full.names = TRUE)))
  }
}

#' @keywords internal
writeSpecList <- function(x, ...){
  if (!inherits(x, "X13Series"))
    stop(sprintf("%s must be of class 'X13Series'.", deparse(substitute(x))))

  spec <- getSpecList(x)
  value <- x[, attr(x, "value")]

  if (is.null(getSpecParameter(spec, "series", "file")) &
      is.null(getSpecParameter(spec, "series", "data"))) {
    f <- length(unique(x$period))
    d <- "("
    for (i in 1:length(value)){
      if (i%%4 == 1) d <- paste0(d, "\n  ")
      d <- sprintf("%s  %s", d, value[i])
    }
    d <- sprintf("%s)", d)
    setSpecParameter(spec, "series", "data") <- d
  }

  if (is.null(getSpecParameter(spec, "series", "start")))
    setSpecParameter(spec, "series", "start") <-
      sprintf("%04d.%02d", x$year[1], x$period[1])

  if (is.null(getSpecParameter(spec, "series", "period")))
    setSpecParameter(spec, "series", "period") <-
    length(unique(x$period))

  specroot <- sprintf("%s/%s", workdir(), sname(x))
  specfile <- sprintf("%s.spc", specroot)

  sink(specfile)
  cat(toString(spec))
  sink()
}

#' @keywords internal
specroot.X13Series <- function(x){
  if (!inherits(x, "X13Series"))
    stop(sprintf("%s must be of class 'X13Series'.", deparse(substitute(x))))
  sprintf("%s/%s", workdir(), sname(x))
}

#' @keywords internal
writeMTA.X13Series <- function(x, ...){
  if (!inherits(x, "X13Series"))
    stop(sprintf("%s must be of class 'X13Series'.", deparse(substitute(x))))
  metafile <- sprintf("%s.mta", specroot.X13Series(x))
  sink(metafile)
  cat(sprintf("%s %s/%s", specroot.X13Series(x), outpdir(), sname(x)))
  sink()
}

#' @keywords internal
importOutput <- function(x, ...){
  if (!inherits(x, "X13Series"))
    stop(sprintf("%s must be of class 'X13Series'.", deparse(substitute(x))))

  df <- list()
  nondf <- list()
  outf <- dir(outpdir(), pattern = sprintf("^%s\\.{1}\\w?", attr(x, "sname")))
  for (f in outf){
    ext <- tail(strsplit(f, '.', fixed = TRUE)[[1]], 1)
    if (!ext %in% c("html", "xdg", "udg"))
      df[[ext]] <- readOutput(x, outpdir(), ext)
    else if (ext %in% c("xdg", "udg")){
      nondf[[ext]] <- readUDG(x, outpdir(), ext)
    }
    else
      nondf[[ext]] <- readLines(sprintf("%s/%s", outpdir(), f))
  }

  res <- mergeList(df, by = c("year", "period"))
  res <- cbind(date=date(res), res)
  res <- merge(x, res, by = c("date","year", "period"), sort = FALSE)
  class(res) <- c("X13SeriesResult", class(res))
  attr(res, "value") <- attr(x, "value")
  for (a in names(nondf))
    attr(res, a) <- nondf[[a]]

  res
}

#' Perform seasonal adjustment.
#'
#' Perform seasonal adjustment with \code{X13-ARIMA-SEATS}.
#'
#' @param x Object on which to perform adjustment.
#'
#' @export
adjust.X13Series <- function(x, purge = TRUE, ...){
  if (purge)
    clean(x)
  writeSpecList(x)
  writeMTA.X13Series(x)
  binpath <- sprintf("%s/x13ashtml", paste0(x13binary::x13path()))
  cmd <- sprintf("cd %s && %s -m %s -s", workdir(), binpath, specroot.X13Series(x))
  system(cmd, intern=TRUE)
  res <- importOutput(x)
  attr(res, "input") <- x
  if (purge)
    clean(x)
  res
}

#' @keywords internal
getCol <- function(x, col, as.vector = FALSE){
  if (!inherits(x, "X13SeriesResult"))
    stop("x must be of class 'X13SeriesResult'.")
  if (as.vector)
    x[, col]
  else
    data.frame(x[, c("year", "period", col)])
}

#' Get seasonally adjusted data from \code{\link{X13SeriesResult}}.
#'
#' @export
sa <- function(x, as.vector = FALSE){
  if (!inherits(x, "X13SeriesResult"))
    stop("x must be of class 'X13SeriesResult'.")
  if ("d11" %in% colnames(x))
    structure(getCol(x, "d11", as.vector), names = c("year", "period", "sa"))
  else if ("s12" %in% colnames(x))
    structure(getCol(x, "s11", as.vector), names = c("year", "period", "sa"))
  else
    NULL
}

#' Get trend data from \code{\link{X13SeriesResult}}.
#'
#' @export
trend <- function(x, as.vector = FALSE){
  if (!inherits(x, "X13SeriesResult"))
    stop("x must be of class 'X13SeriesResult'.")
  if ("d12" %in% colnames(x))
    structure(getCol(x, "d12", as.vector), names = c("year", "period", "trend"))
  else if ("s12" %in% colnames(x))
    structure(getCol(x, "s12", as.vector), names = c("year", "period", "trend"))
  else
    NULL
}

#' Get irregular data from \code{\link{X13SeriesResult}}.
#'
#' @export
irregular <- function(x, as.vector = FALSE){
  if (!inherits(x, "X13SeriesResult"))
    stop("x must be of class 'X13SeriesResult'.")
  if ("d13" %in% colnames(x))
    structure(getCol(x, "d13", as.vector), names = c("year", "period", "irregular"))
  else if ("s13" %in% colnames(x))
    structure(getCol(x, "s13", as.vector), names = c("year", "period", "irregular"))
  else
    NULL
}

#' Get unadjusted data from \code{\link{X13SeriesResult}}.
#'
#' @export
actual <- function(x, as.vector = FALSE){
  if (!inherits(x, "X13SeriesResult"))
    stop("x must be of class 'X13SeriesResult'.")
  if(as.vector)
    x[, attr(x, "value")]
  else
    structure(getCol(x, attr(x, "value"), as.vector),
              names = c("year", "period", "actual"))
}

#'
#' @export
print.X13SeriesResult <- function(x, ...){
  NextMethod(x)
}

#'
#' @export
summary.X13SeriesResult <- function(x, html = FALSE, ...){
  if (html & !is.null(attr(x, "html"))){
    v <- getOption("viewer")
    tf <- tempfile(fileext = ".html")
    writeLines(attr(x, "html"), tf)
    v(tf)
    return(invisible())
  }

  if (!any(names(attributes(x)) %in% c("xdg", "udg")))
    stop("No diagnostics present.")

  if (!is.null(attr(x, "udg")))
    d <- as.data.frame(attr(x, "udg"))
  else if (!is.null(attr(x, "xdg")))
    d <- as.data.frame(attr(x, "xdg"))

  return(d)
}
