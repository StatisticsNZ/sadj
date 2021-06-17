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
                      facfile=NULL,
                      spec_fname=NULL,
                      clean_spec=TRUE,
                      ...){
  args <- list(...)

  if (length(args) > 0) {

    if(!missing(spec_fname))
      warning("Specs provided as separate arguments.  `spec_fname` Will be ignored.")
    if (!missing(speclist))
      warning(sprintf("Specs provided as separate arguments.  Ignoring %s.",
                      deparse(substitute(speclist))))
    speclist <- do.call('X13SpecList', list(...))
  } else if(!missing(speclist)) {
    if(!missing(spec_fname))
          warning("Both `speclist` and `spec_fname` provided.  `spec_fname` Will be ignored.")
  } else if(!is.null(spec_fname) && !rlang::is_empty(spec_fname)) {
    if(file.exists(spec_fname)) {
      speclist <- readSPC(spec_fname)
    } else stop(sprintf("Spec file does not exist:\n%s",spec_fname))

  } else {
    if (tolower(type) == "x11") speclist <- .x11
    else if (tolower(type) == "seats") speclist <- .seats
  }

  # we don't want to error if specdir is null or not supplied
  if(!is.null(spec_dir <- path(speclist))) {
    if(dir.exists(spec_dir)) {
      userwd <- getwd()
      setwd(dirname(spec_dir))
      on.exit(setwd(userwd), add = TRUE)

    } else {
      warning(sprintf("Spec directory does not exist:\n%s.  Current working directory will be used to find files.",spec_dir))
    }

  }




  comp_ser <- specType(speclist)
  # if("series" %in% names(speclist)) comp_ser <- "series" else
  #   if("composite" %in% names(speclist)) comp_ser <- "composite" else
  #     stop("The speclist must contain a `series` spec or a `composite` spec.")

  dat_path <- getSpecParameter(speclist,comp_ser,"file")

  # handle missing sname
  if(missing(sname)){
    if(!is.null(sname(speclist)) && !rlang::is_empty(sname(speclist)))
      sname <- sname(speclist) else
        if(!is.null(dat_path) && !rlang::is_empty(dat_path))
          sname <- stringr::str_remove(basename(dat_path),"[.]dat$") else {
            ser_name <- tolower(getSpecParameter(speclist,comp_ser, "name"))
            if(!is.null(ser_name) && !rlang::is_empty(ser_name)) sname <- ser_name
          }
  }

  if(missing(lname)) lname <- sname

  if(missing(x)) {
    if(!is.null(dat_path)) {

      if (file.exists(dat_path)) {
        x <- readDAT(dat_path)
      } else {
        warning(sprintf("couldn't find DAT file in:\n%s.
                        Searching paths relative to:\n%s.", dat_path,getwd()))
        path_split <- stringr::str_split(dat_path, "/")[[1]] %>% rev()
        for(i in seq_along(path_split)) {
          try_path <- paste(path_split[i:1],collapse = "/")
          if(file.exists(try_path)) {
            x <- readDAT(try_path)

            break
          }
        }
      }
      if ((is.null(x) || rlang::is_empty(x)))
        stop("Unable to find DAT file.")
    } else stop("No time series supplied and none referenced in the spec file.")
  } else if(!is.null(dat_path) ) {
    warning(sprintf("Time series provided, but the `file` argument of the `series` spec is not empty.
            Spec argument will be ignored:\n",dat_path))
  }

  if(!is.null(getSpec(speclist,"series")) && clean_spec) {
    setSpecParameter(speclist, "series","file") <- NULL
    setSpecParameter(speclist, "series","format") <- NULL
  }

  # Fac file ---------------------------------------------
  transformpath <- getSpecParameter(speclist, "transform", "file")

  if (missing(facfile)) {
    if(!is.null(transformpath)) {
      warning("No facfile supplied.  Path to FAC in the `file` argument of the `transform` spec will be used.")
      # try transform path first
      if (file.exists(transformpath)) {
        facfile <- readFAC(transformpath)
      } else {
        warning(sprintf("couldn't find FAC file in:\n%s.
                        Searching paths relative to:\n%s.", transformpath, getwd()))
        path_split <- stringr::str_split(transformpath, "/")[[1]] %>% rev()
        for(i in seq_along(path_split)) {
          try_path <- paste(path_split[i:1],collapse = "/")
          if(file.exists(try_path)) {
            facfile <- readFAC(try_path)
            break
          }

        }
      }
      if ((is.null(facfile) || rlang::is_empty(facfile)))
        warning("Unable to find factor file.")
    }
  } else if(!is.null(transformpath) && !rlang::is_empty(transformpath)) {
    warning(sprintf("FAC file provided, but the `file` argument in the`transform` spec is not empty.
            Spec argument will be ignored: %s",transformpath))

  }

  if(!is.null(getSpecParameter(speclist,"transform","file")) && clean_spec) {
    setSpecParameter(speclist, "transform","file") <- NULL
    setSpecParameter(speclist, "transform","format") <- NULL
  }


  if (inherits(x, "ts")) res <- tsdf(x)
  else res <- data.frame(x)[, c("year", "period", value)]
  res <- cbind(date=date(res),res)
  attr(res, "value") <- value
  attr(res, "SpecList") <- speclist

  # Try to handle snames derived from list element
  # if(missing(sname) && grepl("\\[\\[\"\\w+\"\\]\\]",sname)) sname <- stringr::str_extract_all(sname, "\\w+")[[1]][[2]]

  attr(res, "sname") <- tolower(sname)
  attr(res, "lname") <- lname


  if(!missing(facfile)){
    if(!is.data.frame(facfile))
      stop("facfile is not a dataframe")
    attr(res, "FacFile") <- facfile
  }

  class(res) <- c("X13Series", class(res))
  res
}

#' @export
sname.X13Series <- function(x) attr(x, 'sname')

#' @export
sname.X13SeriesResult <- function(x) sname(attr(x, "input"))

#' @export
lname.X13Series <- function(x) attr(x, 'lname')

#' @export
lname.X13SeriesResult <- function(x) lname(attr(x, "input"))

#' @export
is.X13Series <- function(x) inherits(x, "X13Series")

#' @export
specType.X13Series <- function(x) getSpecList(x) %>% specType()



#' Get a factor file.
#'
#' @param x Input object.
#'
#' @export
getFacFile.X13Series <- function(x){
  attr(x, "FacFile")
}

#' Set a factor file.
#'
#' @param x Input object.
#'
#' @export
"setFacFile<-.X13Series" <- function(x, value){
  attr(x, "FacFile") <- value
  x
}


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
"setSpecList<-.X13Series" <- function(x, value){
  if (inherits(value, "X13SpecList"))
    attr(x, "SpecList") <- value
  else
    stop(sprintf("%s is not of class: X13SpecList.", deparse(substitute(value))))
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
             dir(datdir(), pattern = sprintf("^%s[.].+$", sname(x)), full.names = TRUE),
             dir(datdir(), pattern = sprintf("^%s[_].+[.].+$", sname(x)), full.names = TRUE),
             dir(outpdir(), pattern = sprintf("^%s[.].+$", sname(x)), full.names = TRUE),
             dir(outpdir(), pattern = sprintf("^%s[_].+[.].+$", sname(x)), full.names = TRUE)))
  }
}

#' @keywords internal
writeSpecList <- function(x, ...){
  if (!inherits(x, "X13Series"))
    stop(sprintf("%s must be of class 'X13Series'.", deparse(substitute(x))))

  spec <- getSpecList(x)
  datname <- sprintf("%s/%s.dat",datdir(),sname(x))


  if(specType(spec)=="series"){
    if (!is.null(getSpecParameter(spec, "series", "file")))
      warning(sprintf("The `file` argument of the `series` spec is not empty:\n%s
    But will be overwritten with supplied X13Series."
                      ,getSpecParameter(spec, "series", "file")))
    setSpecParameter(spec, "series", "file") <- datname
    setSpecParameter(spec, "series", "format") <- "datevalue"
    if (!is.null(getSpecParameter(spec, "series", "data"))){
      warning("In the spec 'series', the 'data' parameter is not empty but will be ignored")
      setSpecParameter(spec, "series", "data") <- NULL
    }
    if (is.null(getSpecParameter(spec, "series", "period")))
      setSpecParameter(spec, "series", "period") <-
      length(unique(x$period))

  }

  # Fac file
  facfile <- getFacFile(x)
  transformpath <- getSpecParameter(spec, "transform", "file")
  facpath <- sprintf("%s/%s.fac",facdir(),sname(x))


  if ((is.null(facfile) || rlang::is_empty(facfile))) {
    if(!is.null(transformpath)) {
      # try transform path first
      if (file.exists(transformpath)) {
        warning("No facfile attached to X13Series.  Path to fac in the `file` argument of the 'transform' spec will be used.")
        facfile <- readFAC(transformpath)
        setFacFile(x) <- facfile
      } else {
        warning(sprintf("couldn't find FAC file in:\n%s.
                        Searching paths relative to:\n%s.", transformpath, getwd()))
        path_split <- stringr::str_split(transformpath, "/")[[1]] %>% rev()
        for(i in seq_along(path_split)) {
          try_path <- paste(path_split[i:1],collapse = "/")
          if(file.exists(try_path)) {
            facfile <- readFAC(try_path)
            setFacFile(x) <- facfile
            break
          }

        }
        # stop("factor file path is not valid")
      }
      if ((is.null(facfile) || rlang::is_empty(facfile)))
        warning("Unable to find factor file.")
    }
  }


  if(!is.null(facfile) && !rlang::is_empty(facfile)) {
    if(!is.null(transformpath))
      warning(sprintf("FAC file provided, but the `file` argument in the`transform` spec is not empty.
                      Spec argument will be ignored: %s",transformpath))
    writeFAC(facfile, facpath)
    setSpecParameter(spec, "transform", "file") <- facpath
    setSpecParameter(spec, "transform", "format") <- "datevalue"
  }



  # value <- x[, attr(x, "value")]
  #
  # if (is.null(getSpecParameter(spec, "series", "file")) &
  #     is.null(getSpecParameter(spec, "series", "data"))) {
  #   f <- length(unique(x$period))
  #   d <- "("
  #   for (i in 1:length(value)){
  #     if (i%%4 == 1) d <- paste0(d, "\n  ")
  #     d <- sprintf("%s  %s", d, value[i])
  #   }
  #   d <- sprintf("%s)", d)
  #   setSpecParameter(spec, "series", "data") <- d
  # }

  # if (is.null(getSpecParameter(spec, "series", "start")))
  #   setSpecParameter(spec, "series", "start") <-
  #   sprintf("%04d.%02d", x$year[1], x$period[1])



  specroot <- sprintf("%s/%s", workdir(), sname(x))
  specfile <- sprintf("%s.spc", specroot)

  sink(specfile)
  cat(toString(spec))
  sink()
}

#' @keywords internal
writeSpecList1 <- function(x, ...){
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
  #attr(ap,"sname")
  if(sname(x)=="" || rlang::is_empty(sname(x)))
    stop(sprintf("sname is not valid"))
  if (purge)
    clean(x)
  writeDAT(x)
  writeSpecList(x)
  writeMTA.X13Series(x)
  binpath <- sprintf("%s/x13ashtml", paste0(x13binary::x13path()))
  # cmd <- sprintf("%s -m %s -s", binpath, specroot.X13Series(x))
  cmd <- sprintf("cd %s && %s -m %s -s", workdir(), binpath, specroot.X13Series(x))

  x13_messages <- system(cmd, intern=TRUE)

  # Currently, dumping entirety of x13 messages on error
  saved_option_warn_len <- getOption("warning.length")
  options(warning.length = 8000L)
  on.exit(options(warning.length = saved_option_warn_len), add = TRUE)

  if(x13ErrorDetected(x13_messages))
    stop(paste(x13_messages, collapse="\n"))

  res <- importOutput(x)
  attr(res, "input") <- x
  attr(res,"x13_messages") <- x13_messages

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
summary.X13SeriesResult <- function(x, html = FALSE, stringsAsFactors = FALSE, ...){
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
    d <- as.data.frame(attr(x, "udg"),stringsAsFactors=stringsAsFactors)
  else if (!is.null(attr(x, "xdg")))
    d <- as.data.frame(attr(x, "xdg"),stringsAsFactors=stringsAsFactors)

  return(d)
}


X13Messages.X13SeriesResult <- function(x) cat(paste(attr(x,"x13_messages"), collapse="\n"))
