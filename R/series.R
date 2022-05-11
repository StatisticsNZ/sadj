# #' @export
# `[.X13Series` <- function(x, ..., drop=TRUE) {
#   structure(NextMethod()
#             , class = class(x)
#             , sname = sname(x)
#             , lname = lname(x)
#             , SpecList = getSpecList(x)
#             , FacFile = getFacFile(x)
#             , regfile = getRegFile(x)
#             )
# }


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
X13Series <- function(x=NULL,
                      value = "value",
                      sname = deparse(substitute(x)),
                      lname = sname,
                      type = "x11",
                      speclist,
                      facfile=NULL,
                      regfile=NULL,
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
      setwd(spec_dir)
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

  if(missing(lname))
    lname <- {if(!rlang::is_empty(lname(speclist))) lname(speclist)
                 else sname
    }

  if(missing(x) && comp_ser=="series") {
    if(!is.null(dat_path)) {

      dat_path <- findX13File(dat_path,path(speclist))
      if (file.exists(dat_path)) {
        x <- readDAT(dat_path)
      }
      # else {
      #   warning(sprintf("couldn't find DAT file in:\n%s.
      #                   Searching paths relative to:\n%s.", dat_path,getwd()))
      #   path_split <- stringr::str_split(dat_path, "/")[[1]] %>% rev()
      #   for(i in seq_along(path_split)) {
      #     try_path <- paste(path_split[i:1],collapse = "/")
      #     if(file.exists(try_path)) {
      #       x <- readDAT(try_path)
      #
      #       break
      #     }
      #   }
      # }
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
      warning(sprintf("%s: No facfile supplied.  Path to FAC in the `file` argument of the `transform` spec will be used."
                      , sname))
      # try transform path first
      transformpath <- findX13File(transformpath,path(speclist))
      if (file.exists(transformpath)) {
        facfile <- readFAC(transformpath)
      }
      # else {
      #   warning(sprintf("couldn't find FAC file in:\n%s.
      #                   Searching paths relative to:\n%s.", transformpath, getwd()))
      #   path_split <- stringr::str_split(transformpath, "/")[[1]] %>% rev()
      #   for(i in seq_along(path_split)) {
      #     try_path <- paste(path_split[i:1],collapse = "/")
      #     if(file.exists(try_path)) {
      #       facfile <- readFAC(try_path)
      #       break
      #     }
      #
      #   }
      # }
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


  # Reg file ---------------------------------------------
  regpath <- getSpecParameter(speclist, "regression", "file")

  if (missing(regfile)) {
    if(!is.null(regpath)) {
      warning(sprintf("%s: No regfile supplied.  Path to REG in the `file` argument of the `regression` spec will be used."
                      , sname))
      regpath <- findX13File(regpath,path(speclist))
      if (file.exists(regpath)) {
        regfile <- readREG(regpath)
      }
      if ((is.null(regfile) || rlang::is_empty(regfile)))
        warning("Unable to find regression file.")
    }
  } else if(!is.null(regpath) && !rlang::is_empty(regpath)) {
    warning(sprintf("REG file provided, but the `file` argument in the`regression` spec is not empty.
            Spec argument will be ignored: %s",regpath))

  }

  if(!is.null(getSpecParameter(speclist,"regression","file")) && clean_spec) {
    setSpecParameter(speclist, "regression","file") <- NULL
    setSpecParameter(speclist, "regression","format") <- NULL
  }


  #---------------------------------------------------------------------------


  if(is.null(x)) {
    if(comp_ser!="composite") stop(sprintf("%s: No data supplied and series is not composite", sname))
    res <- data.frame(date=double(),year=integer(),period=integer(),value=double())
  } else {
    if (inherits(x, "ts")) res <- tsdf(x)
    else res <- data.frame(x)[, c("year", "period", value)]
    res <- cbind(date=date(res),res)
  }

  attr(res, "value") <- value
  attr(res, "SpecList") <- speclist

  # Try to handle snames derived from list element
  # if(missing(sname) && grepl("\\[\\[\"\\w+\"\\]\\]",sname)) sname <- stringr::str_extract_all(sname, "\\w+")[[1]][[2]]

  attr(res, "sname") <- tolower(sname)
  attr(res, "lname") <- lname


  # if(!missing(facfile)){
  #   if(!is.data.frame(facfile) & !is.null(facfile))
  #     stop("facfile is not a dataframe")
  #   attr(res, "FacFile") <- facfile
  # }


  if(!is.null(facfile))
    if(is.data.frame(facfile))
      attr(res, "FacFile") <- facfile else
        stop("facfile is not a dataframe")

  if(!is.null(regfile))
    if(is.data.frame(regfile))
      attr(res, "regfile") <- regfile else
        stop("regfile is not a dataframe")


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
hasFac <- function(x) !rlang::is_empty(attr(x, "FacFile"))

#' @export
hasReg <- function(x) !rlang::is_empty(attr(x, "regfile"))

#' @export
is.X13Series <- function(x) inherits(x, "X13Series")

#' @export
specType.X13Series <- function(x) getSpecList(x) %>% specType()

#' Add Outliers
#'
#' @param x
#' @param arima_model if missing and spec parameter is empty, (0 1 1)(0 1 1) will be used.
#' @param update_save Update the regression-save parameter?
#' @param correct_spec Automatically fix a spec to run with outlier regression?
#' @param values
#'
#' @return
#' @export
#'
#' @examples
"addOutliers<-.X13Series" <- function(x, arima_model, update_save = TRUE, correct_spec = TRUE, values){

  # check dates here for new ao's - values
  values_mod <- removeRegAfterEnd(reg_vars = values, s_period = getPeriod(x), s_end_date = tail(x$date,1))
  if(!rlang::is_empty(values_removed <- setdiff(values,values_mod)))
    warning(sprintf("Some regression variables not added b/c their periods occur after the end of the series:
                    %s", paste(values_removed, collapse = ", ")))

  if(rlang::is_empty(values_mod)) return(x)

  s <- getSpecList(x)
  if(missing(arima_model))
    addOutliers(s, update_save = update_save, correct_spec = correct_spec) <- values_mod
  else
    addOutliers(s, arima_model = arima_model
                , update_save = update_save, correct_spec = correct_spec) <- values_mod

  attr(x, "SpecList") <- s
  x

}

#' Add Parameter Values to a SpecList parameter
#'
#' @param x
#' @param spec
#' @param parameter
#' @param values
#'
#' @return
#' @export
#'
#' @examples
"addParamVals<-.X13Series" <- function(x, spec, parameter, values){

  s <- getSpecList(x)
  addParamVals(s, spec, parameter) <- values
  attr(x, "SpecList") <- s
  x
}

#' @export
correctARIMA.X13Series <- function(x, force_transform = FALSE) {
  s <- getSpecList(x)
  attr(x, "SpecList") <- correctARIMA(s, force_transform = force_transform)
  x
}


#' Validates/corrects a series Spec file
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
correctSeriesSpec.X13Series <- function(x) {
  spec_type <- specType(x)
  # correct period. applies to series only
  # 12 is default
  pd <- unique(x[, "period"]) %>% max() %>% as.numeric()
  if(spec_type=="series" && rlang::is_empty(getSpecParameter(x,"series","period")) &&
     pd != 12)
    setSpecParameter(x,"series","period") <- as.character(pd)


  # correct span
  if(!rlang::is_empty(span <- getSpecParameter(x,"series","span"))){
    if(length(span_vals <- getParamVals(x,"series","span"))!=2)
      stop(sprintf("The series span parameter is of length: %s.  It should have 2 and only 2 arguments."),
           length(span_vals))
    # convert span vals to dates
    # span_dates <- list()
    if(span_vals[[1]] !="") {
      span_date <- span_vals[[1]] %>% strsplit("[.]") %>% unlist() %>% as.numeric() %>% c(.,pd)  %>% as.list() %>%
        do.call(ypdToDate,.)
      if(span_date < head(x$date,1)) span_vals[[1]] <- ""

    }

    if(span_vals[[2]] !="") {
      span_date <- span_vals[[2]] %>% strsplit("[.]") %>% unlist() %>% as.numeric() %>% c(.,pd)  %>% as.list() %>%
        do.call(ypdToDate,.)
      if(span_date > tail(x$date,1)) span_vals[[2]] <- ""

    }

    # Check outlier stuff here

    # span_dates <- span_vals %>% strsplit("[.]") %>% map(function(x)as.list(as.numeric(c(x,4)))) %>%
    #   map(~do.call(ypdToDate,.x))
    # # compare span dates to series start and end dates
    # if(span_dates[[1]] < head(x$date,1)) span_vals[[1]] <- ""
    # if(span_dates[[2]] > tail(x$date,1)) span_vals[[2]] <- ""
    if(identical(span_vals, c("",""))) setSpecParameter(x,"series","span") <- NULL
    else setParamVals(x,"series","span") <- span_vals
  }
  x
}

#' Get a factor file.
#'
#' @param x Input object.
#'
#' @export
getFacFile.X13Series <- function(x){
  attr(x, "FacFile")
}

#' Get Parameter Values
#'
#' Returns parameter values as a character vector
#'
#' @param x Object.
#' @param spec The spec name.
#' @param parameter The parameter name.
#'
#' @export
#'
getParamVals.X13Series <- function(x, spec, parameter){
  s <- getSpecList(x)
  getParamVals(s, spec, parameter)
}

#' Get a regression file.
#'
#' @param x Input object.
#'
#' @export
getRegFile.X13Series <- function(x){
  attr(x, "regfile")
}

#' Return Period
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
getPeriod.X13Series <- function(x) {
  unique(x[, "period"]) %>% max() %>% as.numeric()
}

#' Return Period
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
getPeriod.X13SeriesResult <- function(x) {
  unique(x[, "period"]) %>% max() %>% as.numeric()
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

#' Get a specification list.
#'
#' @param x Input object.
#'
#' @export
getSpecList.X13Series <- function(x){
  attr(x, "SpecList")
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

#' Remove a factor file.
#'
#' @param x Input object.
#'
#' @export
removeFacFile.X13Series <- function(x){
  attr(x, "FacFile") <-NULL
  s <- getSpecList(x)
  setSpecParameter(s,"transform","type") <- NULL
  setSpecParameter(s,"transform","mode") <- NULL
  if(rlang::is_empty(getSpec(s,"transform")))
    s <- removeSpec(s,"transform")
  attr(x, "SpecList") <- s

  x
}

#' Remove outlier variables: ao, ls, so, tc
#'
#' @param x
#' @param values
#'
#' @return
#' @export
#'
#' @examples
"removeOutliers<-.X13Series" <- function(x,values){
  s <- getSpecList(x)
  removeOutliers(s) <- values
  attr(x, "SpecList") <- s
  x
}

#' Remove parameter values
#'
#' @param x
#' @param values
#'
#' @return
#' @export
#'
#' @examples
"removeParamVals<-.X13Series" <- function(x, spec, parameter, values){
  s <- getSpecList(x)
  removeParamVals(s, spec, parameter) <- values
  attr(x, "SpecList") <- s
  x
}

#' @export
removeSpec.X13Series <- function(x, specname){
  s <- getSpecList(x)
  s <- removeSpec(s, specname)
  attr(x, "SpecList") <- s
  x
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

#' Set Parameter Values.
#'
#' Allows the user to set multiple parameter values with a character vector.
#'
#' @param x Object to modify.
#' @param spec The spec name.
#' @param parameter The parameter name.
#' @param values A character vector of Regression variables.
#'
#' @export
#'
"setParamVals<-.X13Series" <- function(x, spec, parameter, values) {
  s <- getSpecList(x)
  setParamVals(s, spec, parameter) <- values
  attr(x, "SpecList") <- s
  x
}

#' Set a regression file.
#'
#' @param x Input object.
#'
#' @export
"setRegFile<-.X13Series" <- function(x, value){
  attr(x, "regfile") <- value
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
"setSpecParameter<-.X13Series" <- function(x, spec, name, value){
  s <- getSpecList(x)
  setSpecParameter(s, spec, name) <- value
  attr(x, "SpecList") <- s
  x
}

#' @keywords internal
clean <- function(x, grp_num = 1L){
  if (inherits(x, "X13Series")){

    list(sprintf("%s/%s",workdir(),grp_num), datdir(grp_num=grp_num), facdir(grp_num=grp_num)
         , regdir(grp_num=grp_num), outpdir(grp_num=grp_num)) %>% purrr::walk(function(y){
      unlink(c(dir(y, pattern = sprintf("^%s[.].+$", sname(x)), full.names = TRUE),
               dir(y, pattern = sprintf("^%s[_].+[.].+$", sname(x)), full.names = TRUE)))
    })

    # unlink(c(dir(workdir(), pattern = sprintf("^%s[.].+$", sname(x)), full.names = TRUE),
    #          dir(workdir(), pattern = sprintf("^%s[_].+[.].+$", sname(x)), full.names = TRUE),
    #          dir(datdir(), pattern = sprintf("^%s[.].+$", sname(x)), full.names = TRUE),
    #          dir(datdir(), pattern = sprintf("^%s[_].+[.].+$", sname(x)), full.names = TRUE),
    #          dir(facdir(), pattern = sprintf("^%s[.].+$", sname(x)), full.names = TRUE),
    #          dir(facdir(), pattern = sprintf("^%s[_].+[.].+$", sname(x)), full.names = TRUE),
    #          dir(regdir(), pattern = sprintf("^%s[.].+$", sname(x)), full.names = TRUE),
    #          dir(regdir(), pattern = sprintf("^%s[_].+[.].+$", sname(x)), full.names = TRUE),
    #          dir(outpdir(), pattern = sprintf("^%s[.].+$", sname(x)), full.names = TRUE),
    #          dir(outpdir(), pattern = sprintf("^%s[_].+[.].+$", sname(x)), full.names = TRUE)))
  }
}

#' @keywords internal
writeSpecList <- function(x, grp_num=1L, ...){
  if (!inherits(x, "X13Series"))
    stop(sprintf("%s must be of class 'X13Series'.", deparse(substitute(x))))

  spec <- getSpecList(x)
  datname <- sprintf("%s/%s.dat",datdir(grp_num=grp_num),sname(x))


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


  if(!is.null(facfile <- getFacFile(x)) && !rlang::is_empty(facfile)) {
    facpath <- sprintf("%s/%s.fac",facdir(grp_num=grp_num),sname(x))
    writeFAC(facfile, facpath)
    setSpecParameter(spec, "transform", "file") <- facpath
    setSpecParameter(spec, "transform", "format") <- "datevalue"
  }

  if(!is.null(regfile <- getRegFile(x)) && !rlang::is_empty(regfile)) {
    regpath <- sprintf("%s/%s.dat",regdir(grp_num=grp_num),sname(x))
    writeREG(regfile, regpath)
    setSpecParameter(spec, "regression", "file") <- regpath
    setSpecParameter(spec, "regression", "format") <- "datevalue"
  }

  # specroot <- sprintf("%s/%s/%s", workdir(), grp_num, sname(x))
  specroot <- specroot.X13Series(x, grp_num=grp_num)
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
specroot.X13Series <- function(x, grp_num=1L){
  if (!inherits(x, "X13Series"))
    stop(sprintf("%s must be of class 'X13Series'.", deparse(substitute(x))))
  sprintf("%s/%s/%s", workdir(), grp_num, sname(x))
}

#' @keywords internal
writeMTA.X13Series <- function(x, grp_num=1L, ...){
  if (!inherits(x, "X13Series"))
    stop(sprintf("%s must be of class 'X13Series'.", deparse(substitute(x))))
  metafile <- sprintf("%s.mta", specroot.X13Series(x))
  sink(metafile)
  cat(sprintf("%s %s/%s", specroot.X13Series(x), outpdir(grp_num=grp_num), sname(x)))
  sink()
}

#' @keywords internal
importOutput <- function(x, grp_num=1L, ...){
  if (!inherits(x, "X13Series"))
    stop(sprintf("%s must be of class 'X13Series'.", deparse(substitute(x))))

  df <- list()
  nondf <- list()
  outf <- dir(outpdir(grp_num=grp_num), pattern = sprintf("^%s\\.{1}\\w?", attr(x, "sname")))
  for (f in outf){
    ext <- tail(strsplit(f, '.', fixed = TRUE)[[1]], 1)
    if (!ext %in% c("html", "xdg", "udg"))
      df[[ext]] <- readOutput(x, outpdir(grp_num=grp_num), ext)
    else if (ext %in% c("xdg", "udg")){
      nondf[[ext]] <- readUDG(x, outpdir(grp_num=grp_num), ext)
    }
    else
      nondf[[ext]] <- readLines(sprintf("%s/%s", outpdir(grp_num=grp_num), f))
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
adjust.X13Series <- function(x, purge = TRUE, keep_fixed=FALSE, grp_num=1L, ...){
  #attr(ap,"sname")
  if(sname(x)=="" || rlang::is_empty(sname(x)))
    stop(sprintf("sname is not valid"))
  if (purge)
    clean(x,grp_num=grp_num)

  writeDAT(x, grp_num=grp_num)
  x %>% correctSeriesSpec() %>% writeSpecList(grp_num=grp_num)
  writeMTA.X13Series(x,grp_num=grp_num)
  binpath <- sprintf("%s/x13ashtml", paste0(x13binary::x13path()))
  # cmd <- sprintf("%s -m %s -s", binpath, specroot.X13Series(x))
  cmd <- sprintf("cd %s && %s -m %s -s", workdir(), binpath, specroot.X13Series(x,grp_num=grp_num))

  x13_messages <- system(cmd, intern=TRUE)

  # Currently, dumping entirety of x13 messages on error
  saved_option_warn_len <- getOption("warning.length")
  options(warning.length = 8000L)
  on.exit(options(warning.length = saved_option_warn_len), add = TRUE)

  if(x13ErrorDetected(x13_messages))
    stop(paste(x13_messages, collapse="\n"))

  res <- importOutput(x, grp_num=grp_num)
  attr(res, "input") <- x
  attr(res,"x13_messages") <- x13_messages

  if (purge)
    clean(x,grp_num=grp_num)
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

#' Get the start date of the span parameter
#'
#' @export
spanStartDate.X13Series <- function(x, spec = "series") {
  spanDate.X13Series(x = x, value = c("start"), spec = spec)
  # spec <- tolower(spec)
  # the_span <- getParamVals(x, spec, "span")
  # if(rlang::is_empty(the_span))
  #   return(NA_real_)
  # else {
  #   span_start <- the_span[[1]]
  #   if(rlang::is_empty(span_start)) return(NA_real_)
  #   start_span_split <- span_start %>% strsplit("[.]") %>% .[[1]]
  #   the_year <- start_span_split[[1]] %>% as.numeric()
  #   the_pd <- start_span_split[[2]] %>% as.numeric()
  #   return(ypdToDate(y = the_year, p = the_pd, pd= getPeriod(x)))
  # }
}

#' Get the start or end date of the span parameter
#'
#' @param  value Can take "start" or "end"
#'
#' @export
spanDate.X13Series <- function(x, value = c("start"), spec = "series") {
  spec <- tolower(spec)
  if(specType(x) == "composite" && spec =="series")
    return(NA_real_)
  the_span <- getParamVals(x, spec, "span")
  if(rlang::is_empty(the_span))
    return(NA_real_)
  else {
    res <- the_span %>% map(function(span_val){
      if(rlang::is_empty(span_val) || span_val == "") return(NA_real_)
      start_span_split <- span_val %>% strsplit("[.]") %>% .[[1]]
      the_year <- start_span_split[[1]] %>% as.numeric()
      the_pd <- start_span_split[[2]] %>% as.numeric()
      return(ypdToDate(y = the_year, p = the_pd, pd= getPeriod(x)))

    })
    if(value == "start")
      return(res[[1]])
    else if(value == "end")
      return(res[[2]])
    else
      stop(sprintf("%s is not a valid value. 'start' or 'end' expected.", value))

  }
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

#' Is the x-11 method additive or multiplicative?
#'
#' @param x
#'
#' @return "add", "mult", or NA
#' @export
#'
#' @examples
X11AddMult.X13Series <- function(x) {
  if(!rlang::is_empty(getSpec(x,"x11"))) {
    if(rlang::is_empty(mode <- getSpecParameter(x,"x11","mode")) || mode=="mult")
      "mult"
    else "add"
  } else NA_character_


}

#' Extract t-vals from regression variables
#'
#' @param x
#' @param variables
#'
#' @return
#' @export
#'
#' @examples
tvals.X13SeriesResult <- function(x, otl_types = c("ao", "tc", "ls", "rp"), include_auto =TRUE) {
  udg <- x %>% attr("udg")

  otl_types %<>% toupper()
  otl_rex <- ifelse(include_auto, "^(Auto)?Outlier\\$", "^Outlier\\$")

  variables_full <- x %>% attr("udg") %>% names() %>% grep(otl_rex,., value=TRUE)
  variables_short <- variables_full %>% stringr::str_remove(otl_rex)

  # fix case
  variables_full %>% purrr::set_names(tolower(variables_short)) %>% purrr::map_dbl(function(var_full){
    otl <- udg %>% .[[var_full]]
    if(!rlang::is_empty(otl))
      otl %>% strsplit(split=" ") %>% unlist() %>% .[[3]] %>% as.numeric()
    else
      stop(sprint("%s: %s not found in udg", sname(x), var_full))
  })
}

#' Get the UDG (summary diagnostics)
#'
#' @param x
#'
#' @return A list representing the values in the UDG file
#' @export
#'
udg.X13SeriesResult <- function(x) {
  return(attr(x, "udg"))
}

X13Messages.X13SeriesResult <- function(x) cat(paste(attr(x,"x13_messages"), collapse="\n"))
