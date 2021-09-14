#' @keywords internal
.spec <- list(
  arima = c(
    "model", "title", "ar", "ma"
  ),
  automdl = c(
    "maxorder", "maxdiff", "acceptdefault", "checkmu", "diff", "ljungboxlimit",
    "mixed", "print", "savelog", "arimalimit", "balanced", "exactdiff",
    "fcstlim", "hrinitial", "reducecv", "refectfcst", "urfinal"
  ),
  check = c(
    "maxlag", "print", "qtype", "save", "savelog"
  ),
  composite = c(
    "title", "name", "decimals", "modelspan", "appendfcst", "appendbcst",
    "type", "print", "save", "savelog", "indoutlier"
  ),
  estimate = c(
    "tol", "maxiter", "exact", "outofsample", "print", "save", "savelog",
    "file", "fix"
  ),
  force = c(
    "lambda", "mode", "rho", "round", "start", "target", "type", "usefcst",
    "print", "save", "indforce"
  ),
  forecast = c(
    "maxlead", "maxback", "probability", "exclude", "lognormal", "print", "save"
  ),
  history = c(
    "estimates", "sadjlags", "trendlags", "target", "start", "fstep", "fixmdl",
    "fixreg", "endtable", "print", "save", "savelog", "fixx11reg", "outlier",
    "outlierwin", "refresh", "transformfcst", "x11outlier"
  ),
  metadata = c(
    "keys", "values"
  ),
  identify = c(
    "diff", "sdiff", "maxlag", "print"
  ),
  outlier = c(
    "types", "critical", "method", "span", "lsrun", "print", "save", "savelog",
    "almost", "tcrate"
  ),
  pickmdl = c(
    "mode", "method", "file", "fcstlim", "bcstlim", "qlim", "overdiff",
    "identify", "outofsample", "print", "savelog"
  ),
  regression = c(
    "variables", "print", "save", "savelog", "user", "usertype", "start",
    "data", "aictest", "aicdiff", "tlimit", "chi2test", "chi2testcv",
    "pvaictest", "b", "centeruser", "eastermeans", "noapply", "tcrate", "file", "format"
  ),
  seats = c(
    "appendfcst", "finite", "hpcycle", "noadmiss", "qmax", "rmod", "statseas",
    "out", "print", "printphtrf", "save", "savelog", "tabtables", "bias",
    "epsiv", "epsphi", "hplan", "imean", "maxit", "rmod", "xl"
  ),
  series = c(
    "title", "start", "period", "file", "format", "span", "modelspan", "name",
    "data", "decimals", "precision", "comptype", "compwt", "print", "save",
    "appendfcst", "appendbcst", "type", "divpower", "missingcode", "missingval",
    "saveprecision", "trimzero"
  ),
  slidingspans = c(
    "start", "length", "numspans", "cutchng", "cutseas", "cuttd", "outlier",
    "fixmdl", "fixreg", "print", "save", "savelog", "additivesa", "fixx11reg",
    "x11outlier"
  ),
  spectrum = c(
    "logqs", "print", "save", "savelog", "start", "tukey120", "axis", "decibel",
    "difference", "maxar", "peakwidth", "series", "siglevel", "type"
  ),
  transform = c(
    "function", "adjust", "title", "start", "data", "name", "aicdiff", "mode",
    "type", "print", "save", "savelog", "file", "format", "power", "constant",
    "trimzero"
  ),
  x11 = c(
    "mode", "seasonalma", "trendma", "sigmalim", "title", "appendfcst",
    "appendbcst", "final", "print", "save", "savelog", "type", "calendarsigma",
    "centerseasonal", "keepholiday", "print1stpass", "sfshort", "sigmavec",
    "trendic", "true7term"
  ),
  x11regression = c(
    "variables", "user", "start", "data", "file", "format", "tdprior",
    "aictest", "aicdiff", "span", "sigma", "critical", "outliermethod",
    "outlierspan", "usertype", "prior", "print", "save", "savelog", "almost",
    "centeruser", "eastermeans", "forcecal", "noapply", "reweight", "umdata",
    "umfile", "umformat", "umname", "umprecision", "umstart", "umtrimzero"
  )
)

#' @keywords internal
.param_comma_delim <- c("span")

#' @keywords internal
.x11 <- structure(list(
  series=structure(list(save = "(b1)"),class = "X13Spec", name = "series"),
  x11 = structure(list(mode = "mult"
                       ,sigmalim = "(1.8,2.8)"
                       ,save = "(d8 d10 d11 d12 d13 c17)")
                  ,class = "X13Spec", name="x11"))
  , class = "X13SpecList")


#' @keywords internal
.seats <- structure(list(
  series = structure(list(save = "(b1)"), class = "X13Spec", name="series")
  , transform = structure(list('function' = 'auto'), class = "X13Spec", name="transform")
  , automdl = structure(list(), class = "X13Spec", name ="automdl")
  , seats = structure(list(save = "(s10 s11 s12 s13 s14 s16 s18 cyc ltt)")
                      , class = "X13Spec", name = "seats"))
  , class = "X13SpecList"
)

#' X13-ARIMA-SEATS specification.
#'
#' List containing a single specification group, e.g. \code{series},
#' \code{x11}, \code{outlier}, etc.
#'
#' @param name of specification, e.g. \code{series}, \code{x11}, \code{outlier},
#' \code{composite}, etc.
#' @param ... Variable number of name-value pairs, e.g. \code{mode = "mult"},
#' \code{mode = "sigmalim"}, etc.
#'
#' @export
#'
#' @return A list of name-value pairs that is of class \code{\link{X13Spec}}.  It has a
#' \code{name} attribute which is the name of the specification.
#'
#' @examples
#' spec <- X13Spec(
#'   specname = "x11", mode = "mult", sigmalim = "(1.8,2.8)",
#'   save = "(d8 d10 d11 d12 d13 c17)"
#' )
#'
#' spec

X13Spec <- function(specname = "series", ...){
  specname = tolower(specname)
  if (!specname %in% names(.spec))
    stop(sprintf("Unknown spec: '%s'.", specname))
  args <- list(...)
  if (identical(args, list(NULL)) | length(args) == 0){
    o <- list()
    attr(o,"name") <- specname
    class(o) <- "X13Spec"
    return(o)
  }
  if (is.null(names(args)))
    stop("All spec parameters must be named (1).")
  if (any(names(args)==""))
    stop("All spec parameters must be named (2).")
  names(args) <- tolower(names(args))
  for (param in names(args))
    if (!param %in% .spec[[specname]])
      stop(sprintf("Unknown parameter for spec '%s': %s", specname, param))
  o <- args
  attr(o,"name") <- specname
  class(o) <- "X13Spec"
  return(o)
}

#' X13-ARIMA SEATS specification.
#'
#' A list containing a set of specifications to apply when seasonally adjusting
#' a time series.
#'
#' @param ... Variable number of name-value pairs.  The name is the name of the
#' spec, and the value is a list containing additional name-value pairs where
#' names are spec parameter names and values are the spec parameter values.
#' @param fac_name The name of a factor files as a string.
#'
#' @export
#'
#' @return A list of class \code{\link{X13SpecList}}, where each element in the
#' list is of class \code{\link{X13Spec}}.
#'
#' @examples
#' specl <- X13SpecList(
#'   series = list(
#'     start = "1949.1", period = "12", title = "AirPassengers",
#'     file = "AirPassengers.dat", format = "datevalue", save = "(b1)"
#'   ),
#'   x11 = list(
#'     mode = "mult", sigmalim = "(1.8,2.8)",
#'     save = "(d8 d10 d11 d12 d13 c17)"
#'   )
#' )
#' specl
X13SpecList <- function(..., sname,fac_name){
  args <- list(...)
  if (length(args)==0)
    stop("No specs provided.")
  if (is.null(names(args)))
    stop("All parameters must be named.")
  if (any(names(args)==""))
    stop("All parameters must be named.")
  names(args) <- tolower(names(args))
  for (specname in names(args))
    if (!specname %in% names(.spec))
      stop(sprintf("Unknown spec: '%s'.", specname))
  o <- list()
  for (i in 1:length(args)){
    spec <- c(list(specname = names(args)[i]), args[[i]])
    spec <- do.call("X13Spec", spec)
    o[[i]] <- spec
  }
  if(!missing(fac_name))
    attr(o, "fac_name") <- fac_name #else attr(o, "fac_name") <- character()
  if(!missing(sname))
    attr(o, "sname") <- sname

  # if("series" %in% names(o)) spec_type <- "series" else
  #   if("composite" %in% names(o)) spec_type <- "composite" else
  #     stop("The speclist must contain a `series` spec or a `composite` spec.")
  # attr(o, "spec_type") <- spec_type

  class(o) <- c("X13SpecList", "list")
  names(o) <- sapply(o, function(x) attr(x,"name"))
  o
}

#' @keywords internal
X13SpecComments <- function(comments, ...){
  args <- list(...)
  # o <- list(comments=comments)
  o <- comments
  class(o) <- "X13SpecComments"
  return(o)
}

#' @export
is.X13Spec <- function(x) inherits(x, "X13Spec")

#' @export
is.X13SpecList <- function(x) inherits(x, "X13SpecList")

#' @export
is.X13SpecComments <- function(x) inherits(x, "X13SpecComments")

#' @export
sname.X13SpecList <- function(x) attr(x, 'sname')

#' @export
lname.X13SpecList <- function(x) attr(x, 'lname')

#' Specification type
#'
#' Is the specification a "series" or "composite"
#'
#'
#' @export
specType.X13SpecList <- function(x) {
  spec_names <- names(x)
  if("composite" %in% spec_names && "series" %in% spec_names){
    spec_type <- "both"
    warning("The speclist contains both a `series` and a `composite`.  X13 requires one and only one of these.")
  } else if("series" %in% spec_names) spec_type <- "series" else
    if("composite" %in% spec_names) spec_type <- "composite" else {
      spec_type <- "neither"
      warning("The speclist must contain a `series` spec or a `composite` spec to run X13.")
    }
    spec_type
}

#' Add Outliers: ao, ls, tc, so, rp, qi, qd.
#'
#' This function needs to be generalised to addParameterVals
#'
#' @param x Object.
#' @param value a character vector of valid ao parameters
#'
#' @export
#'
"addOutliers<-.X13SpecList" <- function(x, arima_model="(0 1 1)(0 1 1)",update_save=TRUE, correct_spec=TRUE, values){

  if(!missing(arima_model) || rlang::is_empty(getSpecParameter(x,"arima","model")))
    setSpecParameter(x, "arima","model") <- arima_model

  values %<>% tolower()

  addParamVals(x, "regression", "variables") <- values


  if(update_save) {

    save_vals <- getParamVals(x, "regression", "variables") %>% purrr::map(function(x){
      c("ao","ls", "tc","so") %>% purrr::keep(function(y){
        stringr::str_detect(x,y)
      })
    }) %>% purrr::flatten_chr() %>% unique()

    addParamVals(x, "regression","save") <- save_vals

  }

  if(correct_spec) x <- correctRegression(x)

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
"addParamVals<-.X13SpecList" <- function(x, spec, parameter, values){
  values %<>% tolower()
  param_vals <- getParamVals(x, spec, parameter)
  param_vals <- c(param_vals, values) %>% unique()
  setParamVals(x, spec, parameter) <- param_vals
  x

}

#' @export
"appendSpecComments<-.X13SpecList" <- function(x, value){
  res <- getSpecComments(x)
  res <- X13SpecComments(c(res,value))
  attr(x,"comments") <- res
  # res <- do.call('X13SpecComments', c(comments = comments))
  x

}

#' Correct a SpecList.
#'
#' For example, if the are regression variables in a multiplicative model,
#'  then add to transform funciton=log
#'
#' @param x The Object to correct.
#'
#' @return
#' @export
#'
#' @examples
correctRegression.X13SpecList <- function(x) {
  if(!rlang::is_empty(getSpecParameter(x,"regression","variables"))){
    if(rlang::is_empty(getSpecParameter(x,"transform","function")) &&
      (rlang::is_empty(mode <- getSpecParameter(x,"x11","mode")) || mode=="mult"))
      setSpecParameter(x,"transform","function") <- "log"

    if(rlang::is_empty(getSpecParameter(x,"arima","model")))
      setSpecParameter(x, "arima","model") <- "(0 1 1)(0 1 1)"

    # Add a bit that checks series dates against var dates
  }
  x
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
getParamVals.X13SpecList <- function(x, spec, parameter){
  delim <- ifelse(parameter %in% .param_comma_delim,","," ")
  res <- getSpecParameter(x,spec, parameter)
  if(spec=="arima") return(res)
  if(rlang::is_empty(res)) return(res)
  expr_paren <- "\\(([^)]+)\\)"

  res %<>% stringr::str_trim()
  res %<>% {
    if (stringr::str_detect(res, expr_paren))
      stringr::str_match(res, expr_paren) %>% .[1, 2]
    else
      res
  }

  res %>% stringr::str_trim() %>% stringr::str_split(pattern=delim, simplify = FALSE) %>%
    unlist() %>% stringr::str_trim()
}

#' Get a spec.
#'
#' @param x Input object.
#' @param spec Parameter name.
#'
#' @export
getSpec.X13SpecList <- function(x, spec){
  x[[spec]]
}

#' @export
getSpecComments.X13SpecList <- function(x){
  attr(x,"comments")
}

#' Get a spec parameter.
#'
#' Get the value of a spec parameter.
#'
#' @param x Input object.
#' @param parameter Parameter name.
#'
#' @export
getSpecParameter.X13Spec <- function(x, parameter){
  x[[parameter]]
}

#' Get a spec parameter.
#'
#' Get the value of a spec parameter.
#'
#' @param x Input object.
#' @param spec Parameter name.
#' @param parameter Parameter value.
#'
#' @export
getSpecParameter.X13SpecList <- function(x, spec, parameter){
  x[[spec]][[parameter]]
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
"removeOutliers<-.X13SpecList" <- function(x,values){
  removeParamVals(x, "regression","variables") <- values
  x
}

#' Remove Parameter Values
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
"removeParamVals<-.X13SpecList" <- function(x, spec, parameter, values){
  setParamVals(x, spec, parameter) <- setdiff(getParamVals(x, spec, parameter),values)
  x
}

#' Remove a spec.
#'
#' Remove a spec for objects that support it--currently only for objects of
#' type \code{\link{X13SpecList}}.
#'
#' @param x Object to modify.
#' @param name Parameter name.
#'
#' @export
#'
#' @return A copy of \code{x} with named spec removed.
#'
#' @examples
#' specl <- X13SpecList(
#'   series = list(
#'     start = "1949.1", period = "12", title = "AirPassengers",
#'     file = "AirPassengers.dat",  format = "datevalue", save = "(b1)"
#'   ),
#'   x11 = list(
#'     mode = "mult", sigmalim = "(1.8,2.8)", save = "(d8 d10 d11 d12 d13 c17)"
#'   ),
#'   outlier = NULL
#' )
#' specl
#' specl <- removeSpec(specl, "outlier")
#' specl
removeSpec.X13SpecList <- function(x, specname){
  if (!specname %in% names(x))
    stop(sprintf("Spec %s not found in %s.", specname, deparse(substitute(x))))
  x[[specname]] <- NULL
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
"setParamVals<-.X13SpecList" <- function(x, spec, parameter, values) {
  delim <- ifelse(parameter %in% .param_comma_delim,", "," ")
  values %<>% tolower()
  setSpecParameter(x, spec, parameter) <-{
    if(length(values)==1) paste(values, collapse = delim)
    else sprintf("( %s )",paste(values, collapse = delim))
  }
  x
}

#' Set a spec.
#'
#' @param x Object to modify.
#' @param spec Spec name.
#' @param value Parameter value.
#'
#' @export
#'
#' @examples
#' specl <- X13SpecList(
#'   series = list(
#'     start = "1949.1", period = "12", title = "AirPassengers",
#'     file = "AirPassengers.dat", format = "datevalue", save = "(b1)"
#'   )
#' )
#'
#' setSpec(specl, "x11") <- list(
#'   mode = "mult", sigmalim = "(1.8,2.8)", save = "(d8 d10 d11 d12 d13 c17)"
#' )
#'
#' setSpec(specl) <- X13Spec(specname = "transform", `function`="log")
#'
#' specl
"setSpec<-.X13SpecList" <- function(x, spec, value){
  if (missing(spec) & inherits(value, "X13Spec")){
    x[[attr(value,"name")]] <- value
    return(x)
  }
  spec <- tolower(spec)
  if (is.null(value)) value <- list(NULL)
  if (!spec %in% names(.spec))
    stop(sprintf("Unknown spec: %s.", spec))
  s <- do.call('X13Spec', c(specname = spec, value))
  x[[spec]] <- s
  x
}

#' @export
"setSpecComments<-.X13SpecList" <- function(x, value){
  res <- X13SpecComments(value)
  attr(x,"comments") <- res
  # res <- do.call('X13SpecComments', c(comments = comments))
  x

}

#' Set a spec parameter.
#'
#' @param x Object to modify.
#' @param parameter Parameter name.
#' @param value Parameter value.
#'
#' @export
#'
#' @return An updated version of input \code{x}.
#'
#' @examples
#' spec <- X13Spec(
#'   specname = "x11", mode = "mult", sigmalim = "(1.8,2.8)",
#'   save = "(d8 d10 d11 d12 d13 c17)"
#' )
#'
#' spec
#' setSpecParameter(spec, "mode") <- "add"
#' spec
#' setSpecParameter(spec, "mode") <- NULL
#' spec
"setSpecParameter<-.X13Spec" <- function(x, parameter, value){
  parameter <- tolower(parameter)
  if (!parameter %in% .spec[[attr(x,"name")]])
    stop(sprintf("Unknown parameter for spec '%s': %s", attr(x,"name"), parameter))
  if (is.null(value))
    x[[parameter]] <- NULL
  else
    x[[parameter]] <- ifelse(parameter %in% c("file", "title", "name"),
                             # sprintf("'%s'", value),
                             value,
                             value)
  x
}

#' Set a spec parameter.
#'
#' @param x Object to modify.
#' @param name Spec name.
#' @param parameter Parameter name
#' @param value Parameter value.
#'
#' @export
#'
#' @return An updated version of input \code{x}.
"setSpecParameter<-.X13SpecList" <- function(x, spec, parameter, value){
  parameter <- tolower(parameter)
  if (!spec %in% names(.spec))
    stop(sprintf("Unkown spec: %s", spec))
  if (!parameter %in% .spec[[spec]])
    stop(sprintf("Unknown parameter for spec '%s': %s", spec, parameter))
  if (!spec %in% names(x))
    # x %+% do.call("X13Spec", structure(list(spec, value), names = c("specname", name)))
    setSpec(x,spec) <- structure(as.list(value), names=parameter)


  else{
    if (is.null(value))
      x[[spec]][[parameter]] <- NULL
    else
      x[[spec]][[parameter]] <- ifelse(parameter %in% c("file", "title", "name"),
                                       # sprintf("'%s'", value),
                                       value,
                                       value)
  }
  x
}

#' Write a spec to file
#'
#' @param x The spec to write out.
#' @param fname The full file name.
#'
#' @export
writeSpecToFile.X13SpecList <- function(x, fname, comments=FALSE) {
  # if(!inherits(x,"X13SpecList"))
    # stop(sprintf("Object is not of class X13SpecList."))
  sink(file = fname)
  print(x, comments=comments)
  sink()
}

#' @export
toString.X13Spec <- function(x, ...){
  if (length(x) == 0){
    res <- sprintf("%s {}\n", attr(x,"name"))
    return(res)
  }
  res <- paste0(attr(x,"name"), "{\n")
  for (i in 1:length(x)){
    val <- x[[i]]
    isarray <- substr(val, 1, 1) == "(" & substr(val, nchar(val), nchar(val)) == ")"
    if (names(x)[i]%in%c("file", "title", "name")) {
      res <-
        if (isarray)
          paste0(res, "  ", names(x)[i], "=", val, "\n")
        else paste0(res, "  ", names(x)[i], "=\'", val, "\'\n")
    }
    else {
      if (isarray) {
        if (nchar(val) > (132 - 4 - nchar(names(x)[i]))) {
          newval = gather(split(substr(val, 2, nchar(val) - 1)))
          res <- paste0(res, "  ", names(x)[i], "=(", newval, "\n  )\n")
        } else {
          res <- paste0(res, "  ", names(x)[i], "=", val, "\n")
        }
      }
      else res <- paste0(res, "  ", names(x)[i], "=", val, "\n")
    }
  }
  paste0(res, "}\n")
}

#' @export
print.X13Spec <- function(x, ...){
  cat(toString(x, ...))
}

#' @export
toString.X13SpecComments <- function(x, ...){
  # comments <- x$comments
  res <- ""
    if(!is_empty(x)) {
    for (i in 1:length(x)){
      res <- paste0(res, toString(x[[i]]))
      # if (i < length(x))
      res <- paste0(res, "\n", sep="")
    }
    # res <- paste0(res, "\n", sep="")
  }
  res
}

#' @export
print.X13SpecComments <- function(x, ...){
  cat(toString(x, ...))
}

#' @export
toString.X13SpecList <- function(x, ...){
  res <- ""
  for (i in 1:length(x)){
    res <- paste0(res, toString(x[[i]]))
    if (i < length(x))
      res <- paste0(res, "\n", sep="")
  }
  res
}

#' @export
print.X13SpecList <- function(x, comments=FALSE,...){
  if(comments){
    print(attr(x,"comments"))
    cat("\n")
  }
  cat(toString(x, ...))
}

#' @export
path.X13SpecList <- function(x) attr(x, 'path')

#' Combine \code{\link{X13Spec}} objects.
#'
#' @export
#'
#' @examples
#' s1 <- X13Spec(specname = "x11", mode = "mult")
#'
#' s2 <- X13Spec(
#'   specname = "x11", sigmalim = "(1.8,2.8)",
#'   save = "(d8 d10 d11 d12 d13 c17)"
#' )
#'
#' s1 %+% s2
`%+%.X13Spec` <- function(e1, e2){
  if (!inherits(e2, "X13Spec"))
    stop("X13Spec objects can only be combined with other X13Spec objects.")
  if (attr(e1,"name") != attr(e2,"name"))
    stop(sprintf("Non-matching spec names: %s, %s.", attr(e1,"name"), attr(e2,"name")))
  lhs <- names(e1)
  rhs <- names(e2)
  int <- intersect(lhs, rhs)
  lhs <- lhs[!lhs %in% rhs]
  if (length(lhs) == 0) return(rhs)
  e1 <- as.list(c(e1[lhs], e2))
  class(e1) <- "X13Spec"
  e1
}

#' Combine \code{\link{X13SpecList}} objects, or add an \code{\link{X13Spec}}
#' object to an existing X13SpecList object.
#'
#' @export
#'
#' @examples
#' spec1 <- X13SpecList(series = list(start = "1949.1",
#' period = "12",
#' title = "AirPassengers",
#' file = "AirPassengers.dat",
#' format = "datevalue",
#' save = "(b1)"))
#'
#' spec2 <- X13SpecList(
#'   x11 = list(
#'     mode = "mult", sigmalim = "(1.8,2.8)", save = "(d8 d10 d11 d12 d13 c17)"
#'   )
#' )
#'
#' spec1 %+% spec2
#' spec1 %+% spec2 %+% X13Spec(specname = "transform", `function`="log")
`%+%.X13SpecList` <- function(e1, e2){
  if (inherits(e2, "X13Spec")){
    if (attr(e2,"name") %in% names(e1))
      e2 <- getSpec(e1, attr(e2,"name")) %+% e2
    setSpec(e1) <- e2
    return(e1)
  }
  else if(inherits(e2, "X13SpecList")){
    lhs <- names(e1)
    rhs <- names(e2)
    int <- intersect(lhs, rhs)
    for (s in int)
      setSpec(e1) <- getSpec(e1, s) %+% getSpec(e2, s)
    for (s in rhs[!lhs %in% int])
      setSpec(e1) <- getSpec(e2, s)
    return(e1)
  }
  else
    stop(sprintf("Invalid operand type: %s.", class(e2)))
}
