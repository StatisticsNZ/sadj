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


#' Add Outliers to all series in a group
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
"addOutliers<-.X13SeriesGroup" <- function(x, arima_model,update_save, correct_spec, values){
  x %>% purrr::modify(function(x){
    addOutliers(x, arima_model,update_save, correct_spec) <- values
    x
    })
}

#' Remove outliers from all series in a group.
#'
#' ao, ls, so, tc
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
"removeOutliers<-.X13SeriesGroup" <- function(x, values){
  x %>% purrr::modify(function(x){
    removeOutliers(x) <- values
    x
  })

}

#' Add parameter values to all series in a group
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
"addParamVals<-.X13SeriesGroup" <- function(x, spec, parameter, values){
  x %>% purrr::modify(function(x){
    addParamVals(x,spec, parameter) <- values
    x
  })
}

#' Remove parameter values from all series in a group.
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
"removeParamVals<-.X13SeriesGroup" <- function(x, spec, parameter, values){
  x %>% purrr::modify(function(x){
    removeParamVals(x,spec, parameter) <- values
    x
  })

}


#' @keywords internal
specroot.X13SeriesGroup <- function(x, grp_num=1L){
  if (!inherits(x, "X13SeriesGroup"))
    stop(sprintf("%s must be of class 'X13SeriesGroup'.", deparse(substitute(x))))
  sprintf("%s/%s/%s", workdir(), grp_num, sname(x))
}

writeMTA.X13SeriesGroup <- function(x, grp_num=1L, ...){
  if (!inherits(x, "X13SeriesGroup"))
    stop(sprintf("%s must be of class 'X13SeriesGroup'.", deparse(substitute(x))))
  metafile <- sprintf("%s.mta", specroot.X13SeriesGroup(x, grp_num=grp_num))
  sink(metafile)
  for (series in x)
    cat(sprintf("%s %s/%s\n", specroot.X13Series(series, grp_num=grp_num)
                , outpdir(grp_num=grp_num), sname(series)))
  sink()
}

#' @export
adjust.X13SeriesGroup <- function(x, purge = TRUE, keep_fixed=FALSE, grp_num=1L, ...){

  if (purge){
    unlink(c(dir(sprintf("%s/%s",workdir(),grp_num)
                 , pattern = sprintf("^%s[.].+$", grp_num), full.names = TRUE),
             dir(sprintf("%s/%s",workdir(),grp_num)
                 , pattern = sprintf("^%s[_].+[.].+$", grp_num), full.names = TRUE)))
    x %>% walk(clean, grp_num=grp_num)
  }

   for (series in x){
     series <- correctSeriesSpec(series)
     writeDAT(series, grp_num=grp_num)
     writeSpecList(series,grp_num=grp_num)
   }

  writeMTA.X13SeriesGroup(x, grp_num=grp_num)
  binpath <- sprintf("%s/x13ashtml", paste0(x13binary::x13path()))
  # cmd <- sprintf("%s -m %s -s", binpath, specroot.X13SeriesGroup(x))
  cmd <- sprintf("cd %s && %s -m %s -s", workdir(), binpath, specroot.X13SeriesGroup(x, grp_num=grp_num))

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
    res_ <- importOutput(x[[i]], grp_num=grp_num)
    attr(res_, "input") <- x[[i]]
    res[[sname(x[[i]])]] <- res_
    # if (purge)
    #   clean(x[[i]])
  }
  if (purge){
    unlink(c(dir(sprintf("%s/%s",workdir(),grp_num)
                 , pattern = sprintf("^%s[.].+$", grp_num), full.names = TRUE),
             dir(sprintf("%s/%s",workdir(),grp_num)
                 , pattern = sprintf("^%s[_].+[.].+$", grp_num), full.names = TRUE)))
    x %>% walk(clean, grp_num=grp_num)
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
