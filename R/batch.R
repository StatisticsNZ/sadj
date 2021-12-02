#' @export
`[.X13Batch` <- function(x, ..., drop=TRUE) {
  structure(NextMethod(), class=class(x))
}

#' @export
`[.X13BatchResult` <- function(x, ..., drop=TRUE) {
  structure(NextMethod(), class=class(x))
}

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
"addOutliers<-.X13Batch" <- function(x, arima_model, update_save = TRUE, correct_spec = TRUE, values){
  missing_arima <- missing(arima_model)
  x %>% purrr::modify(function(x){
    if(missing_arima)
      addOutliers(x, update_save = update_save
                  , correct_spec = correct_spec) <- values
    else
      addOutliers(x, arima_model = arima_model, update_save = update_save
                  , correct_spec = correct_spec) <- values
    x
  })
}



#' Create a list of X13SeriesGroup objects from an mta file.
#'
#' @param mta Path to mta file.
#'
#' @export
X13BatchFromMTA <-function(mta_path, parallel=TRUE) {
  grps <- mtaGroups(mta_path)
  if(parallel)
    ser_lists <- grps %>% parallel::mclapply(mtaToX13Series,mc.cores = parallel::detectCores() / 2)
  else
    ser_lists <- grps %>% lapply(mtaToX13Series)

  res <- ser_lists %>% map2(names(ser_lists),~X13ListToGroup(ser_list = .x,sname = .y))

  class(res) <- c("X13Batch", class(res))
  res
}

#' Adjust an X13Batch.
#'
#' @export
adjust.X13Batch <- function(x, purge = TRUE, parallel=TRUE, ...) {

  if(parallel)
    res <- parallel::mcmapply(function(x,y) adjust(x, grp_num=y,purge=purge)
                              , x, as.integer(names(x))
                              , SIMPLIFY = FALSE
                              , mc.cores = parallel::detectCores() / 2)
  else
    res <- mapply(function(x,y) adjust(x, grp_num=y,purge=purge)
                  , x, as.integer(names(x))
                  , SIMPLIFY = FALSE)


  # get the res's that are not group res's
  is_groupres <- (map_chr(res, ~class(.x)[[1]])) == "X13SeriesGroupResult"
  # These get 2 list elements: message, snames. nmaes come from
  err_grps <- x[!is_groupres] %>% purrr::map2(res[!is_groupres], function(x, y){
    list(message = y, snames = names(x))
  })

  res <- res[is_groupres]
  if(!rlang::is_empty(err_grps))
    attr(res,"x13_group_result_errors") <- err_grps

  class(res) <- c("X13BatchResult", class(res))
  res
}

#' Get Regression Variables
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
getRegVars.X13Batch <- function(x) {
  x %>% selectSeries(simplify=FALSE) %>% map_chr(function(x){
    res <- getSpecParameter(x,"regression", "variables")
    if(is.null(res)) NA_character_ else res
  })
}

#' Summarise an X13 Batch object
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
summary.X13Batch <- function(x, ...) {
  groups <- x %>% purrr::map(names) %>% tibble::enframe() %>% tidyr::unnest(cols = c(value)) %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(groups=vctrs::new_vctr(list(name), class = character())) %>%
    dplyr::rename(sname=value)

  selectSeries(x, simplify=FALSE) %>% purrr::map_dfr(function(x){
    type_summ <- getSpecParameter(x, "x11", "type")
    type_summ <- ifelse(rlang::is_empty(type_summ),FALSE, ifelse(type_summ == "summary",TRUE,FALSE))
    list(sname=sname(x), last_period=tail(x$date,1), spec_type=specType(x),
         has_fac=hasFac(x), has_reg=hasReg(x),
         type_summary = type_summ
         )
  }) %>% dplyr::inner_join(groups,.,by="sname") %>% dplyr::arrange(sname)

}

#' Print X13Batch.
#'
#' @export
toString.X13Batch <- function(x, ...){
  purrr::map2_chr(x,names(x),function(y,z){
    names(y) %>% paste(collapse="|") %>% sprintf("%s: %s",z,.) %>%
      sprintf("%s (%s)", .,specType(y[[length(y)]]))
  }) %>% paste(collapse="\n")

}

#' Print X13BatchResult.
#'
#' @export
toString.X13BatchResult <- function(x, ...){
  purrr::map2_chr(x,names(x),function(y,z){
    names(y) %>% paste(collapse="|") %>% sprintf("%s: %s",z,.)
  }) %>% paste(collapse="\n")

}

#' Print X13Batch.
#'
#' @export
print.X13Batch <- function(x, ...){
  cat(toString(x, ...))
}

#' Print X13BatchResult.
#'
#' @export
print.X13BatchResult <- function(x, ...){
  cat(toString(x, ...))
}

#' Remove outliers from all groups in a batch
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
"removeOutliers<-.X13Batch" <- function(x, values){
  x %>% purrr::modify(function(x){
    removeOutliers(x) <- values
    x
  })

}

#' Select unique series
#'
#' @param x
#' @param snames
#'
#' @return
#' @export
#'
#' @examples
selectSeries.X13Batch <- function(x, snames, simplify=if_else(missing(snames), FALSE, TRUE)) {
  if(missing(snames))
    res <- purrr::flatten(x) %>% .[unique(names(.))]
  else
    res <- purrr::flatten(x) %>% .[unique(snames)]

  if(simplify && length(res) == 1)
    res[[1]]
  else
    res

}

#' Select unique series
#'
#' @param x
#' @param snames
#'
#' @return
#' @export
#'
#' @examples
selectSeries.X13BatchResult <- function(x, snames, simplify=TRUE) {
  if(missing(snames))
    res <- purrr::flatten(x) %>% .[unique(names(.))]
  else
    res <- purrr::flatten(x) %>% .[unique(snames)]

  if(simplify && length(res) == 1)
    res[[1]]
  else
    res

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
tvals.X13BatchResult <- function(x, variables) {
  if(missing(variables)) {
    variables <- x %>% selectSeries(simplify=FALSE) %>% map(function(x) {
      x %>% attr("udg") %>% names() %>% grep(rex::rex(start, "Outlier$"),., value=TRUE) %>%
        stringr::str_remove(rex::rex(start, "Outlier$"))
    }) %>% purrr::flatten_chr() %>% unique() %>% sort()
  }
  res <- x %>% selectSeries(simplify=FALSE) %>% map(tvals, variables)
  res %>% bind_rows(.id="series")
}

#' Is the x-11 method additive or multiplicative?
#'
#' @param x
#'
#' @return "add", "mult", or NA
#' @export
#'
#' @examples
X11AddMult.X13Batch <- function(x) {
  x %>% selectSeries(simplify=FALSE) %>% purrr::map_chr(X11AddMult)

}
