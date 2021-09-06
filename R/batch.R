#' Create a list of X13SeriesGroup objects from an mta file.
#'
#' @param mta Path to mta file.
#'
#' @export
X13BatchFromMTA <-function(mta_path) {
  grps <- mtaGroups(mta_path)
  ser_lists <- grps %>% parallel::mclapply(mtaToX13Series,mc.cores = parallel::detectCores() / 2)
  res <- ser_lists %>% map2(names(ser_lists),~X13ListToGroup(ser_list = .x,sname = .y))

  class(res) <- c("X13Batch", class(res))
  res
}

#' Adjust an X13Batch.
#'
#' @export
adjust.X13Batch <- function(x, purge = TRUE, parallel=TRUE, ...) {
  if(parallel)
    res <- parallel::mclapply(x, adjust, purge, mc.cores = parallel::detectCores() / 2)
  else
    res <- lapply(x, adjust, purge)
  class(res) <- c("X13BatchResult", class(res))
  res
}

#' Print X13Batch.
#'
#' @export
toString.X13Batch <- function(x, ...){
  # purrr::map2_chr(x,names(x),function(y,z){
  #   names(y) %>% paste(collapse=" ") %>% sprintf("%s: %s",z,.)
  # }) %>% paste(collapse="\n") %>% cat()
  selectSeries(x) %>% purrr::map_dfr(function(x){
    list(sname=sname(x), end_date=tail(x$date,1))
  })

}

#' Print X13BatchResult.
#'
#' @export
toString.X13BatchResult <- function(x, ...){
  # purrr::map2_chr(x,names(x),function(y,z){
  #   names(y) %>% paste(collapse=" ") %>% sprintf("%s: %s",z,.)
  # }) %>% paste(collapse="\n") %>% cat()
  selectSeries(x) %>% purrr::map_dfr(function(x){
    list(sname=sname(x), end_date=tail(x$date,1))
  })

}

#' Print X13Batch.
#'
#' @export
print.X13Batch <- function(x, ...){
  # cat(toString(x, ...))
  toString(x, ...) %>% print()
}

#' Print X13BatchResult.
#'
#' @export
print.X13BatchResult <- function(x, ...){
  # cat(toString(x, ...))
  toString(x, ...) %>% print()
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
selectSeries.X13Batch <- function(x, snames) {
  if(missing(snames))
    flatten(x) %>% .[unique(names(.))]
  else
    flatten(x) %>% .[unique(snames)]

}
