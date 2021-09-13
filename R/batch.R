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

  series <- selectSeries(x)
  if (purge)
    series %>% purrr::map(clean)

  if(parallel)
    res <- parallel::mclapply(x, adjust, purge=FALSE, mc.cores = parallel::detectCores() / 2)
  else
    res <- lapply(x, adjust, purge=FALSE)
  class(res) <- c("X13BatchResult", class(res))

  if (purge)
    series %>% purrr::map(clean)

  res
}

summary.X13Batch <- function(x) {
  groups <- x %>% purrr::map(names) %>% tibble::enframe() %>% tidyr::unnest(cols = c(value)) %>%
    dplyr::group_by(value) %>% dplyr::summarise(groups=paste(name, collapse="|")) %>%
    dplyr::rename(sname=value)

  selectSeries(x) %>% purrr::map_dfr(function(x){
    list(sname=sname(x), last_period=tail(x$date,1), spec_type=specType(x),
         has_fac=hasFac(x), has_reg=hasReg(x)
         )
  }) %>% merge(groups,.,by="sname") %>% dplyr::arrange(sname)

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

#' Select unique series
#'
#' @param x
#' @param snames
#'
#' @return
#' @export
#'
#' @examples
selectSeries.X13BatchResult <- function(x, snames) {
  if(missing(snames))
    flatten(x) %>% .[unique(names(.))]
  else
    flatten(x) %>% .[unique(snames)]

}
