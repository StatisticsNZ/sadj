#' Create a list of X13SeriesGroup objects from an mta file.
#'
#' @param mta Path to mta file.
#'
#' @export
X13BatchFromMTA <-function(mta_path) {
  grps <- mtaGroups(mta_path)
  ser_lists <- grps %>% parallel::mclapply(mtaToX13Series, mc.cores = parallel::detectCores() / 2)
  res <- ser_lists %>% map2(names(ser_lists),~X13ListToGroup(ser_list = .x,sname = .y))

  class(res) <- c("X13Batch", class(res))
  res
}

#' Adjust an X13Batch.
#'
#' @export
adjust.X13Batch <- function(x, purge = TRUE, ...) {
  res <- parallel::mclapply(x, adjust, purge, mc.cores = parallel::detectCores() / 2)
  class(res) <- c("X13BatchResult", class(res))
  res
}

#' Print X13Batch.
#'
#' @export
toString.X13Batch <- function(x, ...){
  purrr::map2_chr(x,names(x),function(y,z){
    names(y) %>% paste(collapse=" ") %>% sprintf("%s: %s",z,.)
  }) %>% paste(collapse="\n") %>% cat()
}

#' Print X13BatchResult.
#'
#' @export
toString.X13BatchResult <- function(x, ...){
  purrr::map2_chr(x,names(x),function(y,z){
    names(y) %>% paste(collapse=" ") %>% sprintf("%s: %s",z,.)
  }) %>% paste(collapse="\n") %>% cat()
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
