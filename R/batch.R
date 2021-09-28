#' @export
`[.X13Batch` <- function(x, ..., drop=TRUE) {
  structure(NextMethod(), class=class(x))
}

#' @export
`[.X13BatchResult` <- function(x, ..., drop=TRUE) {
  structure(NextMethod(), class=class(x))
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

  # series <- selectSeries(x)
  # if (purge) {
  #   # series %>% purrr::map(clean)
  #   x %>% purrr::walk2(as.integer(names(x)),function(x,y){
  #     x %>% purrr::walk(clean,grp_num=y)
  #   })
  # }


  if(parallel)
    res <- parallel::mcmapply(function(x,y) adjust(x, grp_num=y,purge=purge)
                              , x, as.integer(names(x))
                              , SIMPLIFY = FALSE
                              , mc.cores = parallel::detectCores() / 2)
    # res <- x %>% parallel::mclapply(adjust, purge=FALSE, mc.cores = parallel::detectCores() / 2)
  else
    res <- mapply(function(x,y) adjust(x, grp_num=y,purge=purge)
                  , x, as.integer(names(x))
                  , SIMPLIFY = FALSE)
    # res <- lapply(x, adjust, purge=FALSE)
  class(res) <- c("X13BatchResult", class(res))

  # if (purge)
  #   series %>% purrr::map(clean)
  # if (purge) {
  #   x %>% purrr::walk2(as.integer(names(x)),function(x,y){
  #     x %>% purrr::walk(clean,grp_num=y)
  #   })
  # }

  res
}

# #' @keywords internal
# format.x13grp_name <- function(x) x[[1]] %>% purrr::reduce(paste,sep="|")

# #' @keywords internal
# pillar_shaft.x13grp_name <- function(x, ...) {
#   pillar::new_pillar_shaft_simple(format(x))
# }

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
    # dplyr::summarise(groups=paste(name, collapse="|")) %>%
    dplyr::rename(sname=value)

  selectSeries(x) %>% purrr::map_dfr(function(x){
    list(sname=sname(x), last_period=tail(x$date,1), spec_type=specType(x),
         has_fac=hasFac(x), has_reg=hasReg(x)
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
