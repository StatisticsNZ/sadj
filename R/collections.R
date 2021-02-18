#' Create a new \code{X13mtaCol}
#'
#' @keywords internal
new_X13mtaCol <- function(x=list()) {
  stopifnot(is.list(x))
  structure(x, class = "X13mtaCol")
}


#' Create a new \code{X13DataCol}
#'
#' @keywords internal
new_X13specCol <- function(x=list()) {
  stopifnot(is.list(x))
  structure(x, class = "X13specCol")
}


#' Create a new \code{X13actualCol}
#'
#' @keywords internal
new_X13actualCol <- function(x=list()) {
  stopifnot(is.list(x))
  structure(x, class = "X13actualCol")
}

#' Create a new \code{X13facCol}
#'
#' @keywords internal
new_X13facCol <- function(x=list()) {
  stopifnot(is.list(x))
  structure(x, class = "X13facCol")
}

