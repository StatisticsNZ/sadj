#' Create a new \code{X13facFile}
#'
#' @keywords internal
new_X13facFile <- function(x=data.frame()) {
  stopifnot(is.data.frame(x))
  structure(x, class = "X13facFile")
}

readFacsFromDir <- function(path){

  if (!dir.exists(path))
    stop("Directory does not exist.")

  path %<>% str_remove("[////]$")

  all_facs <- list.files(path, full.names = TRUE, pattern = "\\.fac$")
  names(all_facs) <- list.files(path, pattern = "\\.fac$") %>% str_remove("\\.fac$")
  res <- all_facs %>% map(read.csv)
  class(res) <- "X13facCol"
  return(res)
}
