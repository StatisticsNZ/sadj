#' Create a new \code{X13actual}
#'
#' @keywords internal
new_X13actual <- function(x=data.frame()) {
  stopifnot(is.data.frame(x))
  structure(x, class = "X13actual")
}


readActualsFromDir <- function(path){

  if (!dir.exists(path))
    stop("Directory does not exist.")

  path %<>% str_remove("[////]$")

  all_dats <- list.files(path, full.names = TRUE, pattern = "\\.dat$")
  names(all_dats) <- list.files(path, pattern = "\\.dat$") %>% str_remove("\\.dat$")
  res <- all_dats %>% map(read.csv)
  class(res) <- "X13actualCol"
  return(res)
}
