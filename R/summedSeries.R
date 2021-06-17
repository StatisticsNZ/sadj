

#' @keywords internal
sumX13Results <- function(x, ...){
  args <- list(...)
  if(any(!purrr::map_lgl(series <- c(list(x),args),function(x){
    inherits(x,"X13SeriesResult") || inherits(x,"X13SummedSeriesResult")
  })))
    stop("Arguments are not of type X13SummedSeriesResult or X13SeriesResult.")

  # Should check lengths are the same and dates are the same
  if(!all(purrr::map_lgl(args,function(y)identical(y[["date"]], x[["date"]]))))
    stop("Not all series dates are identical.")

  sum_cols <- c("value","b1","d11","d12")

  res <- sum_cols %>% purrr::set_names() %>% purrr::map(function(x){
    purrr::map(series,x) %>% reduce(`+`)
  }) %>% as.data.frame() %>% cbind(select(x,date,year,period), .)

  class(res) <- c("X13SummedSeriesResult", class(res))
  attr(res,"value") <- "value"
  attr(res,"series_names") <- series %>%
    purrr::map(function(x){
      if (inherits(x,"X13SeriesResult")) sname(x) else attr(x,"series_names")
    }) %>% purrr::flatten_chr()
  # attr(res,"lname") <- attr(res,"sname") <- attr(res,"series_names") %>% paste(collapse=", ")
  return(res)

}

#' @export
Sum.X13SeriesResult <- function(x, ...){
  sumX13Results(x, ...)
}

#' @export
Sum.X13SummedSeriesResult <- function(x, ...){
  sumX13Results(x, ...)

}

plot.X13SummedSeriesResult <- function(x, type = "B1D11D12", interactive = FALSE, ...){
  if (tolower(type) == "b1d11d12")
    plotB1D11D12(x, interactive, ...)
  else stop("Unknown plot type.")
}

#' @export
sname.X13SummedSeriesResult <- function(x) attr(x,"series_names") %>% paste(collapse=", ")

#' @export
lname.X13SummedSeriesResult <- function(x) attr(x,"series_names") %>% paste(collapse=", ")
