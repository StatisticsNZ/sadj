#' @import reshape2
#' @import ggplot2
#'
#' @export
plot.X13SeriesResult <- function(x, interactive = FALSE, type = "default", comp_indirect = FALSE, ...){

  colnames(x)[colnames(x)%in%c(attr(x, "value"))] <- "original"

  if (comp_indirect) {
    colnames(x)[colnames(x)%in%c("isa")] <- "seasadj"
    colnames(x)[colnames(x)%in%c("itn")] <- "trend"
    colnames(x)[colnames(x)%in%c("isf")] <- "sf"

  } else {

    colnames(x)[colnames(x)%in%c("d11", "s11")] <- "seasadj"
    colnames(x)[colnames(x)%in%c("d12", "s12")] <- "trend"
    colnames(x)[colnames(x)%in%c("d10", "s10")] <- "sf"
  }

  x$seasfact <- x$original / x$seasadj
  x$irregular <- x$seasadj / x$trend
  # x$date <- date(x)
  # x$year <- substr(x$year, 3, 4)

  X <- melt(x[, c("year", "period", "date", "original", "seasadj", "trend")],
            id.vars = c("year", "period", "date"))

  if (type == "default")
    p <- ggplot(data = X, aes(x = date, y = value, col = variable)) +
      geom_line() +
      xlab("") +
      ylab("") +
      theme(legend.title=element_blank()) #, legend.position = "bottom")
  else if (type == "seasonal"){
    if (min(x$sf) < 0 & max(x$sf) > 0) intercept <- 0
    else intercept <- 1
    p <- ggplot(data = x, aes(x = year, y = sf, group = period)) +
      facet_wrap(~ period, nrow = 1) +
      geom_hline(aes(yintercept = intercept, col = "red")) +
      geom_line() +
      geom_point() +
      xlab("") +
      ylab("") +
      theme(legend.position="none", axis.text.x = element_blank())
  }
  else return()

  if (interactive & require(plotly)) ggplotly(p, ...)
  else p
}

plot.X13SeriesResult.B1D11D12 <- function(x, ...) {
  stop("Not implemented.")
}

plot.X13SeriesResult.decomp <- function(x, ...) {
  stop("Not implemented.")
}

plot.X13SeriesResult.D10 <- function(x, ...) {
  stop("Not implemented.")
}

plot.X13SeriesResult.D10D8 <- function(x, ...) {
  stop("Not implemented.")
}

plot.X13SeriesResult.D11oD11 <- function(x, ...) {
  stop("Not implemented.")
}

plot.X13SeriesResult.D12oD12 <- function(x, ...) {
  stop("Not implemented.")
}
