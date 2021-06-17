#' @import ggplot2
#'
#' @export
plot.X13Series <- function(x, interactive = FALSE, ...){
  # x$date <- date(x)
  p <- ggplot(data = x, aes_string("date", attr(x, "value"))) +
    geom_line() +
    xlab("") +
    ylab("")

  if (interactive & require(plotly)) ggplotly(p, ...)
  else p
}

#' Plot adjustment results
#'
#' @param x \code{X13SeriesResult}
#' @param type one of \code{B1D11D12}, \code{decomp}, \code{D10}
#' , \code{D10D8}, \code{D11oD11}, \code{D12oD12} or \code{b1d11isa}
#'
#' @import reshape2
#' @import ggplot2
#' @import dplyr
#'
#' @export
plot.X13SeriesResult <- function(x, type = "B1D11D12", interactive = FALSE, ...){
  if (tolower(type) == "b1d11d12")
    plotB1D11D12(x, interactive, ...)
  else if (tolower(type) == "decomp")
    plot.X13SeriesResult.decomp(x, interactive, ...)
  else if (tolower(type) == "d10")
    plot.X13SeriesResult.D10(x, interactive, ...)
  else if (tolower(type) == "d10d8")
    plot.X13SeriesResult.D10D8(x, interactive, ...)
  else if (tolower(type) == "d11od11")
    plot.X13SeriesResult.D11oD11(x, interactive, ...)
  else if (tolower(type) == "d12od12")
    plot.X13SeriesResult.D12oD12(x, interactive, ...)
  else if (tolower(type) == "b1d11isa")
    plot.X13SeriesResult.B1D11ISA(x, interactive, ...)
  else stop("Unknown plot type.")
}

# plot.X13SeriesResult.B1D11D12 <- function(x, interactive = FALSE, ...) {
#   plotB1D11D12(x, interactive, ...)
# }
#
# plot.X13SummedSeriesResult.B1D11D12 <- function(x, interactive = FALSE, ...) {
#   plotB1D11D12(x, interactive, ...)
# }

plotB1D11D12 <- function(x, interactive = FALSE, ...) {
  cols <- colnames(x)
  sa <- if ("d11" %in% cols) "d11" else "s11"
  tr <- if ("d12" %in% cols) "d12" else "s12"

  e <- lapply(c("date", attr(x, "value"), sa, tr), rlang::sym) %>%
    setNames(c("date", "original", "seasonally adjusted", "trend"))

  d <- dplyr::select(x, !!!e) %>%
    reshape2::melt(id.vars = c("date"))

  p <- ggplot2::ggplot(
      data = d,
      aes(x = date, y = value, col = variable)
    ) +
    ggplot2::geom_line() +
    # ggplot2::labs(title = ifelse(inherits(x,"X13SeriesResult"),lname(attr(x, "input")),"")) +
    ggplot2::labs(title = if(inherits(x,"X13SeriesResult")) lname(attr(x, "input")) else
      if(inherits(x,"X13SummedSeriesResult")) sname(x) else "") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::scale_color_discrete(
      name = "series"
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )

  if ("c17" %in% cols) {
    o <- x %>%
      dplyr::filter(c17 < 1) %>%
      dplyr::mutate(outlier = ifelse(c17 == 0, "zero weight", "partial weight")) %>%
      dplyr::select(date, !!rlang::sym(sa), outlier)

    p <- p +
      ggplot2::geom_point(
        data = o,
        aes(x = date, y = !!rlang::sym(sa), shape = outlier),
        inherit.aes = FALSE
      )
  }

  if (interactive & require(plotly)) {
    p <- ggplotly(p, ...)
    # TODO fix up legends here.
    # TODO inc. changing legend location.
  }

  p
}

plot.X13SeriesResult.decomp <- function(x, interactive = FALSE, ...) {
  cols <- colnames(x)
  or <- attr(x, "value")
  tr <- if ("d12" %in% cols) "d12" else "s12"
  sf <- if ("d10" %in% cols) "d10" else "s10"
  ir <- if ("d13" %in% cols) "d13" else "s13"

  e <- lapply(c("date", or, tr, sf, ir), rlang::sym) %>%
    setNames(c("date", "original", "trend", "seasonal factor", "irregular"))

  d <- dplyr::select(x, !!!e) %>%
    reshape2::melt(id.vars = c("date"))

  ystart = if (any(d[d$variable == "irregular", "value"] < 0)) 0 else 1

  p <- ggplot2::ggplot(data = d, aes(x = date, y = value)) +
    ggplot2::facet_wrap(~variable, ncol = 1, scales = "free_y") +
    ggplot2::geom_line(data = d[d$variable == "original",]) +
    ggplot2::geom_line(data = d[d$variable == "trend",]) +
    ggplot2::geom_line(data = d[d$variable == "seasonal factor",]) +
    ggplot2::geom_segment(
      data = d[d$variable == "irregular",],
      aes(x = date, xend = date, y = ystart, yend = value)
    ) +
    ggplot2::labs(title = lname(attr(x, "input"))) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5)
    )

  if (interactive & require(plotly))
    ggplotly(p, ...)
  else
    p
}

plot.X13SeriesResult.D10 <- function(x, interactive = FALSE, ...) {
  args <- list(...)

  ncol <-
    if ("ncol" %in% names(args)) {
      args[["ncol"]]
    } else {
      if (length(unique(x$period)) == 12) 4 else 2
    }

  sf <- if ("d10" %in% colnames(x)) "d10" else "s10"

  d <- x %>% dplyr::select(year, period, !!rlang::sym(sf)) %>%
    dplyr::rename(y = !!rlang::sym(sf)) %>%
    dplyr::inner_join(
      y = {.} %>% group_by(period) %>% summarise(ystart = mean(y)),
      by = c("period")
    )

  p <- ggplot2::ggplot(data = d) +
    ggplot2::facet_wrap(~period, ncol = ncol) +
    geom_segment(aes(x = year, xend = year, y = ystart, yend = y)) +
    geom_hline(aes(yintercept =  ystart), color = "red") +
    ggplot2::labs(title = lname(attr(x, "input"))) +
    ggplot2::ylab("seasonal factor") +
    ggplot2::xlab("") +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5)
    )

  if (interactive & require(plotly))
    ggplotly(p, ...)
  else
    p
}

plot.X13SeriesResult.D10D8 <- function(x, interactive = FALSE, ...) {
  args <- list(...)

  cols <- colnames(x)
  if (!"d10" %in% cols) stop("D10 series not found.")
  if (!"d8" %in% cols) stop("D8 series not found.")

  ncol <-
    if ("ncol" %in% names(args)) {
      args[["ncol"]]
    } else {
      if (length(unique(x$period)) == 12) 4 else 2
    }

  p <- ggplot(data = x) +
    facet_wrap(~period, ncol = ncol) +
    geom_line(aes(x = year, y = d10)) +
    geom_point(aes(x = year, y = d8)) +
    ggplot2::labs(title = lname(attr(x, "input"))) +
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5)
    )

  if (interactive & require(plotly))
    ggplotly(p, ...)
  else
    p
}

plot.X13SeriesResult.D11oD11 <- function(x, interactive = FALSE, ...) {
  cols <- colnames(x)
  if (!"d11" %in% cols) stop("D11 series not found.")
  if (!"sae" %in% cols) stop("SAE series not found (see docs for history spec).")

  mask <- !is.na(x$sae)
  mask[length(mask)] <- TRUE

  d <- x[mask,] %>%
    dplyr::select(date, d11, sae) %>%
    dplyr::rename(
      "seasonally adjusted" = d11,
      "previous seasonally adjusted" = sae
    ) %>%
    reshape2::melt(id.vars = c("date"))

  o <- x %>%
    filter(abs(d11 - sae) / sae * 100 > 1.5) %>%
    select(date, d11) %>%
    mutate(grp = "> 1.5% change")

  p <- ggplot(data = d) +
    geom_line(aes(x = date, y = value, col = variable)) +
    ggplot2::labs(title = lname(attr(x, "input"))) +
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::scale_color_discrete(
      name = "series"
    ) +
    ggplot2::geom_point(
      data = o,
      aes(x = date, y = d11, shape = grp),
      inherit.aes = FALSE
    ) +
    ggplot2::scale_shape_discrete(
      name = ""
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )

  if (interactive & require(plotly))
    ggplotly(p, ...)
  else
    p
}

plot.X13SeriesResult.D12oD12 <- function(x, interactive = FALSE, ...) {
  cols <- colnames(x)
  if (!"d12" %in% cols) stop("D12 series not found.")
  if (!"tre" %in% cols) stop("TRE series not found (see docs for history spec).")

  mask <- !is.na(x$tre)
  mask[length(mask)] <- TRUE

  d <- x[mask,] %>%
    dplyr::select(date, d12, tre) %>%
    dplyr::rename(
      "trend" = d12,
      "previous trend" = tre
    ) %>%
    reshape2::melt(id.vars = c("date"))

  o <- x %>%
    filter(abs(d12 - tre) / tre * 100 > 1.5) %>%
    select(date, d12) %>%
    mutate(grp = "> 1.5% change")

  p <- ggplot(data = d) +
    geom_line(aes(x = date, y = value, col = variable)) +
    ggplot2::labs(title = lname(attr(x, "input"))) +
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::scale_color_discrete(
      name = "series"
    ) +
    ggplot2::geom_point(
      data = o,
      aes(x = date, y = d12, shape = grp),
      inherit.aes = FALSE
    ) +
    ggplot2::scale_shape_discrete(
      name = ""
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )

  if (interactive & require(plotly))
    ggplotly(p, ...)
  else
    p
}




