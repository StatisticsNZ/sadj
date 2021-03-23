# Composite - Indirect Plots -------------------------------

plot.X13SeriesResult.B1D11ISA <- function(x, interactive = FALSE, ...) {
  cols <- colnames(x)
  sa <- if ("d11" %in% cols) "d11" else "s11"
  isa <- "isa"
  # tr <- if ("d12" %in% cols) "d12" else "s12"

  e <- lapply(c("date", attr(x, "value"), sa, isa), rlang::sym) %>%
    setNames(c("date", "original", "direct adjusted", "indirect adjusted"))

  d <- dplyr::select(x, !!!e) %>%
    reshape2::melt(id.vars = c("date"))

  p <- ggplot2::ggplot(
    data = d,
    aes(x = date, y = value, col = variable)
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(title = lname(attr(x, "input"))) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::scale_color_discrete(
      name = "series"
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )


  if (interactive & require(plotly)) {
    p <- ggplotly(p, ...)
    # TODO fix up legends here.
    # TODO inc. changing legend location.
  }

  p
}
