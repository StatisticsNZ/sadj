#' Show result of adjustment as interactive application.
#'
#' @param x {{X13SeriesResult}} resulting from adjusting {{X13Series}}
#'
#' @export
view.X13SeriesResult <- function(x) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cat("Please install shiny and try again.")
  }
  else if (!requireNamespace("plotly", quietly = TRUE)) {
    cat("Please install plotly and try again.")
  }
  else {
    shiny::shinyApp(
      ui = shiny::basicPage(
        shiny::div(style = "padding:20px;",
          shiny::h1(attr(attr(x, "input"), "lname")),
          shiny::tabsetPanel(
            shiny::tabPanel(
             "Original, SA, Trend",
             plotly::plotlyOutput(
               "allseries", height = "700px"
             )
            ),
            shiny::tabPanel(
              "Decomposition",
              plotly::plotlyOutput(
                "decomp", height = "700px"
              )
            ),
            shiny::tabPanel(
             "Seasonal Factors",
             plotly::plotlyOutput(
               "factors", height = "700px"
             )
            ),
            shiny::tabPanel(
              "S-I Ratios",
              plotly::plotlyOutput(
                "si_ratios", height = "700px"
              )
            ),
            shiny::tabPanel(
             "Diagnostics",
             shiny::div(shiny::tableOutput("summary"), style = "padding: 10px;")
            )
          )
        )
      ),
      server = function(input, output) {
        output$allseries <- plotly::renderPlotly({
          plotly::ggplotly(plot(x))
        })

        output$decomp <- plotly::renderPlotly({
          plotly::ggplotly(plot(x, type = "decomp"))
        })

        output$factors <- plotly::renderPlotly({
          plotly::ggplotly(plot(x, type = "D10"))
        })

        output$si_ratios <- plotly::renderPlotly({
          plotly::ggplotly(plot(x, type = "D10D8"))
        })

        output$summary <- shiny::renderTable(
          summary(x)
        )
      },
      onStart = NULL, options = list(), uiPattern = "/"
    )
  }
}

#' Show result of adjustment as interactive application.
#'
#' @param x {{X13SeriesGroupResult}} resulting from adjusting {{X13SeriesGroup}}
#'
#' @export
view.X13SeriesGroupResult <- function(x) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cat("Please install shiny and try again.")
  }
  else if (!requireNamespace("plotly", quietly = TRUE)) {
    cat("Please install plotly and try again.")
  }
  else {
    shiny::shinyApp(
      ui = shiny::basicPage(
        shiny::h1(deparse(substitute(x))),
        shiny::div(
          shiny::selectizeInput(
            "series", "select series:",
            choices = setNames(
              as.list(names(x)),
              sapply(x, function(x) attr(attr(x, "input"), "lname"))
            )
          )
        ),
        shiny::div(
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Original, SA, Trend",
              plotly::plotlyOutput(
                "allseries", height = "600px"
              )
            ),
            shiny::tabPanel(
              "Seasonal Factors",
              plotly::plotlyOutput(
                "factors", height = "600px"
              )
            ),
            shiny::tabPanel(
              "Diagnostics",
              shiny::div(shiny::tableOutput("summary"), style = "padding: 10px;")
            )
          )
        )
      ),
      server = function(input, output) {
        output$allseries <- plotly::renderPlotly({
          plotly::ggplotly(plot(x[[input$series]]))
        })

        output$factors <- plotly::renderPlotly({
          plotly::ggplotly(plot(x[[input$series]], type = "seasonal"))
        })

        output$summary <- shiny::renderTable(
          summary(x[[input$series]])
        )
      },
      onStart = NULL, options = list(), uiPattern = "/"
    )
  }
}
