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
             "Seasonal Factors",
             plotly::plotlyOutput(
               "factors", height = "700px"
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

        output$factors <- plotly::renderPlotly({
          plotly::ggplotly(plot(x, type = "seasonal"))
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
      ui = shiny::fluidPage(
        shiny::titlePanel(deparse(substitute(x))),
        shiny::sidebarLayout(
          shiny::sidebarPanel(width = 3,
            shiny::selectizeInput(
              "series", "select series:",
              choices = setNames(
                as.list(names(x)),
                sapply(empgrp.res, function(x) attr(attr(x, "input"), "lname"))
              )
            )
          ),
          shiny::mainPanel(width = 9,
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Original, SA, Trend",
                plotly::plotlyOutput(
                  "allseries", height = "700px"
                )
              ),
              shiny::tabPanel(
                "Seasonal Factors",
                plotly::plotlyOutput(
                  "factors", height = "700px"
                )
              ),
              shiny::tabPanel(
                "Diagnostics",
                shiny::div(shiny::tableOutput("summary"), style = "padding: 10px;")
              )
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
