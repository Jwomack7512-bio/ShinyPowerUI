#' Create a Custom Plot UI for Shiny
#'
#' This function generates a custom plot UI for Shiny applications with additional features,
#' such as double-click detection for labeling editing.
#'
#' @param id A unique identifier for the plot UI.
#' @param height The height of the plot in pixels (default is 400).
#'
#' @return A Shiny UI tag for the custom plot.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#'
#' ui <- fluidPage(
#'   titlePanel("Bar Chart for mtcars"),
#'   sidebarLayout(
#'     sidebarPanel(
#'       selectInput(
#'         "variable",
#'         "Choose a variable:",
#'         choices = colnames(mtcars),
#'         selected = "mpg")
#'     ),
#'     mainPanel(
#'       plotUI("barChart")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'
#'   barplot_data <- reactive({
#'     ggplot(
#'       mtcars,
#'       aes(x = reorder(.data[[input$variable]],
#'                       .data[[input$variable]]),
#'           y = .data[[input$variable]])) +
#'       geom_bar(stat = "identity", fill = "steelblue") +
#'       labs(title = paste("Bar Chart for", input$variable),
#'            x = input$variable,
#'            y = "Count") +
#'       theme_minimal()
#'   })
#'
#'   callModule(plotServer, "barChart", barplot_data)
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
plotUI <- function(id, height = 400) {
  ns <- NS(id)
  tagList(
    div(
      style = "position:relative;",
      plotOutput(ns("plot"), height = height),
      tags$style(type = "text/css",
                 paste0("#", ns("plot"), " { cursor: default; }")),
      # Use the JavaScript function in your Shiny app
      tags$script(HTML(
        sprintf(
          "$(document).ready(function() {
        var plotElement = document.getElementById('%s');
        if(plotElement) {
          var threshold = 50;
          plotElement.onmousemove = function(evt) {
            var rect = plotElement.getBoundingClientRect();
            if (evt.clientY - rect.top <= threshold ||
                rect.bottom - evt.clientY <= threshold ||
                evt.clientX - rect.left <= threshold) {
              plotElement.style.cursor = 'pointer';
            } else {
              plotElement.style.cursor = 'default';
            }
          };

          function handleDblClick(plotId, labelEditId) {
            return function(evt) {
              var clickedY = evt.clientY;
              var clickedX = evt.clientX;
              var svgBounds = document.getElementById(plotId).getBoundingClientRect();
              var threshold = 50;
              if (clickedY - svgBounds.top <= threshold) {
                Shiny.setInputValue(labelEditId, {zone: 'title', ts: new Date().getTime()});
              } else if (svgBounds.bottom - clickedY <= threshold) {
                Shiny.setInputValue(labelEditId, {zone: 'x', ts: new Date().getTime()});
              } else if (clickedX - svgBounds.left <= threshold) {
                Shiny.setInputValue(labelEditId, {zone: 'y', ts: new Date().getTime()});
              }
            };
          }

          plotElement.addEventListener('dblclick', handleDblClick('%s', '%s'));
        }
      });",
      ns("plot"), ns("plot"), ns("label_edit")
        )
      ))
    )
  )
}
