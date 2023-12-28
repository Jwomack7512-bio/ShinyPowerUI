#' Resizable Shiny Plot Output
#'
#' Wraps a Shiny plot output in a resizable container.
#'
#' @param plot_output A Shiny plot output element, typically from `plotOutput()`.
#' @param autoResizeWithWindow Logical. If `TRUE`, the plot resizes automatically with the window. Default is `TRUE`.
#' @param showBorderOnResize Logical. If `TRUE`, shows a dotted border around the plot while resizing. Default is `TRUE`.
#'
#' @return A Shiny UI element (HTML tags) that wraps the provided plot output, making it resizable.
#'
#' @importFrom shiny tags
#' @importFrom shiny HTML
#'
#' @examples
#' \dontrun{
#' library(shiny)
#'
#' ui <- fluidPage(
#'   titlePanel("Resizable Plot Demo"),
#'   sidebarLayout(
#'     sidebarPanel(
#'       helpText("Resize the plot by dragging from its edges.")
#'     ),
#'     mainPanel(
#'       plotResizable(plotOutput("distPlot"))
#'     )
#'   )
#' )
#'
#' server <- function(input, output) {
#'   output$distPlot <- renderPlot({
#'     hist(rnorm(100))
#'   })
#' }
#'
#' shinyApp(ui = ui, server = server)
#' }
#'

#' @export
plotResizable <- function(plot_output,
                          autoResizeWithWindow = TRUE,
                          showBorderOnResize = TRUE) {

  resizeScript <- if (autoResizeWithWindow) {
    shiny::tags$script(HTML("
      var prevWindowWidth = $(window).width();
      var prevWindowHeight = $(window).height();

      function setResizableLimits() {
          var currentWindowWidth = $(window).width();
          var currentWindowHeight = $(window).height();

          var widthRatio = currentWindowWidth / prevWindowWidth;
          var heightRatio = currentWindowHeight / prevWindowHeight;

          var newPlotWidth = $('#resizablePlot').width() * widthRatio;
          var newPlotHeight = $('#resizablePlot').height() * heightRatio;

          $('#resizablePlot').width(newPlotWidth);
          $('#resizablePlot .shiny-plot-output').width(newPlotWidth);
          $('#resizablePlot').height(newPlotHeight);
          $('#resizablePlot .shiny-plot-output').height(newPlotHeight);

          prevWindowWidth = currentWindowWidth;
          prevWindowHeight = currentWindowHeight;

          Shiny.setInputValue('window_resized', Math.random());
      }

      $(window).resize(setResizableLimits);
    "))
  } else {
    NULL
  }

  borderColor <- if (showBorderOnResize) "'#888'" else "'transparent'"

  shiny::tags$div(
    id = 'resizablePlot',
    plot_output,

    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
      shiny::tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.min.js"),
      shiny::tags$style(HTML("
        #resizablePlot {
          border: 2px dashed transparent;  /* Initially transparent border */
        }
      ")),
      shiny::tags$script(HTML(sprintf("
        $(document).ready(function(){
            $('#resizablePlot').resizable({
                alsoResize: '#resizablePlot .shiny-plot-output',
                start: function(event, ui) {
                    $(this).css({
                        'border-color': %s,  /* Conditionally set the border color when resizing starts */
                        'z-index': '1000'
                    });
                },
                stop: function(event, ui) {
                    $(this).css({
                        'border-color': 'transparent',  /* Reset the border color when resizing stops */
                        'z-index': 'auto'
                    });
                }
            });
        });
      ", borderColor))),
      resizeScript
    )
  )
}
