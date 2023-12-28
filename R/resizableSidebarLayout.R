#' Create a resizable and collapsible sidebar layout for Shiny apps.
#' This Shiny UI layout function provides a responsive two-panel layout with
#' a collapsible sidebar.
#'
#' Features:
#' - Initial sidebar and main content sizes are set to 30% and 70% respectively.
#' - A toggle button, styled as an arrow, allows users to collapse or expand
#'   the sidebar.
#' - The toggle button's background color is customizable, with the default
#'   as a light grey (#e9e9e9).
#' - On hover, the toggle button enhances visibility by changing its
#'   background color and increasing the arrow size.
#'
#' @param sidebarContent UI elements to be placed inside the sidebar.
#' @param mainContent UI elements to be placed in the main content area.
#' @param bgColorBtn (optional) Allows customization of the toggle button's
#'   background color.
#' @param arrowSize (optional) Size of the arrow in the toggle button.
#' @param hideBarByDefault (optional) Boolean indicating whether the sidebar
#'   should be hidden by default.
#'
#' @return A Shiny UI layout with a resizable and collapsible sidebar.
#'
#' @importFrom shiny tags
#' @importFrom shiny HTML
#' @examples
#' \dontrun{
#' library(shiny)
#'
#' ui <- resizableSidebarLayout(
#'   sidebarContent = sidebarPanel(
#'     shiny::tags$h3("Sidebar Content"),
#'     actionButton("btn", "Click Me!")
#'   ),
#'   mainContent = mainPanel(
#'     shiny::tags$h3("Main Content"),
#'     plotOutput("plot")
#'   ),
#'   bgColorBtn = "#3498db",
#'   arrowSize = "large",
#'   hideBarByDefault = FALSE
#' )
#'
#' server <- function(input, output) {
#'   output$plot <- renderPlot({
#'     plot(1:10, main = "Example Plot")
#'   })
#'}
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
resizableSidebarLayout <- function(
    sidebarContent,
    mainContent,
    bgColorBtn = "#e9e9e9",
    arrowSize = "default",
    hideBarByDefault = TRUE
) {

  # Generate a unique ID suffix
  idSuffix <- sample(1000:9999, 1)

  # Create dynamic IDs using the suffix
  containerID <- paste0("container", idSuffix)
  leftPaneID <- paste0("leftPane", idSuffix)
  toggleButtonID <- paste0("toggleButton", idSuffix)
  rightPaneID <- paste0("rightPane", idSuffix)

  initialOpacity <- ifelse(hideBarByDefault, 0, 1)

  tagList(
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
      tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
      tags$style(HTML(
        "
        #container {
          display: flex;
          width: 100%;
          height: 100%;
        }

        #leftPane {
          width: 30%;
          height: 100%;
          overflow: hidden;
          padding: 5px;
        }

        #toggleButton {
          width: 20px;
          cursor: pointer;
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: flex-start;
          background-color: ", bgColorBtn, ";
          transition: background-color 0.3s, font-size 0.3s;  /* Add smooth transitions */
        }

        #toggleButton:hover {
          background-color: #b5b5b5;  /* Change color on hover */
          font-size: 20px;  /* Increase size on hover */
        }

        #rightPane {
          flex-grow: 1;
          height: 100%;
          padding: 5px;
          overflow: hidden;
        }
        "
      ))
    ),

    div(id = "container",
        div(id = "leftPane", sidebarContent),
        div(id = "toggleButton", "⬅"),
        div(id = "rightPane", mainContent)
    ),

    tags$script(HTML(
      "
      $(document).ready(function() {
        var isCollapsed = false;
        $('#toggleButton').click(function() {
          if (!isCollapsed) {
            $('#leftPane').hide();
            $('#toggleButton').html('➡');
            isCollapsed = true;
          } else {
            $('#leftPane').show();
            $('#leftPane').css('width', '30%');  // Reset width to 30% on expand
            $('#rightPane').css('width', '70%'); // Reset width to 70% on expand
            $('#toggleButton').html('⬅');
            isCollapsed = false;
          }
          // Trigger Shiny to resize the outputs
          $(window).trigger('resize');
        });

        $('#leftPane').resizable({
          handles: 'e',
          resize: function(event, ui) {
            var remainingSpace = $('#container').width() - ui.size.width,
                divTwo = $('#rightPane'),
                divTwoWidth = remainingSpace - (divTwo.outerWidth() - divTwo.width()) - $('#toggleButton').outerWidth();
            divTwo.css('width', divTwoWidth + 'px');
          }
        });
      });
      "
    ))
  )
}
