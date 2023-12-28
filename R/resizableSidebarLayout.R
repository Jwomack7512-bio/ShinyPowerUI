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

  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        paste0("
          #", containerID, " {
            display: flex;
            width: 100%;
            min-height: 90vh;
          }

          #", leftPaneID, ", #", rightPaneID, " {
            overflow-y: auto;
            overflow-x: hidden;
          }

          #", leftPaneID, " {
            width: 30%;
            padding-right: 0px;
            padding-left: 0px;
          }

          #", toggleButtonID, " {
            width: 20px;
            cursor: pointer;
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content: flex-start;
            opacity: ", initialOpacity, ";
            transition: opacity 0.3s, background-color 0.3s, font-size 0.3s;
          }

          #", toggleButtonID, ":hover, #", toggleButtonID, ".collapsed {
            opacity: 1;
          }

          #", toggleButtonID, ":hover {
            background-color: #b5b5b5;
            font-size: 20px;
          }

          #", rightPaneID, " {
            flex-grow: 1;
            padding-right: 0px;
            padding-left: 0px;
          }

          /* Media query for screens less than 768 pixels wide */
          @media (max-width: 768px) {
            #", leftPaneID, ", #", rightPaneID, " {
              width: 100% !important;
            }
            #", toggleButtonID, " {
              display: none;
            }
            #", containerID, " {
              flex-direction: column;
            }
          }
        ")
      ))
    ),

    shiny::div(id = containerID,
               shiny::div(id = leftPaneID, sidebarContent),
               shiny::div(id = toggleButtonID,
            style = paste0("background-color:", bgColorBtn, ";"), "<-"),
            shiny::div(id = rightPaneID, mainContent)
    ),

    shiny::tags$script(shiny::HTML(
      sprintf("
        $(document).ready(function() {
          var isCollapsed = false;
          $('#%s').click(function() {
            if (!isCollapsed) {
              $('#%s').hide();
              $('#%s').html('->').addClass('collapsed');
              isCollapsed = true;
            } else {
              $('#%s').show();
              $('#%s').css('width', '30%%');
              $('#%s').css('width', '70%%');
              $('#%s').html('<-).removeClass('collapsed');
              isCollapsed = false;
            }
            $(window).trigger('resize');
          });

          $('#%s').resizable({
            handles: 'e',
            resize: function(event, ui) {
              var remainingSpace = $('#%s').width() - ui.size.width,
                  divTwo = $('#%s'),
                  divTwoWidth = remainingSpace - (divTwo.outerWidth() -
                                       divTwo.width()) - $('#%s').outerWidth();
              divTwo.css('width', divTwoWidth + 'px');
            }
          });
        });
      ",
      toggleButtonID,
      leftPaneID,
      toggleButtonID,
      leftPaneID,
      leftPaneID,
      rightPaneID,
      toggleButtonID,
      leftPaneID,
      containerID,
      rightPaneID,
      toggleButtonID
      ))
    )
  )
}
