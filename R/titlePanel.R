#' titlePanel: A Custom Panel for Shiny UI with Various Styling Options
#'
#' The `titlePanel` function provides a way to create a styled panel
#' in Shiny applications. This custom panel can include widgets,
#' an optional title, and several style customizations.
#'
#' @importFrom shiny div tagList NS HTML
#'
#' @param ... Widgets to be included in the panel.
#' @param title An optional title for the panel. Default is NULL.
#' @param bg_color Background color for the panel. Default is transparent.
#' @param header_font Font for the title. Default is "Arial".
#' @param header_weight Weight for the title font. Default is "normal".
#' @param header_font_size Font size for the title. Default is "18px".
#' @param header_color Color for the title. Default is "grey".
#' @param id User-defined ID for the panel. If not provided, a unique ID will be generated.
#' @param border_thickness Thickness of the panel border. Default is "1px".
#' @param border_color Color of the panel border. Default is "lightgrey".
#' @param border_type Type of the panel border. Default is "solid".
#' @param margin_top Top margin for the panel. Default is "20px".
#' @param height Height of the panel. Default is NULL (auto height).
#' @return A shiny tag list containing the custom panel and its styles.
#'
#' @examples
#' \dontrun{
#' titlePanel(
#'     shiny::sliderInput("slider1", "Slider Input:", min = 0, max = 100, value = 50),
#'     shiny::textInput("text1", "Text Input:"),
#'     title = "Custom Title",
#'     bg_color = "red",
#'     header_font = "Times New Roman",
#'     header_weight = "bold",
#'     header_font_size = "24px",
#'     header_color = "blue",
#'     id = "myCustomID",
#'     border_thickness = "2px",
#'     border_color = "blue",
#'     border_type = "dashed",
#'     margin_top = "15px",
#'     height = "300px"  # Set the height here
#' )
#' }
#' @export
titlePanel <- function(...,
                       title = NULL,
                       bg_color = NULL,
                       header_font = "Arial",
                       header_weight = "normal",
                       header_font_size = "18px",
                       header_color = "grey",
                       id = NULL,
                       border_thickness = "1px",
                       border_color = "lightgrey",
                       border_type = "solid",
                       margin_top = "20px",
                       height = NULL) {

  # If no ID provided, generate a unique ID using NS and timestamp
  if (is.null(id)) {
    id <- paste0("custom-panel-", format(Sys.time(), "%Y%m%d%H%M%OS"))
    ns_id <- shiny::NS(id)  # Namespaced ID
    # Height styling for auto-generated ID
    height_css <- if (!is.null(height)) {
      paste0("height: ", height, ";")
    } else {
      "height: auto;"
    }
  } else {
    ns_id <- shiny::NS(id)  # Namespaced ID
    height_css <- ""
  }

  # Widgets to be included in the panel
  widgets <- list(...)

  # Optional title for the panel
  title_div <- if (!is.null(title)) {
    shiny::div(class = "custom-panel-header", title)
  } else {
    NULL
  }

  # Background color styling
  bg_css <- if (!is.null(bg_color)) {
    paste0("background-color: ", bg_color, ";")
  } else {
    "background-color: transparent;"
  }

  # Create and return the custom panel with its styles
  shiny::tagList(
    shiny::tags$style(shiny::HTML(paste0(
      "#", id, " {
            ", bg_css, "
            border-radius: 10px;
            border: ", border_thickness, " ", border_type, " ", border_color, ";
            padding: 20px;
            position: relative;
            margin-top: ", margin_top, ";
            ", height_css, "
        }

        #", id, " .custom-panel-header {
            position: absolute;
            top: 0;
            left: 50%;
            transform: translate(-50%, -50%);
            background-color: white;
            padding: 5px 10px;
            border-radius: 5px;
            font-size: ", header_font_size, ";
            font-weight: ", header_weight, ";
            font-family: '", header_font, "';
            color: ", header_color, ";
        }"
    ))),
    shiny::div(id = id, class = "custom-panel",
               title_div,
               widgets
    )
  )
}
