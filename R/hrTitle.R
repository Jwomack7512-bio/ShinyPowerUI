#' Create a horizontal line with a title.
#'
#' This function generates a horizontal line with an optional title displayed above or inline. The title position (left, center, right) and colors can be customized.
#'
#' @param textToShow Character string. The text to display as the title.
#' @param position Character string. One of "left", "center", or "right" indicating the horizontal alignment of the title. Default is "center".
#' @param color Character string. The color of the horizontal line. Default is "grey".
#' @param textColor Character string. The color of the title text. Default is "black".
#' @param verticalPosition Character string. One of "top" or "inline" indicating the vertical position of the title with respect to the line. "top" positions the text above the line, while "inline" centers the text with the line. Default is "top".
#' @param fontSize Character string. The font size of the title text. Default is "inherit".
#' @param lineStyle Character string. One of "solid", "dashed", or "dotted" indicating the line style. Default is "solid".
#' @param paddingAbove Character string. The amount of padding above the widget. Default is "10px".
#' @param paddingBelow Character string. The amount of padding below the widget. Default is "10px".
#'
#' @return A \code{shiny.tagList} containing the styled horizontal line and title.
#'
#' @export
#' @importFrom shiny div tagList NS
#'
#' @examples
#' \dontrun{
#' # Create a Shiny app using hrTitle
#' library(shiny)
#'
#' ui <- fluidPage(
#'   hrTitle("Centered Header"),
#'   hrTitle("Left Header", "left"),
#'   hrTitle("Inline Right Header", "right", verticalPosition = "inline"),
#'   hrTitle("My Title" ,fontSize = "18px", lineStyle = "dashed")
#' )
#'
#' server <- function(input, output, session) {}
#'
#' shinyApp(ui, server)
#' }

hrTitle <- function(textToShow,
                    position = "center",
                    color = "grey",
                    textColor = "black",
                    verticalPosition = "top",
                    fontSize = "inherit",  # Default to inherit font size
                    lineStyle = "solid",   # Default to solid line
                    paddingAbove = "10px",  # Default padding above
                    paddingBelow = "10px") {  # Default padding below

  position_class <- switch(position,
                           "left" = "left-aligned",
                           "center" = "centered",
                           "right" = "right-aligned",
                           "centered")

  vertical_class <- switch(verticalPosition,
                           "top" = "top-positioned",
                           "inline" = "inline-positioned",
                           "top-positioned")

  line_style_class <- switch(lineStyle,
                             "solid" = "solid-line",
                             "dashed" = "dashed-line",
                             "dotted" = "dotted-line",
                             "solid-line")

  # CSS embedded within the function
  css <- "
  .header-line {
      position: relative;
      width: 100%;
      margin-bottom: 20px;
      height: 1px;
  }

  .header-line span {
      background-color: white;
      padding: 0 10px;
  }

  .solid-line {
      border-bottom: 1px solid;
  }

  .dashed-line {
      border-bottom: 1px dashed;
  }

  .dotted-line {
      border-bottom: 1px dotted;
  }

  .centered span {
      position: absolute;
      left: 50%;
      transform: translateX(-50%);
  }

  .left-aligned span {
      position: absolute;
      left: 0;
      transform: translateX(0%);
  }

  .right-aligned span {
      position: absolute;
      right: 0;
      transform: translateX(0%);
  }

  .top-positioned span {
      bottom: 0;
      transform: translateY(0%);
  }

  .inline-positioned span {
      bottom: 50%;
      transform: translateY(50%);
  }

  .centered.inline-positioned span {
      transform: translateY(50%) translateX(-50%);
  }
  "

  shiny::tagList(
    shiny::tags$style(css),
    tags$div(
      class = paste("header-line",
                    position_class,
                    vertical_class,
                    line_style_class),
      style = paste("border-color:", color, ";",
                    "font-size:", fontSize, ";",
                    "padding-top:", paddingAbove, ";",
                    "padding-bottom:", paddingBelow, ";"),
      tags$span(textToShow, style = paste("color:", textColor, ";"))
    )
  )
}


