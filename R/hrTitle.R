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
#' @param marginTop Margin applied above widget for spacing. Default 20px.
#' @param margineBottom Margin applied below widget for spacing. Default 20px.
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
#'   hrTitle("My Title" , lineStyle = "dashed", verticalPosition = "inline",
#'           fontSize = "20px"),
#'
#'   hrTitle("My Title" ,
#'           lineStyle = "dashed",
#'           verticalPosition = "inline"),
#'   br(),
#'   hr(),
#'   fluidRow(
#'     column(
#'       width = 4,
#'       tableLayout(
#'         textInput(
#'           inputId = "tab8_TI_hrtitle_message",
#'           label = "Message",
#'           value = "Type Message Here"
#'         ),
#'         colourpicker::colourInput(
#'           inputId = "tab8_color_input_line",
#'           label = "Line Color",
#'           value = "grey",
#'           allowTransparent = TRUE
#'         ),
#'         colourpicker::colourInput(
#'           inputId = "tab8_color_input_text",
#'           label = "Text Color",
#'           value = "Blue",
#'           allowTransparent = TRUE
#'         ),
#'         selectInput(
#'           inputId = "tab8_SI_position_horizontal",
#'           label = "Horizontal Position",
#'           choices = c("left", "center", "right")
#'         ),
#'         selectInput(
#'           inputId = "tab8_SI_position_vertical",
#'           label = "Vertical Position",
#'           choices = c("top", "inline")
#'         ),
#'         numericInput(
#'           inputId = "tab8_NI_font_size",
#'           label = "Font Size",
#'           value = 14,
#'           min = 4,
#'           max = 30
#'         ),
#'         selectInput(
#'           inputId = "tab8_SI_line_style",
#'           label = "Line Style",
#'           choices = c("solid", "dashed", "dotted")
#'         ),
#'         numericInput(
#'           inputId = "tab8_NI_margin_top",
#'           label = "Margin Top",
#'           value = 20
#'         ),
#'         numericInput(
#'           inputId = "tab8_NI_margin_bottom",
#'           label = "Margin Bottom",
#'           value = 20
#'         ),
#'         checkboxInput(
#'           inputId = "tab8_CB_obj_above_or_below",
#'           label = "Object Above and below",
#'           value = FALSE
#'         )
#'       )
#'     ),
#'     column(
#'       width = 8,
#'       br(),
#'       br(),
#'       br(),
#'       conditionalPanel(
#'         condition = "input.tab8_CB_obj_above_or_below",
#'         textInput(
#'           inputId = "tab8_TI_obj_above",
#'           label = "Test Above",
#'           value = ""
#'         )
#'      ),
#'      uiOutput(outputId = "uiOut_render_hrtitle"),
#'      conditionalPanel(
#'         condition = "input.tab8_CB_obj_above_or_below",
#'         textInput(
#'           inputId = "tab8_TI_obj_below",
#'           label = "Test Below",
#'           value = ""
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'
#'   output$uiOut_render_hrtitle <- renderUI({
#'
#'     hrTitle(
#'       textToShow = input$tab8_TI_hrtitle_message,
#'       position = input$tab8_SI_position_horizontal,
#'       color = input$tab8_color_input_line,
#'       textColor = input$tab8_color_input_text,
#'       verticalPosition = input$tab8_SI_position_vertical,
#'       fontSize = paste0(as.character(input$tab8_NI_font_size), "px"),
#'       lineStyle = input$tab8_SI_line_style,
#'       marginTop = paste0(as.character(input$tab8_NI_margin_top), "px"),
#'       marginBottom = paste0(as.character(input$tab8_NI_margin_bottom), "px"),
#'     )
#'   })
#' }
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
                    marginTop = "20px",
                    marginBottom = "20px") {

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
      transform: translateX(-50%) !important;
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
                    "margin-top:", marginTop, ";",
                    "margin-bottom:", marginBottom, ";"),
      tags$span(textToShow, style = paste("color:", textColor, ";"))
    )
  )
}


