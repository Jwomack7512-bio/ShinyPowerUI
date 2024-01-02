#' UI function to create a show/hide code button
#'
#' This function generates the UI components for displaying a button that
#' toggles the visibility of generated code.
#'
#' @param id A unique identifier for the UI elements.
#'
#' @param hyperlink_color Color of the hyperlink text (default: "blue").
#'
#' @param background_color Background color of the button (default: "transparent").
#'
#' @param border_style CSS style for the button border (default: "none").
#'
#' @return A Shiny UI element.
#'
#' @export
showCodeButtonUI <- function(id,
                             hyperlink_color = "blue",
                             background_color = "transparent",
                             border_style = "none") {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shinyjs::useShinyjs(),  # Import the shinyjs library
      # Button to show/hide code
      shiny::actionButton(ns("show_code_button"),
                          label = shiny::HTML(paste0("</> Show Code")),
                          icon = shiny::icon("code"),
                          style = sprintf("color: %s; background-color: %s; border: %s;", hyperlink_color, background_color, border_style)),
      # Display the generated code with toggle visibility
      shiny::div(
        id = ns("code_div"),
        style = "display:none;",
        shiny::verbatimTextOutput(ns("generated_code_text"))
      )
    )
  )
}
