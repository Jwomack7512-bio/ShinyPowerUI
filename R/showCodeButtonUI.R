showCodeButtonUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 12,
      # Button to show/hide code
      shiny::actionButton(ns("show_code_button"),
                          label = shiny::HTML("</> Show Code"),
                          icon = shiny::icon("code")),
      # Display the generated code with toggle visibility
      shiny::div(
        id = ns("code_div"),
        style = "display:none;",
        "TEST",
        shiny::verbatimTextOutput(ns("generated_code_text"))
      )
    )
  )
}
