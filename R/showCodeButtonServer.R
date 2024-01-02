# Module server function
#' Server function to create a show/hide code button
#'
#' This function sets up the server-side logic for a Shiny app to display a
#' button that toggles the visibility of generated code.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param generated_code A function that returns the generated code.
#'
#' @return None
#'
#' @export
showCodeButtonServer <- function(input,
                                 output,
                                 session,
                                 generated_code) {

  # Variable to track the button state
  button_state <- reactiveVal(FALSE)

  # Toggle code visibility on button click
  shiny::observeEvent(input$show_code_button, {
    shinyjs::toggle("code_div")
    button_state(!button_state())
  })

  # Render the generated code text
  output$generated_code_text <- shiny::renderText({
    generated_code()
  })

  # Update button label based on the state
  observe({
    label <- if (button_state()) "Hide Code" else "Show Code"
    shiny::updateActionButton(session, "show_code_button", label = label)
  })
}
