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

  # Render show/hide code button and code text
  output$code_section <- shiny::renderUI({
    showCodeButtonUI(id)
  })

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
    shinyjs::updateActionButton(session, "show_code_button", label = label)
  })
}



# ui <- fluidPage(
#   useShinyjs(),  # Initialize shinyjs
#
#   sidebarPanel(
#     # Input for background color
#     colourpicker::colourInput(
#       "bg_color", "Background Color", value = "transparent", allowTransparent = TRUE
#     ),
#
#     # Input for header font
#     textInput("header_font", "Header Font", value = "Arial"),
#
#     # Input for header weight
#     selectInput("header_weight", "Header Weight",
#                 choices = c("normal", "bold"), selected = "normal"),
#
#     # Input for header font size
#     textInput("header_font_size", "Header Font Size", value = "18px"),
#
#     # Input for header color
#     colourpicker::colourInput(
#       "header_color", "Header Color", value = "grey", allowTransparent = TRUE
#     ),
#
#     # Input for border thickness
#     textInput("border_thickness", "Border Thickness", value = "1px"),
#
#     # Input for border color
#     colourpicker::colourInput(
#       "border_color", "Border Color", value = "lightgrey", allowTransparent = TRUE
#     ),
#
#     # Input for border type
#     selectInput("border_type", "Border Type",
#                 choices = c("solid", "dashed", "dotted"), selected = "solid"),
#
#     # Input for margin top
#     textInput("margin_top", "Margin Top", value = "20px")
#   ),
#
#   mainPanel(
#     # Render the titlePanel based on user inputs
#     uiOutput("custom_title_panel"),
#
#     # Call the showCodeButton module
#     showCodeButtonUI("code_button_module")
#   )
# )

# Define server
# server <- function(input, output, session) {
#   # Render the titlePanel based on user inputs
#   output$custom_title_panel <- renderUI({
#     titlePanel(
#       sliderInput("slider1", "Slider Input:", min = 0, max = 100, value = 50),
#       textInput("text1", "Text Input:"),
#       title = "Custom Title",
#       bg_color = input$bg_color,
#       header_font = input$header_font,
#       header_weight = input$header_weight,
#       header_font_size = input$header_font_size,
#       header_color = input$header_color,
#       border_thickness = input$border_thickness,
#       border_color = input$border_color,
#       border_type = input$border_type,
#       margin_top = input$margin_top
#     )
#   })
#
#   # Generate the code based on user inputs
#   generated_code <- reactive({
#     paste0(
#       "titlePanel(\n",
#       "  sliderInput(\"slider1\", \"Slider Input:\", min = 0, max = 100, value = 50),\n",
#       "  textInput(\"text1\", \"Text Input:\"),\n",
#       "  title = \"Custom Title\",\n",
#       "  bg_color = \"", input$bg_color, "\",\n",
#       "  header_font = \"", input$header_font, "\",\n",
#       "  header_weight = \"", input$header_weight, "\",\n",
#       "  header_font_size = \"", input$header_font_size, "\",\n",
#       "  header_color = \"", input$header_color, "\",\n",
#       "  border_thickness = \"", input$border_thickness, "\",\n",
#       "  border_color = \"", input$border_color, "\",\n",
#       "  border_type = \"", input$border_type, "\",\n",
#       "  margin_top = \"", input$margin_top, "\"\n",
#       ")"
#     )
#   })
#
#   # Call the showCodeButton module
#   callModule(showCodeButtonServer, "code_button_module", generated_code)
#   # showCodeButtonServer("code_button_module", generated_code())
# }
#
# # Run the app
# shinyApp(ui = ui, server = server)
