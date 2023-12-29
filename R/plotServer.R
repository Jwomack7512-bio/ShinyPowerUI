#' Custom Editable Plot Module for Shiny
#'
#' This function creates a custom editable plot module for Shiny applications.
#'
#' @param input The Shiny input object.
#' @param output The Shiny output object.
#' @param session The Shiny session object.
#' @param plot_obj A function that returns a ggplot2 plot object.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(ggplot2)
#'
#' ui <- fluidPage(
#'   titlePanel("Bar Chart for mtcars"),
#'   sidebarLayout(
#'     sidebarPanel(
#'       selectInput(
#'         "variable",
#'         "Choose a variable:",
#'         choices = colnames(mtcars),
#'         selected = "mpg")
#'     ),
#'     mainPanel(
#'       plotUI("barChart")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'
#'   barplot_data <- reactive({
#'     ggplot(
#'       mtcars,
#'       aes(x = reorder(.data[[input$variable]],
#'                       .data[[input$variable]]),
#'           y = .data[[input$variable]])) +
#'       geom_bar(stat = "identity", fill = "steelblue") +
#'       labs(title = paste("Bar Chart for", input$variable),
#'            x = input$variable,
#'            y = "Count") +
#'       theme_minimal()
#'   })
#'
#'   callModule(plotServer, "barChart", barplot_data)
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export

plotServer <- function(input,
                       output,
                       session,
                       plot_obj) {

  # Process Plot Object
  labels <- reactiveValues(
    x = list(
      text = "x",
      size = 14,
      labelSize = 14,
      weight = "plain",
      axisPosition = 0.5,
      axisPositionPadding = 1,
      min = 0,
      max = 100,
      breaks = 10,
      axis.override = FALSE,
      text.override = FALSE
    ),
    y = list(
      text = "y",
      size = 14,
      labelSize = 14,
      weight = "plain",
      axisPosition = 0.5,
      axisPositionPadding = 1,
      min = 0,
      max = 100,
      breaks = 10,
      axis.override = FALSE,
      text.override = FALSE
    ),
    title = list(
      text = "",
      size = 14,
      weight = "plain",
      axisPosition = 0.5,
      axisPositionPadding = 1,
      text.override = FALSE
    )
  )

  # Render plot from input object
  output$plot <- renderPlot({
    p <- plot_obj()

    # IF gtable, return
    if (class(p)[1] == "gtable") {
      return(p)
    }

    if (!labels$x$text.override) {
      if (!isTruthy(p$labels$x)) {
        labels$x$text <- "x"
      } else {
        labels$x$text <- p$labels$x
      }
    }

    if (!labels$y$text.override) {
      if (!isTruthy(p$labels$y)) {
        labels$y$text <- "y"
      } else {
        labels$y$text <- p$labels$y
      }
    }

    if (!labels$title$text.override) {
      if (!isTruthy(p$labels$title)) {
        labels$title$text <- ""
      } else {
        labels$title$text <- p$labels$title
      }
    }

    # Title Settings -----------------------------------------------------------
    p <- p + ggtitle(labels$title$text)
    p <- p + theme(plot.title = element_text(
      size = labels$title$size,
      face = labels$title$weight,
      hjust = labels$title$axisPosition,
      vjust = labels$title$axisPositionPadding
    ))

    # x-axis Settings ----------------------------------------------------------
    lab.x <- labels$x
    p <- p + xlab(labels$x$text)
    p <- p + theme(
      axis.title.x = element_text(
        size = labels$x$size,
        face = labels$x$weight,
        hjust = labels$x$axisPosition,
        vjust = labels$x$axisPositionPadding
      ),
      axis.text.x = element_text(size = labels$x$labelSize)
    )

    if (labels$x$axis.override) {
      p <- p +
        scale_x_continuous(
          limits = c(labels$x$min,  labels$x$max),
          breaks = seq(labels$x$min, labels$x$max, by = labels$x$breaks)
        )
    }

    # y-axis settings ----------------------------------------------------------
    p <- p + ylab(labels$y$text)
    p <- p + theme(
      axis.title.y = element_text(
        size = labels$y$size,
        face = labels$y$weight,
        hjust = labels$y$axisPosition,
        vjust = labels$y$axisPositionPadding
      ),
      axis.text.y = element_text(size = labels$y$labelSize)

    )

    if (labels$y$axis.override) {
      p <- p +
        scale_y_continuous(
          limits = c(labels$y$min,  labels$y$max),
          breaks = seq(labels$y$min, labels$y$max, by = labels$y$breaks)
        )
    }
    return(p)
  })

  # Change x label when double click is detected
  observeEvent(input$label_edit, {
    label.to.edit <- switch(
      input$label_edit$zone,
      "x" = "x",
      "y" = "y",
      "title" = "title",
      # add a default case if needed
      "default" = NA
    )
    if (class(plot_obj())[1] == "gtable") {
      return(NULL)
    }
    showModal(
      modalDialog(
        title = "Edit Plot",
        radioButtons(
          inputId = session$ns("RB_edit_render"),
          label = "Edit",
          choices = c("x-axis" = "x",
                      "y-axis" = "y",
                      "Title" = "title"),
          selected = label.to.edit,
          inline = TRUE
        ),
        hr(),
        # x-axis UI ------------------------------------------------------------
        conditionalPanel(
          condition = "input.RB_edit_render == 'x'", ns = session$ns,
          fluidRow(
            column(
              width = 6,
              textInput(
                inputId = session$ns("newLabel_x"),
                label = "X Label Text",
                value = labels$x$text
              ),
              numericInput(
                inputId = session$ns("fontSize_x"),
                label = "Font Size",
                value = labels$x$size,
                min = 8,
                max = 36
              ),
              selectInput(
                inputId = session$ns("fontWeight_x"),
                label = "Font Weight",
                choices = c("plain", "bold", "italic", "bold.italic"),
                selected = labels$x$weight
              ),
              sliderInput(
                inputId = session$ns("axisPosition_x"),
                label = "Position on main axis",
                value = labels$x$axisPosition,
                min = 0,
                max = 1,
                step = 0.05
              ),
              numericInput(
                inputId = session$ns("axisPositionPadding_x"),
                label = "Padding on Axis",
                value = labels$x$axisPositionPadding,
                min = -5,
                max = 5,
                step = 0.5
              )
            ),
            column(
              width = 6,
              numericInput(
                inputId = session$ns("axisLabelSize_x"),
                label = "Axis Label Size",
                value = labels$x$labelSize,
                min = 8,
                max = 36
              ),
              checkboxInput(
                inputId = session$ns("override_axis_x"),
                label = "Override Axis?",
                value = labels$x$axis.override
              ),
              conditionalPanel(
                condition = "input.override_axis_x", ns = session$ns,
                numericInput(
                  inputId = session$ns("axisMin_x"),
                  label = "Axis Min",
                  value = labels$x$min
                ),
                numericInput(
                  inputId = session$ns("axisMax_x"),
                  label = "Axis Max",
                  value = labels$x$max
                ),
                numericInput(
                  inputId = session$ns("axisBreak_x"),
                  label = "Axis Breaks",
                  value = labels$x$breaks
                )
              )
            )
          )
        ),
        # y-axis UI ------------------------------------------------------------
        conditionalPanel(
          condition = "input.RB_edit_render == 'y'", ns = session$ns,
          fluidRow(
            column(
              width = 6,
              textInput(
                inputId = session$ns("newLabel_y"),
                label = "Y Label Text",
                value = labels$y$text
              ),
              numericInput(
                inputId = session$ns("fontSize_y"),
                label = "Font Size",
                value = labels$y$size,
                min = 8,
                max = 36
              ),
              selectInput(
                inputId = session$ns("fontWeight_y"),
                label = "Font Weight",
                choices = c("plain", "bold", "italic", "bold.italic"),
                selected = labels$y$weight
              ),
              sliderInput(
                inputId = session$ns("axisPosition_y"),
                label = "Position on main axis",
                value = labels$y$axisPosition,
                min = 0,
                max = 1,
                step = 0.05
              ),
              numericInput(
                inputId = session$ns("axisPositionPadding_y"),
                label = "Padding on Axis",
                value = labels$y$axisPositionPadding,
                min = -5,
                max = 5,
                step = 0.5
              )
            ),
            column(
              width = 6,
              numericInput(
                inputId = session$ns("axisLabelSize_y"),
                label = "Axis Label Size",
                value = labels$y$labelSize,
                min = 8,
                max = 36
              ),
              checkboxInput(
                inputId = session$ns("override_axis_y"),
                label = "Override Axis?",
                value = labels$y$override
              ),
              conditionalPanel(
                condition = "input.override_axis_y", ns = session$ns,
                numericInput(
                  inputId = session$ns("axisMin_y"),
                  label = "Axis Min",
                  value = labels$y$min
                ),
                numericInput(
                  inputId = session$ns("axisMax_y"),
                  label = "Axis Max",
                  value = labels$y$max
                ),
                numericInput(
                  inputId = session$ns("axisBreak_y"),
                  label = "Axis Breaks",
                  value = labels$y$breaks
                )
              )
            )
          )
        ),
        # title UI -------------------------------------------------------------
        conditionalPanel(
          condition = "input.RB_edit_render == 'title'", ns = session$ns,
          fluidRow(
            column(
              width = 6,
              textInput(
                inputId = session$ns("newLabel_title"),
                label = "Title Text",
                value = labels$title$text
              ),
              numericInput(
                inputId = session$ns("fontSize_title"),
                label = "Font Size",
                value = labels$title$size,
                min = 8,
                max = 36
              ),
              selectInput(
                inputId = session$ns("fontWeight_title"),
                label = "Font Weight",
                choices = c("plain", "bold", "italic", "bold.italic"),
                selected = labels$title$weight
              ),
              sliderInput(
                inputId = session$ns("axisPosition_title"),
                label = "Position on main axis",
                value = labels$title$axisPosition,
                min = 0,
                max = 1,
                step = 0.05
              ),
              numericInput(
                inputId = session$ns("axisPositionPadding_title"),
                label = "Padding on Axis",
                value = labels$title$axisPositionPadding,
                min = -5,
                max = 5,
                step = 0.5
              )
            )
          )
        ),
        footer = actionButton(session$ns("update_label"), "Update"),
        easyClose = TRUE
      ))
  })

  observeEvent(input$update_label, {
    labels$x$text.override <- TRUE
    labels$y$text.override <- TRUE
    labels$title$text.override <- TRUE

    # Store x axis values ------------------------------------------------------
    labels$x$text      <- input$newLabel_x
    labels$x$size      <- input$fontSize_x
    labels$x$weight    <- input$fontWeight_x
    labels$x$labelSize <- input$axisLabelSize_x

    labels$x$axisPosition        <- input$axisPosition_x
    labels$x$axisPositionPadding <- input$axisPositionPadding_x

    labels$x$axis.override  <- input$override_axis_x
    if (labels$x$axis.override) {
      labels$x$min    <- input$axisMin_x
      labels$x$max    <- input$axisMax_x
      labels$x$breaks <- input$axisBreak_x
    }

    # Store y-axis values ------------------------------------------------------
    labels$y$text      <- input$newLabel_y
    labels$y$size      <- input$fontSize_y
    labels$y$weight    <- input$fontWeight_y
    labels$y$labelSize <- input$axisLabelSize_y

    labels$y$axisPosition        <- input$axisPosition_y
    labels$y$axisPositionPadding <- input$axisPositionPadding_y

    labels$y$axis.override  <- input$override_axis_y
    if (labels$y$axis.override) {
      labels$y$min    <- input$axisMin_y
      labels$y$max    <- input$axisMax_y
      labels$y$breaks <- input$axisBreak_y
    }

    # Store title values -------------------------------------------------------
    labels$title$text      <- input$newLabel_title
    labels$title$text      <- input$newLabel_title
    labels$title$size      <- input$fontSize_title
    labels$title$weight    <- input$fontWeight_title

    labels$title$axisPosition        <- input$axisPosition_title
    labels$title$axisPositionPadding <- input$axisPositionPadding_title
    removeModal()
  })
}
