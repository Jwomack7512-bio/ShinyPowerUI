#' Custom Table Layout for Shiny Widgets
#'
#' This function provides a custom table layout for arranging Shiny widgets with their associated labels.
#'
#' @param ... Shiny UI input elements.
#' @param firstColWidth The width of the first column (for the labels). If NULL, uses adaptive CSS.
#' @param fontSize The font size for the label text. Default is "16px".
#' @param paddingBottom The padding at the bottom of the label text. Default is "15px".
#' @param isBold Logical; If TRUE, the label text is displayed in bold. Default is TRUE.
#'
#' @importFrom shiny tags tagList
#' @importFrom utils head
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     tableLayout(
#'       textInput(inputId = "text1", label = "First Input", value = "Example"),
#'       checkboxInput(inputId = "check1", label = "A Checkbox", value = TRUE)
#'     )
#'   )
#'
#'   server <- function(input, output, session) {}
#'
#'   shinyApp(ui, server)
#' }
#'

tableLayout <- function(..., 
                        firstColWidth = NULL, 
                        fontSize = "16px", 
                        paddingBottom = "15px", 
                        isBold = TRUE) {
  
  widgets <- list(...)
  
  labels <- character(length(widgets))
  
  for (i in seq_along(widgets)) {
    current_widget <- widgets[[i]]
    
    # Check for checkboxInput based on the structure
    if (is.list(current_widget$children[[1]]) &&
        is.list(current_widget$children[[1]]$children[[1]]) &&
        !is.null(current_widget$children[[1]]$children[[1]]$children[[2]]) &&
        is.list(current_widget$children[[1]]$children[[1]]$children[[2]])) {
      
      labels[i] <- current_widget$children[[1]]$children[[1]]$children[[2]]$children
      widgets[[i]]$children[[1]]$children[[1]]$children[[2]] <- NULL
      
      # Check for textInput and similar structures
    } else if (is.list(current_widget$children[[1]]) && 
               !is.null(current_widget$children[[1]]$children)) {
      
      labels[i] <- current_widget$children[[1]]$children
      widgets[[i]]$children[[1]] <- NULL
    }
  }
  
  fontWeight <- ifelse(isBold, "bold", "normal")
  
  rows <- lapply(1:length(labels), function(i) {
    tags$tr(
      tags$td(
        div(
          style = paste0("font-size:", fontSize, ";", 
                         "padding-bottom:", paddingBottom, ";", 
                         "font-weight:", fontWeight, ";"),
          labels[i]
        )
      ),
      tags$td(
        widgets[[i]]
      )
    )
  })
  
  # Adaptive CSS
  adaptive_css <- "
  .adaptive-table td:first-child {
    width: auto;
    white-space: nowrap;
    padding-right: 10px;
  }
  
  .adaptive-table td:last-child {
    width: 100%;
  }
  "
  
  # If firstColWidth is set, adjust CSS accordingly
  fixed_width_css <- paste0(
    ".fixed-width-table td:first-child {
      width: ", firstColWidth, ";
    }
    
    .fixed-width-table td:last-child {
      width: ", 100 - as.numeric(substr(firstColWidth, 1, nchar(firstColWidth) - 1)), "%;
    }"
  )
  
  css_to_use <- ifelse(is.null(firstColWidth), adaptive_css, fixed_width_css)
  table_class <- ifelse(is.null(firstColWidth), "adaptive-table", "fixed-width-table")
  
  shiny::tagList(
    shiny::tags$style(css_to_use),
    tags$table(class = paste("table-widgets", table_class), do.call(tagList, rows))
  )
}
