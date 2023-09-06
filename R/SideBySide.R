#' Side By Side Shiny Widgets
#'
#' Displays shiny widgets side by side.
#' @importFrom shiny div tagList NS HTML

#' @param ... Widgets to be displayed.
#' @param shrinkWidgets Logical. If TRUE, the widgets will be resized based on the number of widgets.
#'
#' @return Display widgets side by side.
#'
#' @export

SideBySide <- function(..., shrinkWidgets = FALSE) {
  widgets <- list(...)
  n_widgets <- length(widgets)

  # Include CSS as part of the function's output
  css_tags <- shiny::tags$style(shiny::HTML("
    .inline-block {
      display: inline-block;
      vertical-align: top;
      margin-right: -4px;
      box-sizing: border-box;
    }
    .no-padding {
      padding: 0px;
    }
  "))

  styled_widgets <- lapply(widgets, function(widget) {
    if (shrinkWidgets) {
      widget_width <- floor(100 / n_widgets) - 1  # Calculate width only when shrinkWidgets is TRUE
      shiny::div(class = "inline-block", style = paste0("width:", widget_width, "%;"), widget)
    } else {
      shiny::div(class = "inline-block", widget)
    }
  })

  # Combine CSS and widgets into a single tagList for output
  output_list <- c(list(css_tags), styled_widgets)
  do.call(shiny::tagList, output_list)
}
