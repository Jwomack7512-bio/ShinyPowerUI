#' Create a table layout with dual columns for Shiny UI input elements.
#'
#' @param labels A vector of label names for the widgets.
#' @param widgets A list of Shiny UI input elements corresponding to the labels
#'   (first set).
#' @param widgets2 A list of Shiny UI input elements corresponding to the labels
#'   (second set).
#' @param headerLabels A vector of two header labels for the table's first row.
#' @param firstColWidth The width of the first column (for the labels).
#' @param fontSize The font size for the label text.
#' @param paddingBottom The padding at the bottom of the label text.
#' @param isBold Logical; If TRUE, the label text is displayed in bold.
#' @param headerFontSize Font size for the table headers.
#' @param headerIsBold Logical; If TRUE, the header text is displayed in bold.
#' @param headerAlignment Alignment of the header text. Options: left, center, right.
#' @param removeFirstCol Logical; If TRUE, the first column (labels) will be
#'   removed.
#' @param colPercents A numeric vector indicating the percentage width of each
#'   column.
#' @param colSpacing Spacing between columns.
#'
#' @return A Shiny table layout with dual columns.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#'
#' ui <- fluidPage(
#'   tableLayoutDualColumns(
#'     labels = c("Label 1", "Label 2", "Label 3"),
#'     widgets = list(
#'       textInput("input1", "Input 1", ""),
#'       sliderInput("input2", "Input 2", 1, 10, 5),
#'       selectInput("input3", "Input 3", choices = c("A", "B", "C"),
#'       selected = "A")
#'     ),
#'     widgets2 = list(
#'       textInput("input4", "Input 4", ""),
#'       sliderInput("input5", "Input 5", 1, 10, 5),
#'       selectInput("input6", "Input 6", choices = c("X", "Y", "Z"),
#'       selected = "X")
#'     ),
#'     headerLabels = c("Inputs Set 1", "Inputs Set 2"),
#'     firstColWidth = "20%",
#'     fontSize = "14px",
#'     paddingBottom = "5px",
#'     isBold = TRUE,
#'     headerFontSize = "16px",
#'     headerIsBold = TRUE,
#'     headerAlignment = "center",
#'     removeFirstCol = FALSE,
#'     colPercents = c(50, 50),
#'     colSpacing = "10px"
#'   )
#' )
#' shinyApp(ui, function(input, output) {})
#' }
#'
#' @export
tableLayoutDualColumns <- function(
    labels,
    widgets,
    widgets2,
    headerLabels = c("Header 1", "Header 2"),
    firstColWidth = "30%",
    fontSize = "16px",
    paddingBottom = "15px",
    isBold = TRUE,
    headerFontSize = "16px",
    headerIsBold = TRUE,
    headerAlignment = "center",
    removeFirstCol = FALSE,
    colPercents = c(50, 50),
    colSpacing = "10px"
) {

  # If removeFirstCol is TRUE, set firstColWidth to "0%" and labels to NULL
  if(removeFirstCol) {
    firstColWidth <- "0%"
    labels <- NULL
  }

  # Set the width for the two columns based on colPercents
  widgetColWidth1 <- paste0(colPercents[1], "%")
  widgetColWidth2 <- paste0(colPercents[2], "%")

  # Set the font weight for the header based on headerIsBold
  headerFontWeight <- ifelse(headerIsBold, "bold", "normal")

  # Check if the lengths of widgets and widgets2 are consistent
  if(length(widgets) != length(widgets2)) {
    stop("Length of widgets must be consistent!")
  }

  # Set the font weight for labels based on isBold
  labelFontWeight <- ifelse(isBold, "bold", "normal")

  # Create the header row if headerLabels is not NULL
  header <- if (!is.null(headerLabels)) {
    shiny::tags$tr(
      if(!removeFirstCol) shiny::tags$td(""),
      shiny::tags$td(
        style = paste0("width:", widgetColWidth1,
                       "; text-align:", headerAlignment,
                       "; font-size:", headerFontSize,
                       "; font-weight:", headerFontWeight,
                       if (removeFirstCol) paste0("; padding-right:",
                                                  colSpacing) else ""),
        headerLabels[1]
      ),
      shiny::tags$td(
        style = paste0("width:", widgetColWidth2,
                       "; text-align:", headerAlignment,
                       "; font-size:", headerFontSize,
                       "; font-weight:", headerFontWeight, ";"),
        headerLabels[2]
      )
    )
  } else NULL

  # Create rows for each label and widgets pair
  rows <- lapply(1:length(widgets), function(i) {
    shiny::tags$tr(
      width = "100%",
      if(!removeFirstCol) {
        shiny::tags$td(
          width = firstColWidth,
          shiny::div(
            style = paste0("font-size:", fontSize, ";",
                           "padding-bottom:", paddingBottom, ";",
                           "font-weight:", labelFontWeight, ";"),
            labels[i]
          )
        )
      },
      shiny::tags$td(
        style = paste0("width:", widgetColWidth1, "; padding: 0px;",
            if (removeFirstCol) paste0(" padding-right:", colSpacing) else ""),
        if(is.null(widgets[[i]])) "" else widgets[[i]]
      ),
      shiny::tags$td(
        style = paste0("width:", widgetColWidth2, "; padding: 0px;"),
        if(is.null(widgets2[[i]])) "" else widgets2[[i]]
      )
    )
  })

  # Create the final table layout
  shiny::tags$table(
    class = "input_table",
    header,
    do.call(tags$tbody, rows)
  )
}
