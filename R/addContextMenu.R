#' Add a context menu to a UI element
#'
#' @param ui_element_id The ID of the UI element to attach the context menu to.
#' @param menu_items A data frame with menu items (name and id).
#' @param itemPadding Padding for context menu items.
#' @param itemFontSize Font size for context menu items.
#' @param menuWidth Width of the context menu.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     titlePanel("Context Menu Example"),
#'     mainPanel(
#'       actionButton("myButton", "Right-click Me!")
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     menu_items <- data.frame(name = c("Option 1", "Option 2", "Option 3"),
#'                              id = c("opt1", "opt2", "opt3"))
#'
#'     addContextMenu("myButton", menu_items,
#'                    itemPadding = '5px',
#'                    itemFontSize = '14px',
#'                    menuWidth = 'auto')
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
addContextMenu <- function(ui_element_id, menu_items,
                           itemPadding = '5px',
                           itemFontSize = '14px',
                           menuWidth = 'auto') {

  js_menu_items <- sprintf("[%s]",
                           paste(sprintf("{name: '%s', id: '%s'}",
                                         menu_items$name, menu_items$id),
                                 collapse = ","))

  js_code <- sprintf("createContextMenu('%s', %s, '%s', '%s', '%s');",
                     ui_element_id, js_menu_items,
                     itemPadding, itemFontSize, menuWidth)

  shiny::tags$script(shiny::HTML(js_code))
}
