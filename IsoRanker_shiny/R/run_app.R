#' Run the Shiny Application
#'
#' @export
#' @import golem
#' @import shinyhelper
#' @import shinyjs
run_app <- function() {
  app_ui <- app_ui  # defined in R/app_ui.R
  app_server <- app_server  # defined in R/app_server.R
  install_missing_packages()  # Ensure dependencies are installed
  shinyApp(ui = app_ui, server = app_server)
}
