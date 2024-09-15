#' Run the R Dev Dashboard Shiny Application
#'
#' This function launches the R Dev Dashboard Shiny application by initializing the user interface and server logic
#' defined in \code{app_ui} and \code{app_server}. It starts the application in your default web browser.
#'
#' @return A Shiny app object that represents the running application.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   run_rdevdash_app()
#' }
#' }
#' @export
run_rdevdash_app <- function() {
  shiny::shinyApp(app_ui, app_server)
}
