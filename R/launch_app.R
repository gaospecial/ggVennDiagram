#' Launch Reactor Data Shiny App
#'
#' @return a shiny app
#' @export
launch_app = function(){
  check_package("shiny")
  app = system.file(
    "shiny",
    "shinyApp.R",
    package = "ggVennDiagram",
    mustWork = TRUE
  )
  if(interactive()) shiny::runApp(app)
}
