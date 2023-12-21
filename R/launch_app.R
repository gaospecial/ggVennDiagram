#' Launch Reactor Data Shiny App
#'
#' @return a shiny app
#' @export
launch_app = function(){
  app = system.file(
    "shiny",
    "shinyApp.R",
    package = "ggVennDiagram",
    mustWork = TRUE
  )
  shiny::runApp(app)
}
