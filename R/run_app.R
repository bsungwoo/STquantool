#' Function to run STquantools app
#' @description ST analysis tool to visualize and quantify multiple datasets
#' @return Starts shiny application for STquantools
#' @examples
#' STquantool::run_app()
#' @export
run_app <- function(){
  shiny::shinyAppDir(system.file("shiny", package = "STquantool"))
}

# Run shiny app
run_app()
