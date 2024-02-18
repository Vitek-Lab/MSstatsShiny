#' Help MSStats UI module for msstats help page.
#'
#' This module shows the msstats help page for general documentation
#'
#' @param id namespace prefix for the module
#' 
#' @return This function returns nothing, as it sets up the MSStats Help UI
#'
#' @export
#' @examples
#' \dontrun{
#' library(shiny)
#' ui <- fluidPage(
#'   msstatsHelpUI("msstats-help")
#' )
#' server <- function(input, output, session) {
#'   # Server logic for msstatsHelpUI
#' }
#' shinyApp(ui, server)
#' }
msstatsHelpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("MSstats Vignette"),
      mainPanel(
        fluidRow("Note this help page will not load if running the application locally in RStudio. To see this page either click 'Open in Browser' (above) or use the link below:"),
        tags$a(href="https://www.bioconductor.org/packages/release/bioc/vignettes/MSstats/inst/doc/MSstats.html", "MSstats Help"),
        fluidRow(                    
          tags$iframe(seamless="seamless",
                      src="https://www.bioconductor.org/packages/release/bioc/vignettes/MSstats/inst/doc/MSstats.html", 
                      height=900, width=1200)
        )
      ))
  )
}
