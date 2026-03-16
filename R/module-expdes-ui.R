#' Expdes UI module for future experiments UI.
#'
#' This function sets up the Expdes UI where it consists of several, 
#' options for users to select and generate plots.
#'
#' @param id namespace prefix for the module
#' 
#' @return This function returns nothing, as it sets up the Expdes UI
#'
#' @export
#' @examples
#' NA
#' 
expdesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      useShinyjs(),
      headerPanel("Design future experiments"),
      p("Calculate power or sample size for future experiments with the same experimental design"),
      p("Statistical model must be run in step 3 before power and sample size calculations can be run."),
      sidebarPanel(
        uiOutput(ns(NAMESPACE_EXPDES$sidebar_controls))
      ),
      mainPanel(
        fluidRow(
          column(9,
                 h4("Plot"),
                 plotlyOutput(ns(NAMESPACE_EXPDES$result_plot)),
                 downloadButton(ns(NAMESPACE_EXPDES$download_future), "Download plot")
          )
        )
      )
    )
  )
}