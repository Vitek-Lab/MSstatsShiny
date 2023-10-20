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
expdesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      useShinyjs(),
      headerPanel("Design future experiments"),
      p("Calculate power or sample size for future experiments with the same experimental design"),
      p("Statistical model must be run in step 3 before power and sample size \
    calculations can be run."),
      p("This section is not currently compatible with TMT experiments."),
      sidebarPanel(
        h4("Choose parameter to estimate"),
        radioButtons(ns("param"), "parameters:", c("Sample size" = "sample", "Power" = "npower")),
        sliderInput(ns("nsample"), "Number of samples", 0,50,4,1),
        sliderInput(ns("power"), "Power", 0,1,0.8,0.1),
        sliderInput(ns("FDR"), "False dicovery rate", 0,1,0.05, 0.01),
        sliderInput(ns("desirFC"), "Desired fold change", 0, 5, c(1.25, 1.75), 0.01)
      ),
      mainPanel(
        fluidRow(
          column(9,
                 h4("Plot"),
                 plotOutput(ns("result_plot"), hover = "plot_hover"),
                 verbatimTextOutput(ns("info")),
                 downloadButton(ns("download_future"), "Download plot")
          )
        )
      )
    )
    
  )
}