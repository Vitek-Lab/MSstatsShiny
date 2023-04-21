#' Home UI module for home page.
#'
#' This function generates the home user interface for MSstatsShiny, a web tool 
#' for the statistical analysis of quantitative proteomic data built around the
#'  R packages MSstats, MSstatsTMT, and MSstatsPTM.
#'
#' @param id namespace prefix for the module
#' 
#' @return This function returns nothing, as it sets up the Home UI
#'
homeUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$head(
        tags$style(HTML('#home-StartPipeline{background-color:orange}
                    #home-Reset{background-color:orange}
                    #statmodel-Design{background-color:orange}
                    #home-Help{background-color:orange}'))
      ),
      headerPanel(div(img(src = "assets/MSstatsLogo.PNG", height = 165, width = 132), "Welcome to MSstatsShiny")),
      tags$br(),
      mainPanel(
        div(tagList(
          h2("About MSstatsShiny"),
          p("This is a web tool for the statistical analysis of quantitative \
        proteomic data. It is built around the R packages ", 
            a(paste0("MSstats v", packageVersion("MSstats")), 
              href="https://www.bioconductor.org/packages/release/bioc/html/MSstats.html"), 
            ", ", 
            a(paste0("MSstatsTMT v", packageVersion("MSstatsTMT")),
              href="https://www.bioconductor.org/packages/release/bioc/html/MSstatsTMT.html"),
            ", and ", 
            a(paste0("MSstatsPTM v", packageVersion("MSstatsPTM")), 
              href="https://www.bioconductor.org/packages/release/bioc/html/MSstatsPTM.html"), ),
          p("This tool is designed to increase the usability of the packages, \
      providing an all in one, end to end, analysis pipeline for proteomic 
        data."),
          br(),
          h2("Please select from the following options to get started"),
          h4("1.", actionButton(inputId = ns("StartPipeline"), label = "Run MSstats Pipeline")),
          h4("2.", actionButton(inputId = ns("Reset"), label = "Reset Pipeline")),
          h4("3.", actionButton(inputId = ns("Help"), label = "Help!", onclick ="window.open('https://groups.google.com/g/msstats', '_blank')")),
          br(),
          h2("Notes"),
          p("- All code and documentation is available on ", a("github", 
                                                               href="https://github.com/Vitek-Lab/MSstatsShiny")),
          p("- Sample Size and Power calculations are currently not available for TMT experiments."),
          p("- Please note that some calculations may take some time to compute.")
        ),
        
        )
      )
    )
    
  )
}