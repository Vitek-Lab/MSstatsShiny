

home = fluidPage(
  tags$head(
    tags$style(HTML('#StartPipeline{background-color:orange}
                    #Reset{background-color:orange}
                    #Design{background-color:orange}
                    #Help{background-color:orange}'))
  ),
  headerPanel(div(img(src = "MSstatsLogo.PNG", height = 165, width = 132), "Welcome to MSstats-Shiny")),
  tags$br(),
  mainPanel(
    div(tagList(
      h2("About MSstats-Shiny"),
      p("This is a web tool for the statistical analysis of quantitative \
        proteomic data. It is built around the R packages ", 
        a("MSstats (v 4.2.0).", 
          href="https://www.bioconductor.org/packages/release/bioc/html/MSstats.html"), 
        " and ", 
        a("MSstatsTMT (v 2.2.7).", href="https://www.bioconductor.org/packages/release/bioc/html/MSstatsTMT.html")),
      p("This tool is designed to increase the usability of the packages, \
      providing an all in one, end to end, analysis pipeline for proteomic 
        data."),
      br(),
      h2("Please select from the following options to get started"),
      h4("1.", actionButton(inputId = "StartPipeline", label = "Run MSstats Pipeline")),
      h4("2.", actionButton(inputId = "Reset", label = "Reset Pipeline")),
      h4("3.", actionButton(inputId = "Help", label = "Help!", onclick ="window.open('https://groups.google.com/g/msstats', '_blank')")),
      br(),
      h2("Features"),
      p("- Analyze data from many different acquisition methods, including \
      Label-Free, DDA, DIA, and TMT"),
      p("- Visualize data at every step of the analysis, from raw data, to \
        summarized results, and modeling results"),
      p("- Design future experiments using the sample size and power analysis \
        UI"),
      h2("Notes"),
      p("- Sample Size and Power calucaltions are currently not available for TMT experiments."),
      p("- Please note that some calculations may take some time to compute."),
      p("- The code for this UI can be accessed online via github ", a("here", 
              href="https://github.com/Vitek-Lab/MSstats-shiny")),
#      p("There are "),
#      verbatimTextOutput("count"),
#      p("people currently using Shiny-MSstats")
      ),
     
    )
  )
)
  

