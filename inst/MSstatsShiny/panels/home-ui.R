home = fluidPage(
  tags$head(
    tags$style(HTML('#StartPipeline{background-color:orange}
                    #Reset{background-color:orange}
                    #Design{background-color:orange}
                    #Help{background-color:orange}'))
  ),
  headerPanel(div(img(src = "MSstatsLogo.PNG", height = 165, width = 132), "Welcome to MSstatsShiny")),
  tags$br(),
  mainPanel(
    div(tagList(
      h2("About MSstatsShiny"),
      p("This is a web tool for the statistical analysis of quantitative \
        proteomic data. It is built around the R packages ", 
        a("MSstats (v 4.4.1).", 
          href="https://www.bioconductor.org/packages/release/bioc/html/MSstats.html"), 
        ", ", 
        a("MSstatsTMT (v 2.4.1).",
          href="https://www.bioconductor.org/packages/release/bioc/html/MSstatsTMT.html"),
        ", and ", 
        a("MSstatsPTM (v 1.6.0).", 
          href="https://www.bioconductor.org/packages/release/bioc/html/MSstatsPTM.html"), ),
      p("This tool is designed to increase the usability of the packages, \
      providing an all in one, end to end, analysis pipeline for proteomic 
        data."),
      br(),
      h2("Please select from the following options to get started"),
      h4("1.", actionButton(inputId = "StartPipeline", label = "Run MSstats Pipeline")),
      h4("2.", actionButton(inputId = "Reset", label = "Reset Pipeline")),
      h4("3.", actionButton(inputId = "Help", label = "Help!", onclick ="window.open('https://groups.google.com/g/msstats', '_blank')")),
      br(),
      h2("Notes"),
      p("- All code and documentation is available on ", a("github", 
                              href="https://github.com/Vitek-Lab/MSstatsShiny")),
      p("- Sample Size and Power calucaltions are currently not available for TMT experiments."),
      p("- Please note that some calculations may take some time to compute.")
      ),
     
    )
  )
)
