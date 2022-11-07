msstats_help = fluidPage(
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