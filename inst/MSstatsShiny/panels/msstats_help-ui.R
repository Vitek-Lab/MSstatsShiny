msstats_help = fluidPage(
  titlePanel("MSstats Vignette"),
  mainPanel(fluidRow(                    
    tags$iframe(seamless="seamless",
                src="https://www.bioconductor.org/packages/release/bioc/vignettes/MSstats/inst/doc/MSstats.html", 
                height=900, width=1200)
  )
))