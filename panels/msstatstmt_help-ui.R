msstatstmt_help = fluidPage(
  titlePanel("MSstatsTMT Vignette"),
  mainPanel(fluidRow(                    
    tags$iframe(seamless="seamless",
                src="https://www.bioconductor.org/packages/release/bioc/vignettes/MSstatsTMT/inst/doc/MSstatsTMT.html", 
                height=900, width=1200)
  )
  ))