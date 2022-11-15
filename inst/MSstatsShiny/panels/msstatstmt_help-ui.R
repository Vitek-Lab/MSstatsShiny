msstatstmt_help = fluidPage(
  titlePanel("MSstatsTMT Vignette"),
  mainPanel(fluidRow(
    fluidRow("Note this help page will not load if running the application locally in RStudio. To see this page either click 'Open in Browser' (above) or use the link below:"),
    tags$a(href="https://www.bioconductor.org/packages/release/bioc/vignettes/MSstatsTMT/inst/doc/MSstatsTMT.html", "MSstatsTMT Help"),
    tags$iframe(seamless="seamless",
                src="https://www.bioconductor.org/packages/release/bioc/vignettes/MSstatsTMT/inst/doc/MSstatsTMT.html", 
                height=900, width=1200)
  )
  ))