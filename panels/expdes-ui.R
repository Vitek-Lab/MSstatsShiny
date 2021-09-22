expdes = fluidPage(
  shinyjs::useShinyjs(),
  headerPanel("Design future experiments"),
  p("Calculate power or sample size for future experiments with the same experimental design"),
  p("PLEASE GNERATE THE STATISTICAL MODEL TO COMPLETE THIS STEP"),
  sidebarPanel(
    h4("Choose parameter to estimate"),
    radioButtons("param", "parameters:", c("Sample size" = "sample", "Power" = "npower")),
    sliderInput("nsample", "Number of samples", 0,100,4,1),
    sliderInput("power", "Power", 0,1,0.8,0.1),
    sliderInput("FDR", "False dicovery rate", 0,1,0.05, 0.01),
    sliderInput("desirFC", "Desired fold change", 0, 5, c(1.25, 1.75), 0.01)
  ),
  mainPanel(
    h4("Plot"),
    plotOutput("result_plot", hover = "plot_hover"),
    verbatimTextOutput("info"),
    downloadButton("download_future", "Download plot")
  )
)
    
         


