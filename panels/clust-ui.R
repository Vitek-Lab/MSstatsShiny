main = mainPanel(
  tabsetPanel(
    tabPanel("Abundance",
             fluidRow(
               column(5,
                      wellPanel(
                        fluidRow(
                          h4("Download summary of protein abundance", tipify(icon("question-circle"), title="Model-based quantification for each condition or for each biological samples per protein. ")),
                          radioButtons("typequant", 
                                       label = h4("Type of summarisation"), 
                                       c("Sample-level summarisation" = "Sample", "Group-level summarisation" = "Group")),
                          radioButtons("format", "Save as", c("matrix" = "matrix", "array" = "long")),
                          downloadButton("download_summary", "Download")
                        )
                      )
               ),
               column(7,
                      h4("Table of abundance"),
                      dataTableOutput("abundance"),
                      h3("Tab under construction")
               )
             )
    )
  )
)

clust = fluidPage(
  headerPanel("Clustering and Classification"),
  p("Quantification and analysis"),
  tags$br(),
  main
)
