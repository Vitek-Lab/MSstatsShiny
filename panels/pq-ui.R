main = mainPanel(
  tabsetPanel(
    tabPanel("Protein quantification",
             fluidRow(
               column(5,
                      wellPanel(
                        fluidRow(
                          h4("Download summary of protein abundance", tipify(icon("question-circle"), title="Model-based quantification for each condition or for each biological samples per protein. ")),
                          radioButtons("typequant", 
                                       label = h4("Type of summarisation"), 
                                       c("Sample-level summarisation" = "Sample", "Group-level summarisation" = "Group")),
                          radioButtons("format", "Save as", c("Wide format" = "matrix", "Long format" = "long")),
                          downloadButton("download_summary", "Download")
                        )
                      )
               ),
               column(7,actionButton(inputId = "proceed4", label = "Next step")),
               column(7,
                      h4("Table of abundance"),
                      dataTableOutput("abundance")
               )
             )
    )
  )
)

pq = fluidPage(
  tags$style(HTML('#proceed4{float:right;}')),
  tags$style(HTML('#proceed4{background-color:orange}')),
  headerPanel("Protein Quantification"),
  p("Quantification of the proteins after preprocessing."),
  p("PLEASE COMPLETE DATA PROCESSING STEP"),
  tags$br(),
  main
)
