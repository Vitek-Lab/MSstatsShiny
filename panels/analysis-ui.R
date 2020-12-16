main = mainPanel(
  tabsetPanel(
    tabPanel("Annotation",
             fluidRow(
               column(5,
                      wellPanel(
                        fluidRow(
                          h4("Functional annotation"),
                          tags$br(),
                          h4("Select species", tipify(icon("question-circle"), title = "Select species to access database on Ensembl")),
                          uiOutput("Species"),
                          tags$br(),
                          h4("Select input type", tipify(icon("question-circle"), title = "Select id type for proteins in dataset")),
                          uiOutput("Filter"),
                          tags$br(),
                          h4("Select attributes to retreive", tipify(icon("question-circle"),title = "Select query output, multiple selections are allowed")),
                          h5("es. go_id, goslim_goa_description etc"),
                          uiOutput("Attributes"),
                          downloadButton("table_annot", "Download table of annotations")
                        )
                      )
               ),
               column(7,
                      h4("Table of annotation"),
                      tableOutput("annotation")
               )
             )
    ),
    tabPanel("Interaction Network",
             fluidRow(
               column(5,
                      wellPanel(
                        fluidRow(
                          h4("Protein Interaction Analysis"),
                          tags$br(),
                          h4("Select Species"),
                          uiOutput("Species1"),
                          tags$br(),
                          sliderInput("score_t", 
                                      label = h5("Score threshold", 
                                                 tipify(icon("question-circle"),
                                                        title = "threshold for the combined scores of the interactions",
                                                        placement = "top")),
                                      min = 0, max = 1000, value = 400),
                          actionButton("interact", "Plot interactions")
                        )
                      )),
               column(7,
                      h4("Network of interactions"),
                      uiOutput("Hits"),
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$br(),
                                       tags$h4("Calculation in progress (it may take a while)...")),
                      tags$br(),
                      plotOutput("network"),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      textOutput("link"),
                      uiOutput("string_link")
               )
             )
             )
  )
)
               


##############################################################

analysis = fluidPage(
  headerPanel("Functional analysis"),
  p("Download summary of protein abundance or perform queries to ", a("Ensembl", href="http://www.ensembl.org/index.html"), "."),
  tags$br(),
  main
)