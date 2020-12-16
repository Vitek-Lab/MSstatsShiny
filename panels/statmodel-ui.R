
statmodel = fluidPage(
  tags$head(
    tags$style(HTML('#submit1{background-color:orange}')),
    tags$style(HTML('#clear1{background-color:orange}')),
    tags$style(HTML('#submit2{background-color:orange}')),
    tags$style(HTML('#clear2{background-color:orange}')),
    tags$style(HTML('#calculate{background-color:orange}')),
    tags$style(HTML('#plotresults{background-color:orange}')),
    tags$style(HTML('#viewresults{background-color:orange}')),
    tags$style(HTML('#submit{background-color:orange}')),
    tags$style(HTML('#clear{background-color:orange}'))
  ),
  headerPanel("Statistical model"),
  p("In this tab a statistical model is built in three steps:"),
  p("(i) Create a contrast matrix with the correct Group comparisons,"), 
  p("(ii) generate the model and "),
  p("(iii) view result plots."),
  p("More info ", a("here", href="https://www.rdocumentation.org/packages/MSstats/versions/3.4.0/topics/groupComparisonPlots")),
    
# statistical model
    
    sidebarPanel(
               fluidRow(
                        radioButtons("def_comp", label=h4("1. Define comparisons - contrast matrix", tipify(icon("question-circle"), title="Choose pairwise comparisons to find significantly expressed proteins")), c("All possible pairwise comparisons" = "all_pair", "Compare all against one" = "all_one", "Create custom comparisons" = "custom"), selected = character(0)),
                        tags$br(),
                        conditionalPanel(condition = "input.def_comp == 'custom'",
                                         uiOutput('choice1'),
                                         h6("vs"),
                                         uiOutput("choice2"),
                                         actionButton("submit", "Add"),
                                         actionButton("clear", "Clear matrix")
                                         ),
                        conditionalPanel(condition = "input.def_comp == 'all_one'",
                                         h5("Compare all groups against:"),
                                         uiOutput("choice3"),
                                         actionButton("submit1", "Submit"),
                                         actionButton("clear1", "Clear matrix")
                                         ),
                        conditionalPanel(condition = "input.def_comp == 'all_pair'",
                                         actionButton("submit2", "Submit"),
                                         actionButton("clear2", "Clear matrix")
                                         ),
                        sliderInput("signif", 
                                    label = h5("Significance level", tipify(icon("question-circle"), title="Probability of rejecting the null hypothesis given that it is true (probability of type I error)")) , 0, 1, 0.05),
                        tags$hr(),
                        h4("2. Group comparison"),
                        actionButton("calculate", "Start"),
                        tags$hr(),
                        ),
# table of significant proteins 
             tags$br(),

               fluidRow(
                 column(12,
                          fluidRow(
                                   selectInput("typeplot", 
                                               label = h4("3. Visualization - select plot type"), c("Volcano Plot" = "VolcanoPlot", "Heatmap"="Heatmap", "Comparison Plot"="ComparisonPlot")),
                                   conditionalPanel(condition = "input.typeplot == 'VolcanoPlot'",
                                                    uiOutput("WhichComp")),
                                   conditionalPanel(condition = "input.typeplot == 'VolcanoPlot' && input.DDA_DIA!=='TMT'",
                                                    checkboxInput("pname", 
                                                                  label = p("display protein name"))),
                                   conditionalPanel(condition = "input.typeplot == 'VolcanoPlot' || input.typeplot == 'Heatmap'",
                                                    selectInput("logp", 
                                                                label = h5("Log transformation of adjusted p-value"),
                                                                c("base 2" = "2", "base 10" = "10"), selected = "10")
                                                    ),
                                   sliderInput("sig", 
                                               label = h5("Significance level", tipify(icon("question-circle"), title="Probability of rejecting the null hypothesis given that it is true (probability of type I error)")) , 0, 1, 0.05),
                                    conditionalPanel(condition = "input.typeplot == 'ComparisonPlot'",
                                                     uiOutput("WhichProt")),
                                   conditionalPanel(condition = "input.typeplot == 'VolcanoPlot' || input.typeplot == 'Heatmap'",
                                                    checkboxInput("FC1", 
                                                                  label = p("Apply specific fold change cutoff for significance")),
                                                    conditionalPanel(condition = "input.FC1 == true",
                                                                     numericInput("FC", "cutoff", 1, 0, 100, 0.1))),
                                   tags$br(),
                                   conditionalPanel(condition = "input.typeplot == 'Heatmap'",
                                                    numericInput("nump", "Number of proteins in heatmap", 100, 1, 180, 1),
                                                    selectInput("cluster",
                                                                label = h5("Cluster analysis", tipify(icon("question-circle"), 
                                                                                                      title= "How to order proteins and comparisons: compute protein dendrogram and reorder based on protein means; compute comparison dendrogram and reorder based on comparison means; or both", 
                                                                                                      placement = "top")), 
                                                                c("protein dendogram" = "protein", "comparison dendogram" = "comparison", "protein and comparison dendograms" = "both"))),
                                   conditionalPanel(condition = "input.typeplot == 'ComparisonPlot'",
                                                    uiOutput("WhichComp1"))
                                   ),
                        actionButton("viewresults", "View plot in browser (only for one comparison/protein)"),
                        actionButton("plotresults", "Save plot results as pdf")
                        
                 )
               )
    ),

fluidRow(
  column(7,
         uiOutput("matrix"),
         conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                          tags$br(),
                          tags$h4("Calculation in progress (it may take a while)...")),
         uiOutput("table_results"),
         tags$br(),
         tags$br(),
         conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                          tags$br(),
                          tags$br(),
                          tags$h4("Calculation in progress...")),
         tags$br(),
         uiOutput("comparison_plots")
  ))
    

)

