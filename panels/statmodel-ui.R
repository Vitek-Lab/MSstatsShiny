
statmodel = fluidPage(
  tags$head(
    tags$style(HTML('#submit3{background-color:orange}')),
    tags$style(HTML('#clear3{background-color:orange}')),
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
  use_busy_spinner(spin = "fading-circle"),
  headerPanel("Statistical modeling and inference"),
  p("In this tab a statistical model is built in three steps:"),
  p("(i) Create a contrast matrix with the correct Group comparisons,"), 
  p("(ii) generate the model and "),
  p("(iii) view result plots."),
  p("More info ", a("here", href="https://www.rdocumentation.org/packages/MSstats/versions/3.4.0/topics/groupComparisonPlots")),
  
  # statistical model
  
  sidebarPanel(
    fluidRow(
      radioButtons("def_comp", label=h4("1. Define comparisons\
                                                          - contrast matrix", 
                                        tipify(icon("question-circle"), 
                                               title="Define what conditions you want to compare here.")), 
                   c("All possible pairwise comparisons" = "all_pair", 
                     "Compare all against one" = "all_one", 
                     "Create custom pairwise comparisons" = "custom",
                     "Create custom non-pairwise comparisons" = "custom_np"), 
                   selected = character(0)),
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
      conditionalPanel(condition = "input.def_comp == 'custom_np'",
                       h5("Non-pairwise Comparison:"),
                       uiOutput('comp_name'),
                       uiOutput('weights'),
                       actionButton("submit3", "Add"),
                       actionButton("clear3", "Clear matrix")
      ),
      tags$hr(),
      h4("2. Group comparison"),
      p("Please add a comparison matrix before modeling."),
      disabled(actionButton("calculate", "Start")),
      tags$hr(),
      conditionalPanel(condition = "input.DDA_DIA == 'TMT'",
                       radioButtons("moderated", 
                                    label= h4("Empirical Bayes moderation", 
                                              tipify(icon("question-circle"), 
          title = "TRUE will moderate t statistic; FALSE (default) uses ordinary t statistic.")), 
                                    c(True = TRUE, False = FALSE))),
      sliderInput("signif",
                  label = h5("Significance level", tipify(icon("question-circle"),
                        title="The alpha used to determine significant results. IE the probability of type I error)")), 
                        0, 1, 0.05),
    ),
    # table of significant proteins 
    tags$br(),
    fluidRow(
      column(12,
             fluidRow(
               selectInput("typeplot", 
                           label = h4("3. Visualization - \
                                                select plot type"), 
                           c("Volcano Plot" = "VolcanoPlot", 
                             "Heatmap"="Heatmap", 
                             "Comparison Plot"="ComparisonPlot"))),
             conditionalPanel(condition = "input.typeplot == 'VolcanoPlot'",
                              uiOutput("WhichComp"),
                              conditionalPanel(
                                condition = "input.DDA_DIA!=='TMT'",
                                checkboxInput("pname", label = p("display protein name"))),
                                selectInput("logp", 
                                            label = h5("Log transformation of adjusted p-value"),
                                            c("base 2" = "2", "base 10" = "10"), selected = "10"),
                                sliderInput("sig", 
                                          label = h5("Adjusted p-value cutoff",
                                                     tipify(icon("question-circle"),
                                                            title="The cutoff used to determine significant results.)")),
                                          0, 1, 0.05),
                                checkboxInput("FC1", 
                                            label = p("Apply specific fold change cutoff for significance")),
                                conditionalPanel(
                                  condition = "input.FC1 == true",
                                  numericInput("FC", "Fold change cutoff", 
                                             1, 0, 100, 
                                             0.1)),
                                tags$br()),
                              conditionalPanel(
                                condition = "input.typeplot == 'ComparisonPlot'",
                                uiOutput("WhichProt"),
                                uiOutput("WhichComp1")),
             
                                
                              conditionalPanel(
                                condition = "input.typeplot == 'Heatmap'",
                                h4("Note: Only one page will be shown in \
                                   browser. To view all proteins please \
                                   view this plot as a pdf."),
                                selectInput("logp", 
                                            label = h5("Log transformation of adjusted p-value"),
                                            c("base 2" = "2", "base 10" = "10"), selected = "10"),
                                checkboxInput("FC1", 
                                              label = p("Apply specific fold change cutoff for significance")),
                                conditionalPanel(
                                  condition = "input.FC1 == true",
                                  numericInput("FC", "cutoff", 
                                               1, 0, 100, 
                                               0.1)),

                                numericInput("nump", "Number of proteins \
                                        per page", 100, 1, 180, 1),
                                selectInput("cluster", 
                                            label = h5("Cluster analysis", 
                                                       tipify(
                                                         icon("question-circle"), 
                                                         title= "Determines how to order proteins and comparisons. protein means, comparison means, or both", 
                                                         placement = "top")), 
                                            c("protein dendogram" = "protein", 
                                              "comparison dendogram" = "comparison", 
                                              "protein and comparison dendograms" = "both"))),

             p("Please note if you want to plot more than one \
                          Volcano Plot comparison, you must save the results \
                          as a pdf."),
             actionButton("viewresults", 
                          "View plot in browser (only for one \
                                     comparison/protein)"),
             actionButton("plotresults", "View plot results as pdf")
      )
    )
  ),
  
  fluidRow(
    column(7, 
           fluidRow(uiOutput('code.button'),
             column(7,offset = 10,
           disabled(actionButton(inputId = "Design", label = "Next Step")),
           tags$br(),
           tags$br(),
           )),
           uiOutput("matrix"),
           # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
           #                  tags$br(),
           #                  tags$h4("Calculation in progress (it may take a while)...")),
           uiOutput("table_results"),
           tags$br(),
           tags$br(),
           # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
           #                  tags$br(),
           #                  tags$br(),
           #                  tags$h4("Calculation in progress...")),
           tags$br(),
           uiOutput("comparison_plots")
    ))
  
  
)
