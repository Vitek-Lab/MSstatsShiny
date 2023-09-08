#' Statmodel UI module for statistical inference UI.
#'
#' This function sets up the Statmodel UI where it consists of several, 
#' options for users to select and upload files.
#'
#' @param id namespace prefix for the module
#' 
#' @return This function returns nothing, as it sets up the Statmodel UI
#'
statmodelUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$head(
        tags$style(HTML('#statmodel-submit3{background-color:orange}')),
        tags$style(HTML('#statmodel-clear3{background-color:orange}')),
        tags$style(HTML('#statmodel-submit1{background-color:orange}')),
        tags$style(HTML('#statmodel-clear1{background-color:orange}')),
        tags$style(HTML('#statmodel-submit2{background-color:orange}')),
        tags$style(HTML('#statmodel-clear2{background-color:orange}')),
        tags$style(HTML('#statmodel-calculate{background-color:orange}')),
        tags$style(HTML('#statmodel-plotresults{background-color:orange}')),
        tags$style(HTML('#statmodel-viewresults{background-color:orange}')),
        tags$style(HTML('#statmodel-submit{background-color:orange}')),
        tags$style(HTML('#statmodel-clear{background-color:orange}')),
        tags$link(rel = "stylesheet", type = "text/css", href = "assets/style.css"),
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
          radioButtons(ns("def_comp"), label=h4("1. Define comparisons\
                                                          - contrast matrix",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                                div("Define what conditions you want to compare here.", class = "icon-tooltip")),
                       
                       c("All possible pairwise comparisons" = "all_pair", 
                         "Compare all against one" = "all_one", 
                         "Create custom pairwise comparisons" = "custom",
                         "Create custom non-pairwise comparisons" = "custom_np"), 
                       selected = character(0)),
          tags$br(),
          conditionalPanel(condition = "input['statmodel-def_comp'] == 'custom'",
                           uiOutput(ns('choice1')),
                           h6("vs"),
                           uiOutput(ns("choice2")),
                           actionButton(ns("submit"), "Add"),
                           actionButton(ns("clear"), "Clear matrix")
          ),
          conditionalPanel(condition = "input['statmodel-def_comp'] == 'all_one'",
                           h5("Compare all groups against:"),
                           uiOutput(ns("choice3")),
                           actionButton(ns("submit1"), "Submit"),
                           actionButton(ns("clear1"), "Clear matrix")
          ),
          conditionalPanel(condition = "input['statmodel-def_comp'] == 'all_pair'",
                           actionButton(ns("submit2"), "Submit"),
                           actionButton(ns("clear2"), "Clear matrix")
          ),
          conditionalPanel(condition = "input['statmodel-def_comp'] == 'custom_np'",
                           h5("Non-pairwise Comparison:"),
                           uiOutput(ns('comp_name')),
                           uiOutput(ns('weights')),
                           actionButton(ns("submit3"), "Add"),
                           actionButton(ns("clear3"), "Clear matrix")
          ),
          tags$hr(),
          h4("2. Group comparison"),
          p("Please add a comparison matrix before modeling."),
          disabled(actionButton(ns("calculate"), "Start")),
          tags$hr(),
          conditionalPanel(condition = "input['loadpage-DDA_DIA'] == 'TMT' || (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] == 'TMT')",
                           radioButtons(ns("moderated"), 
                                        label= h4("Empirical Bayes moderation",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                                  div("TRUE will moderate t statistic; FALSE (default) uses ordinary t statistic.", class = "icon-tooltip")), 
                                        c(True = TRUE, False = FALSE))),
          
          sliderInput(ns("signif"),
                      label = h5("Significance level",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                 div("The alpha used to determine significant results. IE the probability of type I error)", class = "icon-tooltip")), 
                      0, 1, 0.05),
        ),
        # table of significant proteins 
        tags$br(),
        fluidRow(
          column(12,
                 fluidRow(
                   selectInput(ns("typeplot"), 
                               label = h4("3. Visualization - \
                                                select plot type"), 
                               c("Volcano Plot" = "VolcanoPlot", 
                                 "Heatmap"="Heatmap", 
                                 "Comparison Plot"="ComparisonPlot"))), #
                 conditionalPanel(condition = "input['statmodel-typeplot'] == 'VolcanoPlot'",
                                  uiOutput(ns("WhichComp")),
                                  conditionalPanel(
                                    condition = "input['loadpage-DDA_DIA']!=='TMT'",
                                    checkboxInput(ns("pname"), label = p("display protein name"))),
                                  selectInput(ns("logp"), 
                                              label = h5("Log transformation of adjusted p-value"),
                                              c("base 2" = "2", "base 10" = "10"), selected = "10"),
                                  sliderInput(ns("sig"), 
                                              label = h5("Adjusted p-value cutoff",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                                         div("The cutoff used to determine significant results.)", class = "icon-tooltip")),
                                              0, 1, 0.05),
                                  checkboxInput(ns("FC1"), 
                                                label = p("Apply specific fold change cutoff for significance")),
                                  conditionalPanel(
                                    condition = "input['statmodel-FC1'] == true",
                                    numericInput(ns("FC"), "Fold change cutoff", 
                                                 1, 0, 100, 
                                                 0.1)),
                                  tags$br()),
                 conditionalPanel(
                   condition = "input['statmodel-typeplot'] == 'ComparisonPlot'",
                   uiOutput(ns("WhichProt")),
                   uiOutput(ns("WhichComp1"))),
                 
                 
                 conditionalPanel(
                   condition = "input['statmodel-typeplot'] == 'Heatmap'",
                   h4("Note: Only one page will be shown in \
                                   browser. To view all proteins please \
                                   view this plot as a pdf. \
                             Heatmaps require at least two comparisons."),
                   selectInput(ns("logp"), 
                               label = h5("Log transformation of adjusted p-value"),
                               c("base 2" = "2", "base 10" = "10"), selected = "10"),
                   checkboxInput(ns("FC1"), 
                                 label = p("Apply specific fold change cutoff for significance")),
                   conditionalPanel(
                     condition = "input['statmodel-FC1'] == true",
                     numericInput(ns("FC"), "cutoff", 
                                  1, 0, 100, 
                                  0.1)),
                   
                   numericInput(ns("nump"), "Number of proteins \
                                        per page", 100, 1, 180, 1),
                   selectInput(ns("cluster"), 
                               label = h5("Cluster analysis",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                          div("Determines how to order proteins and comparisons. protein means, comparison means, or both", class = "icon-tooltip")), 
                               c("protein dendogram" = "protein", 
                                 "comparison dendogram" = "comparison", 
                                 "protein and comparison dendograms" = "both"))),
                 
                 p("Please note if you want to plot more than one \
                          Volcano Plot comparison, you must save the results \
                          as a pdf."),
                 conditionalPanel(
                   condition = "input['loadpage-BIO'] !== 'PTM'",
                   actionButton(ns("viewresults"), 
                                "View plot in browser (only for one \
                                     comparison/protein)")),
                 downloadButton(ns("plotresults"), "Save plot results as pdf")
          )
        )
      ),
      
      fluidRow(
        column(7, 
               fluidRow(uiOutput(ns('code.button')),
                        column(7,offset = 10,
                               disabled(actionButton(inputId = ns("Design"), label = "Next Step")),
                               tags$br(),
                               tags$br(),
                        )),
               uiOutput(ns("matrix")),
               conditionalPanel(condition = "input['loadpage-BIO']=='PTM'",
                                tabsetPanel(
                                  tabPanel("Adjusted PTM Results", 
                                           uiOutput(ns("adj_table_results"))),
                                  tabPanel("Unadjusted PTM Results", 
                                           uiOutput(ns("unadj_table_results"))),
                                  tabPanel("Protein Results", 
                                           uiOutput(ns("prot_table_results")))
                                )
               ),
               conditionalPanel(condition = "input['loadpage-BIO']!=='PTM'",
                                uiOutput(ns("table_results"))
               ),
               tags$br(),
               uiOutput(ns("comparison_plots"))
        ))
      
      
    )
    
  )
  
}