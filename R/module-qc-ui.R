#' QC UI module for data processing UI.
#'
#' This function sets up the QC UI where it consists of several, 
#' options for users to process data based on previously selected fragments.
#'
#' @param id namespace prefix for the module
#' 
#' @return This function returns nothing, as it sets up the QC UI
#'
#' @export
qcUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      useShinyjs(),
      use_busy_spinner(spin = "fading-circle"),
      tags$head(
        tags$style(HTML('#qc-proceed6{background-color:orange}')),
        tags$link(rel = "stylesheet", type = "text/css", href = "assets/style.css"),
      ),
      headerPanel("Process and quantify data"),
      p("Feature summarization and missing value imputation. Includes options for vizualizing summarization through data tables and multiple plots. All outputs are available to download in 'csv' format."),
      tags$br(),
      sidebarPanel(
        # transformation
        conditionalPanel(condition = "input['loadpage-DDA_DIA'] == 'TMT' || (input['loadpage-BIO'] == 'PTM' && (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] == 'TMT'))",
                         h4("1. Peptide level normalization",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                            div("Global median normalization on peptide level data, equalizes medians across all the channels and runs", class = "icon-tooltip")),
                         checkboxInput(ns("global_norm"), "Yes", value = TRUE)),
        
        conditionalPanel(condition = "input['loadpage-DDA_DIA'] == 'LType' || (input['loadpage-BIO'] == 'PTM' && (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] != 'TMT'))",
                         radioButtons(ns("log"), 
                                      label = h4("1. Log transformation",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                                 div("Logarithmic transformation applied to the Intensity column", class = "icon-tooltip")),
                                      c(log2 = "2", log10 = "10"))),
        
        
        tags$hr(),
        
        conditionalPanel(condition = "input['loadpage-DDA_DIA'] == 'TMT' || (input['loadpage-BIO'] == 'PTM' && (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] == 'TMT'))",
                         selectInput(ns("summarization"), 
                                     h4("2. Summarization method",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                        div("Select method to be used for protein summarization. For details on each option please see Help tab", class = "icon-tooltip")),
                                     c("MSstats" = "msstats", 
                                       "Tukeys median polish" = "MedianPolish", 
                                       "Log(Sum)" = "LogSum","Median" = "Median"), 
                                     selected = "log")),
        
        conditionalPanel(condition = "(input['loadpage-DDA_DIA'] == 'TMT' || (input['loadpage-BIO'] == 'PTM' && (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] == 'TMT'))) && input['qc-summarization'] == 'msstats'",
                         checkboxInput(ns("null"), label =tags$div("Do not apply cutoff",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                          div("Maximum quantile for deciding censored missing values, default is 0.999", class = "icon-tooltip"))
                                       ),
                         numericInput(ns("maxQC"), NULL, 0.999, 0.000, 1.000, 0.001)),
        
        # Normalization
        conditionalPanel(condition = "input['loadpage-DDA_DIA'] == 'LType'",
                         selectInput(ns("norm"), 
                                     label = h4("2. Normalization",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                        div("Normalization to remove systematic bias between MS runs. For more information visit the Help tab", class = "icon-tooltip")),
                                     c("none" = "FALSE", "equalize medians" = "equalizeMedians", 
                                       "quantile" = "quantile", "global standards" = "globalStandards"), 
                                     selected = "equalizeMedians")),
        conditionalPanel(condition = "input['loadpage-BIO'] == 'PTM' && (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] != 'TMT')",
                         selectInput(ns("norm"), 
                                     label = h4("2. Normalization",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                        div("Normalization to remove systematic bias between MS runs. For more information visit the Help tab", class = "icon-tooltip")),
                                     c("none" = "FALSE", "equalize medians" = "equalizeMedians", 
                                       "quantile" = "quantile"), 
                                     selected = "equalizeMedians")),
        conditionalPanel(condition = "input['qc-norm'] == 'globalStandards' &&  (input['loadpage-BIO'] !== 'PTM' && input['loadpage-DDA_DIA'] !== 'TMT')",
                         radioButtons(ns("standards"), "Choose type of standards", 
                                      c("Proteins", "Peptides")),
                         uiOutput(ns("Names"))),
        tags$hr(),
        
        conditionalPanel(
          condition = "input['loadpage-DDA_DIA'] == 'TMT' || (input['loadpage-BIO'] == 'PTM' && (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] == 'TMT'))",
          h4("3. Local protein normalization",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
             div("Reference channel based normalization between MS runs on protein level data. Requires one reference channel in each MS run, annotated by 'Norm' in Condition column of annotation file", class = "icon-tooltip")),
          checkboxInput(ns("reference_norm"), "Yes", value = TRUE),
          tags$hr(),
          h4("4. Filtering"),
          checkboxInput(ns("remove_norm_channel"), "Remove normalization channel", value = TRUE)
          
        ),
        
        
        conditionalPanel(
          condition = "input['loadpage-DDA_DIA'] == 'LType'  || (input['loadpage-BIO'] == 'PTM' && (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] != 'TMT'))",
          
          # features
          
          #h4("3. Used features"),
          radioButtons(ns("features_used"),
                       label = h4("3. Feature subset",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                          div("What features to use in \
                                   summarization. All features or a subset of \
                                   features can be used.", class = "icon-tooltip")),
                       c("Use all features" = "all", "Use top N features" = "topN", 
                         "Remove uninformative features & outliers" = "highQuality")),
          #),
          #checkboxInput("all_feat", "Use all features", value = TRUE),
          conditionalPanel(condition = "input['qc-features_used'] =='topN'",
                           uiOutput(ns("features"))),
          #uiOutput("features"),
          tags$hr(),
          
          ### censoring
          h4("4. Missing values (not random missing or censored)"),
          
          # radioButtons(ns('censInt'),
          #              h5("Assumptions for missing values",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
          #                 div("Processing software report missing values differently; please choose the appropriate options to distinguish missing values and if censored/at random", class = "icon-tooltip")),
          #              c("assume all NA as censored" = "NA", "assume all between 0 \
          #          and 1 as censored" = "0",
          #                "all missing values are random" = "null"),
          #              selected = "NA"),
          # MSstatsShiny:::radioTooltip(id = ns("censInt"), choice = "NA", title = "It assumes that all \
          #        NAs in Intensity column are censored.", placement = "right",
          #                            trigger = "hover"),
          # MSstatsShiny:::radioTooltip(id = ns("censInt"), choice = "0", title = "It assumes that all \
          #        values between 0 and 1 in Intensity column are censored.  NAs \
          #        will be considered as random missing.", placement = "right",
          #                            trigger = "hover"),
          # MSstatsShiny:::radioTooltip(id = ns("censInt"), choice = "null", title = "It assumes that all \
          #        missing values are randomly missing.", placement = "right",
          #                            trigger = "hover"),
          
          radioButtons(ns('censInt'),
                       h5("Assumptions for missing values",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                          div("Processing software report missing values differently; please choose the appropriate options to distinguish missing values and if censored/at random", class = "icon-tooltip")),
                   choiceNames = list(
                     div("assume all NA as censored",class = "icon-wrapper",
                         div("It assumes that all NAs in Intensity column are censored.", class = "icon-tooltip")),
                     div("assume all between 0 and 1 as censored",class = "icon-wrapper",
                         div("It assumes that all values between 0 and 1 in Intensity column are censored.  NAs will be considered as random missing.", class = "icon-tooltip"))
                   ),
                   choiceValues = list(
                     "NA", "0"
                   ),
                       selected = "NA"),
          
          # max quantile for censored
          h5("Max quantile for censored",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
             div("Max quantile for censored", class = "icon-tooltip")),
          checkboxInput(ns("null1"), label = "Do not apply cutoff to censor missing values"),
          numericInput(ns("maxQC1"), NULL, 0.999, 0.000, 1.000, 0.001),
          
          # MBi
          h4("5. Imputation"),
          conditionalPanel(condition = "input['qc-censInt'] == 'NA' || input['qc-censInt'] == '0'",
                           checkboxInput(ns("MBi"), label=tags$div("Model based imputation",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                                                    div("If unchecked the values set as cutoff for censored will be used", class = "icon-tooltip")),value = TRUE
                           )),
          # # cutoff for censored
          # conditionalPanel(condition = "input.censInt == 'NA' || input.censInt == '0'",
          #                  selectInput("cutoff", "cutoff value for censoring", 
          #                              c("min value per feature"="minFeature", 
          #                                "min value per feature and run"="minFeatureNRun", 
          #                                "min value per run"="minRun"))),
          
          
          tags$hr(),
          tags$style(HTML('#qc-run{background-color:orange}')),
          ### summary method
          
          h4("6. Summarization",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
             div("Run-level summarization method", class = "icon-tooltip")),
          p("method: TMP"),
          p("For linear summarzation please use command line"),
          tags$hr(),
          
          # remove features with more than 50% missing 
          checkboxInput(ns("remove50"), "remove runs with over 50% missing values"),
          
        ),
        
        tags$hr(),
        actionButton(ns("run"), "Run protein summarization"),
        # run 
        width = 3
      ),
      column(width = 8,
             mainPanel(
               
               h3("Please run protein summarization in the side panel."),
               h3(textOutput(ns("caption"), container = span)),
               
               tabsetPanel(
                 tabPanel("Summarized Results", 
                          wellPanel(
                            fluidRow(
                              h4("Download summary of protein abundance",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                 div("Model-based quantification for each condition or for each biological samples per protein.", class = "icon-tooltip")),
                              radioButtons(ns("typequant"), 
                                           label = h4("Type of summarization"), 
                                           c("Sample level summarization" = "Sample", 
                                             "Group level summarization" = "Group")),
                              radioButtons(ns("format"), "Save as", c("Wide format" = "matrix", 
                                                                  "Long format" = "long")),
                              actionButton(ns("update_results"), "Update Summarized Results"),
                              downloadButton(ns("download_summary"), "Download")
                            )),
                          #column(7,
                          h4("Table of abundance"),
                          uiOutput(ns("abundance"))
                          #)
                          #)
                 ),
                 tabPanel("Summarization Plots",
                          wellPanel(
                            selectInput(ns("type1"),
                                        label =  h5("Select plot type",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                           div("For details on plotting options please see the Help tab.", class = "icon-tooltip")),
                                        c("Quality Control Plots"="QCPlot", 
                                          "Profile Plots"="ProfilePlot")),
                            conditionalPanel(condition = "input['qc-type1'] === 'ProfilePlot'",
                                             checkboxInput(ns("summ"), "Show plot with summary"),
                                             selectInput(ns("fname"),  
                                                         label = h5("Feature legend",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                                            div("Type of legend to use in plot", class = "icon-tooltip")),
                                                         c("Transition level"="Transition", 
                                                           "Peptide level"="Peptide", 
                                                           "No feature legend"="NA"))
                                             
                            ),
                            
                            uiOutput(ns("Which")),
                            tags$br()
                          ),
                          # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                          #                  tags$br(),
                          #                  tags$br(),
                          #                  tags$h4("Calculation in progress...")),
                          uiOutput(ns("showplot")),
                          disabled(downloadButton(ns("saveplot"), "Save this plot"))
                 ),
                 tabPanel("Download Data", 
                          #verbatimTextOutput('effect'),
                          # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                          #                  tags$br(),
                          #                  tags$br(),
                          #                  tags$h4("Calculation in progress...")),
                          #tags$div(id='download_buttons')
                          tags$br(),
                          conditionalPanel(condition="input['loadpage-BIO'] !== 'PTM'",
                                           disabled(downloadButton(ns("prepr_csv"),"Download .csv of feature level data")),
                                           disabled(downloadButton(ns("summ_csv"),"Download .csv of protein level data"))
                          ),
                          conditionalPanel(condition="input['loadpage-BIO'] == 'PTM'",
                                           disabled(downloadButton(ns("prepr_csv_ptm"),"Download .csv of PTM feature level data")),
                                           disabled(downloadButton(ns("summ_csv_ptm"),"Download .csv of PTM level data")),
                                           tags$br(),
                                           disabled(downloadButton(ns("prepr_csv_prot"),"Download .csv of unmod protein feature level data")),
                                           disabled(downloadButton(ns("summ_csv_prot"),"Download .csv of protein level data"))
                          )
                 )
               )
             ),
             uiOutput(ns('submit.button'))
      )
    )
    )
}