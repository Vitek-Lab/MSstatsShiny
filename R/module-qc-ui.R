qcUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      use_busy_spinner(spin = "fading-circle"),
      tags$style(HTML('#qc-proceed6{background-color:orange}')),
      headerPanel("Process and quantify data"),
      p("Feature summarization and missing value imputation. Includes options for vizualizing summarization through data tables and multiple plots. All outputs are available to download in 'csv' format."),
      tags$br(),
      sidebarPanel(
        
        
        # transformation
        
        conditionalPanel(condition = "input['loadpage-DDA_DIA'] == 'TMT' || (input['loadpage-DDA_DIA'] == 'PTM' && input['loadpage-PTMTMT'] == 'Yes')",
                         h4("1. Peptide level normalization", 
                            tipify(icon("question-circle"), 
                                   title = "Global median normalization on peptide level data, equalizes medians across all the channels and runs")),
                         checkboxInput(ns("global_norm"), "Yes", value = T)),
        
        conditionalPanel(condition = "input['loadpage-DDA_DIA'] == 'SRM_PRM' || input['loadpage-DDA_DIA'] == 'DDA' || input['loadpage-DDA_DIA'] == 'DIA' || (input['loadpage-DDA_DIA'] == 'PTM' && input['loadpage-PTMTMT'] == 'No')",
                         radioButtons(ns("log"), 
                                      label= h4("1. Log transformation", 
                                                tipify(icon("question-circle"), 
                                                       title = "Logarithmic transformation applied to the Intensity column")), 
                                      c(log2 = "2", log10 = "10"))),
        
        
        tags$hr(),
        
        conditionalPanel(condition = "input['loadpage-DDA_DIA'] == 'TMT' || (input['loadpage-DDA_DIA'] == 'PTM' && input['loadpage-PTMTMT'] == 'Yes')",
                         selectInput(ns("summarization"), 
                                     label = h4("2. Summarization method", 
                                                tipify(icon("question-circle"), 
                                                       title = "Select method to be used for protein summarization. For details on each option please see Help tab")), 
                                     c("MSstats" = "msstats", 
                                       "Tukeys median polish" = "MedianPolish", 
                                       "Log(Sum)" = "LogSum","Median" = "Median"), 
                                     selected = "log")),
        
        conditionalPanel(condition = "(input['loadpage-DDA_DIA'] == 'TMT' || (input['loadpage-DDA_DIA'] == 'PTM' && input['loadpage-PTMTMT'] == 'Yes')) && input['qc-summarization'] == 'msstats'",
                         checkboxInput(ns("null"), label = p("Do not apply cutoff", 
                                                         tipify(icon("question-circle"), 
                                                                title = "Maximum quantile for deciding censored missing values, default is 0.999"))),
                         numericInput(ns("maxQC"), NULL, 0.999, 0.000, 1.000, 0.001)),
        
        # Normalization
        conditionalPanel(condition = "input['loadpage-DDA_DIA'] == 'SRM_PRM' || input['loadpage-DDA_DIA'] == 'DDA' || input['loadpage-DDA_DIA'] == 'DIA'",
                         selectInput(ns("norm"), 
                                     label = h4("2. Normalization", 
                                                tipify(icon("question-circle"), 
                                                       title = "Normalization to remove systematic bias between MS runs. For more information visit the Help tab")), 
                                     c("none" = "FALSE", "equalize medians" = "equalizeMedians", 
                                       "quantile" = "quantile", "global standards" = "globalStandards"), 
                                     selected = "equalizeMedians")),
        conditionalPanel(condition = "input['loadpage-DDA_DIA'] == 'PTM' && input['loadpage-PTMTMT'] == 'No'",
                         selectInput(ns("norm"), 
                                     label = h4("2. Normalization", 
                                                tipify(icon("question-circle"), 
                                                       title = "Normalization to remove systematic bias between MS runs. For more information visit the Help tab")), 
                                     c("none" = "FALSE", "equalize medians" = "equalizeMedians", 
                                       "quantile" = "quantile"), 
                                     selected = "equalizeMedians")),
        conditionalPanel(condition = "input['qc-norm'] == 'globalStandards' &&  (input['loadpage-DDA_DIA'] !== 'PTM' && input['loadpage-DDA_DIA'] !== 'TMT')",
                         radioButtons(ns("standards"), "Choose type of standards", 
                                      c("Proteins", "Peptides")),
                         uiOutput(ns("Names"))),
        tags$hr(),
        
        conditionalPanel(
          condition = "input['loadpage-DDA_DIA'] == 'TMT' || (input['loadpage-DDA_DIA'] == 'PTM' && input['loadpage-PTMTMT'] == 'Yes')",
          h4("3. Local protein normalization",
             tipify(icon("question-circle"), 
                    title = "Reference channel based normalization between MS runs on protein level data. Requires one reference channel in each MS run, annotated by 'Norm' in Condition column of annotation file")),
          checkboxInput(ns("reference_norm"), "Yes", value = T),
          tags$hr(),
          h4("4. Filtering"),
          checkboxInput(ns("remove_norm_channel"), "Remove normalization channel", value = T)
          
        ),
        
        
        conditionalPanel(
          condition = "input['loadpage-DDA_DIA'] == 'DDA' || input['loadpage-DDA_DIA'] == 'DIA' || input['loadpage-DDA_DIA'] == 'SRM_PRM' || (input['loadpage-DDA_DIA'] == 'PTM' && input['loadpage-PTMTMT'] == 'No')",
          
          # features
          
          #h4("3. Used features"),
          radioButtons(ns("features_used"),
                       label = h4("3. Feature subset", 
                                  tipify(icon("question-circle"), 
                                         title = "What features to use in \
                                   summarization. All features or a subset of \
                                   features can be used.")),
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
          radioButtons(ns('censInt'), 
                       label = h5("Assumptions for missing values", 
                                  tipify(icon("question-circle"), 
                                         title = "Processing software report missing values differently; please choose the appropriate options to distinguish missing values and if censored/at random")), 
                       c("assume all NA as censored" = "NA", "assume all between 0 \
                   and 1 as censored" = "0", 
                         "all missing values are random" = "null"), 
                       selected = "NA"),
          MSstatsShiny:::radioTooltip(id = ns("censInt"), choice = "NA", title = "It assumes that all \
                 NAs in Intensity column are censored.", placement = "right", 
                                     trigger = "hover"),
          MSstatsShiny:::radioTooltip(id = ns("censInt"), choice = "0", title = "It assumes that all \
                 values between 0 and 1 in Intensity column are censored.  NAs \
                 will be considered as random missing.", placement = "right",
                                     trigger = "hover"),
          MSstatsShiny:::radioTooltip(id = ns("censInt"), choice = "null", title = "It assumes that all \
                 missing values are randomly missing.", placement = "right", 
                                     trigger = "hover"),
          
          # max quantile for censored
          h5("Max quantile for censored", tipify(icon("question-circle"), 
                                                 title = "Max quantile for censored")),
          checkboxInput(ns("null1"), label = "Do not apply cutoff to censor missing values"),
          numericInput(ns("maxQC1"), NULL, 0.999, 0.000, 1.000, 0.001),
          
          
          # MBi
          h4("5. Imputation"),
          conditionalPanel(condition = "input['qc-censInt'] == 'NA' || input['qc-censInt'] == '0'",
                           checkboxInput(ns("MBi"), 
                                         label = p("Model based imputation", 
                                                   tipify(icon("question-circle"), 
                                                          title = "If unchecked the values set as cutoff for censored will be used")), 
                                         value = TRUE
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
          
          h4("6. Summarization", tipify(icon("question-circle"), 
                                        title = "Run-level summarization method")),
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
                              h4("Download summary of protein abundance", 
                                 tipify(icon("question-circle"), 
                                        title="Model-based quantification for each condition or for each biological samples per protein.")),
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
                                        label = h5("Select plot type", 
                                                   tipify(icon("question-circle"), 
                                                          title="For details on plotting options please see the Help tab.")), 
                                        c("Quality Control Plots"="QCPlot", 
                                          "Profile Plots"="ProfilePlot")),
                            conditionalPanel(condition = "input['qc-type1'] === 'ProfilePlot'",
                                             checkboxInput(ns("summ"), "Show plot with summary"),
                                             selectInput(ns("fname"),  
                                                         label = h5("Feature legend", 
                                                                    tipify(icon("question-circle"),
                                                                           title = "Type of legend to use in plot")), 
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
                          conditionalPanel(condition="input['loadpage-DDA_DIA'] !== 'PTM'",
                                           disabled(downloadButton(ns("prepr_csv"),"Download .csv of feature level data")),
                                           disabled(downloadButton(ns("summ_csv"),"Download .csv of protein level data"))
                          ),
                          conditionalPanel(condition="input['loadpage-DDA_DIA'] == 'PTM'",
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