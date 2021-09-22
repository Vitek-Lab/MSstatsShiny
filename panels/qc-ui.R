### sidebar ###

sbp_params = sidebarPanel(
  
  # transformation
  
  conditionalPanel(condition = "input.DDA_DIA == 'TMT'",
                   h4("1. Peptide level normalization", 
                      tipify(icon("question-circle"), 
                      title = "Global median normalization on peptide level data, which equalizes the medians across all the channels and MS runs")),
                   checkboxInput("global_norm", "Yes", value = T)),
  
  conditionalPanel(condition = "input.DDA_DIA !== 'TMT'",
                   radioButtons("log", 
                                label= h4("1. Log transformation", 
                                          tipify(icon("question-circle"), 
                                                 title = "Logarithmic transformation is applied to the Intensities column")), 
                                c(log2 = "2", log10 = "10"))),
  
  
  tags$hr(),
  
  conditionalPanel(condition = "input.DDA_DIA == 'TMT'",
                   selectInput("summarization", 
                               label = h4("2. Summarization method", 
                                          tipify(icon("question-circle"), 
                                                 title = "1. MSstats: Perform missing value imputation and Tukey’s median polish summarization for each MS run separately   2. Tukey’s median polish: Perform Tukey’s median polish summarization for each MS run separately. 3. Median: Perform median summarization for each MS run separately. 4. Log(Sum): Sum the raw intensities of peptides ions matched with each protein and log-transform the sum.")), 
                               c("MSstats" = "msstats", "Tukey’s median polish" = "MedianPolish", "Log(Sum)" = "LogSum","Median" = "Median"), 
                               selected = "log")),
  
  conditionalPanel(condition = "input.DDA_DIA == 'TMT' && input.summarization == 'msstats'",
                                checkboxInput("null", label = p("Do not apply cutoff", 
                                                                 tipify(icon("question-circle"), 
                                                                        title = "We assume missing values are censored. The parameter is the maximum quantile for deciding censored missing value"))),
                                numericInput("maxQC", NULL, 0.999, 0.000, 1.000, 0.001)),
  
  #normalisation
  
  conditionalPanel(condition="input.DDA_DIA !== 'TMT'",
                   selectInput("norm", 
                               label = h4("2. Normalisation", tipify(icon("question-circle"), title = "Choose a normalisation method.  For more information visit the Help tab")), c("none" = "FALSE", "equalize medians" = "equalizeMedians", "quantile" = "quantile", "global standards" = "globalStandards"), selected = "equalizeMedians")),
  conditionalPanel(condition = "input.DDA_DIA !== 'TMT' && input.norm == 'globalStandards'",
                   radioButtons("standards", "Choose type of standards", c("Proteins", "Peptides")),
                   uiOutput("Names")
  ),
  tags$hr(),
  
  conditionalPanel(
    condition = "input.DDA_DIA === 'TMT'",
    h4("3. Local protein normalization",
       tipify(icon("question-circle"), 
              title = "For each protein, perform reference channel-based normalization between MS runs on protein level data. It needs at least one reference channel in each MS run, annotated by ‘Norm’ in Condition column of annotation file")),
    checkboxInput("reference_norm", "Yes", value = T),
    tags$hr(),
    h4("4. Filtering"),
    checkboxInput("remove_norm_channel", "Remove normalization channel", value = T)
    
  ),
 
  
  conditionalPanel(
    condition = "input.DDA_DIA !== 'TMT'",
    
    # features
    
    #h4("3. Used features"),
    radioButtons("features_used",
                 label = h4("3. Used features"),
                 c("Use all features" = "all_feat", "Use top N features" = "n_feat", "Remove uninformative features & outliers" = "clean_features")),
    #checkboxInput("all_feat", "Use all features", value = TRUE),
    conditionalPanel(condition = "input.features_used =='n_feat'",
                     uiOutput("features")),
    #uiOutput("features"),
    tags$hr(),
    
    ### censoring
    h4("4. Missing values (not random missing or censored)"),
    radioButtons('censInt', 
                 label = h5("Assumptions for missing values", tipify(icon("question-circle"), title = "Processing software report missing values differently; please choose the appropriate options to distinguish missing values and if censored/at random")), c("assume all NA as censored" = "NA", "assume all between 0 and 1 as censored" = "0", "all missing values are random" = "null"), selected = "NA"),
    radioTooltip(id = "censInt", choice = "NA", title = "It assumes that all NAs in Intensity column are censored.", placement = "right", trigger = "hover"),
    radioTooltip(id = "censInt", choice = "0", title = "It assumes that all values between 0 and 1 in Intensity column are censored.  NAs will be considered as random missing.", placement = "right", trigger = "hover"),
    radioTooltip(id = "censInt", choice = "null", title = "It assumes that all missing values are randomly missing.", placement = "right", trigger = "hover"),
    
    # max quantile for censored
    h5("Max quantile for censored", tipify(icon("question-circle"), title = "Max quantile for censored")),
    checkboxInput("null1", label = "Do not apply cutoff to censor missing values"),
    numericInput("maxQC1", NULL, 0.999, 0.000, 1.000, 0.001),
    
    
    # MBi
    h4("5. Imputation"),
    conditionalPanel(condition = "input.censInt == 'NA' || input.censInt == '0'",
                     checkboxInput("MBi", 
                                   label = p("Model based imputation", tipify(icon("question-circle"), title = "If unchecked the values set as cutoff for censored will be used")), value = TRUE
                     )),
    # cutoff for censored
    conditionalPanel(condition = "input.censInt == 'NA' || input.censInt == '0'",
                     selectInput("cutoff", "cutoff value for censoring", c("min value per feature"="minFeature", "min value per feature and run"="minFeatureNRun", "min value per run"="minRun"))),
    
    
    tags$hr(),
    tags$style(HTML('#run{background-color:orange}')),
    ### summary method
    
    h4("6. Summarization", tipify(icon("question-circle"), title = "Run-level summarization method")),
    p("method: TMP"),
    p("For linear summarzation please use command line"),
    tags$hr(),
    
    # remove features with more than 50% missing 
    checkboxInput("remove50", "remove runs with over 50% missing values"),
    
  ),
  
  tags$hr(),
  # run 
  
  actionButton("run", "Run preprocessing"),
  width = 3
)

  
### main panel ###  
  
main = mainPanel(
  
  
  tabsetPanel(
    tabPanel("Preprocessed data", 
             #verbatimTextOutput('effect'),
             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                              tags$br(),
                              tags$br(),
                              tags$h4("Calculation in progress...")),
             #tags$div(id='download_buttons')
             tags$br(),
             disabled(downloadButton("prepr_csv","Download .csv of preprocessed data")),
             conditionalPanel(condition = "input.DDA_DIA !== 'TMT'",
                              disabled(downloadButton("summ_csv","Download .csv of summarised data"))
             )
             ),
    tabPanel("Plot", 
             wellPanel(
               p("Please preprocess data to view quality control plots"),
               conditionalPanel(condition = "input.DDA_DIA==='TMT'",
                                selectInput("type1",
                                            label = h5("Select plot type", tipify(icon("question-circle"), title = "Use Profile Plots to view technical/biological variability and missing values; use Condition Plots to view differences in intensity between conditions; use QC Plots to view differences between runs and to check the effects of normalization")), c("Show QC plots"="QCPlot", "Show profile plots"="ProfilePlot"))),
               conditionalPanel(condition = "input.DDA_DIA!=='TMT'",
                                selectInput("type2",
                                            label = h5("Select plot type", tipify(icon("question-circle"), title = "Use Profile Plots to view technical/biological variability and missing values; use Condition Plots to view differences in intensity between conditions; use QC Plots to view differences between runs and to check the effects of normalization")), c("Show QC plots"="QCPlot", "Show profile plots"="ProfilePlot","Show Condition plot"="ConditionPlot"))),
               conditionalPanel(condition = "input.type1==='ProfilePlot' || input.type2==='ProfilePlot'",
                                checkboxInput("summ", "Show plot with summary")
                                ),
               conditionalPanel(condition = "input.type2 === 'ProfilePlot' && input.DDA_DIA!='TMT' && !input.summ",
                                selectInput("fname",  
                                            label = h5("Feature legend", tipify(icon("question-circle"), title = "Print feature level at transition level, peptide level or choose no feature legend")), c("Transition level"="Transition", "Peptide level"="Peptide", "No feature legend"="NA"))
               ),
               conditionalPanel(condition = "input.type2 == 'ConditionPlot'",
                                checkboxInput("cond_scale", "Scale conditional level at x-axis (unequal space at x-axis)", value = FALSE),
                                radioButtons("interval", "width of error bar", c("use Confidence Interval"="CI", "use Standard Deviation"="SD"))
               ),
               uiOutput("Which"),
               tags$br()
             ),
             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                              tags$br(),
                              tags$br(),
                              tags$h4("Calculation in progress...")),
             uiOutput("showplot")
             )
    )
)
  
  
  

########################################################################################

qc = fluidPage(
  tags$style(HTML('#proceed6{background-color:orange}')),
  headerPanel("Data processing"),
  p("Preprocessing of the data is performed through: (i) Log transformation, (ii) Normalisation, (iii) Feature selection, (iv) Imputation for censored missing values, (v) Run-level summarisation.  Please choose preprocessing parameters and hit Run.  More information on the preprocessing step can be found ", 
    a("here", href="https://rdrr.io/bioc/MSstats/man/dataProcess.html", target="_blank")),
  p("Quality of data and preprocessing can be assessed in the plot tab of the main panel."),
  p("Preprocessed data will be used for protein quantification and to build a statistical model to evaluate the changes in protein expression."),
  p("PLEASE UPLOAD DATASET OR SELECT SAMPLE DATASET TO COMPLETE THIS STEP"),
  tags$br(),
  sbp_params,
  column(width = 8,
  main,
  actionButton(inputId = "proceed6", label = "Next step")
 ),
 
)