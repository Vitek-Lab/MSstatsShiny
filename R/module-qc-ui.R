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
#' @examples
#' NA
#'
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
      p("Feature summarization and missing value imputation. Includes options for visualizing summarization through data tables and multiple plots. Summarized tables and processed datasets are available to download in CSV format. Imputation runs only when a feature is observed in some other run AND the analyte has at least one observed feature in the current run."),
      tags$br(),
      sidebarPanel(
        # Peptide-level (global median) normalization: TMT branch
        shinyjs::hidden(div(
          id = ns(NAMESPACE_QC$global_norm_panel),
          h4("Peptide level normalization",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
             div("Global median normalization on peptide level data, equalizes medians across all the channels and runs", class = "icon-tooltip")),
          checkboxInput(ns("global_norm"), "Yes", value = TRUE)
        )),

        # Log transformation: label-free branch, also hidden for protein turnover
        shinyjs::hidden(div(
          id = ns(NAMESPACE_QC$log_section),
          radioButtons(ns("log"),
                       label = h4("Log transformation",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                  div("Logarithmic transformation applied to the Intensity column", class = "icon-tooltip")),
                       c(log2 = "2", log10 = "10"))
        )),


        tags$hr(),

        # Summarization method: TMT branch
        shinyjs::hidden(div(
          id = ns(NAMESPACE_QC$summarization_panel),
          selectInput(ns("summarization"),
                      h4("Summarization method",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                         div("Select method to be used for summarization. For details on each option please see Help tab", class = "icon-tooltip")),
                      c("MSstats" = "msstats",
                        "Tukeys median polish" = "MedianPolish",
                        "Log(Sum)" = "LogSum","Median" = "Median"),
                      selected = "log")
        )),

        # Maximum censored quantile: TMT branch with MSstats summarization
        shinyjs::hidden(div(
          id = ns(NAMESPACE_QC$maxqc_msstats_panel),
          checkboxInput(ns("null"), label =tags$div("Do not apply cutoff",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                                    div("Maximum quantile for deciding censored missing values, default is 0.999", class = "icon-tooltip"))
          ),
          numericInput(ns("maxQC"), NULL, 0.999, 0.000, 1.000, 0.001)
        )),

        # Normalization: label-free branch
        shinyjs::hidden(div(
          id = ns(NAMESPACE_QC$norm_panel),
          selectInput(ns("norm"),
                      label = h4("Normalization",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                 div("Normalization to remove systematic bias between MS runs. For more information visit the Help tab", class = "icon-tooltip")),
                      c("none" = "FALSE", "equalize medians" = "equalizeMedians",
                        "quantile" = "quantile", "global standards" = "globalStandards"),
                      selected = "equalizeMedians")
        )),

        # Global-standards selection: label-free, non-PTM, when norm is globalStandards
        shinyjs::hidden(div(
          id = ns(NAMESPACE_QC$standards_panel),
          div(id = ns(NAMESPACE_QC$standards_type_section),
              radioButtons(ns("standards"), "Choose type of standards",
                           c("Proteins", "Peptides"))
          ),
          uiOutput(ns("Names"))
        )),
        tags$hr(),

        # Reference-channel normalization and filtering: TMT branch
        shinyjs::hidden(div(
          id = ns(NAMESPACE_QC$reference_norm_panel),
          h4("Local normalization",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
             div("Reference channel based normalization between MS runs on summarized data. Requires one reference channel in each MS run, annotated by 'Norm' in Condition column of annotation file", class = "icon-tooltip")),
          checkboxInput(ns("reference_norm"), "Yes", value = TRUE),
          tags$hr(),
          h4("Filtering"),
          checkboxInput(ns("remove_norm_channel"), "Remove normalization channel", value = TRUE)
        )),

        # Feature subset, missing-value handling, imputation and summary method: label-free branch
        shinyjs::hidden(div(
          id = ns(NAMESPACE_QC$lf_options_panel),
          div(
            id = ns(NAMESPACE_QC$feature_subset_panel),
            radioButtons(ns("features_used"),
                         label = h4("Feature subset",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                    div("What features to use in \
                                     summarization. All features or a subset of \
                                     features can be used.", class = "icon-tooltip")),
                         c("Use all features" = "all", "Use top N features" = "topN",
                           "Remove uninformative features & outliers" = "highQuality")),
            shinyjs::hidden(div(
              id = ns(NAMESPACE_QC$features_topn_panel),
              uiOutput(ns("features"))
            ))
          ),
          shinyjs::hidden(div(
            id = ns(NAMESPACE_QC$feature_weights_panel),
            h4("Feature Weighting"),
            checkboxInput(
              ns(NAMESPACE_QC$assign_feature_weights),
              label = tags$div("Assign feature weights", class = "icon-wrapper",
                               icon("question-circle", lib = "font-awesome"),
                               div("Compute per-peptide quality weights (coverage, \
                                intensity, monotonicity, validity) and add them as \
                                extra columns to the Turnover Ratios table.",
                                   class = "icon-tooltip")),
              value = FALSE
            )
          )),
          #uiOutput("features"),
          tags$hr(),

          ### censoring
          div(id = ns(NAMESPACE_QC$censoring_section),
            h4("Missing values (not random missing or censored)"),

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
            numericInput(ns("maxQC1"), NULL, 0.999, 0.000, 1.000, 0.001)
          ),

          # MBi
          h4("Imputation"),
          shinyjs::hidden(div(
            id = ns(NAMESPACE_QC$mbi_panel),
            checkboxInput(ns("MBi"), label=tags$div("Model based imputation",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                                    div("Fills in missing intensities only when (a) the analyte has at least one observed feature in that run, AND (b) the missing feature is observed in at least one other run. Analytes entirely missing from a run, and features never observed in the dataset, are not imputed. If unchecked, the cutoff for censored values is used instead.", class = "icon-tooltip")),value = TRUE
            )
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

          uiOutput(ns("summaryMethodUI")),
          tags$hr(),

          # remove features with more than 50% missing
          checkboxInput(ns("remove50"), "remove runs with over 50% missing values")

        )),

        # Tracer constants (protein turnover only), toggled server-side by
        # register_qc_turnover. Static and shinyjs::hidden(), NOT a renderUI:
        # re-rendering destroys a mounted fileInput, which pushes NULL to the
        # server while the parsed constants live on in a reactiveVal -- the UI
        # would read "no file uploaded" while the fit kept dividing by the old
        # file's values. R/qc-server-sidebar.R:101-105 documents the same rule
        # for the same reason. Three of the old renderUI's dependencies are
        # invalidated by ordinary actions (re-clicking proceed on the load
        # page, editing a TimeVal cell, uploading a GROUP mapping), so this is
        # not a theoretical hazard.
        #
        # The hr() lives INSIDE the div so the rule appears only with the
        # section it introduces. (It is NOT true that the old unconditional hr
        # at this position rendered alone on non-turnover templates -- it
        # separated the last visible control from the Run button. Moving it
        # gives up that separator on those templates, which is the accepted
        # cost of not drawing two rules, or a leading one, on turnover.)
        #
        # The schema sits in the tooltip rather than in the panel body because
        # this sidebar is width = 3 and already runs ~160 lines; every other
        # fileInput in this app has the width = 8 main panel to spread into.
        shinyjs::hidden(div(
          id = ns(NAMESPACE_QC$tracer_constants_panel),
          tags$hr(),
          h4("Tracer constants", class = "icon-wrapper",
             icon("question-circle", lib = "font-awesome"),
             div(paste0(
               "Optional. Corrects each condition's heavy fraction for ",
               "incomplete label enrichment: the heavy fraction is DIVIDED by ",
               "the constant, so 1 means no correction. Leave this empty and ",
               "every condition uses 1. Required columns, case-sensitive: ",
               paste(get_qc_required_tracer_columns(), collapse = ", "),
               ". One row per condition -- the file must cover them all, and ",
               "GROUP values must match your experimental conditions exactly, ",
               "including case. TracerConstant must be between ",
               CONSTANTS_QC$tracer_min, " and ", CONSTANTS_QC$tracer_max,
               ", inclusive. Example: a header row 'GROUP,TracerConstant' ",
               "followed by '0h,0.98', '6h,0.95', '24h,0.93'. Condition names ",
               "must begin with a number of hours, as those do: a d or w ",
               "anywhere in the name is read as days or weeks, so names the ",
               "app cannot read, and names that resolve to the same ",
               "timepoint, are rejected."),
               class = "icon-tooltip")),
          fileInput(ns(NAMESPACE_QC$tracer_constants_file),
                    "Upload tracer constants (CSV)", accept = ".csv"),
          # One visible line, phrased like the sibling uploads at :253-283.
          # The tooltip holds the detail, but tooltips here are hover-only
          # (inst/assets/style.css: .icon-wrapper:hover .icon-tooltip), and
          # this would otherwise be the only upload in the app whose schema
          # needs a mouse.
          p(tags$strong("Required columns: "),
            paste(get_qc_required_tracer_columns(), collapse = ", ")),
          # Required, not optional: a rejected upload blocks Run (plan section
          # 0.1), so without a clear affordance a bad file is a dead end.
          actionButton(ns(NAMESPACE_QC$tracer_constants_clear), "Clear",
                       class = "btn-sm"),
          tags$br(), tags$br(),
          # Makes the absent / pending / rejected / valid states visible. The
          # fileInput keeps displaying the filename of a rejected file, so
          # without this the UI looks like the upload succeeded.
          uiOutput(ns(NAMESPACE_QC$tracer_constants_status))
        )),
        actionButton(ns("run"), "Run summarization"),
        width = 3
      ),
      column(width = 8,
             mainPanel(

               h3("Please run summarization in the side panel."),
               h3(textOutput(ns("caption"), container = span)),

               tabsetPanel(id = ns("qc_tabs"),
                 tabPanel("Upload Summarized Abundances", value = "Data Upload",
                          wellPanel(
                            h4("Upload summarized abundances"),
                            p("Bring FeatureLevelData and ProteinLevelData that were summarized in a previous session or an external pipeline. When the required files are uploaded and the Data Uploading page has not been used, the Statistical Inference Page uses them directly, skipping conversion and summarization on the Data Uploading page."),
                            fileInput(ns("upload_feature_level"), "Upload FeatureLevelData (CSV)", accept = ".csv"),
                            fileInput(ns("upload_protein_level"), "Upload ProteinLevelData (CSV)", accept = ".csv"),
                            p(tags$strong("Required columns (names must match exactly, case-sensitive):")),
                            tags$ul(
                              tags$li(tags$strong("FeatureLevelData: "), "PROTEIN, PEPTIDE, FEATURE, RUN, GROUP, LABEL, INTENSITY"),
                              tags$li(tags$strong("ProteinLevelData: "), "Protein, GROUP, RUN, LogIntensities (add LABEL for protein turnover)")
                            ),
                            # Template-gated (turnover + chemo): GROUP mapping, shown/hidden server-side
                            shinyjs::hidden(div(
                              id = ns(NAMESPACE_QC$data_upload_mapping_panel),
                              tags$hr(),
                              h4("Condition mapping (CSV)",
                                 class = "icon-wrapper",
                                 icon("question-circle", lib = "font-awesome"),
                                 div("Maps each experimental group to its numeric time point (protein turnover) or dose (chemoproteomics). This supplies the condition metadata the Data Uploading page normally collects when you convert data there. The GROUP values in this file must exactly match the GROUP values in your uploaded ProteinLevelData.",
                                     class = "icon-tooltip")),
                              fileInput(ns("upload_condition_mapping"), "Upload GROUP mapping (CSV)", accept = ".csv"),
                              p(tags$strong("Required columns:")),
                              tags$ul(
                                tags$li(tags$strong("Protein turnover: "), "GROUP, TimeVal"),
                                tags$li(tags$strong("Chemoproteomics: "), "GROUP, DoseVal, DoseUnit (uM, nM, mM, or M), DrugName")
                              )
                            )),
                            # Template-gated (turnover only): turnover ratios
                            shinyjs::hidden(div(
                              id = ns(NAMESPACE_QC$data_upload_ratios_panel),
                              tags$hr(),
                              fileInput(ns("upload_turnover_ratios"), "Upload Turnover Ratios (CSV)", accept = ".csv"),
                              p(tags$strong("Required columns: "), "Protein, TimeVal, H_frac, L_frac"),
                              # plan section 0.5: the tracer-constants panel in the side bar is
                              # deliberately unavailable on this flow. Said here rather
                              # than there, because there is exactly where a user on
                              # this flow cannot see it.
                              p("Ratios uploaded here are used as given. The tracer-constants upload in the side panel does not apply on this flow: these H_frac and L_frac values are already computed, so any correction for incomplete label enrichment must be applied before upload.")
                            ))
                          )
                 ),
                 tabPanel("Summarized Results",
                          wellPanel(
                            fluidRow(
                              h4("Download summary of abundance",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                 div("Model-based quantification for each condition or for each biological sample per analyte.", class = "icon-tooltip")),
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
                            uiOutput(ns("plotTypeUI")),
                            # Profile-plot options: shown when plot type is ProfilePlot
                            shinyjs::hidden(div(
                              id = ns(NAMESPACE_QC$profileplot_options_panel),
                              checkboxInput(ns("summ"), "Show plot with summary"),
                              selectInput(ns("fname"),
                                          label = h5("Feature legend",class = "icon-wrapper",icon("question-circle", lib = "font-awesome"),
                                             div("Type of legend to use in plot", class = "icon-tooltip")),
                                          c("Transition level"="Transition",
                                            "Peptide level"="Peptide",
                                            "No feature legend"="NA"))
                            )),
                            # Quality-metric selectors: shown when plot type is QualityMetricsPlot
                            shinyjs::hidden(div(
                              id = ns(NAMESPACE_QC$qualitymetrics_options_panel),
                              uiOutput(ns("qualityMetricSelector"))
                            )),
                            uiOutput(ns("which_protein_for_data_process_plots_ui")),
                            tags$br()
                          ),
                          uiOutput(ns("showplot")),
                 ),
                 tabPanel("Turnover Ratios",
                          uiOutput(ns("turnover_ratios_panel"))
                 ),
                 tabPanel("Download Data",
                          #verbatimTextOutput('effect'),
                          # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                          #                  tags$br(),
                          #                  tags$br(),
                          #                  tags$h4("Calculation in progress...")),
                          #tags$div(id='download_buttons')
                          tags$br(),
                          # Non-PTM feature/protein CSV downloads
                          shinyjs::hidden(div(
                            id = ns(NAMESPACE_QC$nonptm_downloads_panel),
                            disabled(downloadButton(ns("prep_feature_level_data_csv"),"Download .csv of feature level data")),
                            disabled(downloadButton(ns("prep_protein_level_data_csv"),"Download .csv of protein level data"))
                          )),
                          # PTM and unmodified-protein CSV downloads
                          shinyjs::hidden(div(
                            id = ns(NAMESPACE_QC$ptm_downloads_panel),
                            disabled(downloadButton(ns("prep_feature_level_data_csv_ptm"),"Download .csv of PTM feature level data")),
                            disabled(downloadButton(ns("prep_protein_level_data_csv_ptm"),"Download .csv of PTM level data")),
                            tags$br(),
                            disabled(downloadButton(ns("prep_feature_level_data_csv_global_proteome"),"Download .csv of unmod protein feature level data")),
                            disabled(downloadButton(ns("prep_protein_level_data_csv_global_proteome"),"Download .csv of protein level data"))
                          ))
                 )
               )
             ),
             uiOutput(ns('submit.button'))
      )
    )
    )
}