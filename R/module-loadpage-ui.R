#' Loadpage UI module for data selection and upload UI.
#'
#' This function sets up the loadpage UI where it consists of several
#' options for users to select and upload files.
#'
#' @param id namespace prefix for the module
#'
#' @return This function returns nothing, as it sets up the loadpage UI
#'
#' @export
#' @examples
#' NA
#' 
loadpageUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidPage(
      useShinyjs(),
      headerPanel(list("Upload data")),
      
      # Header content (template-aware: proteomics default vs metabolomics text)
      uiOutput(ns("upload_description")),
      
      tags$br(),
      
      # Conditional sample dataset descriptions
      create_sample_dataset_descriptions(ns),
      
      tags$br(),
      
      sidebarPanel(
        # CSS styling
        create_css_styling(),
        
        # Main selection controls
        create_main_selection_controls(ns),
        
        tags$hr(id = ns(NAMESPACE_LOADPAGE$main_selection_divider)),

        # Label-free type selection
        create_label_free_type_selection(ns),
        
        tags$hr(),
        
        # File upload sections
        create_file_upload_sections(ns),
        
        # Processing options
        create_processing_options(ns),
        
        # Action buttons
        tags$div(
          style = "display:flex; gap:8px; align-items:center;",
          disabled(actionButton(inputId = ns("proceed1"), label = "Upload Data")),
          shinyjs::disabled(downloadButton(
            ns("download_msstats_format"),
            "Download MSstats-format CSV"
          ))
        )
      ),
      
      column(width = 8,
             shinyjs::hidden(uiOutput(ns("summary_tables")))
      )
    )
  )
}

#' Create header content with user guidance
#' @noRd
create_header_content <- function() {
  tagList(
    p("To run the MSstats Pipeline, please upload your dataset. The required files",
      "depend on the spectral processing tool used. Generally the raw data and an",
      "annotation file are needed. The output of this step is your experimental",
      "data processed in MSstats format. For examples on how to prepare your input",
      "please review the MSstats ", 
      a("User Guide", 
        href="https://msstats.org/wp-content/uploads/2020/02/MSstats_v3.18.1_manual_2020Feb26-v2.pdf",
        target="_blank")),
    p("PTM data can be processed using MaxQuant, Proteome Discoverer, Spectronaut, Fragpipe, or Skyline, or preformatted into MSstatsPTM format. For details, see the MSstatsPTM ",
      a("documentation", 
        href="https://www.bioconductor.org/packages/release/bioc/vignettes/MSstatsPTM/inst/doc/MSstatsPTM_LabelFree_Workflow.html",
        target="_blank")),
    p("Note: files must be in CSV/TSV format, or Parquet (.parquet/.pq) for DIANN 2.0+ inputs, and under 250 MB when using msstatsshiny.com. When running the app locally, Spectronaut and DIANN reports above this limit can be processed via 'Large file mode' (out-of-memory streaming through MSstatsBig)."),
    p("Some users may have trouble uploading files while using the application via Google Chrome. If the 'Browse...' button does not work please try a different web browser.")
  )
}

#' Create the metabolomics upload description (MZmine / MSstats-format inputs).
#'
#' The metabolomics branch of `output$upload_description`; factored out as a
#' pure builder (mirroring `create_header_content`) so its text is unit-testable.
#' @noRd
create_metabolomics_header_content <- function() {
  tagList(
    p("To run the metabolomics pipeline, upload your MZmine feature quant",
      "table and an annotation file, plus MZmine compound annotations and",
      "(recommended) SIRIUS structure annotations. The output of this step",
      "is your data in MSstats format."),
    p("Note: files must be in CSV/TSV format and under 250 MB when using",
      "msstatsshiny.com.")
  )
}

#' Create conditional descriptions for sample datasets
#' Visibility is driven server-side by
#' `register_loadpage_visibility_observers` (see
#' `R/loadpage-server-converter-options-panel.R`); each description sits in a hidden div
#' that the observer toggles on `filetype == 'sample' && LabelFreeType == <mode>`.
#' @noRd
create_sample_dataset_descriptions <- function(ns) {
  tagList(
    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$sample_dda_description_panel),
      p("The sample dataset for DDA acquisition is taken from the publication ",
        a("Choi, M. et al.  ABRF Proteome Informatics Research Group (iPRG) 2015 Study: Detection of Differentially Abundant Proteins in Label-Free Quantitative LC MS/MS Experiments. Journal of Proteome Research 16.2 (2016): 945-957. ",
          href = "https://pubs.acs.org/doi/10.1021/acs.jproteome.6b00881",
          target = "_blank"))
    )),
    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$sample_dia_description_panel),
      p("The sample dataset for DIA acquisition is taken from the publication ",
        a("Selevsek, N. et al. Reproducible and Consistent Quantification of the Saccharomyces Cerevisiae Proteome by SWATH-Mass Spectrometry. Molecular & Cellular Proteomics: MCP 14.3 (2015): 739-749. ",
          href = "http://www.mcponline.org/content/14/3/739.long",
          target="_blank"))
    )),
    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$sample_srm_prm_description_panel),
      p("The sample dataset for SRM/PRM acquisition is taken from the publication ",
        a("Picotti, P. et al. Full dynamic range proteome analysis of S. cerevisiae by targeted proteomics. Cell (2009), 138, 795-806.",
          href = "http://www.cell.com/cell/fulltext/S0092-8674(09)00715-6",
          target="_blank"))
    ))
  )
}

#' Create CSS styling for the UI
#' @noRd
create_css_styling <- function() {
  tags$head(
    tags$style(HTML('#loadpage-proceed1{background-color:orange}')),
    tags$style(HTML('#loadpage-reset1{background-color:orange}')),
    tags$link(rel = "stylesheet", type = "text/css", href = "assets/style.css")
  )
}

#' Create main selection controls (biological question, label type, file type)
#' @noRd
create_main_selection_controls <- function(ns) {
  tagList(
    # Biological Question
    radioButtons(ns("BIO"),
                 label = h4("1. Biological Question", class = "icon-wrapper",
                             icon("question-circle", lib = "font-awesome"),
                             div("Select the biological question of interest.", class = "icon-tooltip")),
                 c("Protein"="Protein", "Peptide"="Peptide","PTM"="PTM")
    ),
    
    # Label Type
    radioButtons(ns("DDA_DIA"),
                 label = h4("2. Label Type", class = "icon-wrapper",
                             icon("question-circle", lib = "font-awesome"),
                             div("Label-free will process all label-free acquisitions including DDA/DIA/SRM/PRM.", class = "icon-tooltip")),
                 c("Label-Free"="LType", "TMT"="TMT")
    ),
    
    # File Type
    radioButtons(ns("filetype"),
                 label = uiOutput(ns("filetype_header")),
                 choices = LOADPAGE_FILETYPE_CHOICES,
                 selected = character(0)
    )
  )
}

#' Create label-free type selection (visibility driven server-side).
#' @noRd
create_label_free_type_selection <- function(ns) {
  shinyjs::hidden(div(
    id = ns(NAMESPACE_LOADPAGE$label_free_type_selection_panel),
    radioButtons(ns(NAMESPACE_LOADPAGE$label_free_type),
                 label = h4("4. Type of Label-Free type", class = "icon-wrapper",
                             icon("question-circle", lib = "font-awesome"),
                             div("Choose the spectral processing tool used to process your data", class = "icon-tooltip")),
                 choices = c("DDA" = "DDA", "DIA" ="DIA", "SRM/PRM" ="SRM_PRM"),
                 selected = character(0)
    )
  ))
}

#' Create all file upload sections
#' @noRd
create_file_upload_sections <- function(ns) {
  tagList(
    # Standard quantification uploads
    create_standard_uploads(ns),
    
    # MSstats format uploads
    create_msstats_uploads(ns),
    
    # Skyline uploads
    create_skyline_uploads(ns),
    
    # DIANN uploads
    create_diann_uploads(ns),
    
    # Spectronaut uploads
    create_spectronaut_uploads(ns),
    
    # PTM FragPipe uploads
    create_ptm_fragpipe_uploads(ns),
    
    # MaxQuant uploads
    create_maxquant_uploads(ns),
    
    # PTM uploads (MaxQuant, PD, Spectronaut, Skyline)
    create_ptm_uploads(ns),
    
    # DIA-Umpire uploads
    create_ump_uploads(ns),
    
    # MZmine uploads (metabolomics)
    create_mzmine_uploads(ns),

    # Standard annotation uploads
    create_standard_annotation_uploads(ns)
  )
}

#' Create standard quantification file uploads (visibility driven server-side).
#' @noRd
create_standard_uploads <- function(ns) {
  shinyjs::hidden(div(
    id = ns(NAMESPACE_LOADPAGE$standard_quant_upload_panel),
    h4("4. Upload quantification dataset"),
    fileInput(ns('data'), "", multiple = FALSE, accept = NULL)
  ))
}

#' Create standard annotation file uploads (visibility driven server-side).
#' @noRd
create_standard_annotation_uploads <- function(ns) {
  shinyjs::hidden(div(
    id = ns(NAMESPACE_LOADPAGE$standard_annot_upload_panel),
    h4("5. Upload annotation File", class = "icon-wrapper",
       icon("question-circle", lib = "font-awesome"),
       div("Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.", class = "icon-tooltip")),
    fileInput(ns('annot'), "", multiple = FALSE, accept = c(".csv"))
  ))
}

#' Create MSstats format file uploads (visibility driven server-side).
#' @noRd
create_msstats_uploads <- function(ns) {
  tagList(
    # Regular MSstats format
    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$msstats_regular_upload_panel),
      h4("4. Upload data in MSstats Format"),
      fileInput(ns('msstatsdata'), "", multiple = FALSE, accept = NULL)
    )),

    # PTM MSstats format.
    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$msstats_ptm_upload_panel),
      h4("4. Upload PTM data in MSstats Format"),
      fileInput(ns('msstatsptmdata'), "", multiple = FALSE, accept = NULL),

      h4("5. (Optional) Upload unmodified data in MSstats Format"),
      fileInput(ns('unmod'), "", multiple = FALSE, accept = NULL),
      tags$br()
    ))
  )
}

#' Create Skyline file uploads (visibility driven server-side).
#' @noRd
create_skyline_uploads <- function(ns) {
  shinyjs::hidden(div(
    id = ns(NAMESPACE_LOADPAGE$skyline_upload_panel),
    h4("4. Upload MSstats report from Skyline"),
    fileInput(ns('skylinedata'), "", multiple = FALSE, accept = NULL)
  ))
}

#' Create MZmine file uploads (metabolomics; visibility driven server-side).
#' @noRd
create_mzmine_uploads <- function(ns) {
  shinyjs::hidden(div(
    id = ns(NAMESPACE_LOADPAGE$mzmine_upload_panel),
    fileInput(ns("mzmine_input"),
              h5("MZmine feature quant table", class = "icon-wrapper",
                 icon("question-circle", lib = "font-awesome"),
                 div("The feature intensity table exported from MZmine (features x samples).",
                     class = "icon-tooltip"))),
    fileInput(ns("mzmine_annotation"),
              h5("Annotation file", class = "icon-wrapper",
                 icon("question-circle", lib = "font-awesome"),
                 div("Maps each run/sample to its condition and bioreplicate.",
                     class = "icon-tooltip"))),
    fileInput(ns("mzmine_annotations"),
              h5("MZmine compound annotations", class = "icon-wrapper",
                 icon("question-circle", lib = "font-awesome"),
                 div("MZmine's feature-to-compound identifications.",
                     class = "icon-tooltip"))),
    fileInput(ns("sirius_annotations"),
              h5("SIRIUS annotations (optional, recommended)", class = "icon-wrapper",
                 icon("question-circle", lib = "font-awesome"),
                 div("Optional SIRIUS structure identifications; recommended to improve compound naming.",
                     class = "icon-tooltip")))
  ))
}

#' Create DIANN file uploads
#'
#' Mirrors the Spectronaut layout (`create_spectronaut_uploads`): a stack
#' of `uiOutput()` slots that the server renders conditionally based on
#' `input$filetype == 'diann'` and the `big_file_diann` mode toggle.
#' @noRd
create_diann_uploads <- function(ns) {
  tagList(
    uiOutput(ns("diann_header_ui")),
    uiOutput(ns("diann_file_selection_ui")),
    uiOutput(ns("diann_options_ui"))
  )
}

#' Create DIANN header
#' @noRd
create_diann_header <- function() {
  h4("4. Upload MSstats report from DIANN")
}

#' Create DIANN mode selector (Local only)
#' @noRd
create_diann_mode_selector <- function(ns, selected = FALSE) {
  checkboxInput(ns("big_file_diann"), "Large file mode", value = selected)
}

#' Create DIANN standard file input
#' @noRd
create_diann_standard_ui <- function(ns) {
  fileInput(ns('dianndata'), "", multiple = FALSE, accept = NULL)
}

#' Create DIANN large file selection UI
#' @noRd
create_diann_large_file_ui <- function(ns) {
  tagList(
    shinyFiles::shinyFilesButton(ns("big_diann_browse"), "Browse for local file...", "Please select a file", multiple = FALSE),
    verbatimTextOutput(ns("dianndata_big_path"))
  )
}

#' Create DIANN large file filter / cutoff options
#'
#' Exposes `bigDIANNtoMSstatsFormat`'s converter knobs: MBR, three q-value
#' cutoffs (global / precursor / protein-group), and the quantification
#' column (reuses the same default the regular DIANN 1.x path's
#' `intensity_column` defaults to).
#' @noRd
create_diann_large_filter_options <- function(ns,
                                              mbr_def = TRUE,
                                              quantcol_def = "FragmentQuantCorrected",
                                              global_qv_def = 0.01,
                                              qv_def = 0.01,
                                              pg_qv_def = 0.01) {
  tagList(
    tags$hr(),
    h4("Options for large file processing"),
    checkboxInput(ns("big_diann_MBR"), "MBR Enabled", value = mbr_def),
    textInput(ns("big_diann_quantification_column"),
              h5("Quantification column", class = "icon-wrapper",
                 icon("question-circle", lib = "font-awesome"),
                 div("Column in the DIANN report to use as the intensity measure. Use 'auto' for DIANN 2.0+ (per-fragment columns); otherwise the legacy column name (default: FragmentQuantCorrected).",
                     class = "icon-tooltip")),
              value = quantcol_def),
    numericInput(ns("big_diann_global_qvalue_cutoff"),
                 "Global Q-value cutoff", value = global_qv_def, min = 0, max = 1, step = 0.01),
    numericInput(ns("big_diann_qvalue_cutoff"),
                 "Q-value cutoff", value = qv_def, min = 0, max = 1, step = 0.01),
    numericInput(ns("big_diann_pg_qvalue_cutoff"),
                 "Protein group Q-value cutoff", value = pg_qv_def, min = 0, max = 1, step = 0.01)
  )
}

#' Create DIANN large file options (feature processing)
#' @noRd
create_diann_large_bottom_ui <- function(ns,
                                         max_feature_def = 100,
                                         unique_peps_def = FALSE,
                                         agg_psms_def = FALSE,
                                         few_obs_def = FALSE,
                                         backend_def = "arrow") {
  tagList(
    numericInput(ns("big_diann_max_feature_count"), "Max feature count",
                 value = max_feature_def, min = 1),
    checkboxInput(ns("big_diann_filter_unique_peptides"), "Use unique peptides",
                  value = unique_peps_def),
    checkboxInput(ns("big_diann_aggregate_psms"), "Aggregate PSMs to peptides",
                  value = agg_psms_def),
    checkboxInput(ns("big_diann_filter_few_obs"), "Filter features with few observations",
                  value = few_obs_def),
    selectInput(ns("big_diann_backend"), "Backend",
                choices = c("arrow", "sparklyr"),
                selected = backend_def)
  )
}

#' Create DIANN large file annotation override + anomaly UI
#'
#' Renders an optional annotation upload that overrides DIANN's embedded
#' Run / Condition / BioReplicate columns, plus the "Calculate Anomaly
#' Scores" controls. `bigDIANNtoMSstatsFormat` accepts the annotation
#' data frame directly via its `annotation` argument.
#'
#' Anomaly scoring is a two-step pipeline in the large-file path:
#'   (1) `bigDIANNtoMSstatsFormat` runs with `calculateAnomalyScores = TRUE`
#'       and `anomalyModelFeatures = c("Ms1ProfileCorr", "Evidence", "RT",
#'       "Predicted.RT")`, which carries those columns through the
#'       out-of-memory reduce/preprocess steps.
#'   (2) After `dplyr::collect`, `DeltaRT = RT - Predicted.RT` is
#'       engineered in-memory and `MSstatsConvert::MSstatsAnomalyScores`
#'       fits the isolation-forest model on
#'       `c("Ms1ProfileCorr", "Evidence", "DeltaRT")` to produce the
#'       `AnomalyScores` column.
#'
#' A run-order CSV is required (Run + Order columns) — `MSstatsAnomalyScores`
#' uses it for temporal feature engineering.
#' @noRd
create_diann_large_annotation_ui <- function(ns, calculate_anomaly_def = FALSE) {
  tagList(
    tags$hr(),
    h5("Annotation file (optional)",
       class = "icon-wrapper",
       icon("question-circle", lib = "font-awesome"),
       div("Upload a CSV/TSV with columns Run, BioReplicate, Condition (and any extras). When supplied, the converter merges it on Run and overrides any Condition / BioReplicate values from DIANN's embedded annotation. Required for paired designs and other layouts the report itself cannot express.",
           class = "icon-tooltip")),
    fileInput(ns("big_diann_annotation"), label = NULL,
              multiple = FALSE, accept = c(".csv", ".tsv", ".txt")),
    checkboxInput(ns("big_diann_calculate_anomaly_scores"),
                  label = tags$span(
                    "Calculate Anomaly Scores",
                    class = "icon-wrapper",
                    icon("question-circle", lib = "font-awesome"),
                    div("Carries Ms1ProfileCorr, Evidence, RT, and Predicted.RT through the out-of-memory steps, then engineers DeltaRT = RT - Predicted.RT in-memory after collect and fits MSstatsConvert::MSstatsAnomalyScores on c(Ms1ProfileCorr, Evidence, DeltaRT). Requires a run order CSV.",
                        class = "icon-tooltip")),
                  value = calculate_anomaly_def),
    # Big-file-path anomaly run-order fileInput, emitted by this helper (called
    # from the diann_options_ui renderUI) only when the checkbox is ticked —
    # the same renderUI-gated pattern as the other three anomaly spots. The
    # upload is dropped on a big_file / converter / checkbox toggle (accepted).
    if (isTRUE(calculate_anomaly_def)) {
      fileInput(ns(NAMESPACE_LOADPAGE$big_diann_run_order_file),
                label = h5("Upload Run Order File",
                           class = "icon-wrapper",
                           icon("question-circle", lib = "font-awesome"),
                           div("CSV with two columns: 'Run' (sequence name matching the converter output) and 'Order' (chronological run number, e.g. 1, 2, 3...).",
                               class = "icon-tooltip")),
                multiple = FALSE, accept = c(".csv"))
    }
  )
}

#' Create Spectronaut file uploads
#' @noRd
create_spectronaut_uploads <- function(ns) {
  tagList(
    uiOutput(ns("spectronaut_header_ui")),
    uiOutput(ns("spectronaut_file_selection_ui")),
    uiOutput(ns("spectronaut_options_ui")),
    uiOutput(ns("spectronaut_intensity_ui")),
    uiOutput(ns("spectronaut_turnover_ui"))
  )
}

#' Create Spectronaut header
#' @noRd
create_spectronaut_header <- function() {
  h4("4. Upload MSstats scheme output from Spectronaut")
}

#' Create Spectronaut mode selector (Local only)
#' @noRd
create_spectronaut_mode_selector <- function(ns, selected = FALSE) {
  checkboxInput(ns("big_file_spec"), "Large file mode", value = selected)
}

#' Create Spectronaut standard file input
#' @noRd
create_spectronaut_standard_ui <- function(ns) {
  fileInput(ns('specdata'), "", multiple = FALSE, accept = NULL)
}

#' Create Spectronaut large file selection UI
#' @noRd
create_spectronaut_large_file_ui <- function(ns) {
  tagList(
    shinyFiles::shinyFilesButton(ns("big_file_browse"), "Browse for local file...", "Please select a file", multiple = FALSE),
    verbatimTextOutput(ns("specdata_big_path"))
  )
}

#' Create Spectronaut large file filter options
#' @noRd
create_spectronaut_large_filter_options <- function(ns, excluded_def = FALSE, identified_def = FALSE, qval_def = TRUE) {
  tagList(
    tags$hr(),
    h4("Options for large file processing"),
    checkboxInput(ns("filter_by_excluded"), "Filter by excluded from quantification", value = excluded_def),
    checkboxInput(ns("filter_by_identified"), "Filter by identified", value = identified_def),
    checkboxInput(ns("filter_by_qvalue"), "Filter by q-value", value = qval_def)
  )
}

#' Create Spectronaut Q-value cutoff input
#' @noRd
create_spectronaut_qvalue_cutoff_ui <- function(ns, cutoff_def = 0.01) {
  numericInput(ns("qvalue_cutoff"), "Q-value cutoff", value = cutoff_def, min = 0, max = 1, step = 0.01)
}

#' Create Spectronaut large file options (Bottom part)
#' @noRd
create_spectronaut_large_bottom_ui <- function(ns, max_feature_def = 20, unique_peps_def = FALSE, agg_psms_def = FALSE, few_obs_def = FALSE) {
  tagList(
    numericInput(ns("max_feature_count"), "Max feature count", value = max_feature_def, min = 1),
    checkboxInput(ns("filter_unique_peptides"), "Use unique peptides", value = unique_peps_def),
    checkboxInput(ns("aggregate_psms"), "Aggregate PSMs to peptides", value = agg_psms_def),
    checkboxInput(ns("filter_few_obs"), "Filter features with few observations", value = few_obs_def)
  )
}

#' Create Spectronaut large file annotation override + anomaly UI
#'
#' Invoked from `output$spectronaut_options_ui` (server-side renderUI) only
#' when `big_file_spec == TRUE` and `is_web_server == FALSE`. It declares
#' `ns("calculate_anomaly_scores")` / `ns("run_order_file")` — the same ids the
#' regular Spectronaut path emits from `output$spectronaut_anomaly_ui`. Both
#' copies are renderUI-gated on mutually exclusive `big_file_spec` states, so
#' they never coexist in the DOM and the shared ns() ids never collide. The
#' run-order fileInput is emitted only when the checkbox is ticked (no
#' conditionalPanel), so its upload is dropped on any rebuild or checkbox toggle.
#'
#' @noRd
create_spectronaut_large_annotation_ui <- function(ns, calculate_anomaly_def = FALSE) {
  tagList(
    tags$hr(),
    h5("Annotation file (optional)",
       class = "icon-wrapper",
       icon("question-circle", lib = "font-awesome"),
       div("Upload a CSV/TSV with columns Run, BioReplicate, Condition (and any extras). When supplied, the converter merges it on Run and overrides any Condition / BioReplicate values from Spectronaut's R.Condition / R.Replicate. Required for paired designs and other layouts Spectronaut's own annotation cannot express.",
           class = "icon-tooltip")),
    fileInput(ns("big_spec_annotation"), label = NULL,
              multiple = FALSE, accept = c(".csv", ".tsv", ".txt")),
    checkboxInput(ns("calculate_anomaly_scores"),
                  label = tags$span(
                    "Calculate Anomaly Scores",
                    class = "icon-wrapper",
                    icon("question-circle", lib = "font-awesome"),
                    div("Runs the same anomaly scoring pipeline as the regular Spectronaut path: the converter carries FG.ShapeQualityScore (MS2)/(MS1) and EGDeltaRT through the out-of-memory steps, then MSstatsConvert::MSstatsAnomalyScores fits the isolation-forest model on the collected data and adds an AnomalyScores column. Requires a run order CSV.",
                        class = "icon-tooltip")),
                  value = calculate_anomaly_def),
    # Run-order fileInput emitted only when the checkbox is ticked (renderUI-
    # gated, no conditionalPanel); the upload is dropped on any rebuild.
    if (isTRUE(calculate_anomaly_def)) {
      fileInput(ns("run_order_file"),
                label = h5("Upload Run Order File",
                           class = "icon-wrapper",
                           icon("question-circle", lib = "font-awesome"),
                           div("CSV with two columns: 'Run' (sequence name matching the converter output) and 'Order' (chronological run number, e.g. 1, 2, 3...).",
                               class = "icon-tooltip")),
                multiple = FALSE, accept = c(".csv"))
    }
  )
}

#' Create the DIANN regular-path anomaly UI: the Calculate Anomaly Scores
#' checkbox and, when it is ticked, the nested run-order fileInput. Emitted by
#' `output$diann_anomaly_ui` (renderUI) so it mounts only on the regular DIANN
#' path (filetype == 'diann' && !big_file_diann) — the same renderUI-gated
#' pattern as `create_spectronaut_anomaly_ui`. The run-order upload is dropped
#' on a converter switch, big_file toggle, or checkbox toggle (accepted
#' tradeoff). Uses the `diann_*` ns() ids (distinct from the big-file
#' `big_diann_*` ids), so there is no cross-path collision.
#' @noRd
create_diann_anomaly_ui <- function(ns, calculate_anomaly_def = FALSE) {
  tagList(
    checkboxInput(ns(NAMESPACE_LOADPAGE$diann_calculate_anomaly_scores),
                  label = tags$span(
                    "Calculate Anomaly Scores",
                    class = "icon-wrapper",
                    icon("question-circle", lib = "font-awesome"),
                    div("Engineers DeltaRT = RT - Predicted.RT in the raw DIANN report, then calls MSstatsConvert::MSstatsAnomalyScores via DIANNtoMSstatsFormat with quality_metrics c(Ms1ProfileCorr, Evidence, DeltaRT) and temporal directions c(mean_decrease, mean_decrease, dispersion_increase). Requires a run order CSV.",
                        class = "icon-tooltip")
                  ),
                  value = calculate_anomaly_def),
    if (isTRUE(calculate_anomaly_def)) {
      fileInput(ns(NAMESPACE_LOADPAGE$diann_run_order_file),
                label = h5("Upload Run Order File", class = "icon-wrapper",
                           icon("question-circle", lib = "font-awesome"),
                           div("CSV with two columns: 'Run' (sequence name matching the DIANN report's Run column) and 'Order' (chronological run number, e.g. 1, 2, 3...).", class = "icon-tooltip")),
                multiple = FALSE, accept = c(".csv"))
    }
  )
}

#' Create the Spectronaut regular-path anomaly UI: the Calculate Anomaly Scores
#' checkbox + its nested run-order fileInput. Emitted by
#' `output$spectronaut_anomaly_ui` (renderUI) so it mounts only on the regular
#' path (filetype == 'spec' && !big_file_spec); the big-file copy comes from
#' `create_spectronaut_large_annotation_ui`. renderUI gating keeps the two from
#' coexisting, so their shared `ns("calculate_anomaly_scores")` /
#' `ns("run_order_file")` ids never collide. The run-order fileInput is emitted
#' only when the checkbox is ticked (no conditionalPanel), so its upload is
#' dropped on a converter switch, big_file toggle, or checkbox toggle (accepted).
#' @noRd
create_spectronaut_anomaly_ui <- function(ns, calculate_anomaly_def = FALSE) {
  tagList(
    checkboxInput(ns("calculate_anomaly_scores"),
                  label = tags$span(
                    "Calculate Anomaly Scores",
                    class = "icon-wrapper",
                    icon("question-circle", lib = "font-awesome"),
                    div("Calculate anomaly scores for each feature based on a random forest model. This requires a CSV file containing the order of your MS runs.",
                        class = "icon-tooltip")
                  ),
                  value = calculate_anomaly_def),
    if (isTRUE(calculate_anomaly_def)) {
      fileInput(ns("run_order_file"),
                label = h5("Upload Run Order File", class = "icon-wrapper",
                           icon("question-circle", lib = "font-awesome"),
                           div("The run order file should be a CSV with two columns: 'Run' and 'Order'. 'Run' contains the sequence name, and 'Order' contains the chronological run number (e.g., 1, 2, 3...).", class = "icon-tooltip")),
                multiple = FALSE, accept = c(".csv"))
    }
  )
}

#' Create PTM FragPipe uploads (visibility driven server-side).
#' @noRd
create_ptm_fragpipe_uploads <- function(ns) {
  shinyjs::hidden(div(
    id = ns(NAMESPACE_LOADPAGE$ptm_fragpipe_upload_panel),
    h4("4. Upload PTM msstats dataset"),
    fileInput(ns('ptmdata'), "", multiple = FALSE, accept = NULL),

    h4("5. Upload PTM annotation file"),
    fileInput(ns('annotation'), "", multiple = FALSE, accept = c(".csv")),

    h4("6. Upload global profiling msstats dataset (optional)"),
    fileInput(ns('globaldata'), "", multiple = FALSE, accept = NULL),

    h4("7. Upload global profiling annotation file (optional)"),
    fileInput(ns('globalannotation'), "", multiple = FALSE, accept = c(".csv")),

    h4("Select the options for pre-processing"),
    textInput(ns("mod_id_col"),
              h5("Please enter the name of the modification id column", class = "icon-wrapper",
                 icon("question-circle", lib = "font-awesome"),
                 div("Only part of the string is required. For example if your mod id column is named 'STY.1221.12' you only need to enter 'STY' here.", class = "icon-tooltip")),
              value = "STY"),

    textInput(ns("localization_cutoff"),
              h5("Please enter the localization_cutoff", class = "icon-wrapper",
                 icon("question-circle", lib = "font-awesome"),
                 div("The probability cutoff used to determine if a modification should be marked or not. If a site cannot be localized it may be dropped depending on the option below.", class = "icon-tooltip")),
              value = ".75"),

    radioButtons(ns("remove_unlocalized_peptides"),
                 h5("Remove unlocalized peptides", class = "icon-wrapper",
                    icon("question-circle", lib = "font-awesome"),
                    div("Should peptides without all sites localized be kept or removed.", class = "icon-tooltip")),
                 c(Yes=TRUE, No=FALSE), inline=TRUE)
  ))
}

#' Create MaxQuant file uploads (visibility driven server-side).
#' @noRd
create_maxquant_uploads <- function(ns) {
  shinyjs::hidden(div(
    id = ns(NAMESPACE_LOADPAGE$maxquant_upload_panel),
    h4("4. Upload evidence.txt File"),
    fileInput(ns('evidence'), "", multiple = FALSE, accept = NULL),

    h4("5. Upload proteinGroups.txt File"),
    fileInput(ns('pGroup'), "", multiple = FALSE, accept = NULL),

    h4("6. Upload annotation File", class = "icon-wrapper",
       icon("question-circle", lib = "font-awesome"),
       div("Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.", class = "icon-tooltip")),
    fileInput(ns('annot1'), "", multiple = FALSE, accept = c(".csv"))
  ))
}

#' Create modification ID selector UI for Metamorpheus PTM
#'
#' @param ns Namespace function.
#' @param mod_choices Character vector of modification IDs extracted from preview,
#'   or empty character(0) if no preview data is available.
#' @return A tagList with either a dropdown + optional text input, or a fallback text input.
#' @noRd
create_meta_mod_id_selector <- function(ns, mod_choices = character(0)) {
  if (length(mod_choices) > 0) {
    choices <- c(mod_choices, "Other" = "__other__")
    tagList(
      h4("Modification IDs", class = "icon-wrapper",
         icon("question-circle", lib = "font-awesome"),
         div("Select the modification ID to filter for PTMs. Select Other to manually enter a custom ID pattern.",
             class = "icon-tooltip")),
      selectizeInput(ns("mod_id_meta_select"),
                     label = NULL,
                     choices = choices,
                     selected = NULL,
                     multiple = FALSE,
                     options = list(placeholder = "Select a modification...")),
      uiOutput(ns("mod_id_meta_other_input"))
    )
  } else {
    tagList(
      h4("Modification IDs", class = "icon-wrapper",
         icon("question-circle", lib = "font-awesome"),
         div("Enter the modification ID pattern to filter for PTMs (e.g. phosphorylation pattern from Metamorpheus output).",
             class = "icon-tooltip")),
      textInput(ns("mod_id_meta_custom"), label = NULL, value = "")
    )
  }
}

#' Create PTM file uploads (for MaxQuant, PD, Spectronaut, Skyline, Metamorpheus).
#' Visibility driven server-side. Redundant TMT clauses in the original JS
#' conditions collapse away (`BIO=='PTM' || (BIO=='PTM' && DDA_DIA=='TMT')`
#' is just `BIO=='PTM'`).
#' @noRd
create_ptm_uploads <- function(ns) {
  tagList(
    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$ptm_uploads_panel),
      h4("4. Upload PTM Input File"),
      fileInput(ns('ptm_input'), "", multiple = FALSE, accept = NULL),

      h4("5. Upload annotation File", class = "icon-wrapper",
         icon("question-circle", lib = "font-awesome"),
         div("Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.", class = "icon-tooltip")),
      fileInput(ns('ptm_annot'), "", multiple = FALSE, accept = c(".csv")),

      h4("6. Upload fasta File", class = "icon-wrapper",
         icon("question-circle", lib = "font-awesome"),
         div("Upload FASTA file. This file allows us to identify where in the protein sequence a modification occurs.", class = "icon-tooltip")),
      fileInput(ns('fasta'), "", multiple = FALSE),

      h4("7. (Recommended) Upload Unmodified Protein Input File"),
      fileInput(ns('ptm_protein_input'), "", multiple = FALSE, accept = NULL)
    )),

    # MaxQuant specific PTM
    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$ptm_maxquant_pgroup_panel),
      h4("8. (Optional) Upload Unmodified Protein proteinGroups.txt File"),
      fileInput(ns('ptm_pgroup'), "", multiple = FALSE, accept = NULL)
    )),

    # Metamorpheus specific PTM
    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$ptm_metamorpheus_extras_panel),
      h4("8. (Recommended) Upload Unmodified Protein Annotation File"),
      fileInput(
        ns("ptm_protein_annot"),
        "",
        multiple = FALSE,
        accept = c(".csv", ".tsv")
      ),

      uiOutput(ns("mod_id_meta_ui"))
    )),

    # PTM modification labels
    create_ptm_modification_labels(ns),

    # FASTA file column name
    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$ptm_fasta_id_column_panel),
      h4("FASTA file column name", class = "icon-wrapper",
         icon("question-circle", lib = "font-awesome"),
         div("Name of column in FASTA file that matches with Protein name column in input. It is critical the values in both columns match so that the modfication can be identified.", class = "icon-tooltip")),
      textInput(ns("fasta_id_column"), "", value="uniprot_iso")
    ))
  )
}

#' Create PTM modification label inputs (visibility driven server-side).
#' These three panels are mutually exclusive at runtime — one per converter.
#' The original JS conditions had a redundant `|| (BIO=='PTM' && DDA_DIA=='TMT')`
#' clause that the server predicates fold away to `BIO=='PTM' && filetype==<x>`.
#' @noRd
create_ptm_modification_labels <- function(ns) {
  tagList(
    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$ptm_mod_id_maxq_panel),
      h4("Modification Label", class = "icon-wrapper",
         icon("question-circle", lib = "font-awesome"),
         div("Indicate if experiment was processed using TMT labeling", class = "icon-tooltip")),
      textInput(ns("mod_id_maxq"), "", value="\\(Phospho \\(STY\\)\\)")
    )),

    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$ptm_mod_id_pd_panel),
      h4("Modification Label", class = "icon-wrapper",
         icon("question-circle", lib = "font-awesome"),
         div("Indicate if experiment was processed using TMT labeling", class = "icon-tooltip")),
      textInput(ns("mod_id_pd"), "", value="\\(Phospho\\)")
    )),

    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$ptm_mod_id_spec_panel),
      h4("Modification Label", class = "icon-wrapper",
         icon("question-circle", lib = "font-awesome"),
         div("Indicate if experiment was processed using TMT labeling", class = "icon-tooltip")),
      textInput(ns("mod_id_spec"), "", value="\\[Phospho \\(STY\\)\\]")
    ))
  )
}

#' Create DIA-Umpire file uploads (visibility driven server-side).
#' @noRd
create_ump_uploads <- function(ns) {
  shinyjs::hidden(div(
    id = ns(NAMESPACE_LOADPAGE$dia_umpire_upload_panel),
    h4("4. Upload FragSummary.xls File"),
    fileInput(ns('fragSummary'), "", multiple = FALSE, accept = NULL),

    h4("5. Upload PeptideSummary.xls File"),
    fileInput(ns('peptideSummary'), "", multiple = FALSE, accept = NULL),

    h4("6. Upload ProtSummary.xls File"),
    fileInput(ns('protSummary'), "", multiple = FALSE, accept = NULL),

    h4("7. Upload Annotation File", class = "icon-wrapper",
       icon("question-circle", lib = "font-awesome"),
       div("Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.", class = "icon-tooltip")),
    fileInput(ns('annot2'), "", multiple = FALSE, accept = c(".csv"))
  ))
}

#' Create processing options
#' @noRd
create_processing_options <- function(ns) {
  tagList(
    tags$hr(),
    
    # TMT processing options
    create_tmt_options(ns),
    
    # Label-free processing options
    create_label_free_options(ns)
  )
}

#' Create TMT processing options (rendered server-side).
#'
#' The previous code declared `ns("which.proteinid")` in two mutually
#' exclusive `conditionalPanel`s with different defaults (PD ->
#' "Protein.Accessions", MaxQuant -> "Proteins"). Mounting both as hidden
#' divs would collide on a single ns() id. The single
#' `output[[tmt_options_ui]]` renderUI in R/loadpage-server-converter-options-panel.R
#' replaces both panels — it emits one textInput with the converter-
#' appropriate default on first build and carries the user's typed value
#' across filetype flips via isolate().
#' @noRd
create_tmt_options <- function(ns) {
  uiOutput(ns(NAMESPACE_LOADPAGE$tmt_options_ui))
}

#' Create label-free processing options (visibility driven server-side).
#' @noRd
create_label_free_options <- function(ns) {
  tagList(
    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$label_free_options_panel),
      h4("Select the options for pre-processing"),
      checkboxInput(ns("unique_peptides"), "Use unique peptides", value = TRUE),
      checkboxInput(ns("remove"), "Remove proteins with 1 feature", value = FALSE),
      # Quality filtering options
      create_quality_filtering_options(ns)
    )),

    # DIANN specific options — visibility driven server-side
    # (R/loadpage-server-converter-options-panel.R::register_loadpage_visibility_observers).
    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$diann_lf_options_panel),
      checkboxInput(ns(NAMESPACE_LOADPAGE$diann_2plus), "DIANN 2.0+", value = FALSE),
      shinyjs::hidden(div(
        id = ns(NAMESPACE_LOADPAGE$diann_intensity_column_panel),
        textInput(ns(NAMESPACE_LOADPAGE$intensity_column),
                  h5("Intensity Column Name", class = "icon-wrapper",
                     icon("question-circle", lib = "font-awesome"),
                     div("Enter the column name containing intensity values for DIANN versions prior to 2.0", class = "icon-tooltip")),
                  value = "FragmentQuantCorrected")
      )),
      uiOutput(ns("diann_turnover_ui"))
    ))
  )
}

#' Create quality filtering options
#' @noRd
create_quality_filtering_options <- function(ns) {
  tagList(
    # Q-value filter (Skyline / Spectronaut / DIANN regular) — visibility
    # driven server-side. MBR is a DIANN-only sub-checkbox inside the cutoff
    # block. State must persist across visibility flips, so we use nested
    # hidden divs and observers, not renderUI.
    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$qval_filter_panel),
      checkboxInput(ns(NAMESPACE_LOADPAGE$q_val), "Filter with Q-value"),
      shinyjs::hidden(div(
        id = ns(NAMESPACE_LOADPAGE$qval_cutoff_panel),
        shinyjs::hidden(div(
          id = ns(NAMESPACE_LOADPAGE$qval_mbr_panel),
          checkboxInput(ns(NAMESPACE_LOADPAGE$mbr), "MBR Enabled", value = FALSE)
        )),
        numericInput(ns(NAMESPACE_LOADPAGE$q_cutoff), "Q-value cutoff", 0.01, 0, 1, 0.01)
      ))
    )),
    
    # Spectronaut regular-path anomaly scoring (Calculate Anomaly Scores
    # checkbox + nested run-order fileInput). Emitted server-side by
    # `output$spectronaut_anomaly_ui` so it mounts only on the regular path
    # (filetype == 'spec' && !big_file_spec); the big-file path emits its own
    # copy from `output$spectronaut_options_ui`. renderUI keeps the two copies
    # from coexisting, so their shared `ns("calculate_anomaly_scores")` /
    # `ns("run_order_file")` ids never collide.
    uiOutput(ns("spectronaut_anomaly_ui")),

    # DIANN regular-path anomaly scoring. Emitted server-side by
    # `output$diann_anomaly_ui` (renderUI) so it mounts only on the regular
    # DIANN path (filetype == 'diann' && !big_file_diann) — the same renderUI
    # pattern as the Spectronaut regular anomaly UI above. The run-order
    # fileInput is emitted by the renderUI when the checkbox is ticked.
    uiOutput(ns("diann_anomaly_ui")),
    
    # OpenSWATH M-score filter — visibility driven server-side. The nested
    # cutoff numeric must stay mounted across `m_score` toggles to preserve
    # the user's value; predicate AND-includes the parent `filetype=='open'`
    # so swapping converter hides the inner div even if `m_score` is still
    # TRUE.
    shinyjs::hidden(div(
      id = ns(NAMESPACE_LOADPAGE$openswath_mscore_panel),
      checkboxInput(ns(NAMESPACE_LOADPAGE$m_score), "Filter with M-score"),
      shinyjs::hidden(div(
        id = ns(NAMESPACE_LOADPAGE$openswath_mscore_cutoff_panel),
        numericInput(ns("m_cutoff"), "M-score cutoff", 0.01, 0, 1, 0.01)
      ))
    ))
  )
}
