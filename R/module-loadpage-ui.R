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
      
      # Header content
      create_header_content(),
      
      tags$br(),
      
      # Conditional sample dataset descriptions
      create_sample_dataset_descriptions(),
      
      tags$br(),
      
      sidebarPanel(
        # CSS styling
        create_css_styling(),
        
        # Main selection controls
        create_main_selection_controls(ns),
        
        tags$hr(),
        
        # Label-free type selection
        create_label_free_type_selection(ns),
        
        tags$hr(),
        
        # File upload sections
        create_file_upload_sections(ns),
        
        # Processing options
        create_processing_options(ns),
        
        # Action button
        disabled(actionButton(inputId = ns("proceed1"), label = "Upload Data"))
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
    p("Note: files must be in CSV/TSV format, or Parquet (.parquet/.pq) for DIANN 2.0+ inputs, and under 250 MB when using msstatsshiny.com (no limit when running locally)."),
    p("Some users may have trouble uploading files while using the application via Google Chrome. If the 'Browse...' button does not work please try a different web browser.")
  )
}

#' Create conditional descriptions for sample datasets
#' @noRd
create_sample_dataset_descriptions <- function() {
  tagList(
    conditionalPanel(
      condition = "input['loadpage-filetype'] == 'sample' && input['loadpage-LabelFreeType'] == 'DDA'",
      p("The sample dataset for DDA acquisition is taken from the publication ",
        a("Choi, M. et al.  ABRF Proteome Informatics Research Group (iPRG) 2015 Study: Detection of Differentially Abundant Proteins in Label-Free Quantitative LC MS/MS Experiments. Journal of Proteome Research 16.2 (2016): 945-957. ",
          href = "https://pubs.acs.org/doi/10.1021/acs.jproteome.6b00881",
          target = "_blank"))
    ),
    conditionalPanel(
      condition = "input['loadpage-filetype'] == 'sample' && input['loadpage-LabelFreeType'] == 'DIA'",
      p("The sample dataset for DIA acquisition is taken from the publication ",
        a("Selevsek, N. et al. Reproducible and Consistent Quantification of the Saccharomyces Cerevisiae Proteome by SWATH-Mass Spectrometry. Molecular & Cellular Proteomics: MCP 14.3 (2015): 739-749. ", 
          href = "http://www.mcponline.org/content/14/3/739.long", 
          target="_blank"))
    ),
    conditionalPanel(
      condition = "input['loadpage-filetype'] == 'sample' && input['loadpage-LabelFreeType'] == 'SRM_PRM'",
      p("The sample dataset for SRM/PRM acquisition is taken from the publication ",
        a("Picotti, P. et al. Full dynamic range proteome analysis of S. cerevisiae by targeted proteomics. Cell (2009), 138, 795-806.", 
          href = "http://www.cell.com/cell/fulltext/S0092-8674(09)00715-6", 
          target="_blank"))
    )
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
                 label = h4("3. Type of File", class = "icon-wrapper", 
                             icon("question-circle", lib = "font-awesome"),
                             div("Choose the spectral processing tool used to process your data", class = "icon-tooltip")),
                 choices = c("Example dataset" = "sample",
                             "MSstats Format" = "msstats",
                             "Skyline" = "sky", "MaxQuant" = "maxq",
                             "Progenesis" = "prog", "Proteome Discoverer" = "PD",
                             "OpenMS" = "openms", "Spectronaut" = "spec",
                             "OpenSWATH" = "open", "DIA-Umpire" = "ump",
                             "SpectroMine" = "spmin", "FragPipe" = "phil", "DIANN"="diann"),
                 selected = character(0)
    )
  )
}

#' Create label-free type selection
#' @noRd
create_label_free_type_selection <- function(ns) {
  conditionalPanel(
    condition="input['loadpage-BIO'] != 'PTM' && input['loadpage-filetype'] == 'sample' && input['loadpage-DDA_DIA'] == 'LType'", 
    radioButtons(ns("LabelFreeType"),
                 label = h4("4. Type of Label-Free type", class = "icon-wrapper", 
                             icon("question-circle", lib = "font-awesome"),
                             div("Choose the spectral processing tool used to process your data", class = "icon-tooltip")),
                 choices = c("DDA" = "DDA", "DIA" ="DIA", "SRM/PRM" ="SRM_PRM"),
                 selected = character(0)
    )
  )
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
    
    # Standard annotation uploads
    create_standard_annotation_uploads(ns)
  )
}

#' Create standard quantification file uploads
#' @noRd
create_standard_uploads <- function(ns) {
  conditionalPanel(
    condition = "(input['loadpage-filetype'] =='10col' || input['loadpage-filetype'] =='prog' || input['loadpage-filetype'] =='PD' || input['loadpage-filetype'] =='open'||
                   input['loadpage-filetype'] =='openms' || input['loadpage-filetype'] =='spmin' || input['loadpage-filetype'] == 'phil') && input['loadpage-BIO'] != 'PTM'",
    h4("4. Upload quantification dataset"),
    fileInput(ns('data'), "", multiple = FALSE, accept = NULL),
    create_separator_buttons(ns, "sep_data")
  )
}

#' Create standard annotation file uploads
#' @noRd
create_standard_annotation_uploads <- function(ns) {
  conditionalPanel(
    condition = "(input['loadpage-filetype'] == 'sky' || input['loadpage-filetype'] == 'prog' || input['loadpage-filetype'] == 'PD' || input['loadpage-filetype'] == 'spec' || input['loadpage-filetype'] == 'open'|| input['loadpage-filetype'] =='spmin' || input['loadpage-filetype'] == 'phil' || input['loadpage-filetype'] == 'diann') && input['loadpage-BIO'] != 'PTM'",
    h4("5. Upload annotation File", class = "icon-wrapper", 
       icon("question-circle", lib = "font-awesome"),
       div("Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.", class = "icon-tooltip")),
    fileInput(ns('annot'), "", multiple = FALSE, accept = c(".csv"))
  )
}

#' Create MSstats format file uploads
#' @noRd
create_msstats_uploads <- function(ns) {
  tagList(
    # Regular MSstats format
    conditionalPanel(
      condition = "input['loadpage-filetype'] == 'msstats' && (input['loadpage-BIO'] != 'PTM' && (input['loadpage-BIO'] != 'PTM' && input['loadpage-DDA_DIA'] != 'TMT'))",
      h4("4. Upload data in MSstats Format"),
      fileInput(ns('msstatsdata'), "", multiple = FALSE, accept = NULL),
      create_separator_buttons(ns, "sep_msstatsdata")
    ),
    
    # PTM MSstats format
    conditionalPanel(
      condition = "input['loadpage-filetype'] == 'msstats' && (input['loadpage-BIO'] == 'PTM' || (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] == 'TMT'))",
      h4("4. Upload PTM data in MSstats Format"),
      fileInput(ns('msstatsptmdata'), "", multiple = FALSE, accept = NULL),
      create_separator_buttons(ns, "sep_msstatsptmdata"),
      
      h4("5. (Optional) Upload unmodified data in MSstats Format"),
      fileInput(ns('unmod'), "", multiple = FALSE, accept = NULL),
      tags$br()
    )
  )
}

#' Create Skyline file uploads
#' @noRd
create_skyline_uploads <- function(ns) {
  conditionalPanel(
    condition = "input['loadpage-filetype'] == 'sky' && input['loadpage-BIO'] != 'PTM'",
    h4("4. Upload MSstats report from Skyline"),
    fileInput(ns('skylinedata'), "", multiple = FALSE, accept = NULL),
    create_separator_buttons(ns, "sep_skylinedata")
  )
}

#' Create DIANN file uploads
#' @noRd
create_diann_uploads <- function(ns) {
  conditionalPanel(
    condition = "input['loadpage-filetype'] == 'diann' && input['loadpage-BIO'] != 'PTM'",
    h4("4. Upload MSstats report from DIANN"),
    fileInput(ns('dianndata'), "", multiple = FALSE, accept = NULL),
    create_separator_buttons(ns, "sep_dianndata")
  )
}

#' Create Spectronaut file uploads
#' @noRd
create_spectronaut_uploads <- function(ns) {
  conditionalPanel(
    condition = "input['loadpage-filetype'] == 'spec' && input['loadpage-BIO'] != 'PTM'",
    h4("4. Upload MSstats scheme output from Spectronaut"),

    # Checkbox to switch between upload methods
    checkboxInput(ns("big_file_spec"), "Use local file browser (for large files)"),

    # Standard fileInput, shown when checkbox is NOT checked
    conditionalPanel(
      condition = "!input['loadpage-big_file_spec']",
      fileInput(ns('specdata'), "", multiple = FALSE, accept = NULL)
    ),

    # Local browser button, shown when checkbox IS checked
    conditionalPanel(
      condition = "input['loadpage-big_file_spec']",
      shinyFiles::shinyFilesButton(ns("specdata_big_browse"), "Browse for local file...", "Please select a file", multiple = FALSE),
      verbatimTextOutput(ns("specdata_big_path"))
    ),

    create_separator_buttons(ns, "sep_specdata")
  )
}

#' Create PTM FragPipe uploads
#' @noRd
create_ptm_fragpipe_uploads <- function(ns) {
  conditionalPanel(
    condition = "input['loadpage-filetype'] == 'phil' && input['loadpage-BIO'] == 'PTM'",
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
                 div("Only part of the string is required. For example if your mod id column is named 'STY:1221.12' you only need to enter 'STY' here.", class = "icon-tooltip")),
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
  )
}

#' Create MaxQuant file uploads
#' @noRd
create_maxquant_uploads <- function(ns) {
  conditionalPanel(
    condition = "input['loadpage-filetype'] == 'maxq' && input['loadpage-BIO'] != 'PTM' && (input['loadpage-DDA_DIA'] == 'TMT' || input['loadpage-DDA_DIA'] == 'LType')",
    h4("4. Upload evidence.txt File"),
    fileInput(ns('evidence'), "", multiple = FALSE, accept = NULL),
    
    h4("5. Upload proteinGroups.txt File"),
    fileInput(ns('pGroup'), "", multiple = FALSE, accept = NULL),
    
    h4("6. Upload annotation File", class = "icon-wrapper", 
       icon("question-circle", lib = "font-awesome"),
       div("Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.", class = "icon-tooltip")),
    fileInput(ns('annot1'), "", multiple = FALSE, accept = c(".csv"))
  )
}

#' Create PTM file uploads (for MaxQuant, PD, Spectronaut, Skyline)
#' @noRd
create_ptm_uploads <- function(ns) {
  tagList(
    conditionalPanel(
      condition = "(input['loadpage-filetype'] == 'maxq' || input['loadpage-filetype'] == 'PD' || input['loadpage-filetype'] == 'spec' || input['loadpage-filetype'] == 'sky') && (input['loadpage-BIO'] == 'PTM' || (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] == 'TMT'))",
      h4("4. Upload PTM input.txt File"),
      fileInput(ns('ptm_input'), "", multiple = FALSE, accept = NULL),
      
      h4("5. Upload annotation File", class = "icon-wrapper", 
         icon("question-circle", lib = "font-awesome"),
         div("Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.", class = "icon-tooltip")),
      fileInput(ns('ptm_annot'), "", multiple = FALSE, accept = c(".csv")),
      
      h4("6. Upload fasta File", class = "icon-wrapper", 
         icon("question-circle", lib = "font-awesome"),
         div("Upload FASTA file. This file allows us to identify where in the protein sequence a modification occurs.", class = "icon-tooltip")),
      fileInput(ns('fasta'), "", multiple = FALSE),
      
      h4("7. (Optional) Upload Unmodified Protein input.txt File"),
      fileInput(ns('ptm_protein_input'), "", multiple = FALSE, accept = NULL)
    ),
    
    # MaxQuant specific PTM
    conditionalPanel(
      condition = "(input['loadpage-filetype'] == 'maxq') && (input['loadpage-BIO'] == 'PTM' || (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] == 'TMT'))",
      h4("8. (Optional) Upload Unmodified Protein proteinGroups.txt File"),
      fileInput(ns('ptm_pgroup'), "", multiple = FALSE, accept = NULL)
    ),
    
    # PTM modification labels
    create_ptm_modification_labels(ns),
    
    # FASTA file column name
    conditionalPanel(
      condition = "(input['loadpage-filetype'] == 'maxq' || input['loadpage-filetype'] == 'PD' || input['loadpage-filetype'] == 'spec' || input['loadpage-filetype'] == 'sky') && (input['loadpage-BIO'] == 'PTM' || (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] == 'TMT'))",
      h4("FASTA file column name", class = "icon-wrapper", 
         icon("question-circle", lib = "font-awesome"),
         div("Name of column in FASTA file that matches with Protein name column in input. It is critical the values in both columns match so that the modfication can be identified.", class = "icon-tooltip")),
      textInput(ns("fasta_id_column"), "", value="uniprot_iso")
    )
  )
}

#' Create PTM modification label inputs
#' @noRd
create_ptm_modification_labels <- function(ns) {
  tagList(
    conditionalPanel(
      condition = "(input['loadpage-filetype'] == 'maxq') && (input['loadpage-BIO'] == 'PTM' || (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] == 'TMT'))",
      h4("Modification Label", class = "icon-wrapper", 
         icon("question-circle", lib = "font-awesome"),
         div("Indicate if experiment was processed using TMT labeling", class = "icon-tooltip")),
      textInput(ns("mod_id_maxq"), "", value="\\(Phospho \\(STY\\)\\)")
    ),
    
    conditionalPanel(
      condition = "(input['loadpage-filetype'] == 'PD') && (input['loadpage-BIO'] == 'PTM' || (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] == 'TMT'))",
      h4("Modification Label", class = "icon-wrapper", 
         icon("question-circle", lib = "font-awesome"),
         div("Indicate if experiment was processed using TMT labeling", class = "icon-tooltip")),
      textInput(ns("mod_id_pd"), "", value="\\(Phospho\\)")
    ),
    
    conditionalPanel(
      condition = "(input['loadpage-filetype'] == 'spec') && (input['loadpage-BIO'] == 'PTM' || (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] == 'TMT'))",
      h4("Modification Label", class = "icon-wrapper", 
         icon("question-circle", lib = "font-awesome"),
         div("Indicate if experiment was processed using TMT labeling", class = "icon-tooltip")),
      textInput(ns("mod_id_spec"), "", value="\\[Phospho \\(STY\\)\\]")
    )
  )
}

#' Create DIA-Umpire file uploads
#' @noRd
create_ump_uploads <- function(ns) {
  conditionalPanel(
    condition = "input['loadpage-filetype'] == 'ump'",
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
  )
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

#' Create TMT processing options
#' @noRd
create_tmt_options <- function(ns) {
  tagList(
    conditionalPanel(
      condition = "input['loadpage-filetype'] && input['loadpage-DDA_DIA'] == 'TMT' && input['loadpage-filetype'] == 'PD'",
      h4("Select the options for pre-processing"),
      textInput(ns("which.proteinid"), 
                h5("Protein Name Column", class = "icon-wrapper", 
                   icon("question-circle", lib = "font-awesome"),
                   div("Enter the column in your data containing protein names", class = "icon-tooltip")),
                value = "Protein.Accessions")
    ),
    
    conditionalPanel(
      condition = "input['loadpage-filetype'] && input['loadpage-DDA_DIA'] == 'TMT' && input['loadpage-filetype'] == 'maxq'",
      h4("Select the options for pre-processing"),
      textInput(ns("which.proteinid"), 
                h5("Protein Name Column", class = "icon-wrapper", 
                   icon("question-circle", lib = "font-awesome"),
                   div("Enter the column in your data containing protein names", class = "icon-tooltip")),
                value = "Proteins")
    )
  )
}

#' Create label-free processing options
#' @noRd
create_label_free_options <- function(ns) {
  tagList(
    conditionalPanel(
      condition = "input['loadpage-filetype'] && input['loadpage-DDA_DIA'] == 'LType' && input['loadpage-filetype'] != 'sample' && input['loadpage-filetype'] != 'MRF'",
      h4("Select the options for pre-processing"),
      checkboxInput(ns("unique_peptides"), "Use unique peptides", value = TRUE),
      checkboxInput(ns("remove"), "Remove proteins with 1 peptide and charge", value = FALSE)
    ),
    
    conditionalPanel(
      condition = "input['loadpage-filetype'] && input['loadpage-DDA_DIA'] == 'LType' && input['loadpage-filetype'] != 'sample'",
      checkboxInput(ns("remove"), "Remove proteins with 1 feature", value = FALSE),
      
      # Quality filtering options
      create_quality_filtering_options(ns)
    ),
    
    # DIANN specific options
    conditionalPanel(
      condition = "input['loadpage-filetype'] == 'diann' && input['loadpage-DDA_DIA'] == 'LType'",
      checkboxInput(ns("diann_2plus"), "DIANN 2.0+", value = TRUE),
      conditionalPanel(
        condition = "!input['loadpage-diann_2plus']",
        textInput(ns("intensity_column"), 
                  h5("Intensity Column Name", class = "icon-wrapper", 
                     icon("question-circle", lib = "font-awesome"),
                     div("Enter the column name containing intensity values for DIANN versions prior to 2.0", class = "icon-tooltip")),
                  value = "FragmentQuantCorrected")
      )
    )
  )
}

#' Create quality filtering options
#' @noRd
create_quality_filtering_options <- function(ns) {
  tagList(
    conditionalPanel(
      condition = "input['loadpage-filetype'] == 'sky' || input['loadpage-filetype'] == 'spec'|| input['loadpage-filetype'] == 'diann'",
      checkboxInput(ns("q_val"), "Filter with Q-value"),
      conditionalPanel(
        condition = "input['loadpage-q_val']",
        conditionalPanel(
          condition = "input['loadpage-filetype'] == 'diann'",
          checkboxInput(ns("MBR"), "MBR Enabled", value = FALSE)
        ),
        numericInput(ns("q_cutoff"), "Q-value cutoff", 0.01, 0, 1, 0.01)
      )
    ),
    
    conditionalPanel(
      condition = "input['loadpage-filetype'] == 'spec'",
      checkboxInput(ns("calculate_anomaly_scores"), 
                    label = tags$span(
                      "Calculate Anomaly Scores",
                      class = "icon-wrapper",
                      icon("question-circle", lib = "font-awesome"),
                      div("Calculate anomaly scores for each feature based on a random forest model. This requires a CSV file containing the order of your MS runs.", 
                          class = "icon-tooltip")
                    ), 
                    value = FALSE),
      conditionalPanel(
        condition = "input['loadpage-calculate_anomaly_scores']",
        fileInput(ns("run_order_file"), 
                  label = h5("Upload Run Order File", class = "icon-wrapper",
                             icon("question-circle", lib = "font-awesome"),
                             div("The run order file should be a CSV with two columns: 'Run' and 'Order'. 'Run' contains the sequence name, and 'Order' contains the chronological run number (e.g., 1, 2, 3...).", class = "icon-tooltip")),
                  multiple = FALSE, accept = c(".csv"))
      )
    ),
    
    conditionalPanel(
      condition = "input['loadpage-filetype'] == 'open'",
      checkboxInput(ns("m_score"), "Filter with M-score"),
      conditionalPanel(
        condition = "input['loadpage-m_score']",
        numericInput(ns("m_cutoff"), "M-score cutoff", 0.01, 0, 1, 0.01)
      )
    )
  )
}


#' Create column separator radio buttons
#' 
#' @param ns Namespace function
#' @param input_id ID for the radio button input
#' @noRd
create_separator_buttons <- function(ns, input_id) {
  radioButtons(ns(input_id),
               label = h5("Column separator in uploaded file", class = "icon-wrapper", 
                          icon("question-circle", lib = "font-awesome"),
                          div("Choose how columns are separated in the uploaded file", class = "icon-tooltip")),
               c(Comma=",", Semicolon=";", Tab="\t", Pipe="|"),
               inline = TRUE
  )
}