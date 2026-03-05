# =============================================================================
# HELPER FUNCTIONS - UI Components
# =============================================================================

createFileUploadInput <- function(ns) {
  conditionalPanel(
    condition = "!output.hasValidDataComparison",
    ns = ns,
    div(
      tags$label(
        "Upload Differential Abundance Results:",
        class = "icon-wrapper",
        icon("question-circle", lib = "font-awesome"),
        div("Upload a CSV file with three columns: Protein (identifier), log2FC (log2 fold change), and adj.pvalue (adjusted p-value).", 
            class = "icon-tooltip")
      ),
      fileInput(ns("dataUpload"), 
                label = NULL,  # Remove since we're handling it above
                accept = c(".csv"), 
                buttonLabel = "Browse...", 
                placeholder = "No file selected")
    )
  )
}

createDataSourceInfo <- function(ns) {
  conditionalPanel(
    condition = "output.hasValidDataComparison",
    ns = ns,
    div(style = "padding: 10px; background-color: #d9edf7; border: 1px solid #bce8f1; border-radius: 4px; margin-bottom: 15px;",
        tags$i(class = "fa fa-info-circle", style = "color: #31708f; margin-right: 8px;"),
        tags$span("Using data from comparison analysis.", 
                  style = "color: #31708f; font-weight: bold;")
    )
  )
}

# Add this new helper function for the label dropdown
createLabelDropdown <- function(ns) {
  div(
    tags$label(
      "Select Comparison:",
      class = "icon-wrapper",
      icon("question-circle", lib = "font-awesome"),
      div("Choose which experimental comparison to analyze from your uploaded data.", 
          class = "icon-tooltip")
    ),
    selectInput(ns("selectedLabel"),
                label = NULL,  # Remove since we're handling it above
                choices = NULL,  # Will be populated dynamically
                selected = NULL,
                multiple = FALSE)
  )
}

createProteinIdRadioButtons <- function(ns) {
  div(
    tags$label(
      "Protein ID Type:",
      class = "icon-wrapper",
      icon("question-circle", lib = "font-awesome"),
      div("Select the type of protein identifier used in your data. Uniprot Mnemonic uses readable names (e.g., P53_HUMAN), while Uniprot uses alphanumeric codes (e.g., P04637).", 
          class = "icon-tooltip")
    ),
    radioButtons(ns("proteinIdType"), 
                 label = NULL,  # Remove since we're handling it above
                 choices = list("Uniprot Mnemonic" = "Uniprot_Mnemonic", 
                                "Uniprot" = "Uniprot"),
                 selected = "Uniprot")
  )
}

createDisplayLabelRadioButtons <- function(ns) {
  div(
    tags$label(
      "Node Label Display:",
      class = "icon-wrapper",
      icon("question-circle", lib = "font-awesome"),
      div("Choose how protein nodes are labeled in the network visualization. Protein Name shows the full protein name, Gene Name shows the HGNC gene symbol.", 
          class = "icon-tooltip")
    ),
    radioButtons(ns("displayLabelType"),
                 label = NULL,  # Remove since we're handling it above
                 choices = list("Protein Name" = "id",
                                "Gene Name" = "hgncName"),
                 selected = "id")
  )
}

createParameterSliders <- function(ns) {
  tagList(
    div(
      tags$label(
        "Adjusted P-Value:",
        class = "icon-wrapper",
        icon("question-circle", lib = "font-awesome"),
        div("Statistical significance threshold. Only proteins with adjusted p-values below this cutoff will be included in the network.", 
            class = "icon-tooltip")
      ),
      sliderInput(ns("pValue"), 
                  label = NULL,  # Remove since we're handling it above
                  min = 0, max = 1, value = 0.05)
    ),
    div(
      tags$label(
        "Absolute LogFC Cutoff:",
        class = "icon-wrapper",
        icon("question-circle", lib = "font-awesome"),
        div("Minimum absolute log fold change value for including proteins. Higher values focus on more dramatically changed proteins.", 
            class = "icon-tooltip")
      ),
      sliderInput(ns("absLogFC"),
                  label = NULL,  # Remove since we're handling it above
                  min = 0, max = 5, value = 0.5, step = 0.1)
    ),
    div(
      tags$label(
        "INDRA Evidence Cutoff:",
        class = "icon-wrapper",
        icon("question-circle", lib = "font-awesome"),
        div("Minimum number of supporting evidence lines required for including a protein regulatory relationship. Each count reflects a separate line of support, such as a sentence in a paper or an entry in a curated database, not necessarily distinct publications. Higher values increase confidence but may reduce network size.", 
            class = "icon-tooltip")
      ),
      sliderInput(ns("evidence"), 
                  label = NULL,  # Remove since we're handling it above
                  min = 0, max = 50, value = 5)
    )
  )
}

createFilterDropdowns <- function(ns) {
  tagList(
    div(
      tags$label(
        "INDRA Statement Types:",
        class = "icon-wrapper",
        icon("question-circle", lib = "font-awesome"),
        div("Filter regulatory relationships by biological mechanism type. Select specific regulation types or 'All Types' to include all available mechanisms.", 
            class = "icon-tooltip")
      ),
      selectInput(ns("statementTypes"),
                  label = NULL,  # Remove since we're handling it above
                  choices = list("All Types" = "all",
                                 "Complex" = "Complex",
                                 "Inhibition" = "Inhibition", 
                                 "Activation" = "Activation",
                                 "Increase Amount" = "IncreaseAmount",
                                 "Decrease Amount" = "DecreaseAmount",
                                 "Phosphorylation" = "Phosphorylation",
                                 "Dephosphorylation" = "Dephosphorylation",
                                 "Ubiquitination" = "Ubiquitination",
                                 "Deubiquitination" = "Deubiquitination",
                                 "Sumoylation" = "Sumoylation",
                                 "Desumoylation" = "Desumoylation",
                                 "Hydroxylation" = "Hydroxylation",
                                 "Dehydroxylation" = "Dehydroxylation",
                                 "Acetylation" = "Acetylation",
                                 "Deacetylation" = "Deacetylation",
                                 "Glycosylation" = "Glycosylation",
                                 "Deglycosylation" = "Deglycosylation",
                                 "Farnesylation" = "Farnesylation",
                                 "Defarnesylation" = "Defarnesylation",
                                 "Geranylgeranylation" = "Geranylgeranylation",
                                 "Degeranylgeranylation" = "Degeranylgeranylation",
                                 "Palmitoylation" = "Palmitoylation",
                                 "Depalmitoylation" = "Depalmitoylation",
                                 "Myristoylation" = "Myristoylation",
                                 "Demyristoylation" = "Demyristoylation",
                                 "Ribosylation" = "Ribosylation",
                                 "Deribosylation" = "Deribosylation",
                                 "Methylation" = "Methylation",
                                 "Demethylation" = "Demethylation"),
                  selected = "all",
                  multiple = TRUE)
    ),
    div(
      tags$label(
        "INDRA Sources:",
        class = "icon-wrapper",
        icon("question-circle", lib = "font-awesome"),
        div("Filter regulatory relationships by data source. Different sources use various methods to identify protein regulatory relationships (literature mining, manual curation, etc.).", 
            class = "icon-tooltip")
      ),
      selectInput(ns("sources"),
                  label = NULL,  # Remove since we're handling it above
                  choices = list("All Sources" = "all",
                                 "Text Mining Systems" = list(
                                   "REACH" = "reach",
                                   "TRIPS/DRUM" = "trips",
                                   "Sparser" = "sparser",
                                   "Eidos" = "eidos",
                                   "TEES" = "tees",
                                   "MedScan" = "medscan",
                                   "RLIMS-P" = "rlimsp",
                                   "ISI/AMR" = "isi",
                                   "Geneways" = "geneways",
                                   "GNBR" = "gnbr",
                                   "SemRep" = "semrep"
                                 ),
                                 "Curated Databases" = list(
                                   "BEL" = "bel",
                                   "BioPAX" = "biopax",
                                   "SIGNOR" = "signor",
                                   "BioGRID" = "biogrid",
                                   "HPRD" = "hprd",
                                   "TRRUST" = "trrust",
                                   "PhosphoELM" = "phosphoelm",
                                   "VirHostNet" = "virhostnet",
                                   "UbiBrowser" = "ubibrowser",
                                   "ACSN" = "acsn",
                                   "WormBase" = "wormbase",
                                   "CTD" = "ctd",
                                   "DrugBank" = "drugbank",
                                   "DGI" = "dgi",
                                   "TAS" = "tas",
                                   "CROG" = "crog",
                                   "CREEDS" = "creeds"
                                 )),
                  selected = "all",
                  multiple = TRUE)
    ),
    div(
      tags$label(
        "Force Include Proteins (optional):",
        class = "icon-wrapper",
        icon("question-circle", lib = "font-awesome"),
        div("Search for specific proteins to include in the network analysis regardless of other filtering criteria. As a hidden feature, you can also search by any biological agent, e.g. drugs, GO terms, etc!", 
            class = "icon-tooltip")
      ),
      # Container for selected proteins
      div(id = ns("selectedProteinsContainer"),
          style = "margin-bottom: 10px;",
          uiOutput(ns("selectedProteinsTags"))
      ),
      # Search input
      div(
        style = "display: flex; gap: 10px; align-items: flex-start;",
        textInput(ns("proteinSearchInput"),
                  label = NULL,
                  placeholder = "Type protein name or gene symbol...",
                  width = "70%"),
        actionButton(ns("proteinSearchButton"),
                     "Search",
                     icon = icon("search"),
                     style = "margin-top: 0px;")
      ),
      # Search results dropdown
      div(
        style = "margin-bottom: 15px;",  # Add margin below the results container
        uiOutput(ns("proteinSearchResults"))
      )
    )
  )
}

createCurationFilterCheckbox <- function(ns) {
  div(
    tags$label(
      "Filter Out Incorrect Statements:",
      class = "icon-wrapper",
      icon("question-circle", lib = "font-awesome"),
      div("When checked, excludes protein regulatory relationships that have been manually curated as incorrect in the INDRA database.", 
          class = "icon-tooltip")
    ),
    checkboxInput(ns("filterByCuration"),
                  label = "Filter out statements curated as incorrect",
                  value = FALSE)
  )
}

createDisplayNetworkButton <- function(ns) {
  tagList(
    actionButton(ns("showNetwork"), 
                 "Display Network", 
                 class = "btn-primary"),
    br(), br(),
    div(id = ns("loadingIndicator"),
        style = "display: none; text-align: center;",
        tags$i(class = "fa fa-spinner fa-spin fa-2x", style = "color: #3c8dbc;"),
        br(),
        tags$span("Processing network data...", style = "color: #3c8dbc; font-weight: bold;")
    )
  )
}

# =============================================================================
# HELPER FUNCTIONS - Box Components
# =============================================================================

createDataUploadBox <- function(ns) {
  box(
    title = "Data Upload and Settings",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    createDataSourceInfo(ns),
    createFileUploadInput(ns),
    createLabelDropdown(ns),  
    createProteinIdRadioButtons(ns),
    createDisplayLabelRadioButtons(ns),
    createParameterSliders(ns),
    createFilterDropdowns(ns),
    createCurationFilterCheckbox(ns),
    createDisplayNetworkButton(ns)
  )
}

createNetworkVisualizationBox <- function(ns) {
  box(
    title = "Network Visualization",
    status = "success",
    solidHeader = TRUE,
    width = 12,
    div(
      style = "width: 100%; height: 600px;",
      MSstatsBioNet::cytoscapeNetworkOutput(ns("network"), height = "600px")
    )
  )
}

createEdgesTableBox <- function(ns) {
  box(
    title = "Edges Table",
    status = "warning",
    solidHeader = TRUE,
    width = 12,
    DTOutput(ns("edgesTable"))
  )
}

createNodesTableBox <- function(ns) {
  box(
    title = "Nodes Table",
    status = "info",
    solidHeader = TRUE,
    width = 12,
    DTOutput(ns("nodesTable"))
  )
}

# =============================================================================
# HELPER FUNCTIONS - Tab Items
# =============================================================================

createNetworkSettingsTab <- function(ns) {
  fluidRow(
    # Left column - Settings
    column(width = 4,
           createDataUploadBox(ns)
    ),
    # Right column - Visualization and Tables
    column(width = 8,
           div(
             style = "text-align: center; width: 100%;",
             p(
               "Explore your differential abundance analysis results with respect to what is known in prior literature in the form of protein regulatory networks. If your differential abundance analysis results were not obtained through MSstatsShiny, you can upload your differential abundance analysis results as a CSV file.",
               style = "margin: 0;"
             )
           ),
           createDownloadBoxes(ns),
           createNetworkVisualizationBox(ns),
           createEdgesTableBox(ns),
           createNodesTableBox(ns)
    )
  )
}

# =============================================================================
# HELPER FUNCTIONS - Navigation
# =============================================================================

createDashboardHeader <- function() {
  dashboardHeader(
    title = div(
      style = "display: flex; align-items: center; flex-wrap: wrap;",
      h4(
        "Powered by ",
        style = "margin: 0; margin-right: 15px;",
        img(
          src = "https://raw.githubusercontent.com/gyorilab/indra_db/master/doc/indra_db_logo.png", # Replace with your actual logo path
          height = "30px",
          style = "vertical-align: middle; margin: 0 3px;"
        )
      )
    )
  )
}


createDashboardBody <- function(ns) {
  dashboardBody(
    createNetworkSettingsTab(ns)
  )
}

createDownloadBoxes <- function(ns) {
  div(
    style = "text-align: right; padding: 10px; display: flex; justify-content: flex-end; gap: 10px;",
    uiOutput(ns("network.html.button")),
    uiOutput(ns("network.code.button"))
    
  )
}

# =============================================================================
# MAIN UI FUNCTION
# =============================================================================

#' Network UI module for visualizing protein regulatory networks from INDRA.
#' @param id namespace prefix for the module
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody menuItem tabItems tabItem box
#' @importFrom DT DTOutput
#' @export
networkUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    dashboardPage(
      createDashboardHeader(),
      dashboardSidebar(disable = TRUE),
      createDashboardBody(ns)
    )
  )
}
