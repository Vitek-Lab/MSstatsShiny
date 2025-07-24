# =============================================================================
# HELPER FUNCTIONS - External Dependencies
# =============================================================================

createCytoscapeScripts <- function() {
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.19.1/cytoscape.min.js"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/graphlib/2.1.8/graphlib.min.js"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/dagre/0.8.5/dagre.min.js"),
    tags$script(src = "https://unpkg.com/cytoscape-dagre@2.3.0/cytoscape-dagre.js"),
    tags$script("
            Shiny.addCustomMessageHandler('runCytoscape', function(message) {
                console.log(message)
                eval(message);  // Executes the Cytoscape.js code in the frontend
            });
        ")
  )
}

# =============================================================================
# HELPER FUNCTIONS - UI Components
# =============================================================================

createFileUploadInput <- function(ns) {
  conditionalPanel(
    condition = "!output.hasValidDataComparison",
    ns = ns,
    fileInput(ns("dataUpload"), 
              "Upload CSV File", 
              accept = c(".csv"), 
              buttonLabel = "Browse...", 
              placeholder = "No file selected")
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
  selectInput(ns("selectedLabel"),
              "Select Comparison",
              choices = NULL,  # Will be populated dynamically
              selected = NULL,
              multiple = FALSE)
}

createProteinIdRadioButtons <- function(ns) {
  radioButtons(ns("proteinIdType"), 
               "Protein ID Type",
               choices = list("Uniprot Mnemonic" = "Uniprot_Mnemonic", 
                              "Uniprot" = "Uniprot"),
               selected = "Uniprot")
}

createDisplayLabelRadioButtons <- function(ns) {
  radioButtons(ns("displayLabelType"),
               "Node Label Display",
               choices = list("Protein Name" = "id",
                              "Gene Name" = "hgncName"),
               selected = "id")
}

createParameterSliders <- function(ns) {
  tagList(
    sliderInput(ns("pValue"), 
                "Adjusted P-Value", 
                min = 0, max = 1, value = 0.05),
    sliderInput(ns("evidence"), 
                "Evidence Cutoff", 
                min = 0, max = 50, value = 5),
    sliderInput(ns("absLogFC"),
                "Absolute LogFC Cutoff",
                min = 0, max = 5, value = 0.5, step = 0.1)
  )
}

createFilterDropdowns <- function(ns) {
  tagList(
    selectInput(ns("statementTypes"),
                "Statement Types",
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
                multiple = TRUE),
    selectInput(ns("sources"),
                "Sources",
                choices = list("All Sources" = "all",
                               "Reach" = "reach",
                               "Trips" = "trips",
                               "Sparser" = "sparser",
                               "Medscan" = "medscan",
                               "TEES" = "tees",
                               "ISI" = "isi",
                               "Geneways" = "geneways",
                               "RLIMS-P" = "rlimsp",
                               "Eidos" = "eidos",
                               "GNBR" = "gnbr",
                               "SemRep" = "semrep",
                               "BEL" = "bel",
                               "BioPAX" = "biopax",
                               "SIGNOR" = "signor",
                               "BioGRID" = "biogrid",
                               "HPRD" = "hprd",
                               "TRRUST" = "trrust",
                               "PhosphoELM" = "phosphoelm",
                               "VirHostNet" = "virhostnet",
                               "OmniPath" = "omnipath",
                               "UbiBrowser" = "ubibrowser",
                               "ACSN" = "acsn",
                               "WormBase" = "wormbase",
                               "CTD" = "ctd",
                               "DrugBank" = "drugbank",
                               "DGI" = "dgi",
                               "TAS" = "tas",
                               "CROG" = "crog",
                               "CREEDS" = "creeds"),
                selected = "all",
                multiple = TRUE),
    div(
      style = "position: relative;",
      # Standard selectInput with normal label
      selectInput(ns("selectedProteins"),
                  "Force Include Proteins (optional)",
                  choices = NULL,  # Will be populated dynamically
                  selected = NULL,
                  multiple = TRUE),
      # Separate tooltip positioned over the label area
      div(
        class = "icon-wrapper",
        style = "position: absolute; top: 8px; right: 10px; z-index: 1000;",
        icon("question-circle", lib = "font-awesome"),
        div("Select specific proteins to force to include in the network query. These proteins will be included in the network analysis regardless of other filtering criteria. Leave empty to include all proteins that meet the filtering conditions.", 
            class = "icon-tooltip")
      )
    )
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
    createDisplayNetworkButton(ns)
  )
}

createColorLegend <- function() {
  div(
    style = "position: absolute; top: 10px; right: 10px; background-color: rgba(255, 255, 255, 0.9); 
             border: 1px solid #ddd; border-radius: 5px; padding: 10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); 
             z-index: 1000; min-width: 150px;",
    
    # Legend title
    div(
      style = "font-weight: bold; font-size: 12px; margin-bottom: 8px; text-align: center; color: #333;",
      "LogFC Legend"
    ),
    
    # Color gradient bar
    div(
      style = "height: 20px; width: 100%; background: linear-gradient(to right, #ADD8E6, #D3D3D3, #FFA590); 
               border: 1px solid #ccc; border-radius: 3px; margin-bottom: 5px;"
    ),
    
    # Labels container
    div(
      style = "display: flex; justify-content: space-between; font-size: 10px; color: #666;",
      tags$span("Downregulated", style = "font-weight: bold;"),
      tags$span("0", style = "font-weight: bold;"),
      tags$span("Upregulated", style = "font-weight: bold;")
    ),
    
    # Additional info
    div(
      style = "margin-top: 8px; font-size: 9px; color: #888; text-align: center; border-top: 1px solid #eee; padding-top: 5px;",
      "Node colors represent log fold change values"
    )
  )
}

createNetworkVisualizationBox <- function(ns) {
  box(
    title = "Network Visualization",
    status = "success",
    solidHeader = TRUE,
    width = 12,
    # Container with relative positioning for the legend
    div(
      style = "position: relative; width: 100%; height: 500px;",
      
      # Main network container
      tags$div(
        id = ns("cy"),
        style = "width: 100%; height: 100%; display: flex; justify-content: center; align-items: center;"
      ),
      
      # Color legend overlay
      createColorLegend()
    )
  )
}

createEdgesTableBox <- function(ns) {
  box(
    title = "Edges Table",
    status = "warning",
    solidHeader = TRUE,
    width = 12,
    div(style = "overflow-x: auto;",
        DTOutput(ns("edgesTable"))
    )
  )
}

createNodesTableBox <- function(ns) {
  box(
    title = "Nodes Table",
    status = "info",
    solidHeader = TRUE,
    width = 12,
    div(style = "overflow-x: auto;",
        DTOutput(ns("nodesTable"))
    )
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
  dashboardHeader(title = "Protein Interaction Network")
}

createDashboardBody <- function(ns) {
  dashboardBody(
    createNetworkSettingsTab(ns)
  )
}

# =============================================================================
# MAIN UI FUNCTION
# =============================================================================

#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody menuItem tabItems tabItem box
#' @importFrom DT DTOutput
networkUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    createCytoscapeScripts(),
    dashboardPage(
      createDashboardHeader(),
      dashboardSidebar(disable = TRUE),
      createDashboardBody(ns)
    )
  )
}
