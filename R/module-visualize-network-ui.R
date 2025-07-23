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
  fileInput(ns("dataUpload"), 
            "Upload CSV File", 
            accept = c(".csv"), 
            buttonLabel = "Browse...", 
            placeholder = "No file selected")
}

createProteinIdRadioButtons <- function(ns) {
  radioButtons(ns("proteinIdType"), 
               "Protein ID Type",
               choices = list("Uniprot Mnemonic" = "Uniprot_Mnemonic", 
                              "Uniprot" = "Uniprot"),
               selected = "Uniprot")
}

createParameterSliders <- function(ns) {
  tagList(
    sliderInput(ns("pValue"), 
                "P Value", 
                min = 0, max = 1, value = 0.05),
    sliderInput(ns("evidence"), 
                "Evidence Cutoff", 
                min = 0, max = 50, value = 5)
  )
}

createDisplayNetworkButton <- function(ns) {
  actionButton(ns("showNetwork"), 
               "Display Network", 
               class = "btn-primary")
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
    createFileUploadInput(ns),
    createProteinIdRadioButtons(ns),
    createParameterSliders(ns),
    createDisplayNetworkButton(ns)
  )
}

createNetworkVisualizationBox <- function(ns) {
  box(
    title = "Network Visualization",
    status = "success",
    solidHeader = TRUE,
    width = 12,
    tags$div(id = ns("cy"),
             style = "width: 100%; height: 500px; display: flex; justify-content: center; align-items: center;")
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
  tabItem(
    tabName = "networkSettings",
    fluidRow(
      createDataUploadBox(ns)
    )
  )
}

createNetworkVisualizationTab <- function(ns) {
  tabItem(
    tabName = "networkVisualization",
    fluidRow(
      createNetworkVisualizationBox(ns),
      createEdgesTableBox(ns),
      createNodesTableBox(ns)
    )
  )
}

# =============================================================================
# HELPER FUNCTIONS - Navigation
# =============================================================================

createSidebarMenu <- function() {
  sidebarMenu(
    menuItem("Network Settings", 
             tabName = "networkSettings", 
             icon = icon("sliders-h")),
    menuItem("Network Visualization", 
             tabName = "networkVisualization", 
             icon = icon("project-diagram"))
  )
}

createDashboardHeader <- function() {
  dashboardHeader(title = "Protein Interaction Network")
}

createDashboardSidebar <- function() {
  dashboardSidebar(
    createSidebarMenu()
  )
}

createDashboardBody <- function(ns) {
  dashboardBody(
    tabItems(
      createNetworkSettingsTab(ns),
      createNetworkVisualizationTab(ns)
    )
  )
}

# =============================================================================
# MAIN UI FUNCTION
# =============================================================================

#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody sidebarMenu menuItem tabItems tabItem box
#' @importFrom DT DTOutput
networkUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    createCytoscapeScripts(),
    dashboardPage(
      createDashboardHeader(),
      createDashboardSidebar(),
      createDashboardBody(ns)
    )
  )
}
