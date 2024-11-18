networkUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
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
    ),
  dashboardPage(
    dashboardHeader(title = "Protein Interaction Network"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Network Settings", tabName = "networkSettings", icon = icon("sliders-h")),
        menuItem("Network Visualization", tabName = "networkVisualization", icon = icon("project-diagram"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "networkSettings",
          fluidRow(
            box(
              title = "Data Upload and Settings",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fileInput(ns("dataUpload"), "Upload CSV File", accept = c(".csv")),
              radioButtons(ns("proteinIdType"), "Protein ID Type",
                           choices = list("Uniprot Mnemonic" = "Uniprot_Mnemonic", "Uniprot" = "Uniprot"),
                           selected = "Uniprot_Mnemonic"),
              sliderInput(ns("pValue"), "P Value", min = 0, max = 1, value = 0.5),
              sliderInput(ns("logFC"), "Log Fold Change (logFC)", min = 0, max = 10, value = 5),
              actionButton(ns("showNetwork"), "Display Network", class = "btn-primary")
            )
          )
        ),
        tabItem(
          tabName = "networkVisualization",
          fluidRow(
            box(
              title = "Network Visualization",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              tags$div(id = ns("cy"),
                style = "width: 100%; height: 500px; display: flex; justify-content: center; align-items: center;"
              )
            ),
            box(
              title = "Edges Table",
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              DTOutput(ns("edgesTable"))
            )
          )
        )
      )
    )
  ))
}
