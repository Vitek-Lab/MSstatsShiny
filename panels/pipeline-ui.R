
## Load tabs
source("panels/uploaddata-ui.R", local = T)
source("panels/qc-ui.R", local = T)
source("panels/pq-ui.R", local = T)
source("panels/statmodel-ui.R", local = T)
source("panels/report-ui.R", local = T)

pipeline <- fluidPage(
  
  navbarPage(title = "tabs",
             id = 'tabsset',
             tabPanel("1. Upload Data", icon = icon("send"), loadpage),
             tabPanel("2. Data processing",value = "DataProcessing", icon = icon("gears"), qc),
             tabPanel("3. Protein quantification", value = "PQ",icon = icon("calculator"), pq),
             tabPanel("4. Statistical model", value = "StatsModel", icon = icon("magic"), statmodel),
             tabPanel("Download logfile", value = "Download", icon = icon("download"), report)
  ),
  
  headerPanel("MSstats Pipeline"),
  
  sidebarLayout(
    ## Main panel ------------------------------------------------------------------
    mainPanel(
    
      # Output: Tabset w/ plot, summary, and table ----
      #tabsetPanel(type = "tabs",

      #)
    ),
    sidebarPanel(
      h2("How to Use"),
      p("Please follow the subtabs in the order indicated by the numbering. Once a \
          step is finished you can click the orange 'Next Step' button to proceed. If \
          you need to go back to an old tab, simply click the subtab you want to go \
          back to.")
    )
  ),
  
  
)
