library(shiny)
library(shinyBS)
library(shinyjs)
library(shinybusy)
library(DT)
library(htmltools)
library(uuid)
library(Hmisc)
library(dplyr)
library(data.table)
library(tidyr)
library(MSstats)
library(MSstatsTMT)
library(MSstatsConvert)
library(MSstatsPTM)

#####################################################

shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize=10000*1024^2) 
  session$allowReconnect(TRUE)
  observe({
    toggleClass(condition = TRUE,
                class = "disabled",
                selector = "#tablist li a[data-value='Data processing']")
  })
  
  observeEvent(input$Design, {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Future")
  })
  
  observeEvent(input$StartPipeline, {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Uploaddata")
  })
  
  source("panels/loadpage-server.R", local = TRUE)

  observeEvent(input$proceed, {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Uploaddata")
  })

  # data preprocessing
  source("panels/qc-server.R", local = TRUE)
  # statistical model
  source("panels/statmodel-server.R", local = TRUE)
  # future experiment
  source("panels/expdes-server.R", local = TRUE)
  
  statmodel= reactiveFileReader(1000, session, "panels/statmodel-ui.R", source)
  output$statmodel = renderUI(statmodel())
 
  observe({
    
    if(input$DDA_DIA %in% c("TMT", "PTM")){
      hideTab(inputId = "tablist", target = "PQ")
      hideTab(inputId = "tablist", target = "Future")
    }
    
    if(!(input$DDA_DIA %in% c("TMT", "PTM"))){
      showTab(inputId = "tablist", target = "PQ")
      showTab(inputId = "tablist", target = "Future")
    }
  })
  
  observeEvent(input$Reset, {
    refresh()
  })
}
)
