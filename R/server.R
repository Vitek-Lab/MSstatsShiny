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
# library(sqldf)

#####################################################

shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize=10000*1024^2)
  session$allowReconnect(TRUE)
  observe({
    toggleClass(condition = TRUE,
                class = "disabled",
                selector = "#tablist li a[data-value='Data processing']")
  })
  # browser()
  observeEvent(input$"statmodel-Design", {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Future")
  })
  observeEvent(input$"home-StartPipeline", {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Uploaddata")
  })

  loadpage_inputs <- loadpageServer("loadpage", parent_session = session)
  qc_inputs <- callModule(qcServer, "qc",session, loadpage_inputs)
  statmodel_inputs <- callModule(statmodelServer, "statmodel",session, loadpage_inputs,qc_inputs)
  callModule(expdesServer, "expdes",session, loadpage_inputs,qc_inputs,statmodel_inputs)
  observeEvent(input$proceed, {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Uploaddata")
  })

  # statmodel= reactiveFileReader(1000, session, "panels/statmodel-ui.R", source)
  # output$statmodel = renderUI(statmodel())
  # 
  observe({
    if(input$"loadpage-DDA_DIA" %in% c("TMT", "PTM")){
      hideTab(inputId = "tablist", target = "PQ")
      hideTab(inputId = "tablist", target = "Future")
    }

    if(!(input$"loadpage-DDA_DIA" %in% c("TMT", "PTM"))){
      showTab(inputId = "tablist", target = "PQ")
      showTab(inputId = "tablist", target = "Future")
    }
  })

  observeEvent(input$"home-Reset", {
    refresh()
  })
  
}
)
