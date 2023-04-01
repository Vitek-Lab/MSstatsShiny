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

server <- function(input, output, session) {
  options(shiny.maxRequestSize=10000*1024^2)
  session$allowReconnect(TRUE)
  observe({
    toggleClass(condition = TRUE,
                class = "disabled",
                selector = "#tablist li a[data-value='Data processing']")
  })
  
  observeEvent(input$"statmodel-Design", {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Future")
  })
  observeEvent(input$"home-StartPipeline", {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Uploaddata")
  })
  
  loadpage_input <- loadpageServer("loadpage", parent_session = session)
  qc_input <- callModule(qcServer, "qc",session, reactive(loadpage_input))
  statmodel_input <- callModule(statmodelServer, "statmodel",session, reactive(loadpage_input),reactive(qc_input))
  callModule(expdesServer, "expdes",session, reactive(loadpage_input),reactive(qc_input),reactive(statmodel_input))
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

# shinyServer(function(input, output, session) {
#   options(shiny.maxRequestSize=10000*1024^2)
#   session$allowReconnect(TRUE)
#   observe({
#     toggleClass(condition = TRUE,
#                 class = "disabled",
#                 selector = "#tablist li a[data-value='Data processing']")
#   })
# 
#   observeEvent(input$"statmodel-Design", {
#     updateTabsetPanel(session = session, inputId = "tablist", selected = "Future")
#   })
#   observeEvent(input$"home-StartPipeline", {
#     updateTabsetPanel(session = session, inputId = "tablist", selected = "Uploaddata")
#   })
# 
#   loadpage_input <- loadpageServer("loadpage", parent_session = session)
#   qc_input <- callModule(qcServer, "qc",session, reactive(loadpage_input))
#   statmodel_input <- callModule(statmodelServer, "statmodel",session, reactive(loadpage_input),reactive(qc_input))
#   callModule(expdesServer, "expdes",session, reactive(loadpage_input),reactive(qc_input),reactive(statmodel_input))
#   observeEvent(input$proceed, {
#     updateTabsetPanel(session = session, inputId = "tablist", selected = "Uploaddata")
#   })
# 
#   # statmodel= reactiveFileReader(1000, session, "panels/statmodel-ui.R", source)
#   # output$statmodel = renderUI(statmodel())
#   # 
#   observe({
#     if(input$"loadpage-DDA_DIA" %in% c("TMT", "PTM")){
#       hideTab(inputId = "tablist", target = "PQ")
#       hideTab(inputId = "tablist", target = "Future")
#     }
# 
#     if(!(input$"loadpage-DDA_DIA" %in% c("TMT", "PTM"))){
#       showTab(inputId = "tablist", target = "PQ")
#       showTab(inputId = "tablist", target = "Future")
#     }
#   })
# 
#   observeEvent(input$"home-Reset", {
#     refresh()
#   })
#   
# }
# )
