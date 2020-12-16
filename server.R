options(shiny.maxRequestSize=2000*1024^2)
library(shiny)
library(MSstats)
library(shinyBS)
library(uuid)
library(shinyjs)
library(biomaRt)
library(Biobase)
library(DT)
library(plotly)
library(ggrepel)
library(marray)
library(gplots)
#library(STRINGdb)
if (FALSE) require("V8")
#library(MSnbase)
library(MSstatsBioData)

###### global functions ###########

xy_str <- function(e) {
  if(is.null(e)) return("NULL\n")
  paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
}



#####################################################


shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize=350*1024^2) 
  session$allowReconnect(TRUE)
  observe({
    toggleClass(condition = TRUE,
                class = "disabled",
                selector = "#tablist li a[data-value='Data processing']")
  })
  # load data
  source("panels/loadpage-server.R", local = T)
#  source("panels/home-server.R", local = T)
  
  observeEvent(input$proceed, {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Uploaddata")
  })
  
  onclick("reset1", {
    shinyjs::runjs("location.reload()")
  })
  
  observeEvent(input$proceed1, {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Uploaddata")
  })
  
  
  source("panels/utils.R", local = T)
  # data preprocessing
  source("panels/qc-server.R", local = T)
  # protein quantification
  source("panels/pq-server.R", local = T)
  # statistical model
  source("panels/statmodel-server.R", local = T)
  # functional analysis
#  source("panels/analysis-server.R", local = T)
  # clustering/classification
#  source("panels/clust-server.R", local = T)
  # future experiment
  source("panels/expdes-server.R", local = T)
  # report
  source("panels/report-server.R", local = T)
  
  # statmodel<- reactiveFileReader(1000, session, "panels/statmodel-ui.R", source)
  # output$statmodel <- renderUI(statmodel())
 
  observe({
    #currentTab <<- input$tablist
    #updateTabsetPanel(session = session, inputId = "tablist", selected = currentTab)
    
    if(input$DDA_DIA=="TMT"){
      hideTab(inputId = "tablist", target = "PQ")
      hideTab(inputId = "tablist", target = "Future")
    }
    
    if(input$DDA_DIA!="TMT"){
      showTab(inputId = "tablist", target = "PQ")
      showTab(inputId = "tablist", target = "Future")
    }
    
  })
  
  onclick("reset1", {
    shinyjs::runjs("location.reload()")
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Uploaddata")
  })
  

}
)

