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

radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
                                                 $(document).ready(function() {
                                                 setTimeout(function() {
                                                 $('input', $('#", id, "')).each(function(){
                                                 if(this.getAttribute('value') == '", choice, "') {
                                                 opts = $.extend(", options, ", {html: true});
                                                 $(this.parentElement).tooltip('destroy');
                                                 $(this.parentElement).tooltip(opts);
                                                 }
                                                 })
                                                 }, 500)
                                                 });
                                                 ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
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
  source("panels/utils.R", local = T)
  source("panels/home-ui.R", local = T)
  source("panels/pipeline-ui.R", local = T)
  source("panels/expdes-ui.R", local = T)
  source("panels/help-ui.R", local = T)
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
#  source("panels/home-server.R", local = T)
  
  observeEvent(input$StartPipeline, {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "StartPipeline")
  })
  
  observeEvent(input$Design, {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Future")
  })
  
  observeEvent(input$Help, {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Help")
  })
  
  onclick("reset1", {
    shinyjs::runjs("location.reload()")
  })
  
  observeEvent(input$proceed1, {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "StartPipeline")
  })
  
  
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

