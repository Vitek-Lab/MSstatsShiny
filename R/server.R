server <- function(input, output, session) {
  options(shiny.maxRequestSize=10000*1024^2)
  session$allowReconnect(TRUE)
  observe({
    toggleClass(condition = TRUE,
                class = "disabled",
                selector = "#tablist li a[data-value='Data processing']")
  })
  
  observeEvent(input$"statmodel-Design", {
    updateTabsetPanel(session = session, inputId = "tablist", 
                      selected = "Future")
  })
  observeEvent(input$"home-StartPipeline", {
    updateTabsetPanel(session = session, inputId = "tablist", 
                      selected = "Uploaddata")
  })
  
  loadpage_values <- loadpageServer("loadpage", parent_session = session)
  loadpage_input = loadpage_values$input
  get_data = loadpage_values$getData
  
  qc_values <- callModule(qcServer, "qc",session, reactive(loadpage_input),get_data)
  qc_input = qc_values$input
  preprocess_data = qc_values$preprocessData
  
  statmodel_values <- callModule(statmodelServer, "statmodel",session, 
                                reactive(loadpage_input),reactive(qc_input),get_data,preprocess_data)
  statmodel_input = statmodel_values$input
  data_comparison = statmodel_values$dataComparison
  
  callModule(expdesServer, "expdes",session, reactive(loadpage_input),
             reactive(qc_input),reactive(statmodel_input),data_comparison)
  observeEvent(input$proceed, {
    updateTabsetPanel(session = session, inputId = "tablist", 
                      selected = "Uploaddata")
  })
  
  # statmodel= reactiveFileReader(1000, session, "panels/statmodel-ui.R", source)
  # output$statmodel = renderUI(statmodel())
  # 
  observe({
    if(input$"loadpage-DDA_DIA" %in% c("TMT") && input$"loadpage-BIO" %in% c("PTM") ){
      hideTab(inputId = "tablist", target = "PQ")
      hideTab(inputId = "tablist", target = "Future")
    }
    
    if(!(input$"loadpage-DDA_DIA" %in% c("TMT")) && !(input$"loadpage-BIO" %in% c("PTM"))){
      showTab(inputId = "tablist", target = "PQ")
      showTab(inputId = "tablist", target = "Future")
    }
  })
  
  observeEvent(input$"home-Reset", {
    refresh()
  })
}