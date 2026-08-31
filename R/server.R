#' @title Server function for the MSstatsShiny app
#' @description This functions generates the Server object for MSstatsShiny app.
#' 
#' @param input shiny server input
#' @param output shiny server output
#' @param session session object for shiny to connect to
#' @return Server object for shinyUI 
#' @importFrom shinyjs toggleClass
#' @export
#' @examples
#' NA
#' 
server = function(input, output, session) {
  isWebServer = Sys.getenv("SHINY_ENV", "development") == "production"
  maxRequestSize = if (isWebServer) 250*1024^2 else 1000000*1024^2
  options(shiny.maxRequestSize=maxRequestSize)
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
  
  app_template = reactive({
    template <- input$app_template
    if (is.null(template) || !nzchar(template)) {
      TEMPLATES$default
    } else {
      template
    }
  })

  loadpage_values = loadpageServer("loadpage", parent_session = session, is_web_server = isWebServer, app_template = app_template)
  loadpage_input = loadpage_values$input
  get_data = loadpage_values$getData
  get_condition_metadata = loadpage_values$getConditionMetadata

  # qcServer - update to direct call if refactored, otherwise keep callModule for now
  qc_values = callModule(qcServer, "qc", session, reactive(loadpage_input), get_data,
                         app_template, get_condition_metadata)
  qc_input = qc_values$input
  preprocess_data = qc_values$preprocessData
  get_turnover_ratios = qc_values$turnoverRatios
  get_tracer_constants = qc_values$tracerConstants

  statmodel_values = statmodelServer(
    id = "statmodel",
    parent_session = session,
    loadpage_input = reactive(loadpage_input),
    qc_input = reactive(qc_input),
    get_data = get_data,
    preprocess_data = preprocess_data,
    app_template = app_template,
    turnover_ratios = get_turnover_ratios,
    condition_metadata = get_condition_metadata,
    tracer_constants = get_tracer_constants
  )
  statmodel_input = statmodel_values$input
  data_comparison = statmodel_values$dataComparison
  statmodel_contrast = statmodel_values$contrast
  
  # expdesServer - keep callModule if not yet refactored
  callModule(expdesServer, "expdes", session, reactive(loadpage_input),
             reactive(qc_input), app_template, data_comparison,
             preprocess_data, statmodel_contrast)
  
  observeEvent(input$proceed, {
    updateTabsetPanel(session = session, inputId = "tablist", 
                      selected = "Uploaddata")
  })
  
  # visualizeNetworkServer - keep callModule if not yet refactored
  visualizeNetworkServer("network", parent_session = session, dataComparison = data_comparison, app_template = app_template)
  
  observe({
    if(input$"loadpage-DDA_DIA" %in% c("TMT") && input$"loadpage-BIO" %in% c("PTM")) {
      hideTab(inputId = "tablist", target = "PQ")
      hideTab(inputId = "tablist", target = "Future")
    }
    
    if(!(input$"loadpage-DDA_DIA" %in% c("TMT")) && !(input$"loadpage-BIO" %in% c("PTM"))) {
      showTab(inputId = "tablist", target = "PQ")
      showTab(inputId = "tablist", target = "Future")
    }
  })
  
  observeEvent(input$"home-Reset", {
    refresh()
  })
}