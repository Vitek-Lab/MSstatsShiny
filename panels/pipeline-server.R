shinyServer(function(input, output, session) {
  
  source("panels/loadpage-server.R", local = T)
  
  observeEvent(input$proceed, {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Uploaddata")
  })
  
  onclick("reset1", {
    shinyjs::runjs("location.reload()")
  })
  
  observeEvent(input$proceed1, {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "Uploaddata")
  })
})