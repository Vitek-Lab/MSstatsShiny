# quantification

abundance <- reactive({
  validate(need(preprocess_data(),
                message = "PLEASE COMPLETE DATA PROCESSING STEP"))
  quantification(preprocess_data(),
                 type = input$typequant,
                 format = input$format)
})

# downloads

output$download_summary <- downloadHandler(
  filename = function() {
    paste("abundance-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(abundance(), file)
  }
)

# abundance

output$abundance <- renderDataTable(abundance())

observeEvent(input$proceed4, {
  updateTabsetPanel(session = session, inputId = "tablist", selected = "StatsModel")
})
