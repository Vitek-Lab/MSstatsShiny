# quantification

abundance <- reactive({
  quantification(preprocess_data(),
                 type = input$typequant,
                 format = input$format)
})

# downloads

output$download_summary <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(abundance(), file)
  }
)

# abundance

output$abundance <- renderDataTable(abundance())
