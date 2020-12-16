vals <- reactiveValues(count=0)

isolate(vals$count <- vals$count + 1)

session$onSessionEnded(function(){
  
  isolate(vals$count <- vals$count - 1)
})


output$count <- renderText({
  vals$count
})

observeEvent(input$Foo, {
  js$enableTab("Data processing")
})

