# 
# 
# output$input1 <- renderText(input$DDA_DIA)
# output$input2 <- renderText(input$filetype)
# output$input3 <- renderText(input$log)
# output$input4 <- renderText(input$norm)
# output$input5 <- renderText(input$standards)
# output$input6 <- renderText(input$names)
# output$input7 <- renderText(input$censInt)
# output$input8 <- renderText(input$cutoff)
# output$input9 <- renderText(quantile())
# output$input10 <- renderText(features())
# output$input11 <- renderText(input$n_feat)
# output$output1.1 <- renderPrint(str(get_data()))
# output$output2.1 <- renderPrint(str(preprocess_data()))
# output$input12 <- renderText(comp_list$dList)
# output$output3.1 <- renderTable(matrix_build())
# output$output4 <- renderText(nrow(SignificantProteins()))
# output$input13 <- renderText(input$signif)
# output$output4.1 <- renderTable(SignificantProteins())


output$pdf <- downloadHandler(
  filename = "report.pdf",
  content = function(file) {
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)
    params <- list(p1=input$DDA_DIA, 
                   p2=input$log, 
                   p3=input$norm, 
                   p4=input$standards,
                   p5=input$names,
                   p6=input$remove50,
                   p7=input$censInt,
                   p8=input$cutoff,
                   p9=input$maxQC,
                   p10=input$MBi,
                   p11=input$all_feat,
                   p12=input$n_feat)
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params
    )
    
  })  