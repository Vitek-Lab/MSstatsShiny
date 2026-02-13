# ============================================================================
# Results Table and Download Functions
# ============================================================================

render_results_table = function(output, session, data_comparison, SignificantProteins) {
  ns = session$ns
  
  output$table_results = renderUI({
    req(data_comparison())
    req(SignificantProteins())
    
    if (is.null(SignificantProteins())) {
      tagList(tags$br())
    } else {
      tagList(
        tags$br(),
        h2("Results"),
        h5("There are ", textOutput(ns("number"), inline = TRUE), "significant proteins"),
        tags$br(),
        dataTableOutput(ns("significant")),
        downloadButton(ns("download_compar"), "Download all modeling results"),
        downloadButton(ns("download_signif"), "Download significant proteins")
      )
    }
  })
  
  output$significant = renderDataTable({ SignificantProteins() })
  output$number = renderText({ nrow(SignificantProteins()) })
}

create_download_handlers = function(output, data_comparison, SignificantProteins, 
                                    data_comparison_code) {
  output$download_compar = downloadHandler(
    filename = function() paste("test_result-", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(data_comparison()$ComparisonResult, file)
  )
  
  output$download_code = downloadHandler(
    filename = function() paste("mstats-code-", Sys.Date(), ".R", sep = ""),
    content = function(file) writeLines(data_comparison_code(), file)
  )
  
  output$download_signif = downloadHandler(
    filename = function() paste("data-", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(SignificantProteins(), file)
  )
}

extract_significant_proteins = function(data_comp, loadpage_input, signif_threshold) {
  if (loadpage_input$BIO == "PTM") {
    data_comp$ADJUSTED.Model[data_comp$ADJUSTED.Model$adj.pvalue < signif_threshold,]
  } else if (loadpage_input$DDA_DIA == "TMT") {
    data_comp$ComparisonResult[data_comp$ComparisonResult$adj.pvalue < signif_threshold,]
  } else {
    data_comp$ComparisonResult[which(data_comp$ComparisonResult$adj.pvalue < signif_threshold),]
  }
}
