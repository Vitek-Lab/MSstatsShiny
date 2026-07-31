# ============================================================================
# Results Table and Download Functions
# ============================================================================

render_results_table = function(output, session, data_comparison, SignificantProteins,
                                app_template = reactive(TEMPLATES$default)) {
  ns = session$ns

  output$table_results = renderUI({
    req(data_comparison())
    req(SignificantProteins())
    tagList(
      tags$br(),
      h2("Results"),
      h5("There are ", textOutput(ns("number"), inline = TRUE), "significant proteins"),
      tags$br(),
      dataTableOutput(ns("significant")),
      downloadButton(ns("download_compar"), "Download all modeling results"),
      downloadButton(ns("download_signif"), "Download significant proteins")
    )
  })

  output$significant = renderDataTable(
    rename_protein_column_for_display(SignificantProteins(), app_template),
    options = list(scrollX = TRUE))
  output$number = renderText({ nrow(SignificantProteins()) })
}

render_ptm_results_tables = function(output, session, data_comparison, SignificantProteins) {
  ns = session$ns

  output$adj_table_results = renderUI({
    req(data_comparison())
    req(SignificantProteins())
    significant = SignificantProteins()
    if (is.null(significant$ADJUSTED.Model)) {
      tagList(tags$br())
    } else {
      tagList(
        tags$br(),
        h2("Adjusted PTM Modeling Results"),
        h5("There are ", textOutput(ns("number_adj"), inline = TRUE), "significant PTMs"),
        tags$br(),
        dataTableOutput(ns("adj_significant")),
        downloadButton(ns("download_compar_adj"), "Download all modeling results"),
        downloadButton(ns("download_signif_adj"), "Download significant PTMs")
      )
    }
  })

  output$unadj_table_results = renderUI({
    req(data_comparison())
    req(SignificantProteins())
    significant = SignificantProteins()
    if (is.null(significant$PTM.Model)) {
      tagList(tags$br())
    } else {
      tagList(
        tags$br(),
        h2("Unadjusted PTM Modeling Results"),
        h5("There are ", textOutput(ns("number_unadj"), inline = TRUE), "significant PTMs"),
        tags$br(),
        dataTableOutput(ns("unadj_significant")),
        downloadButton(ns("download_compar_unadj"), "Download all modeling results"),
        downloadButton(ns("download_signif_unadj"), "Download significant PTMs")
      )
    }
  })

  output$prot_table_results = renderUI({
    req(data_comparison())
    req(SignificantProteins())
    significant = SignificantProteins()
    if (is.null(significant$PROTEIN.Model)) {
      tagList(tags$br())
    } else {
      tagList(
        tags$br(),
        h2("Protein Modeling Results"),
        h5("There are ", textOutput(ns("number_prot"), inline = TRUE), "significant proteins"),
        tags$br(),
        dataTableOutput(ns("prot_significant")),
        downloadButton(ns("download_compar_prot"), "Download all modeling results"),
        downloadButton(ns("download_signif_prot"), "Download significant proteins")
      )
    }
  })

  output$adj_significant = renderDataTable({ SignificantProteins()$ADJUSTED.Model }, options = list(scrollX = TRUE))
  output$unadj_significant = renderDataTable({ SignificantProteins()$PTM.Model }, options = list(scrollX = TRUE))
  output$prot_significant = renderDataTable({ SignificantProteins()$PROTEIN.Model }, options = list(scrollX = TRUE))

  output$number_adj = renderText({ nrow(SignificantProteins()$ADJUSTED.Model) })
  output$number_unadj = renderText({ nrow(SignificantProteins()$PTM.Model) })
  output$number_prot = renderText({ nrow(SignificantProteins()$PROTEIN.Model) })
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

create_ptm_download_handlers = function(output, data_comparison, SignificantProteins) {
  output$download_compar_adj = downloadHandler(
    filename = function() paste("ptm_adjusted_result-", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(data_comparison()$ADJUSTED.Model, file)
  )
  output$download_signif_adj = downloadHandler(
    filename = function() paste("ptm_adjusted_significant-", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(SignificantProteins()$ADJUSTED.Model, file)
  )
  output$download_compar_unadj = downloadHandler(
    filename = function() paste("ptm_unadjusted_result-", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(data_comparison()$PTM.Model, file)
  )
  output$download_signif_unadj = downloadHandler(
    filename = function() paste("ptm_unadjusted_significant-", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(SignificantProteins()$PTM.Model, file)
  )
  output$download_compar_prot = downloadHandler(
    filename = function() paste("ptm_protein_result-", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(data_comparison()$PROTEIN.Model, file)
  )
  output$download_signif_prot = downloadHandler(
    filename = function() paste("ptm_protein_significant-", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(SignificantProteins()$PROTEIN.Model, file)
  )
}

extract_significant_proteins = function(data_comp, loadpage_input, signif_threshold) {
  if (loadpage_input$BIO == "PTM") {
    list(
      ADJUSTED.Model = if (!is.null(data_comp$ADJUSTED.Model))
        data_comp$ADJUSTED.Model[data_comp$ADJUSTED.Model$adj.pvalue < signif_threshold, ]
      else NULL,
      PTM.Model = if (!is.null(data_comp$PTM.Model))
        data_comp$PTM.Model[data_comp$PTM.Model$adj.pvalue < signif_threshold, ]
      else NULL,
      PROTEIN.Model = if (!is.null(data_comp$PROTEIN.Model))
        data_comp$PROTEIN.Model[data_comp$PROTEIN.Model$adj.pvalue < signif_threshold, ]
      else NULL
    )
  } else if (loadpage_input$DDA_DIA == "TMT") {
    data_comp$ComparisonResult[data_comp$ComparisonResult$adj.pvalue < signif_threshold,]
  } else {
    data_comp$ComparisonResult[which(data_comp$ComparisonResult$adj.pvalue < signif_threshold),]
  }
}
