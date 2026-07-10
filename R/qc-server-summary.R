# QC Summarized Results tab: protein-abundance quantification, the abundance
# table, and its CSV download.

#' Register the QC Summarized Results tab outputs.
#' @noRd
register_qc_summary <- function(input, output, session, loadpage_input,
                                preprocess_data, app_template) {

  abundance_table_reactive_values = reactiveValues()

  observeEvent(loadpage_input()$proceed1, {
    abundance_table_reactive_values$results = NULL
  })

  abundance = eventReactive(input$update_results, {
    validate(need(preprocess_data(),
                  message = "PLEASE COMPLETE DATA PROCESSING"))

    if (loadpage_input()$BIO == "PTM" && loadpage_input()$DDA_DIA == "TMT"){
      temp = copy(preprocess_data())
      setnames(temp$PTM$ProteinLevelData,
               c("Abundance", "Condition", "BioReplicate"),
               c("LogIntensities", "GROUP", "SUBJECT"))
      abundance_table_reactive_values$results = quantification(temp$PTM,
                                        type = input$typequant,
                                        format = input$format,
                                        use_log_file = FALSE)
    } else if (loadpage_input()$BIO == "PTM" && loadpage_input()$DDA_DIA != "TMT"){
      temp = copy(preprocess_data())
      abundance_table_reactive_values$results =quantification(temp$PTM,
                                       type = input$typequant,
                                       format = input$format,
                                       use_log_file = FALSE)
    } else if (loadpage_input()$DDA_DIA == "TMT"){
      temp = copy(preprocess_data())
      temp$ProteinLevelData = copy(temp$ProteinLevelData)
      setnames(temp$ProteinLevelData,
               c("Abundance", "Condition", "BioReplicate"),
               c("LogIntensities", "GROUP", "SUBJECT"))
      abundance_table_reactive_values$results = quantification(temp,
                                        type = input$typequant,
                                        format = input$format,
                                        use_log_file = FALSE)
    } else if (!is.null(app_template) && !is.null(app_template()) &&
               app_template() == TEMPLATES$protein_turnover) {
      # TODO: Refactor quantification function to handle LABEL column
      abundance_table_reactive_values$results <- preprocess_data()$ProteinLevelData
    } else{
      temp = copy(preprocess_data())
      abundance_table_reactive_values$results =quantification(temp,
                                       type = input$typequant,
                                       format = input$format,
                                       use_log_file = FALSE)
    }

    return(abundance_table_reactive_values$results)
  })

  output$abundance = renderUI({
    ns <- session$ns
    req(abundance())
    if (is.null(abundance_table_reactive_values$results)) {
      tagList(
        tags$br())
    } else {
      tagList(
        dataTableOutput(ns("abundanceTable")) )
    }
  })
  output$abundanceTable = renderDataTable(abundance())

  output$download_summary = downloadHandler(
    filename = function() {
      paste("Abundance-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(abundance(), file)
    }
  )
}
