# ============================================================================
# Visualization Options and Plotting Functions
# ============================================================================

render_group_comparison_plot_inputs = function(output, session, rownames, get_data, input, loadpage_input, condition_list, contrast) {
  ns = session$ns
  
  output[[NAMESPACE_STATMODEL$visualization_which_comparison]] = renderUI({
    selectInput(ns(NAMESPACE_STATMODEL$visualization_which_comparison),
                label = h5("Select comparison to plot"), 
                c("all", rownames()), selected = "all")
  })
  
  output[[NAMESPACE_STATMODEL$visualization_which_protein]] = renderUI({
    selectInput(ns(NAMESPACE_STATMODEL$visualization_which_protein),
                label = h4("which protein to plot"), 
                unique(get_data()$ProteinName))
  })
  
  output[[NAMESPACE_STATMODEL$visualization_plot_options_conditional_panel]] <- renderUI({
    plot_type <- input[[NAMESPACE_STATMODEL$visualization_plot_type]]
    
    if (plot_type == CONSTANTS_STATMODEL$plot_type_volcano_plot) {
      show_protein_name <- !is.null(loadpage_input()$DDA_DIA) &&
        loadpage_input()$DDA_DIA != "TMT"
      create_volcano_plot_options(ns, show_protein_name)
    } else if (plot_type == CONSTANTS_STATMODEL$plot_type_comparison_plot) {
      create_comparison_plot_options(ns)
    } else if (plot_type == CONSTANTS_STATMODEL$plot_type_heatmap) {
      create_heatmap_options(ns)
    } else if (plot_type == CONSTANTS_STATMODEL$plot_type_response_curve) {
      create_response_curve_options(ns)
    } else {
      NULL
    }
  })
  
  output[[NAMESPACE_STATMODEL$visualization_fold_change_input]] <- renderUI({
    req(input[[NAMESPACE_STATMODEL$visualization_fold_change_checkbox]])
    if (input[[NAMESPACE_STATMODEL$visualization_fold_change_checkbox]]) {
      numericInput(ns(NAMESPACE_STATMODEL$visualization_fold_change_input), "Fold change cutoff", 1, 0, 100, 0.1)
    }
  })
  
  output[[NAMESPACE_STATMODEL$visualization_response_curve_which_drug]] = renderUI({
    if (input[[NAMESPACE_STATMODEL$visualization_plot_type]] == 
        CONSTANTS_STATMODEL$plot_type_response_curve) {
      response_curve_setup_matrix = prepare_dose_response_fit(contrast$matrix)
      unique_drugs = unique(response_curve_setup_matrix$drug)
      unique_drugs_without_control = unique_drugs[unique_drugs != "DMSO"]
      selectInput(session$ns(NAMESPACE_STATMODEL$visualization_response_curve_which_drug),
                  label = h5("Select Treatment"), 
                  unique_drugs_without_control, selected = unique_drugs_without_control[[1]])
    } else {
      NULL
    }
  })
}

create_group_comparison_plot = function(input, loadpage_input, data_comparison) {
  show_modal_spinner()
  fold_change_cutoff <- ifelse(!is.null(input[[NAMESPACE_STATMODEL$visualization_fold_change_input]]), input[[NAMESPACE_STATMODEL$visualization_fold_change_input]], FALSE)
  
  tryCatch({
    if (input[[NAMESPACE_STATMODEL$visualization_plot_type]] == CONSTANTS_STATMODEL$plot_type_volcano_plot && input[[NAMESPACE_STATMODEL$visualization_which_comparison]] == "all") {
      remove_modal_spinner()
      stop('** Cannot generate multiple plots in a screen. Please refine selection or save to a pdf. **')
    }
    if (loadpage_input$BIO == "PTM") {
      plot_result = groupComparisonPlotsPTM(
        data_comparison,
        input[[NAMESPACE_STATMODEL$visualization_plot_type]],
        sig = input[[NAMESPACE_STATMODEL$visualization_volcano_significance_cutoff]],
        FCcutoff = fold_change_cutoff,
        logBase.pvalue = as.integer(input[[NAMESPACE_STATMODEL$visualization_logp_base]]),
        ProteinName = input[[NAMESPACE_STATMODEL$visualization_volcano_display_protein_name]],
        which.Comparison = input[[NAMESPACE_STATMODEL$visualization_which_comparison]],
        address = FALSE
      )
    } else if (loadpage_input$DDA_DIA == "TMT") {
      plot_result = groupComparisonPlots(
        data = data_comparison$ComparisonResult,
        type = input[[NAMESPACE_STATMODEL$visualization_plot_type]],
        sig = input[[NAMESPACE_STATMODEL$visualization_volcano_significance_cutoff]],
        FCcutoff = fold_change_cutoff,
        logBase.pvalue = as.numeric(input[[NAMESPACE_STATMODEL$visualization_logp_base]]),
        ProteinName = input[[NAMESPACE_STATMODEL$visualization_volcano_display_protein_name]],
        numProtein = input[[NAMESPACE_STATMODEL$visualization_heatmap_number_proteins]],
        clustering = input[[NAMESPACE_STATMODEL$visualization_heatmap_cluster_option]],
        which.Comparison = input[[NAMESPACE_STATMODEL$visualization_which_comparison]],
        which.Protein = input[[NAMESPACE_STATMODEL$visualization_which_protein]],
        height = input[[NAMESPACE_STATMODEL$visualization_plot_height_slider]],
        address = "Ex_",
        isPlotly = TRUE
      )[[1]]
    } else {
      plot_result = groupComparisonPlots(
        data = data_comparison$ComparisonResult,
        type = input[[NAMESPACE_STATMODEL$visualization_plot_type]],
        sig = input[[NAMESPACE_STATMODEL$visualization_volcano_significance_cutoff]],
        FCcutoff = fold_change_cutoff,
        logBase.pvalue = as.numeric(input[[NAMESPACE_STATMODEL$visualization_logp_base]]),
        ProteinName = input[[NAMESPACE_STATMODEL$visualization_volcano_display_protein_name]],
        numProtein = input[[NAMESPACE_STATMODEL$visualization_heatmap_number_proteins]],
        clustering = input[[NAMESPACE_STATMODEL$visualization_heatmap_cluster_option]],
        which.Comparison = input[[NAMESPACE_STATMODEL$visualization_which_comparison]],
        which.Protein = input[[NAMESPACE_STATMODEL$visualization_which_protein]],
        height = input[[NAMESPACE_STATMODEL$visualization_plot_height_slider]],
        address = "Ex_",
        isPlotly = TRUE
      )[[1]]
    }
    remove_modal_spinner()
    return(plot_result)
  }, error = function(e) {
    remove_modal_spinner()
    message("An error occurred: ", conditionMessage(e))
  })
}

create_download_plot_handler = function(output) {
  output[[NAMESPACE_STATMODEL$visualization_download_plot_results]] = downloadHandler(
    filename = function() paste("SummaryPlot-", Sys.Date(), ".zip", sep = ""),
    content = function(file) {
      files = list.files(getwd(), pattern = "^Ex_", full.names = TRUE)
      file_info = file.info(files)
      latest_file = files[which.max(file_info$mtime)]
      file.copy(latest_file, file)
    }
  )
}
