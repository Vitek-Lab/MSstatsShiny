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
  
  output[[NAMESPACE_STATMODEL$visualization_plot_options_conditional_panel]] = renderUI({
    plot_type = input[[NAMESPACE_STATMODEL$visualization_plot_type]]
    
    if (plot_type == CONSTANTS_STATMODEL$plot_type_volcano_plot) {
      show_protein_name = !is.null(loadpage_input()$DDA_DIA) &&
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
  
  output[[NAMESPACE_STATMODEL$visualization_fold_change_input]] = renderUI({
    req(input[[NAMESPACE_STATMODEL$visualization_fold_change_checkbox]])
    if (input[[NAMESPACE_STATMODEL$visualization_fold_change_checkbox]]) {
      numericInput(ns(NAMESPACE_STATMODEL$visualization_fold_change_input), "Fold change cutoff", 1, 0, 100, 0.1)
    }
  })
  
  output[[NAMESPACE_STATMODEL$visualization_response_curve_which_drug]] = renderUI({
    req(contrast$matrix)
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
  fold_change_cutoff = ifelse(!is.null(input[[NAMESPACE_STATMODEL$visualization_fold_change_input]]), input[[NAMESPACE_STATMODEL$visualization_fold_change_input]], FALSE)
  
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
    showNotification(conditionMessage(e), type = "error", duration = 8)
  })
}
create_download_plot_handler <- function(output, input, contrast, preprocess_data) {
  output[[NAMESPACE_STATMODEL$visualization_download_plot_results]] <- downloadHandler(
    filename = function() {
      if (input[[NAMESPACE_STATMODEL$visualization_plot_type]] ==
        CONSTANTS_STATMODEL$plot_type_response_curve) {
        paste("ResponseCurvePlot-", Sys.Date(), ".zip", sep = "")
      } else {
        paste("SummaryPlot-", Sys.Date(), ".zip", sep = "")
      }
    },
    content = function(file) {
      if (input[[NAMESPACE_STATMODEL$visualization_plot_type]] ==
        CONSTANTS_STATMODEL$plot_type_response_curve) {
        # Generate response curve plot
        matrix <- contrast$matrix
        protein_level_data <- merge(preprocess_data()$ProteinLevelData, matrix, by = "GROUP")
        dia_prepared <- prepare_dose_response_fit(data = protein_level_data)

        response_plot <- visualizeResponseProtein(
          data = dia_prepared,
          protein_name = input[[NAMESPACE_STATMODEL$visualization_which_protein]],
          drug_name = input[[NAMESPACE_STATMODEL$visualization_response_curve_which_drug]],
          ratio_response = isTRUE(input[[NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale]]),
          show_ic50 = TRUE,
          add_ci = TRUE,
          transform_dose = input[[NAMESPACE_STATMODEL$modeling_response_curve_log_xaxis]],
          n_samples = 1000,
          increasing = input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]]
        )

        # Save plot to a temp PDF, then zip it
        temp_dir <- tempdir()
        pdf_path <- file.path(temp_dir, "Ex_ResponseCurvePlot.pdf")
        ggplot2::ggsave(pdf_path,
          plot = response_plot, device = "pdf",
          width = 10, height = 8
        )

        zip_path <- file.path(temp_dir, "Ex_ResponseCurvePlot.zip")
        utils::zip(zip_path, files = pdf_path, flags = "-j")
        file.copy(zip_path, file)
      } else {
        # Existing behavior for other plot types (TODO: refactor in future issue)
        files <- list.files(getwd(), pattern = "^Ex_", full.names = TRUE)
        file_info <- file.info(files)
        latest_file <- files[which.max(file_info$mtime)]
        file.copy(latest_file, file)
      }
    }
  )
}