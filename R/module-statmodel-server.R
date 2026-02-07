# ============================================================================
# Contrast Matrix Building Functions
# ============================================================================

get_experimental_conditions = function(loadpage_input, preprocess_data) {
  if (loadpage_input$BIO == "PTM" & 
      ((loadpage_input$BIO == "PTM" & loadpage_input$DDA_DIA == "TMT") | 
       loadpage_input$filetype == 'phil')) {
    levels(preprocess_data$PTM$ProteinLevelData$Condition)
  } else if (loadpage_input$BIO == "PTM" & 
             (loadpage_input$BIO == "PTM" & loadpage_input$DDA_DIA != "TMT")) {
    levels(preprocess_data$PTM$ProteinLevelData$GROUP)
  } else if (loadpage_input$DDA_DIA == "TMT") {
    levels(preprocess_data$ProteinLevelData$Condition)
  } else {
    levels(preprocess_data$ProteinLevelData$GROUP)
  }
}

#' Get contrast panel UI based on mode
#' @noRd
get_contrast_panel_ui <- function(mode, ns) {
  if (is.null(mode) || length(mode) == 0) {
    return(NULL)
  }
  
  if (mode == CONSTANTS_STATMODEL$comparison_mode_custom_pairwise) {
    build_custom_pairwise_panel(ns)
  } else if (mode == CONSTANTS_STATMODEL$comparison_mode_all_vs_one) {
    build_all_vs_one_panel(ns)
  } else if (mode == CONSTANTS_STATMODEL$comparison_mode_all_pairwise) {
    build_all_pairwise_panel(ns)
  } else if (mode == CONSTANTS_STATMODEL$comparison_mode_custom_nonpairwise) {
    build_custom_nonpairwise_panel(ns)
  } else if (mode == CONSTANTS_STATMODEL$comparison_mode_response_curve) {
    build_response_curve_panel(ns)
  } else {
    NULL
  }
}

render_all_against_one_inputs = function(output, session, condition_list) {
  ns = session$ns
  
  output[[NAMESPACE_STATMODEL$comparisons_all_vs_one_choice]] = renderUI({
    selectInput(ns(NAMESPACE_STATMODEL$comparisons_all_vs_one_choice), "", condition_list())
  })
}

render_custom_pairwise_inputs = function(output, session, condition_list) {
  ns = session$ns
  
  output[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1]] = renderUI({
    selectInput(ns(NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1), "Group 1", condition_list())
  })
  
  output[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2]] = renderUI({
    selectInput(ns(NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2), "Group 2", condition_list())
  })
}

render_custom_non_pairwise_inputs = function(output, session, condition_list) {
  ns = session$ns
  output[[NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights]] = renderUI({
    lapply(1:length(condition_list()), function(i) {
      list(numericInput(ns(paste0(
        NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, i)
      ), 
      label = condition_list()[i], value = 0))
    })
  })
}

validate_contrast_inputs = function(input, contrast_mode, condition_list) {
  if (contrast_mode == CONSTANTS_STATMODEL$comparison_mode_custom_pairwise) {
    validate(
      need(input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1]] != input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2]], "Please select different groups")
    )
  } else if (contrast_mode == CONSTANTS_STATMODEL$comparison_mode_custom_nonpairwise) {
    wt_sum = sum(sapply(1:length(condition_list), function(i) {
      input[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, i)]]
    }))
    
    validate(
      need(wt_sum == 0, "The contrast weights should sum up to 0")
    )
  }
}

build_custom_pairwise_contrast = function(input, condition_list, contrast, comp_list, row) {
  if (input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1]] == input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2]]) {
    return(contrast$matrix)
  }
  
  index1 = which(condition_list == input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1]])
  index2 = which(condition_list == input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2]])
  
  comp_list$dList = unique(c(isolate(comp_list$dList), 
                             paste(input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1]], "vs", input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2]], sep = " ")))
  
  contrast$row = matrix(row, nrow = 1)
  contrast$row[index1] = 1
  contrast$row[index2] = -1
  
  if (is.null(contrast$matrix)) {
    contrast$matrix = contrast$row
  } else {
    contrast$matrix = rbind(contrast$matrix, contrast$row)
    contrast$matrix = rbind(contrast$matrix[!duplicated(contrast$matrix),])
  }
  
  rownames(contrast$matrix) = comp_list$dList
  colnames(contrast$matrix) = condition_list
  
  return(contrast$matrix)
}

build_custom_non_pairwise_contrast = function(input, condition_list, contrast, comp_list, row) {
  wt_sum = sum(sapply(1:length(condition_list), function(i) {
    input[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, i)]]
  }))
  
  if (wt_sum != 0) {
    return(contrast$matrix)
  }
  
  comp_list$dList = unique(c(isolate(comp_list$dList), 
                             input[[NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_name]]))
  contrast$row = matrix(row, nrow = 1)
  
  for (index in 1:length(condition_list)) {
    contrast$row[index] = input[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, index)]]
  }
  
  if (is.null(contrast$matrix)) {
    contrast$matrix = contrast$row
  } else {
    contrast$matrix = rbind(contrast$matrix, contrast$row)
    contrast$matrix = rbind(contrast$matrix[!duplicated(contrast$matrix),])
  }
  
  rownames(contrast$matrix) = comp_list$dList
  colnames(contrast$matrix) = condition_list
  
  return(contrast$matrix)
}

build_all_against_one_contrast = function(input, condition_list, contrast, comp_list, row, loadpage_input) {
  index3 = which(condition_list == input[[NAMESPACE_STATMODEL$comparisons_all_vs_one_choice]])
  
  for (index in 1:length(condition_list)) {
    if (index == index3) next
    
    comp_list$dList = c(isolate(comp_list$dList),
                        paste(condition_list[index], "vs", input[[NAMESPACE_STATMODEL$comparisons_all_vs_one_choice]], sep = " "))
    
    contrast$row = matrix(row, nrow = 1)
    contrast$row[index] = 1
    contrast$row[index3] = -1
    
    if (is.null(contrast$matrix)) {
      contrast$matrix = contrast$row
    } else {
      contrast$matrix = rbind(contrast$matrix, contrast$row)
    }
  }
  
  rownames(contrast$matrix) = comp_list$dList
  colnames(contrast$matrix) = condition_list
  
  return(contrast$matrix)
}

build_all_pair_contrast = function(input, condition_list, contrast, comp_list, row, loadpage_input) {
  contrast$matrix = NULL
  comp_list$dList = NULL
  
  for (index in 1:length(condition_list)) {
    for (index1 in 1:length(condition_list)) {
      if (index == index1) next
      if (index < index1) {
        comp_list$dList = c(isolate(comp_list$dList),
                            paste(condition_list[index], "vs", condition_list[index1], sep = " "))
        
        contrast$row = matrix(row, nrow = 1)
        contrast$row[index] = 1
        contrast$row[index1] = -1
        
        if (is.null(contrast$matrix)) {
          contrast$matrix = contrast$row
        } else {
          contrast$matrix = rbind(contrast$matrix, contrast$row)
          contrast$matrix = rbind(contrast$matrix[!duplicated(contrast$matrix),])
        }
        
        rownames(contrast$matrix) = comp_list$dList
        colnames(contrast$matrix) = condition_list
      }
    }
  }
  
  return(contrast$matrix)
}

build_response_curve_matrix = function(condition_list) {
  matrix = data.frame(GROUP = as.character(condition_list))
  matrix = matrix %>% mutate(
    is_control = str_detect(toupper(GROUP), "^(DMSO|CONTROL|VEHICLE)$"),
    measurements = str_extract_all(GROUP, "[0-9.]+[a-zA-Z]+")
  )
  controls = matrix %>% filter(is_control) %>% select(GROUP, is_control)
  treatments = matrix %>% filter(!is_control) %>%
    mutate(
      value = as.numeric(str_extract(measurements, "[0-9.]+")),
      unit = str_extract(measurements, "[a-zA-Z]+"),
      measurement_type = case_when(
        unit %in% c("nM", "uM", "mM", "M", "mg", "ug") ~ "dose",
        unit %in% c("h", "hr", "hrs", "min", "d", "day") ~ "time",
        unit %in% c("C", "F", "K") ~ "temperature",
        TRUE ~ "treatment"
      )
    ) 
    if (length(unique(treatments$unit)) > 1) {
      showNotification(
        paste("Multiple units of measurement detected in group names: ",
              paste(unique(treatments$unit), collapse = ", "),
              " Edit the metadata table to ensure consistent units."),
        type = "warning",
        duration = 10
      )
    }
    treatments = treatments %>%
      pivot_wider(
        id_cols = c(GROUP, is_control),
        names_from = measurement_type,
        values_from = c(value, unit),
        names_glue = "{measurement_type}_{.value}"
      )
    matrix = bind_rows(controls, treatments)
    value_cols = grep("_value$", colnames(matrix), value = TRUE)
    for (col in value_cols) {
      matrix[[col]][matrix$is_control] = 0
    }
    if ("dose_value" %in% colnames(matrix)) {
      matrix = matrix %>% 
        mutate(
          drug = ifelse(
            is_control,
            GROUP,
            str_extract(GROUP, "^[^_0-9]+") %>% str_trim()
          )
        )
    }
    matrix = matrix %>% select(-is_control)
    
    return(matrix)
}

# A hacky function to make metadata compatible with MSstatsResponse format
# based on build_response_curve_matrix output
prepare_dose_response_fit = function(data) {
  if (!("drug" %in% colnames(data))) {
    column_names = colnames(data)
    intervention_cols = grep("time|temperature|treatment", column_names, 
                             ignore.case = TRUE, value = TRUE)
    if (length(intervention_cols) > 0) {
      intervention_type = sub("_.*", "", intervention_cols[1])
      data$drug = intervention_type
      intervention_value = paste0(intervention_type, "_value")
    } else {
      stop("No intervention columns found (time, temperature, or treatment)")
    }
  } else {
    intervention_value = "dose_value"
  }
  
  # Create subset with renamed columns
  subset_df = data[, c("Protein", "drug", intervention_value, "LogIntensities")]
  colnames(subset_df) = c("protein", "drug", "dose", "response")
  
  return(subset_df)
}

#' Update a matrix or data frame from a DT cell edit event
#'
#' @param mat The matrix or data.frame to be updated.
#' @param info The `input$table_cell_edit` object from a DT edit event.
#'
#' @return The updated matrix or data.frame.
#' @noRd
update_matrix_from_edit = function(mat, info) {
  # DT provides 1-based indices for rows and columns in the edit event
  i <- info$row
  j <- info$col
  v <- info$value
  
  # Coerce the new value to the type of the target column to maintain data integrity
  if (is.data.frame(mat)) {
    # For data frames, coerce to the column's class.
    # tryCatch prevents the app from crashing if the user enters an invalid
    # value (e.g., text in a numeric column). If coercion fails, the original value is kept.
    v <- tryCatch(as(v, class(mat[[j]])), error = function(e) v)
    mat[i, j] <- v
  } else {
    # For matrices, all elements have the same type. Coerce to the matrix's class.
    v <- tryCatch(as(v, class(mat[1, 1])), error = function(e) v)
    mat[i, j] <- v
  }
  return(mat)
}

#' Get TMT moderation radio button conditioned on if experiment is TMT
#' @noRd
get_tmt_moderation_radio_button <- function(loadpage_input, ns) {
  if (loadpage_input$DDA_DIA == "TMT") {
    create_moderation_radio_buttons(ns)
  }
}

# Todo: Add helper function to build dose response curve mapper matrix

# ============================================================================
# Plotting Functions
# ============================================================================

render_group_comparison_plot_inputs = function(output, session, rownames, get_data, input, loadpage_input, condition_list,contrast) {
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
  

  # to the drug column of the user-defined matrix
  output[[NAMESPACE_STATMODEL$visualization_response_curve_which_drug]] = renderUI({
    if (input[[NAMESPACE_STATMODEL$visualization_plot_type]] == 
        CONSTANTS_STATMODEL$plot_type_response_curve) {
        response_curve_setup_matrix = contrast$matrix
        unique_drugs = unique(response_curve_setup_matrix$drug)
        unique_drugs_without_control = unique_drugs[unique_drugs != "DMSO"]
        selectInput(session$ns(NAMESPACE_STATMODEL$visualization_response_curve_which_drug),
                    label = h5("Select Drug"), 
                    unique_drugs_without_control, selected = unique_drugs_without_control[[1]])
    } else {
      NULL
    }
  })
}

create_group_comparison_plot = function(input, loadpage_input, data_comparison) {
  show_modal_spinner()
  fold_change_cutoff <- ifelse(!is.null(input[[NAMESPACE_STATMODEL$visualization_fold_change_input]]), input[[NAMESPACE_STATMODEL$visualization_fold_change_input]], FALSE)
  
  # Todo: Add dose response curve plot function for typeplot == dose response curve
  
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
        height = input$height,
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
        height = input$height,
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

# ============================================================================
# Results Functions
# ============================================================================

extract_significant_proteins = function(data_comp, loadpage_input, signif_threshold) {
  # Todo: Make any adjustments for significant proteins for dose response curves
  if (loadpage_input$BIO == "PTM") {
    data_comp$ADJUSTED.Model[data_comp$ADJUSTED.Model$adj.pvalue < signif_threshold,]
  } else if (loadpage_input$DDA_DIA == "TMT") {
    data_comp$ComparisonResult[data_comp$ComparisonResult$adj.pvalue < signif_threshold,]
  } else {
    data_comp$ComparisonResult[which(data_comp$ComparisonResult$adj.pvalue < signif_threshold),]
  }
}

generate_analysis_code = function(qc_input, loadpage_input, comp_mat, input) {
  # Todo: make adjustments for adding dose response curve code
  codes = preprocessDataCode(qc_input, loadpage_input)
  
  codes = paste(codes, "\n# Create the contrast matrix\n", sep = "")
  codes = paste(codes, "contrast.matrix = NULL\n", sep = "")
  
  for (i in 1:nrow(comp_mat)) {
    codes = paste(codes, "comparison = matrix(c(", 
                  toString(comp_mat[i,]), "),nrow=1)\n", sep = "")
    codes = paste(codes, "contrast.matrix = rbind(contrast.matrix, comparison)\n", sep = "")
  }
  
  codes = paste(codes, "row.names(contrast.matrix)=c(\"", 
                paste(row.names(comp_mat), collapse = '","'), "\")\n", sep = "")
  codes = paste(codes, "colnames(contrast.matrix)=c(\"", 
                paste(colnames(comp_mat), collapse = '","'), "\")\n", sep = "")
  
  if (loadpage_input$DDA_DIA == "TMT") {
    codes = paste(codes, "\n# Model-based comparison\n", sep = "")
    codes = paste(codes, "model = MSstatsTMT::groupComparisonTMT(summarized,
                       contrast.matrix = contrast.matrix,
                       moderated = ", input[[NAMESPACE_STATMODEL$modeling_tmt_moderation]], ",\t\t\t\t
                       adj.method = \"BH\",
                       remove_norm_channel = TRUE,
                       remove_empty_channel = TRUE
                       )\n", sep = "")
  } else if (loadpage_input$BIO == "PTM") {
    dt = if ((loadpage_input$BIO == "PTM" & loadpage_input$DDA_DIA == "TMT") | 
             loadpage_input$filetype == 'phil') "TMT" else "LabelFree"
    
    codes = paste(codes, "\n# Model-based comparison\n", sep = "")
    codes = paste(codes, "model = MSstatsPTM::groupComparisonPTM(summarized, '",
                  dt, "', \t\t\t\t
                      contrast.matrix = contrast.matrix)\n", sep = "")
  } else {
    codes = paste(codes, "\n# Model-based comparison\n", sep = "")
    codes = paste(codes, "model = MSstats::groupComparison(contrast.matrix, summarized)\n", sep = "")
  }
  
  if (loadpage_input$BIO == "PTM") {
    codes = paste(codes, "groupComparisonPlotsPTM(data=model,
                               type=\"Enter VolcanoPlot, Heatmap, or ComparisonPlot\",
                               which.Comparison=\"all\",
                               which.PTM=\"all\",
                               address=\"\")\n", sep = "")
  } else {
    codes = paste(codes, "groupComparisonPlots(data=model$ComparisonResult,
                               type=\"Enter VolcanoPlot, Heatmap, or ComparisonPlot\",
                               which.Comparison=\"all\",
                               which.Protein=\"all\",isPlotly=FALSE,
                               address=\"\")\n", sep = "")
  }
  
  return(codes)
}

create_download_handlers = function(output, data_comparison, SignificantProteins, 
                                    data_comparison_code) {
  output[[NAMESPACE_STATMODEL$visualization_download_plot_results]] = downloadHandler(
    filename = function() paste("SummaryPlot-", Sys.Date(), ".zip", sep = ""),
    content = function(file) {
      files = list.files(getwd(), pattern = "^Ex_", full.names = TRUE)
      file_info = file.info(files)
      latest_file = files[which.max(file_info$mtime)]
      file.copy(latest_file, file)
    }
  )
  
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

# ============================================================================
# Main Server Function
# ============================================================================

#' Statmodel Server module for stat inference
#'
#' This function sets up the Statmodel server to process data based on user
#' selected inputs
#'
#' @param id namespace prefix for the module
#' @param parent_session session of the main calling module
#' @param loadpage_input input object from loadpage UI
#' @param qc_input input object from QC UI
#' @param get_data stored function that returns the data from loadpage
#' @param preprocess_data stored function that returns preprocessed data
#' 
#' @importFrom MSstatsResponse visualizeResponseProtein
#' 
#' @return list object with user selected options and matrix build
#'
#' @export
#' @examples
#' NA
#'
statmodelServer = function(id, parent_session, loadpage_input, qc_input, 
                           get_data, preprocess_data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Initialize reactive values
      condition_list = reactive({ 
        get_experimental_conditions(loadpage_input(), preprocess_data()) 
      })
      row = reactive({ rep(0, length(condition_list())) })
      contrast = reactiveValues(matrix = NULL, row = NULL)
      comp_list = reactiveValues(dList = NULL)
      significant = reactiveValues(result = NULL)
      
      # UI visibility
      observe({
        if (loadpage_input()$DDA_DIA == "TMT" | loadpage_input()$BIO == "PTM") {
          hide("Design")
        } else {
          shinyjs::show("Design")
        }
      })
      
      output[[NAMESPACE_STATMODEL$comparisons_conditional_panel]] <- renderUI({
        get_contrast_panel_ui(input[[NAMESPACE_STATMODEL$comparison_mode]], session$ns)
      })
      
      # Render contrast matrix inputs
      render_all_against_one_inputs(output, session, condition_list)
      render_custom_pairwise_inputs(output, session, condition_list)
      render_custom_non_pairwise_inputs(output, session, condition_list)
      
      Rownames = eventReactive(input[[NAMESPACE_STATMODEL$comparisons_submit]], {
        req(input[[NAMESPACE_STATMODEL$comparison_mode]])
        req(loadpage_input()$DDA_DIA)
        tryCatch({ rownames(matrix_build()) }, error = function(e) {})
      })
      
      render_group_comparison_plot_inputs(output, session, Rownames, get_data, input, loadpage_input, condition_list,contrast)
      
      # Reset on configuration change
      observeEvent(c(input[[NAMESPACE_STATMODEL$comparison_mode]], loadpage_input()$proceed1), {
        contrast$matrix = NULL
        comp_list$dList = NULL
        significant$result = NULL
      })
      
      # Validate contrast inputs
      check_cond = eventReactive(
        input[[NAMESPACE_STATMODEL$comparisons_submit]], {
          req(input[[NAMESPACE_STATMODEL$comparison_mode]])
          req(loadpage_input()$DDA_DIA)
          validate_contrast_inputs(input, input[[NAMESPACE_STATMODEL$comparison_mode]], condition_list())
        })
      
      # Build contrast matrix
      matrix_build = eventReactive(
        input[[NAMESPACE_STATMODEL$comparisons_submit]], {
          req(input[[NAMESPACE_STATMODEL$comparison_mode]])
          req(loadpage_input()$DDA_DIA)
          
          if (input[[NAMESPACE_STATMODEL$comparison_mode]] == CONSTANTS_STATMODEL$comparison_mode_custom_pairwise) {
            contrast$matrix = build_custom_pairwise_contrast(
              input, condition_list(), contrast, comp_list, row())
          } else if (input[[NAMESPACE_STATMODEL$comparison_mode]] == CONSTANTS_STATMODEL$comparison_mode_custom_nonpairwise) {
            contrast$matrix = build_custom_non_pairwise_contrast(
              input, condition_list(), contrast, comp_list, row())
          } else if (input[[NAMESPACE_STATMODEL$comparison_mode]] == CONSTANTS_STATMODEL$comparison_mode_all_vs_one) {
            contrast$matrix = build_all_against_one_contrast(
              input, condition_list(), contrast, comp_list, row(), loadpage_input())
          } else if (input[[NAMESPACE_STATMODEL$comparison_mode]] == CONSTANTS_STATMODEL$comparison_mode_all_pairwise) {
            contrast$matrix = build_all_pair_contrast(
              input, condition_list(), contrast, comp_list, row(), loadpage_input())
          } else if (input[[NAMESPACE_STATMODEL$comparison_mode]] == CONSTANTS_STATMODEL$comparison_mode_response_curve) {
            contrast$matrix = build_response_curve_matrix(
              condition_list())
          }
          
          enable(NAMESPACE_STATMODEL$modeling_start)
          return(contrast$matrix)
        })
      
      # Clear matrix
      observeEvent(input[[NAMESPACE_STATMODEL$comparisons_clear]], {
        disable(NAMESPACE_STATMODEL$modeling_start)
        comp_list$dList = NULL
        contrast$matrix = NULL
      })
      
      output[[NAMESPACE_STATMODEL$modeling_tmt_moderation]] <- renderUI({
        get_tmt_moderation_radio_button(loadpage_input(), session$ns)
      })
      
      # Run analysis
      data_comparison = eventReactive(input[[NAMESPACE_STATMODEL$modeling_start]], {
        req(contrast$matrix)
        matrix = contrast$matrix
        if (input[[NAMESPACE_STATMODEL$comparison_mode]] == 
            CONSTANTS_STATMODEL$comparison_mode_response_curve) {
          fitResponseCurves(input, matrix, preprocess_data())
        } else {
          dataComparison(input, qc_input(), loadpage_input(), matrix, preprocess_data())
        }
      })
      
      data_comparison_code = eventReactive(input[[NAMESPACE_STATMODEL$modeling_start]], {
        req(contrast$matrix)
        comp_mat = contrast$matrix
        generate_analysis_code(qc_input(), loadpage_input(), comp_mat, input)
      })
      
      SignificantProteins = eventReactive(input[[NAMESPACE_STATMODEL$modeling_start]], {
        data_comp = data_comparison()
        extract_significant_proteins(data_comp, loadpage_input(), input[[NAMESPACE_STATMODEL$modeling_significance_level]])
      })
      
      # Handle edits to the contrast matrix from the UI
      observeEvent(input$table_cell_edit, {
        # Use isolate() to get a snapshot of the matrix. This is crucial to prevent
        # a reactive loop where updating the matrix would re-trigger this observer.
        current_matrix <- isolate(contrast$matrix)
        
        updated_matrix <- update_matrix_from_edit(current_matrix, input$table_cell_edit)
        
        # Update the reactive value. This will trigger re-rendering of the table.
        contrast$matrix <- updated_matrix
      })
      
      # Matrix output
      output$message = renderText({ check_cond() })
      output$table = renderDataTable({
        # This table now directly depends on contrast$matrix, so it updates on build or edit.
        req(contrast$matrix)
        mat <- contrast$matrix
        
        # Define editable options, disabling the 'GROUP' column for response curves
        editable_options <- list(target = 'cell')
        # Perform a case-insensitive check for the 'GROUP' column for robustness.
        if (any(toupper(colnames(mat)) == "GROUP")) {
          group_col_idx <- which(toupper(colnames(mat)) == "GROUP")
          editable_options$disable <- list(columns = group_col_idx)
        }
        
        DT::datatable(mat, editable = editable_options, options = list(scrollX = TRUE))
      })
      
      output$matrix = renderUI({
        ns = session$ns
        tagList(
          # CSS rule to ensure text in editable cells is always black.
          # This overrides conflicting styles that may turn the text white during editing.
          tags$head(tags$style(HTML(
            "table.dataTable td input { color: black !important; }"
          ))),
          h2("Comparison matrix"),
          p(tags$i("This table is interactive. Click values to edit.")),
          br(),
          textOutput(ns("message")),
          br(),
          if (is.null(contrast$matrix)) "" else dataTableOutput(ns("table"))
        )
      })
      
      # Results rendering
      render_results_table(output, session, data_comparison, SignificantProteins)
      
      # Download handlers
      create_download_handlers(output, data_comparison, SignificantProteins, 
                               data_comparison_code)
      
      # Plot rendering
      observeEvent(input[[NAMESPACE_STATMODEL$visualization_view_results]], {
        ns = session$ns
        if (loadpage_input()$BIO == "PTM") {
          output$comp_plots = renderPlot({ 
            create_group_comparison_plot(
              input, loadpage_input(), data_comparison()
            )
          })
          op = plotOutput(ns("comp_plots"))
        } else if (input[[NAMESPACE_STATMODEL$visualization_plot_type]] == 
                   CONSTANTS_STATMODEL$plot_type_response_curve) {
          matrix = contrast$matrix
          protein_level_data <- merge(preprocess_data()$ProteinLevelData, matrix, by = "GROUP")
          dia_prepared <- prepare_dose_response_fit(
            data = protein_level_data
          )
          output$comp_plots = renderPlot({ 
            visualizeResponseProtein(
              data = dia_prepared,
              protein_name = input[[NAMESPACE_STATMODEL$visualization_which_protein]],
              drug_name = input[[NAMESPACE_STATMODEL$visualization_response_curve_which_drug]],
              ratio_response = TRUE,
              show_ic50 = TRUE,
              add_ci = TRUE, 
              transform_dose = TRUE, 
              n_samples = 1000,
              increasing = FALSE 
            )
          })
          op = plotOutput(ns("comp_plots"))
        } else {
          output$comp_plots = renderPlotly({ 
            create_group_comparison_plot(
              input, loadpage_input(), data_comparison()
            )
          })
          op = plotlyOutput(ns("comp_plots"), height = input$height)
        }
        
        insertUI(
          selector = paste0("#", ns("comparison_plots")),
          ui = tags$div(
            op,
            conditionalPanel(
              condition = paste0("input['statmodel-", NAMESPACE_STATMODEL$visualization_plot_type, "'] == '", CONSTANTS_STATMODEL$plot_type_volcano_plot, "' && input['loadpage-BIO']!='PTM'"),
              h5("Hover over plot for details")
            ),
            conditionalPanel(
              condition = paste0("input['statmodel-", NAMESPACE_STATMODEL$visualization_plot_type, "'] == '", CONSTANTS_STATMODEL$plot_type_heatmap, "'"),
              sliderInput(ns("height"), "Plot height", 
                          value = 500, min = 200, max = 1300, post = "px")
            )
          )
        )
      })
      
      # Enable controls after calculation
      observeEvent(input[[NAMESPACE_STATMODEL$modeling_start]], {
        enable("Design")
        enable(NAMESPACE_STATMODEL$visualization_plot_type)
        enable(NAMESPACE_STATMODEL$visualization_which_comparison)
        enable("download_code")
        
        output$code.button = renderUI({
          ns = session$ns
          downloadButton(ns("download_code"), "Download analysis code", 
                         icon("download"),
                         style = "color: #000000; background-color: #75ba82; border-color: #000000")
        })
      })
      
      return(list(
        input = input,
        dataComparison = data_comparison
      ))
    }
  )
}