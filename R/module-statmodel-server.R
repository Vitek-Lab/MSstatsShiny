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

render_response_curve_inputs = function(output, session, condition_list) {
  ns = session$ns
  
  output[[NAMESPACE_STATMODEL$comparisons_response_curve_choice]] = renderUI({
    selectInput(ns(NAMESPACE_STATMODEL$comparisons_response_curve_choice), 
                "Condition:", 
                condition_list()
    )
  })
}

# Todo: Add helper function to render dose response curve inputs

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

build_response_curve_matrix = function(input, contrast) {
  if (is.null(contrast$matrix)) {
    contrast$matrix = data.frame(
      Condition = input[[NAMESPACE_STATMODEL$comparisons_response_curve_choice]],
      X_axis = input[[NAMESPACE_STATMODEL$comparisons_response_curve_xaxis]],
      Amount = input[[NAMESPACE_STATMODEL$comparisons_response_curve_amount]],
      stringsAsFactors = FALSE
    )
  } else {
    contrast$matrix = rbind(contrast$matrix, data.frame(
      Condition = input[[NAMESPACE_STATMODEL$comparisons_response_curve_choice]],
      X_axis = input[[NAMESPACE_STATMODEL$comparisons_response_curve_xaxis]],
      Amount = input[[NAMESPACE_STATMODEL$comparisons_response_curve_amount]],
      stringsAsFactors = FALSE
    ))
    contrast$matrix = rbind(contrast$matrix[!duplicated(contrast$matrix$Condition),])
  }
  
  return(contrast$matrix)
}

# Todo: Add helper function to build dose response curve mapper matrix

# ============================================================================
# Plotting Functions
# ============================================================================

render_group_comparison_plot_inputs = function(output, session, rownames, get_data) {
  ns = session$ns
  
  output$WhichComp = renderUI({
    selectInput(ns("whichComp"),
                label = h5("Select comparison to plot"), 
                c("all", rownames()), selected = "all")
  })
  
  output$WhichProt = renderUI({
    selectInput(ns("whichProt"),
                label = h4("which protein to plot"), 
                unique(get_data()[[1]]))
  })
  # Todo: Add plot inputs for dose response curves for which protein to plot (or re-use whichProt)
}

create_group_comparison_plot = function(input, loadpage_input, data_comparison) {
  show_modal_spinner()
  
  # Todo: Add dose response curve plot function for typeplot == dose response curve
  
  tryCatch({
    if (toupper(input$typeplot) == "VOLCANOPLOT" && input$whichComp == "all") {
      remove_modal_spinner()
      stop('** Cannot generate multiple plots in a screen. Please refine selection or save to a pdf. **')
    }
    if (loadpage_input$BIO == "PTM") {
      plot_result = groupComparisonPlotsPTM(
        data_comparison,
        input$typeplot,
        sig = input$sig,
        FCcutoff = input$FC,
        logBase.pvalue = as.integer(input$logp),
        ProteinName = input$pname,
        which.Comparison = input$whichComp,
        address = FALSE
      )
    } else if (loadpage_input$DDA_DIA == "TMT") {
      plot_result = groupComparisonPlots(
        data = data_comparison$ComparisonResult,
        type = input$typeplot,
        sig = input$sig,
        FCcutoff = input$FC,
        logBase.pvalue = as.numeric(input$logp),
        ProteinName = input$pname,
        numProtein = input$nump,
        clustering = input$cluster,
        which.Comparison = input$whichComp,
        which.Protein = input$whichProt,
        height = input$height,
        address = "Ex_",
        isPlotly = TRUE
      )[[1]]
    } else {
      plot_result = groupComparisonPlots(
        data = data_comparison$ComparisonResult,
        type = input$typeplot,
        sig = input$sig,
        FCcutoff = input$FC,
        logBase.pvalue = as.numeric(input$logp),
        ProteinName = input$pname,
        numProtein = input$nump,
        clustering = input$cluster,
        which.Comparison = input$whichComp,
        which.Protein = input$whichProt,
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
                       moderated = ", input$moderated, ",\t\t\t\t
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
  output$plotresults = downloadHandler(
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
      render_response_curve_inputs(output, session, condition_list)
      
      Rownames = eventReactive(input[[NAMESPACE_STATMODEL$comparisons_submit]], {
                                   req(input[[NAMESPACE_STATMODEL$comparison_mode]])
                                   req(loadpage_input()$DDA_DIA)
                                   tryCatch({ rownames(matrix_build()) }, error = function(e) {})
                                 })
      
      render_group_comparison_plot_inputs(output, session, Rownames, get_data)
      
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
                  input, contrast)
            }
            
            enable("calculate")
            return(contrast$matrix)
          })
      
      # Clear matrix
      observeEvent(input[[NAMESPACE_STATMODEL$comparisons_clear]], {
                       disable("calculate")
                       comp_list$dList = NULL
                       contrast$matrix = NULL
                     })
      
      # Run analysis
      data_comparison = eventReactive(input$calculate, {
        matrix = matrix_build()
        dataComparison(input, qc_input(), loadpage_input(), matrix, preprocess_data())
      })
      
      data_comparison_code = eventReactive(input$calculate, {
        comp_mat = matrix_build()
        generate_analysis_code(qc_input(), loadpage_input(), comp_mat, input)
      })
      
      SignificantProteins = eventReactive(input$calculate, {
        data_comp = data_comparison()
        extract_significant_proteins(data_comp, loadpage_input(), input$signif)
      })
      
      # Matrix output
      output$message = renderText({ check_cond() })
      output$table = renderDataTable({ matrix_build() })
      
      output$matrix = renderUI({
        ns = session$ns
        tagList(
          h2("Comparison matrix"),
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
      observeEvent(input$viewresults, {
        ns = session$ns
        if (loadpage_input()$BIO == "PTM") {
          output$comp_plots = renderPlot({ 
            create_group_comparison_plot(
              input, loadpage_input(), data_comparison()
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
              condition = "input['statmodel-typeplot'] == 'VolcanoPlot' && input['loadpage-BIO']!='PTM'",
              h5("Hover over plot for details")
            ),
            conditionalPanel(
              condition = "input['statmodel-typeplot'] == 'Heatmap'",
              sliderInput(ns("height"), "Plot height", 
                          value = 500, min = 200, max = 1300, post = "px")
            )
          )
        )
      })
      
      # Enable controls after calculation
      observeEvent(input$calculate, {
        enable("Design")
        enable("typeplot")
        enable("WhichComp")
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