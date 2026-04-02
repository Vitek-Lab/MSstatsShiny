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
      
      output[[NAMESPACE_STATMODEL$comparisons_conditional_panel]] = renderUI({
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
      output[[NAMESPACE_STATMODEL$modeling_section_header]] <- renderUI({
        get_modeling_section_header(input[[NAMESPACE_STATMODEL$comparison_mode]])
      })
      
      # Filter visualization dropdown based on comparison mode
      observeEvent(input[[NAMESPACE_STATMODEL$comparison_mode]], {
        req(input[[NAMESPACE_STATMODEL$comparison_mode]])
        mode <- input[[NAMESPACE_STATMODEL$comparison_mode]]
        if (mode == CONSTANTS_STATMODEL$comparison_mode_response_curve) {
          updateSelectInput(session, NAMESPACE_STATMODEL$visualization_plot_type,
            choices = c("Dose Response Curve" = CONSTANTS_STATMODEL$plot_type_response_curve)
          )
        } else {
          updateSelectInput(session, NAMESPACE_STATMODEL$visualization_plot_type,
            choices = c(
              "Volcano Plot" = CONSTANTS_STATMODEL$plot_type_volcano_plot,
              "Heatmap" = CONSTANTS_STATMODEL$plot_type_heatmap,
              "Comparison Plot" = CONSTANTS_STATMODEL$plot_type_comparison_plot
            )
          )
        }
      }, ignoreInit = TRUE)

      # Reset on configuration change
      observeEvent(c(input[[NAMESPACE_STATMODEL$comparison_mode]], loadpage_input()$proceed1), {
        contrast$matrix = NULL
        comp_list$dList = NULL
        significant$result = NULL

        # Auto-build response curve metadata when dose response mode is selected
        if (isTRUE(input[[NAMESPACE_STATMODEL$comparison_mode]] == 
            CONSTANTS_STATMODEL$comparison_mode_response_curve)) {
          tryCatch({
            rc_matrix <- build_response_curve_matrix(condition_list())
            if (is.null(rc_matrix) || nrow(rc_matrix) == 0) {
              stop("Unable to auto-build group metadata from the current conditions.")
            }
            contrast$matrix <- rc_matrix
            enable(NAMESPACE_STATMODEL$modeling_start)
          }, error = function(e) {
            contrast$matrix <- NULL
            disable(NAMESPACE_STATMODEL$modeling_start)
            showNotification(conditionMessage(e), type = "error", duration = 6)
          })
        }
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
      
      output[[NAMESPACE_STATMODEL$modeling_response_curve_fitting_options]] = renderUI({
        get_response_curve_fitting_options(
          input[[NAMESPACE_STATMODEL$comparison_mode]], session$ns)
      })
      
      output[[NAMESPACE_STATMODEL$modeling_tmt_moderation]] = renderUI({
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
        current_matrix = isolate(contrast$matrix)
        
        updated_matrix = update_matrix_from_edit(current_matrix, input$table_cell_edit)
        
        # Update the reactive value. This will trigger re-rendering of the table.
        contrast$matrix = updated_matrix
      })
      
      # Matrix output
      output$message = renderText({ check_cond() })
      output$table = renderDataTable({
        # This table now directly depends on contrast$matrix, so it updates on build or edit.
        req(contrast$matrix)
        mat = contrast$matrix
        
        # Define editable options, disabling the 'GROUP' column for response curves
        editable_options = list(target = 'cell')
        # Perform a case-insensitive check for the 'GROUP' column for robustness.
        if (any(toupper(colnames(mat)) == "GROUP")) {
          group_col_idx = which(toupper(colnames(mat)) == "GROUP")
          editable_options$disable = list(columns = group_col_idx)
        }
        
        DT::datatable(mat, editable = editable_options, options = list(scrollX = TRUE))
      })
      
      output$matrix = renderUI({
        ns = session$ns
        mode = input[[NAMESPACE_STATMODEL$comparison_mode]]
        matrix_title = if (isTRUE(mode == CONSTANTS_STATMODEL$comparison_mode_response_curve)) {
          "Group Metadata"
        } else {
          "Comparison matrix"
        }
        tagList(
          tags$head(tags$style(HTML(
            "table.dataTable td input { color: black !important; }"
          ))),
          h2(matrix_title),
          p(tags$i("This table is interactive. Click values to edit.")),
          if (!is.null(input[[NAMESPACE_STATMODEL$comparison_mode]]) &&
              input[[NAMESPACE_STATMODEL$comparison_mode]] %in% c(
                CONSTANTS_STATMODEL$comparison_mode_all_pairwise,
                CONSTANTS_STATMODEL$comparison_mode_all_vs_one,
                CONSTANTS_STATMODEL$comparison_mode_custom_pairwise
              )) {
            p(tags$i("A value of −1 represents the control group, and a value of 1 represents the treatment group"))
          },
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
      create_download_plot_handler(output, input, contrast, preprocess_data, data_comparison, loadpage_input)
      
      # Plot rendering
      output[[NAMESPACE_STATMODEL$visualization_plot_output]] = renderUI({
        req(input[[NAMESPACE_STATMODEL$visualization_view_results]])
        ns = session$ns
        
        if (loadpage_input()$BIO == "PTM") {
          output_plot = renderPlot({ 
            create_group_comparison_plot(
              input, loadpage_input(), data_comparison()
            )
          })
          
        } else if (input[[NAMESPACE_STATMODEL$visualization_plot_type]] == 
                   CONSTANTS_STATMODEL$plot_type_response_curve) {
          matrix = contrast$matrix
          protein_level_data = merge(preprocess_data()$ProteinLevelData, matrix, by = "GROUP")
          dia_prepared = prepare_dose_response_fit(data = protein_level_data)
          
          output_plot = renderPlot({ 
            visualizeResponseProtein(
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
          })
          
        } else {
          output_plot = renderPlotly({ 
            create_group_comparison_plot(
              input, loadpage_input(), data_comparison()
            )
          })
        }
        
        # Return the UI
        tags$div(
          output_plot,
          if (input[[NAMESPACE_STATMODEL$visualization_plot_type]] == CONSTANTS_STATMODEL$plot_type_volcano_plot && 
                loadpage_input()$BIO != "PTM") {
            h5("Hover over plot for details")
          },
          if (input[[NAMESPACE_STATMODEL$visualization_plot_type]] == CONSTANTS_STATMODEL$plot_type_heatmap) {
            sliderInput(ns(NAMESPACE_STATMODEL$visualization_plot_height_slider), "Plot height", 
                        value = 500, min = 200, max = 1300, post = "px")
          }
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