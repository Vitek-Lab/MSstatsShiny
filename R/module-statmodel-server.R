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
#' @param app_template reactive returning the selected template name (e.g. TEMPLATES$default)
#' @param turnover_ratios reactive returning the calculated turnover ratios
#' @param condition_metadata reactive returning the condition metadata table
#' @param tracer_constants reactive returning the tracer-constant provenance
#'   record snapshotted by the QC page at Run (values / source / file), or
#'   NULL when the data-processing page has not been run in this session
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
                           get_data, preprocess_data,
                           app_template = reactive(TEMPLATES$default),
                           turnover_ratios = reactive(NULL),
                           condition_metadata = reactive(NULL),
                           tracer_constants = reactive(NULL)) {
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

      # Apply template-specific defaults when the template selection changes.
      # Each template can override UI control defaults here.
      observeEvent(app_template(), {
        template = app_template()
        if (template == TEMPLATES$protein_turnover) {
          updateRadioButtons(session, NAMESPACE_STATMODEL$comparison_mode,
                             choices = c("Create turnover time-course curves" = CONSTANTS_STATMODEL$comparison_mode_response_curve),
                             selected = CONSTANTS_STATMODEL$comparison_mode_response_curve)
          updateSelectInput(session, NAMESPACE_STATMODEL$visualization_plot_type,
                            choices = c("Turnover Curve" = CONSTANTS_STATMODEL$plot_type_response_curve),
                            selected = CONSTANTS_STATMODEL$plot_type_response_curve)
          updateCheckboxInput(session, NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend, value = TRUE)
          shinyjs::hide("statmodel_contrast_header", asis = TRUE)
          shinyjs::hide("statmodel_workflow_bullet_default", asis = TRUE)
          shinyjs::show("statmodel_workflow_bullet_response_curve", asis = TRUE)
        } else if (template == TEMPLATES$chemoproteomics) {
          updateRadioButtons(session, NAMESPACE_STATMODEL$comparison_mode,
                             choices = c("Create dose-response curves" = CONSTANTS_STATMODEL$comparison_mode_response_curve),
                             selected = CONSTANTS_STATMODEL$comparison_mode_response_curve)
          updateSelectInput(session, NAMESPACE_STATMODEL$visualization_plot_type,
                            choices = c("Dose-Response Curve" = CONSTANTS_STATMODEL$plot_type_response_curve),
                            selected = CONSTANTS_STATMODEL$plot_type_response_curve)
          updateCheckboxInput(session, NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend, value = FALSE)
          shinyjs::hide("statmodel_contrast_header", asis = TRUE)
          shinyjs::hide("statmodel_workflow_bullet_default", asis = TRUE)
          shinyjs::show("statmodel_workflow_bullet_response_curve", asis = TRUE)
        } else {
          updateRadioButtons(session, NAMESPACE_STATMODEL$comparison_mode,
                             choices = c(
                               "All possible pairwise comparisons"      = CONSTANTS_STATMODEL$comparison_mode_all_pairwise,
                               "Compare all against one"                = CONSTANTS_STATMODEL$comparison_mode_all_vs_one,
                               "Create custom pairwise comparisons"     = CONSTANTS_STATMODEL$comparison_mode_custom_pairwise,
                               "Create custom non-pairwise comparisons" = CONSTANTS_STATMODEL$comparison_mode_custom_nonpairwise
                             ),
                             selected = character(0))
          updateSelectInput(session, NAMESPACE_STATMODEL$visualization_plot_type,
                            choices = default_template_plot_type_choices(
                              include_qq = !isTRUE(loadpage_input()$BIO == "PTM")
                            ))
          updateCheckboxInput(session, NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend, value = FALSE)
          shinyjs::show("statmodel_contrast_header", asis = TRUE)
          shinyjs::show("statmodel_workflow_bullet_default", asis = TRUE)
          shinyjs::hide("statmodel_workflow_bullet_response_curve", asis = TRUE)
        }
      }, ignoreInit = FALSE)

      # UI visibility
      observe({
        if (isTRUE(loadpage_input()$DDA_DIA == "TMT") || isTRUE(loadpage_input()$BIO == "PTM")) {
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
      
      render_group_comparison_plot_inputs(output, session, Rownames, get_data, input, loadpage_input, condition_list, contrast, app_template, condition_metadata, preprocess_data)

      output[[NAMESPACE_STATMODEL$comparisons_exclude_conditions]] <- renderUI({
        req(input[[NAMESPACE_STATMODEL$comparison_mode]] ==
              CONSTANTS_STATMODEL$comparison_mode_response_curve)
        conditions <- condition_list()
        tooltip_text <- if (isTRUE(app_template() == TEMPLATES$protein_turnover)) {
          "Select time points to exclude from turnover curve fitting (e.g., outlier time points)."
        } else {
          "Select conditions to exclude from dose-response modeling (e.g., quality control conditions like PDPD)."
        }
        selectizeInput(
          session$ns(NAMESPACE_STATMODEL$comparisons_exclude_conditions),
          label = h5("Exclude conditions from analysis", class = "icon-wrapper",
                     icon("question-circle", lib = "font-awesome"),
                     div(tooltip_text, class = "icon-tooltip")),
          choices = conditions,
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = "None excluded")
        )
      })

      output[[NAMESPACE_STATMODEL$modeling_section_header]] <- renderUI({
        get_modeling_section_header(input[[NAMESPACE_STATMODEL$comparison_mode]], app_template())
      })

      # Auto-generate unique default name for non-pairwise comparisons
      observe({
        req(input[[NAMESPACE_STATMODEL$comparison_mode]] ==
              CONSTANTS_STATMODEL$comparison_mode_custom_nonpairwise)
        existing_names <- if (!is.null(contrast$matrix)) rownames(contrast$matrix) else character(0)
        # Find the next available "custom comparison N"
        n <- 1
        while (paste0("custom comparison ", n) %in% existing_names) {
          n <- n + 1
        }
        updateTextInput(session,
                        NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_name,
                        value = paste0("custom comparison ", n))
      })
      
      # Reset on configuration change
      observeEvent(c(input[[NAMESPACE_STATMODEL$comparison_mode]], loadpage_input()$proceed1), {
        contrast$matrix = NULL
        comp_list$dList = NULL
        significant$result = NULL

        # Auto-build response curve metadata when dose response mode is selected
        if (isTRUE(input[[NAMESPACE_STATMODEL$comparison_mode]] ==
            CONSTANTS_STATMODEL$comparison_mode_response_curve)) {
          tryCatch({
            if (app_template() == TEMPLATES$protein_turnover) {
              meta <- tryCatch(condition_metadata(), error = function(e) NULL)
              if (!is.null(meta) && nrow(meta) > 0) {
                rc_matrix <- data.frame(GROUP = meta$Condition, TimeVal = meta$TimeVal,
                                        stringsAsFactors = FALSE)
              } else {
                showNotification(
                  "Please enter time values for each condition on the Data Uploading page.",
                  type = "warning", duration = 8
                )
                disable(NAMESPACE_STATMODEL$modeling_start)
                return()
              }
            } else if (app_template() == TEMPLATES$chemoproteomics) {
              meta <- tryCatch(condition_metadata(), error = function(e) NULL)
              if (!is.null(meta) && nrow(meta) > 0 && "DoseVal" %in% colnames(meta)) {
                is_ctrl <- grepl("^(dmso|control|vehicle)$", tolower(meta$Condition))
                rc_matrix <- data.frame(
                  GROUP      = meta$Condition,
                  dose_value = convert_dose_to_molar(suppressWarnings(as.numeric(meta$DoseVal)), if ("DoseUnit" %in% colnames(meta)) meta$DoseUnit else "nM"),
                  drug       = ifelse(is_ctrl, meta$Condition, if ("DrugName" %in% colnames(meta)) meta$DrugName else parse_drug_name_from_conditions(meta$Condition)),
                  stringsAsFactors = FALSE
                )
              } else {
                showNotification(
                  "Please enter dose values for each condition on the Data Uploading page.",
                  type = "warning", duration = 8
                )
                disable(NAMESPACE_STATMODEL$modeling_start)
                return()
              }
            }
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

      # Propagate loadpage metadata edits to contrast$matrix for response-curve templates.
      # Fires whenever condition_metadata() changes (e.g. user corrects a cell in loadpage).
      observeEvent(tryCatch(condition_metadata(), error = function(e) NULL), {
        req(isTRUE(input[[NAMESPACE_STATMODEL$comparison_mode]] ==
                     CONSTANTS_STATMODEL$comparison_mode_response_curve))
        meta <- tryCatch(condition_metadata(), error = function(e) NULL)
        req(!is.null(meta) && nrow(meta) > 0)

        if (isTRUE(app_template() == TEMPLATES$protein_turnover)) {
          contrast$matrix <- data.frame(GROUP = meta$Condition, TimeVal = meta$TimeVal,
                                        stringsAsFactors = FALSE)
          enable(NAMESPACE_STATMODEL$modeling_start)
        } else if (isTRUE(app_template() == TEMPLATES$chemoproteomics) &&
                   "DoseVal" %in% colnames(meta)) {
          is_ctrl <- grepl("^(dmso|control|vehicle)$", tolower(meta$Condition))
          contrast$matrix <- data.frame(
            GROUP      = meta$Condition,
            dose_value = convert_dose_to_molar(suppressWarnings(as.numeric(meta$DoseVal)), if ("DoseUnit" %in% colnames(meta)) meta$DoseUnit else "nM"),
            drug       = ifelse(is_ctrl, meta$Condition, if ("DrugName" %in% colnames(meta)) meta$DrugName else parse_drug_name_from_conditions(meta$Condition)),
            stringsAsFactors = FALSE
          )
          enable(NAMESPACE_STATMODEL$modeling_start)
        }
      }, ignoreNULL = TRUE)

      # Re-filter matrix when excluded conditions change (default template only;
      # protein_turnover and chemoproteomics metadata comes from loadpage, not condition names)
      observeEvent(input[[NAMESPACE_STATMODEL$comparisons_exclude_conditions]], {
        req(input[[NAMESPACE_STATMODEL$comparison_mode]] ==
              CONSTANTS_STATMODEL$comparison_mode_response_curve)
        req(isTRUE(app_template() == TEMPLATES$chemoproteomics))
        tryCatch({
          all_conditions <- condition_list()
          excluded <- input[[NAMESPACE_STATMODEL$comparisons_exclude_conditions]]
          filtered_conditions <- setdiff(all_conditions, excluded)
          if (length(filtered_conditions) < 2) {
            showNotification("At least 2 conditions are required after exclusion.", type = "error")
            return()
          }
          meta <- tryCatch(condition_metadata(), error = function(e) NULL)
          if (is.null(meta) || nrow(meta) == 0 || !("DoseVal" %in% colnames(meta))) {
            stop("Unable to build group metadata from the included conditions.")
          }
          meta <- meta[meta$Condition %in% filtered_conditions, , drop = FALSE]
          if (nrow(meta) == 0) {
            stop("Unable to build group metadata from the included conditions.")
          }
          is_ctrl <- grepl("^(dmso|control|vehicle)$", tolower(meta$Condition))
          rc_matrix <- data.frame(
            GROUP      = meta$Condition,
            dose_value = convert_dose_to_molar(suppressWarnings(as.numeric(meta$DoseVal)), if ("DoseUnit" %in% colnames(meta)) meta$DoseUnit else "nM"),
            drug       = ifelse(is_ctrl, meta$Condition, if ("DrugName" %in% colnames(meta)) meta$DrugName else parse_drug_name_from_conditions(meta$Condition)),
            stringsAsFactors = FALSE
          )
          if (is.null(rc_matrix) || nrow(rc_matrix) == 0) {
            stop("Unable to build group metadata from the included conditions.")
          }
          contrast$matrix <- rc_matrix
          enable(NAMESPACE_STATMODEL$modeling_start)
        }, error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 6)
        })
      }, ignoreInit = TRUE, ignoreNULL = FALSE)

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
            if (app_template() == TEMPLATES$protein_turnover) {
              meta <- tryCatch(condition_metadata(), error = function(e) NULL)
              if (!is.null(meta) && nrow(meta) > 0) {
                contrast$matrix = data.frame(GROUP = meta$Condition, TimeVal = meta$TimeVal,
                                             stringsAsFactors = FALSE)
              }
            } else if (app_template() == TEMPLATES$chemoproteomics) {
              meta <- tryCatch(condition_metadata(), error = function(e) NULL)
              if (!is.null(meta) && nrow(meta) > 0 && "DoseVal" %in% colnames(meta)) {
                is_ctrl <- grepl("^(dmso|control|vehicle)$", tolower(meta$Condition))
                contrast$matrix = data.frame(
                  GROUP      = meta$Condition,
                  dose_value = convert_dose_to_molar(suppressWarnings(as.numeric(meta$DoseVal)), if ("DoseUnit" %in% colnames(meta)) meta$DoseUnit else "nM"),
                  drug       = ifelse(is_ctrl, meta$Condition, if ("DrugName" %in% colnames(meta)) meta$DrugName else parse_drug_name_from_conditions(meta$Condition)),
                  stringsAsFactors = FALSE
                )
              }
            }
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
          input[[NAMESPACE_STATMODEL$comparison_mode]], session$ns, app_template())
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
          if (isTRUE(app_template() %in% c(TEMPLATES$protein_turnover, TEMPLATES$chemoproteomics))) {
            meta <- tryCatch(condition_metadata(), error = function(e) NULL)
            if (!is.null(meta)) {
              check_cols <- intersect(c("DoseVal", "TimeVal", "DoseUnit"), colnames(meta))
              if (any(sapply(check_cols, function(col) "?" %in% meta[[col]]))) {
                showNotification(
                  "Please fill in all '?' values in the condition metadata table on the Data Uploading page before running the analysis.",
                  type = "error", duration = 8
                )
                req(FALSE)
              }
            }
          }
          if (app_template() == TEMPLATES$protein_turnover) {
            ratios <- turnover_ratios()
            increasing <- isTRUE(input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]])
            dia_prepared <- prepare_turnover_for_dose_response(ratios, increasing = increasing)
            turnover_weights <- if ("weight" %in% colnames(dia_prepared)) dia_prepared$weight else NULL
            response_results <- doseResponseFit(
              data      = dia_prepared,
              weights   = turnover_weights,
              increasing    = increasing,
              transform_dose = FALSE,
              ratio_response = FALSE,
              precalculated_ratios = TRUE
            )
            list(ComparisonResult = response_results)
          } else if (app_template() == TEMPLATES$chemoproteomics) {
            meta <- condition_metadata()
            req(!is.null(meta) && "DoseVal" %in% colnames(meta))
            is_ctrl <- grepl("^(dmso|control|vehicle)$", tolower(meta$Condition))
            matrix <- data.frame(
              GROUP      = meta$Condition,
              dose_value = convert_dose_to_molar(suppressWarnings(as.numeric(meta$DoseVal)), if ("DoseUnit" %in% colnames(meta)) meta$DoseUnit else "nM"),
              drug       = ifelse(is_ctrl, meta$Condition, if ("DrugName" %in% colnames(meta)) meta$DrugName else parse_drug_name_from_conditions(meta$Condition)),
              stringsAsFactors = FALSE
            )
            fitResponseCurves(input, matrix, preprocess_data(), transform_dose = TRUE)
          } else {
            fitResponseCurves(input, matrix, preprocess_data(), transform_dose = TRUE)
          }
        } else {
          dataComparison(input, qc_input(), loadpage_input(), matrix, preprocess_data())
        }
      })
      
      data_comparison_code = eventReactive(input[[NAMESPACE_STATMODEL$modeling_start]], {
        req(contrast$matrix)
        comp_mat = contrast$matrix
        # Isolated by eventReactive, so this is a snapshot taken when the model
        # is run -- and the QC page already snapshotted it at summarization, so
        # the script can only ever cite constants that produced ratios the user
        # actually saw (plan Decision I). tryCatch mirrors every other optional
        # reactive read in this module: a caller that supplies something raising
        # here must not take the Download-code button out on all four templates.
        tracer_used = tryCatch(tracer_constants(), error = function(e) NULL)
        generate_analysis_code(qc_input(), loadpage_input(), comp_mat, input,
                               app_template(), tracer_used)
      })
      
      SignificantProteins = eventReactive(input[[NAMESPACE_STATMODEL$modeling_start]], {
        data_comp = data_comparison()
        extract_significant_proteins(data_comp, loadpage_input(), input[[NAMESPACE_STATMODEL$modeling_significance_level]])
      })
      
      # Handle edits to the contrast matrix from the UI
      observeEvent(input$table_cell_edit, {
        current_matrix = isolate(contrast$matrix)
        updated_matrix = update_matrix_from_edit(current_matrix, input$table_cell_edit)
        contrast$matrix = updated_matrix
      })
      
      # Matrix output
      output$message = renderText({ check_cond() })
      output$table = renderDataTable({
        req(contrast$matrix)
        mat = contrast$matrix
        
        editable_options = list(target = 'cell')
        if (any(toupper(colnames(mat)) == "GROUP")) {
          group_col_idx = which(toupper(colnames(mat)) == "GROUP")
          editable_options$disable = list(columns = group_col_idx)
        }
        
        DT::datatable(mat, editable = editable_options, options = list(scrollX = TRUE))
      })
      
      output$matrix = renderUI({
        if (!is.null(app_template) && !is.null(app_template()) &&
            app_template() %in% c(TEMPLATES$protein_turnover, TEMPLATES$chemoproteomics)) {
          return(NULL)
        }
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
            p(tags$i("A value of -1 represents the control group, and a value of 1 represents the treatment group"))
          },
          br(),
          textOutput(ns("message")),
          br(),
          if (is.null(contrast$matrix)) "" else dataTableOutput(ns("table"))
        )
      })
      
      # Results rendering
      render_results_table(output, session, data_comparison, SignificantProteins, app_template = app_template)
      render_ptm_results_tables(output, session, data_comparison, SignificantProteins)

      # Download handlers
      create_download_handlers(output, data_comparison, SignificantProteins,
                               data_comparison_code)
      create_ptm_download_handlers(output, data_comparison, SignificantProteins)
      create_download_plot_handler(output, input, contrast, preprocess_data, data_comparison, loadpage_input, app_template, turnover_ratios, condition_metadata)
      
      # Plot rendering
      output[[NAMESPACE_STATMODEL$visualization_plot_output]] = renderUI({
        req(input[[NAMESPACE_STATMODEL$visualization_view_results]])
        ns = session$ns
        
        if (input[[NAMESPACE_STATMODEL$visualization_plot_type]] ==
                   CONSTANTS_STATMODEL$plot_type_response_curve) {
          if (app_template() == TEMPLATES$protein_turnover) {
            req(turnover_ratios())
            increasing <- isTRUE(input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]])
            dia_prepared <- prepare_turnover_for_dose_response(turnover_ratios(), add_zero_timepoint = TRUE, increasing = increasing)
            turnover_weights <- if ("weight" %in% colnames(dia_prepared)) dia_prepared$weight else NULL
          } else {
            meta <- condition_metadata()
            req(!is.null(meta) && "DoseVal" %in% colnames(meta))
            is_ctrl <- grepl("^(dmso|control|vehicle)$", tolower(meta$Condition))
            matrix <- data.frame(
              GROUP      = meta$Condition,
              dose_value = convert_dose_to_molar(suppressWarnings(as.numeric(meta$DoseVal)), if ("DoseUnit" %in% colnames(meta)) meta$DoseUnit else "nM"),
              drug       = ifelse(is_ctrl, meta$Condition, if ("DrugName" %in% colnames(meta)) meta$DrugName else parse_drug_name_from_conditions(meta$Condition)),
              stringsAsFactors = FALSE
            )
            protein_level_data = merge(preprocess_data()$ProteinLevelData, matrix, by = "GROUP")
            dia_prepared = prepare_dose_response_fit(data = protein_level_data)
          }

          output_plot = renderPlot({
            if (app_template() == TEMPLATES$protein_turnover) {
              visualizeResponseProtein(
                data = dia_prepared,
                protein_name = input[[NAMESPACE_STATMODEL$visualization_which_protein]],
                drug_name = "time",
                weights = turnover_weights,
                show_weights = !is.null(turnover_weights),
                ratio_response = FALSE,
                show_ic50 = TRUE,
                add_ci = FALSE,
                transform_dose = FALSE,
                n_samples = 1000,
                increasing = increasing,
                precalculated_ratios = TRUE,
                color_by = "BaseSequence",
                target_response = 0.5,
                y_lab = "Turnover Ratio",
                x_lab = "time (hrs)"
              )
            } else {
              visualizeResponseProtein(
                data = dia_prepared,
                protein_name = input[[NAMESPACE_STATMODEL$visualization_which_protein]],
                drug_name = input[[NAMESPACE_STATMODEL$visualization_response_curve_which_drug]],
                ratio_response = isTRUE(input[[NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale]]),
                show_ic50 = TRUE,
                add_ci = TRUE,
                transform_dose = TRUE,
                n_samples = 1000,
                increasing = input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]]
              )
            }
          })

        } else if (input[[NAMESPACE_STATMODEL$visualization_plot_type]] ==
                   CONSTANTS_STATMODEL$plot_type_qq_plot) {
          output_plot = renderPlot({
            req(input[[NAMESPACE_STATMODEL$visualization_which_protein]])
            show_modal_spinner()
            tryCatch({
              MSstats::groupComparisonQCPlots(
                data = data_comparison(),
                type = "QQPlots",
                which.Protein = input[[NAMESPACE_STATMODEL$visualization_which_protein]],
                address = FALSE
              )
            }, error = function(e) {
              showNotification(conditionMessage(e), type = "error", duration = 8)
              NULL
            }, finally = { remove_modal_spinner() })
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
        dataComparison = data_comparison,
        contrast = contrast
      ))
    }
  )
}