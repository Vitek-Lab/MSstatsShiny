# ============================================================================
# Visualization Options and Plotting Functions
# ============================================================================

render_group_comparison_plot_inputs = function(output, session, rownames, get_data, input, loadpage_input, condition_list, contrast, app_template = reactive(TEMPLATES$default), condition_metadata = reactive(NULL)) {
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
      if (!is.null(app_template) && app_template() == TEMPLATES$protein_turnover) {
        # For protein turnover the drug axis is always "time"
        selectInput(session$ns(NAMESPACE_STATMODEL$visualization_response_curve_which_drug),
                    label = h5("Select Treatment"),
                    choices = "time", selected = "time")
      } else {
        if (isTRUE(app_template() == TEMPLATES$chemoproteomics)) {
          meta <- tryCatch(condition_metadata(), error = function(e) NULL)
          if (!is.null(meta) && "DoseVal" %in% colnames(meta)) {
            is_ctrl <- grepl("^(dmso|control|vehicle)$", tolower(meta$Condition))
            rc_mat <- data.frame(
              GROUP      = meta$Condition,
              dose_value = convert_dose_to_molar(suppressWarnings(as.numeric(meta$DoseVal)), if ("DoseUnit" %in% colnames(meta)) meta$DoseUnit else "nM"),
              drug       = ifelse(is_ctrl, meta$Condition, if ("DrugName" %in% colnames(meta)) meta$DrugName else parse_drug_name_from_conditions(meta$Condition)),
              stringsAsFactors = FALSE
            )
          }
        } 
        response_curve_setup_matrix = prepare_dose_response_fit(rc_mat)
        unique_drugs = unique(response_curve_setup_matrix$drug)
        unique_drugs_without_control = unique_drugs[!grepl("^(dmso|control|vehicle)$", tolower(unique_drugs))]
        selectInput(session$ns(NAMESPACE_STATMODEL$visualization_response_curve_which_drug),
                    label = h5("Select Treatment"),
                    unique_drugs_without_control, selected = unique_drugs_without_control[[1]])
      }
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
#' Get filename for plot download based on plot type
#' @param plot_type the current plot type string
#' @return filename string ending in .zip
get_download_plot_filename <- function(plot_type) {
  if (plot_type == CONSTANTS_STATMODEL$plot_type_response_curve) {
    paste("ResponseCurvePlot-", Sys.Date(), ".zip", sep = "")
  } else {
    paste("SummaryPlot-", Sys.Date(), ".zip", sep = "")
  }
}

#' Zip PDF files and copy to download destination
#' @param pdf_files character vector of PDF file paths
#' @param dest_file destination file path for the download
#' @return TRUE if successful, FALSE otherwise
zip_and_copy_plot <- function(pdf_files, dest_file) {
  if (length(pdf_files) == 0) {
    showNotification("No plot files were generated.", type = "error")
    return(FALSE)
  }
  zip_path <- tempfile("PlotDownload-", fileext = ".zip")
  on.exit(unlink(zip_path, force = TRUE), add = TRUE)
  utils::zip(zipfile = zip_path, files = pdf_files, flags = "-j")
  copied <- file.copy(zip_path, dest_file, overwrite = TRUE)
  if (!isTRUE(copied)) {
    showNotification("Failed to prepare plot download.", type = "error")
    return(FALSE)
  }
  return(TRUE)
}

#' @importFrom ggplot2 ggsave
#' @importFrom utils zip
create_download_plot_handler <- function(output, input, contrast, preprocess_data, data_comparison, loadpage_input, app_template = reactive(TEMPLATES$default), condition_metadata = reactive(NULL)) {
  output[[NAMESPACE_STATMODEL$visualization_download_plot_results]] <- downloadHandler(
    filename = function() {
      get_download_plot_filename(input[[NAMESPACE_STATMODEL$visualization_plot_type]])
    },
    content = function(file) {
      tryCatch(
        {
          if (input[[NAMESPACE_STATMODEL$visualization_plot_type]] ==
            CONSTANTS_STATMODEL$plot_type_response_curve) {
            # Generate response curve plot
            matrix <- contrast$matrix
            if (is.null(matrix)) {
              showNotification("Please build a contrast matrix first.", type = "error")
              return(NULL)
            }
            if (!is.null(app_template) && app_template() == TEMPLATES$protein_turnover) {
              ratios <- tryCatch(turnover_ratios(), error = function(e) NULL)
              if (is.null(ratios)) {
                showNotification("Turnover ratios not yet calculated.", type = "error")
                return(NULL)
              }
              dia_prepared <- prepare_turnover_for_dose_response(ratios)
            } else {
              if (isTRUE(app_template() == TEMPLATES$chemoproteomics)) {
                meta <- tryCatch(condition_metadata(), error = function(e) NULL)
                if (!is.null(meta) && "DoseVal" %in% colnames(meta)) {
                  is_ctrl <- grepl("^(dmso|control|vehicle)$", tolower(meta$Condition))
                  matrix <- data.frame(
                    GROUP      = meta$Condition,
                    dose_value = convert_dose_to_molar(suppressWarnings(as.numeric(meta$DoseVal)), if ("DoseUnit" %in% colnames(meta)) meta$DoseUnit else "nM"),
                    drug       = ifelse(is_ctrl, meta$Condition, if ("DrugName" %in% colnames(meta)) meta$DrugName else parse_drug_name_from_conditions(meta$Condition)),
                    stringsAsFactors = FALSE
                  )
                }
              }
              protein_level_data <- merge(preprocess_data()$ProteinLevelData, matrix, by = "GROUP")
              dia_prepared <- prepare_dose_response_fit(data = protein_level_data)
            }

            if (!is.null(app_template) && app_template() == TEMPLATES$protein_turnover) {
              response_plot <- visualizeResponseProtein(
                data = dia_prepared,
                protein_name = input[[NAMESPACE_STATMODEL$visualization_which_protein]],
                drug_name = input[[NAMESPACE_STATMODEL$visualization_response_curve_which_drug]],
                ratio_response = isTRUE(input[[NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale]]),
                show_ic50 = TRUE,
                add_ci = TRUE,
                transform_dose = FALSE,
                n_samples = 1000,
                increasing = input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]],
                precalculated_ratios = TRUE,
                color_by = "BaseSequence",
                target_response = 0.5
              )
            } else {
              response_plot <- visualizeResponseProtein(
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

            # Save plot to a temp PDF, then zip and copy
            pdf_path <- tempfile("ResponseCurvePlot-", fileext = ".pdf")
            on.exit(unlink(pdf_path, force = TRUE), add = TRUE)
            ggplot2::ggsave(pdf_path,
              plot = response_plot, device = "pdf",
              width = 10, height = 8
            )
            if (!zip_and_copy_plot(pdf_path, file)) return(NULL)
          } else {
            # Generate group comparison plot using a session-scoped temp directory
            plot_type <- input[[NAMESPACE_STATMODEL$visualization_plot_type]]
            fold_change_cutoff <- ifelse(
              !is.null(input[[NAMESPACE_STATMODEL$visualization_fold_change_input]]),
              input[[NAMESPACE_STATMODEL$visualization_fold_change_input]], 0
            )

            # Use a temp directory so the function saves the PDF natively
            temp_dir <- tempfile("plot_download_")
            dir.create(temp_dir)
            on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
            address_prefix <- file.path(temp_dir, "Ex_")

            if (loadpage_input()$BIO == "PTM") {
              groupComparisonPlotsPTM(
                data_comparison(),
                plot_type,
                sig = input[[NAMESPACE_STATMODEL$visualization_volcano_significance_cutoff]],
                FCcutoff = fold_change_cutoff,
                logBase.pvalue = as.integer(input[[NAMESPACE_STATMODEL$visualization_logp_base]]),
                ProteinName = input[[NAMESPACE_STATMODEL$visualization_volcano_display_protein_name]],
                which.Comparison = input[[NAMESPACE_STATMODEL$visualization_which_comparison]],
                address = address_prefix
              )
            } else {
              groupComparisonPlots(
                data = data_comparison()$ComparisonResult,
                type = plot_type,
                sig = input[[NAMESPACE_STATMODEL$visualization_volcano_significance_cutoff]],
                FCcutoff = fold_change_cutoff,
                logBase.pvalue = as.numeric(input[[NAMESPACE_STATMODEL$visualization_logp_base]]),
                ProteinName = input[[NAMESPACE_STATMODEL$visualization_volcano_display_protein_name]],
                numProtein = input[[NAMESPACE_STATMODEL$visualization_heatmap_number_proteins]],
                clustering = input[[NAMESPACE_STATMODEL$visualization_heatmap_cluster_option]],
                which.Comparison = input[[NAMESPACE_STATMODEL$visualization_which_comparison]],
                which.Protein = input[[NAMESPACE_STATMODEL$visualization_which_protein]],
                height = input[[NAMESPACE_STATMODEL$visualization_plot_height_slider]],
                address = address_prefix,
                isPlotly = FALSE
              )
            }

            # Find the PDF files the function saved to the temp directory and zip them
            pdf_files <- list.files(temp_dir, pattern = "\\.pdf$", full.names = TRUE)
            if (!zip_and_copy_plot(pdf_files, file)) return(NULL)
          }
        },
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          return(NULL)
        }
      )
    }
  )
}