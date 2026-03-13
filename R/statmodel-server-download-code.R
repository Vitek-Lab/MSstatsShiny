generate_analysis_code = function(qc_input, loadpage_input, comp_mat, input) {
  codes = preprocessDataCode(qc_input, loadpage_input)

  # Check if this is a response curve analysis
  is_response_curve = !is.null(input[[NAMESPACE_STATMODEL$comparison_mode]]) &&
    input[[NAMESPACE_STATMODEL$comparison_mode]] == CONSTANTS_STATMODEL$comparison_mode_response_curve

  if (is_response_curve) {
    increasing = input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]]
    transform_dose = input[[NAMESPACE_STATMODEL$modeling_response_curve_log_xaxis]]
    ratio_response = isTRUE(input[[NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale]])

    codes = paste(codes, "\n# Set up dose response analysis\n", sep = "")
    codes = paste(codes, "library(MSstatsResponse)\n", sep = "")
    codes = paste(codes, "condition_names = c(\"",
                  paste(comp_mat$GROUP, collapse = "\",\""), "\")\n", sep = "")
    codes = paste(codes, "contrast_matrix = setup_metadata(condition_names)\n", sep = "")

    codes = paste(codes, "\n# Prepare data for dose response fitting\n", sep = "")
    codes = paste(codes, "protein_level_data = merge(summarized$ProteinLevelData, contrast_matrix, by = \"GROUP\")\n", sep = "")
    codes = paste(codes, "prepared_data = prepare_dose_response_fit(protein_level_data)\n", sep = "")

    codes = paste(codes, "\n# Fit dose response curves\n", sep = "")
    codes = paste(codes, "response_results = doseResponseFit(\n",
                  "  data = prepared_data,\n",
                  "  increasing = ", increasing, ",\n",
                  "  transform_dose = ", transform_dose, ",\n",
                  "  ratio_response = FALSE\n)\n", sep = "")

    codes = paste(codes, "\n# Visualize response curves\n", sep = "")
    codes = paste(codes, "visualizeResponseProtein(\n",
                  "  data = prepared_data,\n",
                  "  protein_name = \"Enter protein name here\",\n",
                  "  drug_name = \"Enter drug name here\",\n",
                  "  ratio_response = ", ratio_response, ",\n",
                  "  show_ic50 = TRUE,\n",
                  "  add_ci = TRUE,\n",
                  "  transform_dose = ", transform_dose, ",\n",
                  "  n_samples = 1000,\n",
                  "  increasing = ", increasing, "\n)\n", sep = "")

    return(codes)
  }

  # Standard analysis - build contrast matrix
  codes = paste(codes, "\n# Create the contrast matrix\n", sep = "")
  codes = paste(codes, "contrast.matrix = NULL\n", sep = "")
  
  for (i in seq_len(nrow(comp_mat))) {
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