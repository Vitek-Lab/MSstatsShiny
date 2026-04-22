# ============================================================================
# Contrast Matrix Building Functions
# ============================================================================

#' Convert dose values from their measured unit to molar (M)
#' @noRd
convert_dose_to_molar <- function(dose_val, dose_unit) {
  multipliers <- c(nM=1e-9, nm=1e-9, uM=1e-6, um=1e-6, mM=1e-3, mm=1e-3, M=1, m=1)
  if (length(dose_unit) == 1) dose_unit <- rep(dose_unit, length(dose_val))
  mult <- unname(multipliers[dose_unit])
  mult[is.na(mult)] <- 1
  dose_val * mult
}

#' Parse dose unit from condition names (vectorized, per condition)
#' @noRd
parse_dose_unit_from_conditions <- function(conditions) {
  is_ctrl <- grepl("^(dmso|control|vehicle)$", tolower(trimws(conditions)))
  measurement <- str_extract(conditions, "[0-9.]+[a-zA-Z]+")
  unit <- str_extract(measurement, "[a-zA-Z]+")
  unit[is_ctrl] <- ""
  unit[!is_ctrl & (is.na(unit) | nchar(unit) == 0)] <- "?"
  unit
}

#' Parse drug name from condition names (returns "Treatment" if unparseable)
#' @noRd
parse_drug_name_from_conditions <- function(conditions) {
  is_ctrl <- grepl("^(dmso|control|vehicle)$", tolower(trimws(conditions)))
  result <- character(length(conditions))
  result[is_ctrl] <- conditions[is_ctrl]
  non_ctrl_idx <- which(!is_ctrl)
  if (length(non_ctrl_idx) == 0) return(result)
  prefixes <- gsub("[_ .-]?[0-9.]+[a-zA-Z]+.*$", "", conditions[non_ctrl_idx])
  prefixes <- trimws(prefixes, whitespace = "[ _\t.-]")
  prefixes[nchar(prefixes) == 0] <- "Treatment"
  result[non_ctrl_idx] <- prefixes
  result
}

#' Auto-fill numeric dose/time value from condition name
#' @noRd
autofill_condition_value <- function(conditions) {
  is_ctrl <- grepl("^(dmso|control|vehicle)$", tolower(trimws(conditions)))
  measurement <- str_extract(conditions, "[0-9.]+[a-zA-Z]+")
  value <- suppressWarnings(as.numeric(str_extract(measurement, "[0-9.]+")))
  value[is_ctrl] <- 0
  failed <- conditions[!is_ctrl & is.na(value)]
  if (length(failed) > 0) {
    showNotification(
      paste0(
        "Could not parse a dose value from: ",
        paste(failed, collapse = ", "),
        ". Edit the metadata table to enter values manually."
      ),
      type = "warning",
      duration = 10
    )
  }
  value
}

#' Get experimental conditions from preprocessed data
#'
#' @param loadpage_input List containing BIO, DDA_DIA, and filetype parameters
#' @param preprocess_data Preprocessed data object containing ProteinLevelData
#'
#' @return Character vector of condition levels
#' @noRd
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
#'
#' @param mode Character string indicating the comparison mode
#' @param ns Namespace function for module
#'
#' @return UI element or NULL
#' @noRd
get_contrast_panel_ui = function(mode, ns) {
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

#' Render all-against-one comparison input UI
#'
#' @param output Shiny output object
#' @param session Shiny session object
#' @param condition_list Reactive expression containing list of conditions
#'
#' @return NULL (side effect: renders UI)
#' @noRd
render_all_against_one_inputs = function(output, session, condition_list) {
  ns = session$ns
  
  output[[NAMESPACE_STATMODEL$comparisons_all_vs_one_choice]] = renderUI({
    selectInput(ns(NAMESPACE_STATMODEL$comparisons_all_vs_one_choice), "", condition_list())
  })
}

#' Render custom pairwise comparison input UI
#'
#' @param output Shiny output object
#' @param session Shiny session object
#' @param condition_list Reactive expression containing list of conditions
#'
#' @return NULL (side effect: renders UI)
#' @noRd
render_custom_pairwise_inputs = function(output, session, condition_list) {
  ns = session$ns
  
  output[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1]] = renderUI({
    selectInput(ns(NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1), "Group 1", condition_list())
  })
  
  output[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2]] = renderUI({
    selectInput(ns(NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2), "Group 2", condition_list())
  })
}

#' Render custom non-pairwise comparison input UI
#'
#' @param output Shiny output object
#' @param session Shiny session object
#' @param condition_list Reactive expression containing list of conditions
#'
#' @return NULL (side effect: renders UI)
#' @noRd
render_custom_non_pairwise_inputs = function(output, session, condition_list) {
  ns = session$ns
  output[[NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights]] = renderUI({
    lapply(seq_along(condition_list()), function(i) {
      list(numericInput(ns(paste0(
        NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, i)
      ), 
      label = condition_list()[i], value = 0))
    })
  })
}

#' Validate contrast inputs based on comparison mode
#'
#' @param input Shiny input object
#' @param contrast_mode Character string indicating the comparison mode
#' @param condition_list Character vector of condition names
#'
#' @return NULL (side effect: validates inputs, throws error if invalid)
#' @noRd
validate_contrast_inputs = function(input, contrast_mode, condition_list) {
  if (contrast_mode == CONSTANTS_STATMODEL$comparison_mode_custom_pairwise) {
    validate(
      need(input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1]] != input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2]], "Please select different groups")
    )
  } else if (contrast_mode == CONSTANTS_STATMODEL$comparison_mode_custom_nonpairwise) {
    wt_sum = sum(sapply(seq_along(condition_list), function(i) {
      input[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, i)]]
    }))
    
    validate(
      need(wt_sum == 0, "The contrast weights should sum up to 0")
    )
  }
}

#' Build custom pairwise contrast matrix
#'
#' @param input Shiny input object
#' @param condition_list Character vector of condition names
#' @param contrast List containing contrast row and matrix
#' @param comp_list List containing comparison labels (dList)
#' @param row Numeric vector template for contrast row
#'
#' @return Updated contrast matrix
#' @noRd
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

#' Build custom non-pairwise contrast matrix
#'
#' @param input Shiny input object
#' @param condition_list Character vector of condition names
#' @param contrast List containing contrast row and matrix
#' @param comp_list List containing comparison labels (dList)
#' @param row Numeric vector template for contrast row
#'
#' @return Updated contrast matrix
#' @noRd
build_custom_non_pairwise_contrast = function(input, condition_list, contrast, comp_list, row) {
  wt_sum = sum(sapply(seq_along(condition_list), function(i) {
    input[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, i)]]
  }))
  
  if (wt_sum != 0) {
    return(contrast$matrix)
  }
  
  comp_list$dList = unique(c(isolate(comp_list$dList), 
                             input[[NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_name]]))
  contrast$row = matrix(row, nrow = 1)
  
  for (index in seq_along(condition_list)) {
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

#' Build all-against-one contrast matrix
#'
#' @param input Shiny input object
#' @param condition_list Character vector of condition names
#' @param contrast List containing contrast row and matrix
#' @param comp_list List containing comparison labels (dList)
#' @param row Numeric vector template for contrast row
#' @param loadpage_input List containing load page parameters (unused)
#'
#' @return Updated contrast matrix
#' @noRd
build_all_against_one_contrast = function(input, condition_list, contrast, comp_list, row, loadpage_input) {
  index3 = which(condition_list == input[[NAMESPACE_STATMODEL$comparisons_all_vs_one_choice]])
  
  for (index in seq_along(condition_list)) {
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

#' Build all-pairwise contrast matrix
#'
#' @param input Shiny input object (unused)
#' @param condition_list Character vector of condition names
#' @param contrast List containing contrast row and matrix
#' @param comp_list List containing comparison labels (dList)
#' @param row Numeric vector template for contrast row
#' @param loadpage_input List containing load page parameters (unused)
#'
#' @return Updated contrast matrix
#' @noRd
build_all_pair_contrast = function(input, condition_list, contrast, comp_list, row, loadpage_input) {
  contrast$matrix = NULL
  comp_list$dList = NULL
  
  for (index in seq_along(condition_list)) {
    for (index1 in seq_along(condition_list)) {
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

#' Filter excluded conditions from a response curve matrix
#'
#' @param matrix Data frame. The response curve metadata matrix.
#' @param excluded Character vector. Condition names to exclude.
#' @return Filtered data frame with excluded conditions removed.
#' @noRd
.filter_excluded_conditions <- function(matrix, excluded = NULL) {
  if (is.null(excluded) || length(excluded) == 0) return(matrix)
  matrix[!(matrix$GROUP %in% excluded), , drop = FALSE]
}

#' Format turnover ratios for dose-response fitting
#'
#' Applies the column renaming and NA-imputation required by doseResponseFit /
#' visualizeResponseProtein, using TimeVal from the (user-edited) metadata
#' matrix stored in contrast$matrix as the dose axis.
#'
#' @param ratios Data frame from calculateTurnoverRatios (Protein, GROUP,
#'   H_frac columns required)
#' @return Data frame with columns: protein, drug, dose, response
#' @noRd
prepare_turnover_for_dose_response <- function(ratios) {
  result <- ratios[!is.na(ratios$H_frac), ]
  result$protein  <- as.character(result$Protein)
  result$drug     <- "time"
  result$dose     <- as.numeric(result$TimeVal)
  result$response <- result$H_frac
  keep_cols <- c("protein", "drug", "dose", "response")
  if ("BaseSequence" %in% colnames(result)) {
    keep_cols <- c(keep_cols, "BaseSequence")
  }
  result[, keep_cols]
}




#' Prepare data for dose-response fitting
#'
#' Transforms data into MSstatsResponse-compatible format by selecting and
#' renaming appropriate columns for dose-response analysis.
#'
#' @param data Data frame with GROUP, dose_value, and drug columns
#'
#' @return Data frame with columns: protein, drug, dose, response
#' @noRd
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
      validate(need(FALSE, "No intervention columns found (time, temperature, or treatment)"))
    }
  } else {
    intervention_value = "dose_value"
  }
  
  cols_to_use = c(
    protein = if("Protein" %in% colnames(data)) "Protein" else NA,
    drug = "drug",
    dose = intervention_value,
    response = if("LogIntensities" %in% colnames(data)) "LogIntensities" else NA
  )
  cols_to_use = cols_to_use[!is.na(cols_to_use)]
  subset_df = data[, cols_to_use, drop = FALSE]
  colnames(subset_df) = names(cols_to_use)
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
  i = info$row
  j = info$col
  v = info$value
  
  # Coerce the new value to the type of the target column to maintain data integrity
  if (is.data.frame(mat)) {
    # For data frames, coerce to the column's class.
    # tryCatch prevents the app from crashing if the user enters an invalid
    # value (e.g., text in a numeric column). If coercion fails, the original value is kept.
    v = tryCatch(as(v, class(mat[[j]])), error = function(e) v)
    mat[i, j] = v
  } else {
    # For matrices, all elements have the same type. Coerce to the matrix's class.
    v = tryCatch(as(v, class(mat[1, 1])), error = function(e) v)
    mat[i, j] = v
  }
  return(mat)
}
