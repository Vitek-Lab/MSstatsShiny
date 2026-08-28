#' Build the downloadable reproducible-analysis script.
#'
#' @param tracer_constants The tracer-constant provenance record snapshotted by
#'   the QC page at Run: a list of `values` (named numeric, keyed by raw
#'   condition string), `source` ("upload" / "none") and `file`. `NULL` means the
#'   QC page was never run in this session -- a reachable state, not a defect
#'   (plan section 0.6), because the Download-code button only requires a
#'   contrast matrix. Trailing with a NULL default so the existing positional
#'   call sites -- production plus nine in the tests -- stay valid (plan
#'   Decision E).
#' @noRd
generate_analysis_code = function(qc_input, loadpage_input, comp_mat, input,
                                  app_template = TEMPLATES$default,
                                  tracer_constants = NULL) {
  codes = preprocessDataCode(qc_input, loadpage_input)

  # Check if this is a response curve analysis
  is_response_curve = !is.null(input[[NAMESPACE_STATMODEL$comparison_mode]]) &&
    input[[NAMESPACE_STATMODEL$comparison_mode]] == CONSTANTS_STATMODEL$comparison_mode_response_curve

  if (is_response_curve) {
    increasing = input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]]
    transform_dose = TRUE
    ratio_response = isTRUE(input[[NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale]])

    codes = paste(codes, "\n# Set up dose response analysis\n", sep = "")
    codes = paste(codes, "library(MSstatsResponse)\n", sep = "")

    if (isTRUE(app_template == TEMPLATES$protein_turnover)) {
      # Resolved HERE rather than at the top of the function, deliberately: on
      # every other template comp_mat is a bare matrix, where `$GROUP` throws
      # "$ operator is invalid for atomic vectors" and would take the
      # Download-code button out on all four templates.
      if (is.null(tracer_constants)) {
        tracer_constants = list(
          values = qc_default_tracer_constants(as.character(comp_mat$GROUP)),
          source = CONSTANTS_QC$tracer_source_not_run,
          file   = NULL
        )
      }
      codes = paste(codes, build_turnover_analysis_code(qc_input, comp_mat, increasing,
                                                       tracer_constants), sep = "")
      return(codes)
    }

    # Serialize the contrast matrix as a data frame
    codes = paste(codes, "group_metadata = data.frame(\n", sep = "")
    codes = paste(codes, "  GROUP = c(\"", paste(comp_mat$GROUP, collapse = "\",\""), "\"),\n", sep = "")
    for (col_name in setdiff(colnames(comp_mat), "GROUP")) {
      col_values = comp_mat[[col_name]]
      if (is.numeric(col_values)) {
        codes = paste(codes, "  ", col_name, " = c(", paste(col_values, collapse = ","), "),\n", sep = "")
      } else {
        codes = paste(codes, "  ", col_name, " = c(\"", paste(col_values, collapse = "\",\""), "\"),\n", sep = "")
      }
    }
    codes = paste(codes, "  stringsAsFactors = FALSE\n)\n", sep = "")

    # Merge and map columns explicitly (prepare_dose_response_fit is internal to MSstatsShiny)
    codes = paste(codes, "\n# Merge metadata with protein-level data\n", sep = "")
    codes = paste(codes, "protein_level_data = merge(summarized$ProteinLevelData, group_metadata, by = \"GROUP\")\n", sep = "")

    codes = paste(codes, "\n# Map columns to MSstatsResponse format\n", sep = "")
    codes = paste(codes, "protein_level_data$protein = protein_level_data$Protein\n", sep = "")
    codes = paste(codes, "protein_level_data$response = protein_level_data$LogIntensities\n", sep = "")

    # Determine the intervention column based on what's in the contrast matrix
    if ("drug" %in% colnames(comp_mat)) {
      codes = paste(codes, "protein_level_data$drug = protein_level_data$drug\n", sep = "")
      codes = paste(codes, "protein_level_data$dose = protein_level_data$dose_value\n", sep = "")
    } else {
      intervention_cols = grep("time|temperature|treatment", colnames(comp_mat),
                               ignore.case = TRUE, value = TRUE)
      if (length(intervention_cols) > 0) {
        intervention_type = sub("_.*", "", intervention_cols[1])
        value_col = paste0(intervention_type, "_value")
        codes = paste(codes, "protein_level_data$drug = \"", intervention_type, "\"\n", sep = "")
        codes = paste(codes, "protein_level_data$dose = protein_level_data$", value_col, "\n", sep = "")
      }
    }

    codes = paste(codes, "prepared_data = protein_level_data[, c(\"protein\", \"drug\", \"dose\", \"response\")]\n", sep = "")

    codes = paste(codes, "\n# Fit dose response curves\n", sep = "")
    codes = paste(codes, "response_results = doseResponseFit(\n",
                  "  data = prepared_data,\n",
                  "  increasing = ", increasing, ",\n",
                  "  transform_dose = ", transform_dose, ",\n",
                  "  ratio_response = FALSE\n)\n", sep = "")

    codes = paste(codes, "\n# Visualize response curves\n", sep = "")
    if (app_template == TEMPLATES$protein_turnover) {
      codes = paste(codes, "visualizeResponseProtein(\n",
                    "  data = prepared_data,\n",
                    "  protein_name = \"Enter protein name here\",\n",
                    "  drug_name = \"time\",\n",
                    "  ratio_response = FALSE,\n",
                    "  show_ic50 = TRUE,\n",
                    "  add_ci = FALSE,\n",
                    "  transform_dose = FALSE,\n",
                    "  n_samples = 1000,\n",
                    "  increasing = ", increasing, ",\n",
                    "  precalculated_ratios = TRUE,\n",
                    "  color_by = \"BaseSequence\",\n",
                    "  target_response = 0.5\n)\n", sep = "")
    } else {
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
    }

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
  
  if (isTRUE(loadpage_input$BIO == "PTM")) {
    dt = if ((isTRUE(loadpage_input$BIO == "PTM") & isTRUE(loadpage_input$DDA_DIA == "TMT")) |
             isTRUE(loadpage_input$filetype == 'phil')) "TMT" else "LabelFree"

    codes = paste(codes, "\n# Model-based comparison\n", sep = "")
    codes = paste(codes, "model = MSstatsPTM::groupComparisonPTM(summarized, '",
                  dt, "', \t\t\t\t
                      contrast.matrix = contrast.matrix)\n", sep = "")
  } else if (isTRUE(loadpage_input$DDA_DIA == "TMT")) {
    codes = paste(codes, "\n# Model-based comparison\n", sep = "")
    codes = paste(codes, "model = MSstatsTMT::groupComparisonTMT(summarized,
                       contrast.matrix = contrast.matrix,
                       moderated = ", input[[NAMESPACE_STATMODEL$modeling_tmt_moderation]], ",\t\t\t\t
                       adj.method = \"BH\",
                       remove_norm_channel = TRUE,
                       remove_empty_channel = TRUE
                       )\n", sep = "")
  } else {
    codes = paste(codes, "\n# Model-based comparison\n", sep = "")
    codes = paste(codes, "model = MSstats::groupComparison(contrast.matrix, summarized)\n", sep = "")
  }
  
  plot_type = input[[NAMESPACE_STATMODEL$visualization_plot_type]]

  if (loadpage_input$BIO == "PTM") {
    codes = paste(codes, "groupComparisonPlotsPTM(data=model,
                               type=\"Enter VolcanoPlot, Heatmap, or ComparisonPlot\",
                               which.Comparison=\"all\",
                               which.PTM=\"all\",
                               address=\"\")\n", sep = "")
  } else if (!is.null(plot_type) && plot_type == CONSTANTS_STATMODEL$plot_type_qq_plot) {
    codes = paste(codes, "MSstats::groupComparisonQCPlots(data=model,
                               type=\"QQPlots\",
                               which.Protein=\"Enter a single protein name\",
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


#' The provenance comment stamped above the tracer_constants vector.
#'
#' Three states, not two (plan section 0.6). The one v1 of this feature would
#' have written -- "# No tracer constants file uploaded" -- was asserted on a
#' path where a REJECTED file had silently fallen back to all-1s, so the comment
#' actively vouched for the wrong thing. Exactly one of the three lines is
#' emitted, so the script says what happened rather than what was intended.
#'
#' The three wordings are deliberately chosen so that no one of them contains a
#' distinguishing phrase of another. "uploaded on the data-processing page" was
#' previously a substring of the no-upload sentence ("... was uploaded on the
#' data-processing page"), which made any grep for the upload state -- in a test,
#' a script, or by a user reading the file -- match the case where nothing was
#' uploaded at all. See the mutual-exclusivity test in test-module-turnover.R.
#' @noRd
build_tracer_provenance_comment <- function(tracer_constants) {
  source <- tracer_constants$source
  if (is.null(source)) source <- CONSTANTS_QC$tracer_source_not_run

  if (identical(source, CONSTANTS_QC$tracer_source_upload)) {
    file <- tracer_constants$file
    # encodeString, not paste0: a file name containing a newline would otherwise
    # split the comment and leave the remainder as (invalid) code.
    # length() rather than is.null() alone: `||` errors on a zero-length operand
    # in current R, so a character(0) file name would take the whole script
    # generator down instead of degrading to the generic label.
    label <- if (length(file) != 1L || is.na(file) || !nzchar(file)) "an uploaded file"
             else encodeString(as.character(file), quote = "\"")
    return(paste0(
      "# Tracer constants: uploaded on the data-processing page, from ", label, ".\n"))
  }
  if (identical(source, CONSTANTS_QC$tracer_source_none)) {
    return(paste0(
      "# Tracer constants: no file was supplied on the data-processing page, so\n",
      "# every condition uses 1 (no tracer correction).\n"))
  }
  paste0(
    "# Tracer constants: the data-processing page was not run in this session,\n",
    "# so every condition uses 1 (no tracer correction).\n")
}

#' Serialize a double as R source that reads back as the identical double.
#'
#' `paste0(1/3)` emits 15 significant digits, so the constant in the downloaded
#' script is NOT the constant the app divided by -- a reproducible script that
#' silently reproduces something else. The shortest round-tripping form is
#' preferred so ordinary values stay readable ("0.9", not "0.90000000000000002").
#'
#' The 17-digit fallback is best-effort: R's own string-to-double conversion is
#' not always correctly rounded (verified on R 4.5.2, `as.numeric(sprintf("%.17g",
#' 0.123456789012345678))` is 1 ULP off), so a hand-crafted double need not
#' round-trip through any decimal form. Irrelevant in practice here, because
#' tracer constants arrive as short decimal text in a CSV.
#' @noRd
format_r_double <- function(value) {
  # suppressWarnings because as.numeric("NA") warns. Callers are expected to have
  # rejected non-finite values already, but a warning escaping a downloadHandler
  # is a confusing way to learn that they did not.
  for (digits in 15:17) {
    text <- sprintf(paste0("%.", digits, "g"), value)
    if (identical(suppressWarnings(as.numeric(text)), value)) return(text)
  }
  sprintf("%.17g", value)
}

#' Generate reproducible code for the protein-turnover dose-response pipeline.
#'
#' Mirrors the app's turnover flow: calculateTurnoverRatios (with the resolved
#' tracer constants), an optional calculatePeptideWeights step when
#' "Assign feature weights" is checked, then a weighted doseResponseFit and
#' visualizeResponseProtein. Kept as a plain string builder so it can be
#' unit-tested without a running session.
#'
#' @param qc_input The QC module input list (the assign_feature_weights checkbox
#'   lives here).
#' @param comp_mat The turnover contrast matrix (GROUP + TimeVal columns); its
#'   GROUP column supplies the condition names the tracer constants are keyed by.
#' @param increasing Logical passed through to the fit / visualization.
#' @param tracer_constants Required, not defaulted (plan Decision G.2): a list of
#'   `values` (named numeric keyed by raw condition string), `source` (one of the
#'   CONSTANTS_QC$tracer_source_* provenance states) and `file`. Defaulting it
#'   would reinstate the original bug -- emitting all-1s that look deliberate --
#'   so the caller is forced to say which constants were actually used.
#' @return A character scalar of R code, appended after `library(MSstatsResponse)`.
#' @noRd
build_turnover_analysis_code <- function(qc_input, comp_mat, increasing,
                                        tracer_constants) {
  conditions <- as.character(comp_mat$GROUP)
  weighting  <- isTRUE(qc_input[[NAMESPACE_QC$assign_feature_weights]])

  # The vector is no longer rebuilt from comp_mat$GROUP, so nothing structurally
  # guarantees it covers the conditions being fitted any more. It can genuinely
  # drift: the snapshot is taken at QC Run, while comp_mat is rebuilt from
  # condition_metadata afterwards and its GROUP cells are editable in the
  # contrast table. An uncovered condition would emit c(...)[["2h"]] -> NA,
  # H_frac / NA -> NA, i.e. a script that runs to completion and fits nothing.
  # Failing here is loud but honest; emitting a quietly wrong script is not.
  values <- tracer_constants$values
  if (is.null(values) || length(values) == 0 || is.null(names(values))) {
    stop("Cannot generate reproducible code: no tracer constants are recorded ",
         "for this turnover analysis. Run protein summarization on the ",
         "data-processing page first.")
  }
  if (length(conditions) == 0) {
    stop("Cannot generate reproducible code: the contrast matrix lists no ",
         "experimental conditions.")
  }
  # A condition name that is blank cannot be written as an R argument name:
  # `c("" = 1)` is a PARSE error ("attempt to use zero-length variable name"),
  # so emitting it produces a script that does not even load. Verified reachable
  # without an upload -- a blank Condition cell in the annotation reaches the
  # all-1s default path, which runs none of the upload observer's name checks.
  # The message names the annotation file because the metadata table disables
  # editing on the Condition column (plan section 0.3).
  blank_conditions <- is.na(conditions) | !nzchar(trimws(conditions))
  if (any(blank_conditions)) {
    stop("Cannot generate reproducible code: ", sum(blank_conditions),
         " experimental condition(s) have a blank name. Give every condition a ",
         "name in the annotation file and load the data again.")
  }
  # Keys are trimmed on both sides for the same reason
  # qc_resolve_tracer_constants trims (plan section 0.4): a condition carried in
  # from Excel as "0h " is not editable in the metadata table, so a strict match
  # would be a dead end the user cannot clear.
  #
  # Trimming can however collapse two distinct keys onto one ("0h" and "0h "),
  # and match() would then hand BOTH conditions the first one's value -- a wrong
  # constant, silently. Only a repeated key carrying DIFFERING values is
  # actually ambiguous: repeated with the same value it cannot mispair, and
  # rejecting that would newly break the benign duplicate the all-1s default
  # can produce. qc_resolve_tracer_constants rejects a colliding file before it
  # can be snapshotted, so this is defence in depth rather than a live path.
  upload_keys <- trimws(as.character(names(values)))
  repeated <- unique(upload_keys[duplicated(upload_keys)])
  ambiguous <- repeated[vapply(repeated, function(key) {
    length(unique(values[upload_keys == key])) > 1L
  }, logical(1))]
  if (length(ambiguous) > 0) {
    stop("Cannot generate reproducible code: the recorded tracer constants are ",
         "ambiguous for condition(s): ", paste(ambiguous, collapse = ", "),
         ". Re-upload the tracer constants file with one row per condition.")
  }
  matched <- match(trimws(conditions), upload_keys)
  if (anyNA(matched)) {
    stop("Cannot generate reproducible code: the recorded tracer constants do ",
         "not cover condition(s): ",
         paste(conditions[is.na(matched)], collapse = ", "),
         ". Re-run protein summarization on the data-processing page so the ",
         "constants match the current conditions.")
  }
  # is.finite, not anyNA: Inf passes an NA check and emits a literal `Inf` into
  # the script, where H_frac / Inf is 0 and every L_frac becomes 1 -- a script
  # that runs to completion and reports total turnover everywhere. Verified.
  tracer_vals <- suppressWarnings(as.numeric(values[matched]))
  if (!all(is.finite(tracer_vals))) {
    bad <- !is.finite(tracer_vals)
    stop("Cannot generate reproducible code: the recorded tracer constant for ",
         "condition(s) ", paste(conditions[bad], collapse = ", "),
         " is not a finite number.")
  }

  # Keyed by the RAW condition name, matching the app: calculateTurnoverRatios
  # re-keys the vector with parse_timepoint(names(x)), so make.names()-style
  # sanitization here would change which constant a condition receives.
  # encodeString(quote = "\"") rather than paste0-ing quotes (plan Decision J):
  # a condition containing a quote or a backslash otherwise produces a script
  # that does not parse, and dropping make.names() is what newly exposes that.
  tracer_pairs <- paste0("  ", encodeString(conditions, quote = "\""), " = ",
                         vapply(tracer_vals, format_r_double, character(1)),
                         collapse = ",\n")

  code <- paste0(
    "\n", build_tracer_provenance_comment(tracer_constants),
    "tracer_constants = c(\n", tracer_pairs, "\n)\n",

    "\n# Calculate turnover (Heavy/Light) ratios. Use protein-level data when any\n",
    "# condition has replicate runs; otherwise fall back to feature-level data.\n",
    "pld = summarized$ProteinLevelData\n",
    "samples_per_condition = tapply(pld$RUN, pld$GROUP, function(x) length(unique(x)))\n",
    "if (any(samples_per_condition > 1, na.rm = TRUE)) {\n",
    "  turnover_ratios = calculateTurnoverRatios(\n",
    "    summarized$ProteinLevelData,\n",
    "    channel_col = \"LABEL\", heavy_label = \"H\", light_label = \"L\",\n",
    "    time_col = \"GROUP\", peptide_col = \"Protein\", protein_col = \"Protein\",\n",
    "    intensity_col = \"LogIntensities\", run_col = \"RUN\",\n",
    "    agg_function = max, normalize_tracer = TRUE, tracer_constants = tracer_constants)\n",
    "} else {\n",
    "  turnover_ratios = calculateTurnoverRatios(\n",
    "    summarized$FeatureLevelData,\n",
    "    channel_col = \"LABEL\", heavy_label = \"H\", light_label = \"L\",\n",
    "    time_col = \"GROUP\", peptide_col = \"PEPTIDE\", protein_col = \"PROTEIN\",\n",
    "    intensity_col = \"INTENSITY\", run_col = \"RUN\",\n",
    "    agg_function = max, normalize_tracer = TRUE, tracer_constants = tracer_constants)\n",
    "}\n"
  )

  if (weighting) {
    code <- paste0(
      code,
      "\n# Assign per-peptide quality weights (coverage, intensity, monotonicity,\n",
      "# validity). Adds a 'weight' column used to down-weight low-quality peptides.\n",
      "turnover_ratios = calculatePeptideWeights(turnover_ratios)\n"
    )
  }

  # Column mapping matches prepare_turnover_for_dose_response(). Real turnover
  # designs include a 0hr baseline, so no synthetic t=0 anchor is needed here.
  frac_col <- if (isTRUE(increasing)) "H_frac" else "L_frac"
  target_cols <- if (weighting) {
    "c(\"protein\", \"drug\", \"dose\", \"response\", \"BaseSequence\", \"weight\")"
  } else {
    "c(\"protein\", \"drug\", \"dose\", \"response\", \"BaseSequence\")"
  }

  code <- paste0(
    code,
    "\n# Map columns to the dose-response format\n",
    "frac_col = \"", frac_col, "\"\n",
    "prepared_data = turnover_ratios[!is.na(turnover_ratios[[frac_col]]), ]\n",
    "prepared_data$protein  = as.character(prepared_data$Protein)\n",
    "prepared_data$drug     = \"time\"\n",
    "prepared_data$dose     = as.numeric(prepared_data$TimeVal)\n",
    "prepared_data$response = prepared_data[[frac_col]]\n",
    "keep_cols = intersect(", target_cols, ", colnames(prepared_data))\n",
    "prepared_data = prepared_data[, keep_cols, drop = FALSE]\n"
  )

  weights_arg <- if (weighting) "  weights = prepared_data$weight,\n" else ""

  code <- paste0(
    code,
    "\n# Fit turnover time-course curves\n",
    "response_results = doseResponseFit(\n",
    "  data = prepared_data,\n",
    weights_arg,
    "  increasing = ", increasing, ",\n",
    "  transform_dose = FALSE,\n",
    "  ratio_response = FALSE,\n",
    "  precalculated_ratios = TRUE\n)\n",

    "\n# Visualize a single protein's turnover curve\n",
    "visualizeResponseProtein(\n",
    "  data = prepared_data,\n",
    "  protein_name = \"Enter protein name here\",\n",
    "  drug_name = \"time\",\n",
    if (weighting) "  weights = prepared_data$weight,\n  show_weights = TRUE,\n" else "",
    "  ratio_response = FALSE,\n",
    "  show_ic50 = TRUE,\n",
    "  add_ci = FALSE,\n",
    "  transform_dose = FALSE,\n",
    "  n_samples = 1000,\n",
    "  increasing = ", increasing, ",\n",
    "  precalculated_ratios = TRUE,\n",
    "  color_by = \"BaseSequence\",\n",
    "  target_response = 0.5\n)\n"
  )

  code
}