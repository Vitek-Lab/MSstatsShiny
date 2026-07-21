# QC Data Upload tab: parse and validate user-supplied FeatureLevelData /
# ProteinLevelData CSVs, show the tab only until load-page data exists, and swap
# the uploaded tables into an effective preprocess reactive that the
# stat-analysis page consumes in place of QC summarization output.

# ----------------------------------------------------------------------------
# Pure column / completeness helpers. Functions of plain vectors and booleans so
# they can be unit-tested directly without a Shiny session.
# ----------------------------------------------------------------------------

#' Required ProteinLevelData columns for an uploaded summarization table.
#'
#' Confirmed against MSstats::MSstatsSummarizationOutput. LABEL (heavy/light
#' channel) is only required for the protein-turnover template.
#' @noRd
qc_required_protein_columns <- function(template) {
  cols = c("Protein", "GROUP", "RUN", "LogIntensities")
  if (!is.null(template) && template == TEMPLATES$protein_turnover) {
    cols = c(cols, "LABEL")
  }
  cols
}

#' Required FeatureLevelData columns for an uploaded summarization table.
#'
#' Confirmed against MSstats::MSstatsSummarizationOutput (PROTEIN and INTENSITY
#' are upper-case; TRANSITION is intentionally omitted because it can be a
#' placeholder for non-fragment workflows). Template-independent for now; the
#' argument is kept for symmetry with qc_required_protein_columns.
#' @noRd
qc_required_feature_columns <- function(template) {
  c("PROTEIN", "PEPTIDE", "FEATURE", "RUN", "GROUP", "LABEL", "INTENSITY")
}

#' Columns in `required_cols` that are absent from `present_cols`.
#' @noRd
qc_missing_upload_columns <- function(present_cols, required_cols) {
  setdiff(required_cols, present_cols)
}

#' Whether the uploaded set is complete enough to bypass QC summarization.
#'
#' Default templates require FeatureLevelData and ProteinLevelData. Chemoproteomics
#' additionally requires a valid GROUP mapping. Protein turnover requires
#' ProteinLevelData, the turnover-ratios table, and a valid GROUP mapping;
#' FeatureLevelData is not used on the turnover response-curve path (the fit
#' consumes the ratios table directly).
#' @noRd
qc_uploads_complete <- function(template, has_feature, has_protein, has_turnover,
                                has_mapping) {
  if (!is.null(template) && template == TEMPLATES$protein_turnover) {
    has_protein && has_turnover && has_mapping
  } else if (!is.null(template) && template == TEMPLATES$chemoproteomics) {
    has_feature && has_protein && has_mapping
  } else {
    has_feature && has_protein
  }
}

#' Required GROUP mapping columns for the uploaded condition-metadata CSV.
#'
#' Protein turnover needs GROUP + TimeVal; chemoproteomics needs GROUP, DoseVal,
#' DoseUnit, and DrugName (DoseUnit feeds the dose-to-molar conversion, so it is
#' required rather than defaulted).
#' @noRd
qc_required_mapping_columns <- function(template) {
  if (!is.null(template) && template == TEMPLATES$protein_turnover) {
    c("GROUP", "TimeVal")
  } else if (!is.null(template) && template == TEMPLATES$chemoproteomics) {
    c("GROUP", "DoseVal", "DoseUnit", "DrugName")
  } else {
    character(0)
  }
}

#' Required columns for an uploaded turnover-ratios CSV.
#'
#' Matches what prepare_turnover_for_dose_response reads from the
#' MSstatsResponse::calculateTurnoverRatios output (BaseSequence is optional).
#' @noRd
qc_required_ratios_columns <- function() {
  c("Protein", "TimeVal", "H_frac", "L_frac")
}

#' Translate an uploaded GROUP mapping CSV into the condition_metadata format.
#'
#' The load page stores condition_metadata with a Condition column; the uploaded
#' CSV uses GROUP (identical values, user-facing name). Renames GROUP to
#' Condition and keeps the template-specific value columns, all coerced to
#' character to match the load-page format.
#' @noRd
qc_mapping_to_condition_metadata <- function(parsed, template) {
  df = data.frame(Condition = as.character(parsed$GROUP), stringsAsFactors = FALSE)
  if (!is.null(template) && template == TEMPLATES$protein_turnover) {
    df$TimeVal = as.character(parsed$TimeVal)
  } else if (!is.null(template) && template == TEMPLATES$chemoproteomics) {
    df$DoseVal = as.character(parsed$DoseVal)
    if ("DoseUnit" %in% colnames(parsed)) {
      df$DoseUnit = as.character(parsed$DoseUnit)
    }
    if ("DrugName" %in% colnames(parsed)) {
      df$DrugName = as.character(parsed$DrugName)
    }
  }
  df
}

#' Whether every dose unit is one convert_dose_to_molar recognizes.
#'
#' convert_dose_to_molar silently treats an unrecognized unit as molar
#' (multiplier 1), so units are validated up front. Accepts nM, uM, mM, M
#' (case-insensitive, surrounding whitespace ignored).
#' @noRd
qc_dose_units_valid <- function(units) {
  if (is.null(units) || length(units) == 0) return(FALSE)
  normalized = tolower(trimws(as.character(units)))
  all(!is.na(normalized) & normalized %in% c("nm", "um", "mm", "m"))
}

#' Errors for a GROUP mapping that must have one row per ProteinLevelData GROUP.
#'
#' Returns a character vector of messages (empty when valid): unknown mapping
#' groups, ProteinLevelData groups missing from the mapping, and duplicated
#' mapping rows. Missing or duplicated groups otherwise corrupt the GROUP join.
#' @noRd
qc_mapping_group_errors <- function(mapping_groups, protein_groups) {
  mapping_groups = as.character(mapping_groups)
  protein_groups = as.character(protein_groups)
  errors = character(0)
  unknown = setdiff(mapping_groups, protein_groups)
  if (length(unknown) > 0) {
    errors = c(errors, paste0("GROUP mapping has GROUP value(s) not found in ProteinLevelData: ",
                              paste(unknown, collapse = ", "), "."))
  }
  missing_groups = setdiff(protein_groups, mapping_groups)
  if (length(missing_groups) > 0) {
    errors = c(errors, paste0("GROUP mapping is missing row(s) for ProteinLevelData GROUP(s): ",
                              paste(missing_groups, collapse = ", "), "."))
  }
  duplicated_groups = unique(mapping_groups[duplicated(mapping_groups)])
  if (length(duplicated_groups) > 0) {
    errors = c(errors, paste0("GROUP mapping has duplicate row(s) for GROUP(s): ",
                              paste(duplicated_groups, collapse = ", "), "."))
  }
  errors
}

#' Whether every value in the named columns is numeric-coercible and finite.
#'
#' Returns FALSE if any column is absent or holds a blank / non-numeric /
#' non-finite value. Rejects turnover-ratios uploads that would fail the fit
#' silently.
#' @noRd
qc_values_numeric_finite <- function(df, cols) {
  if (!all(cols %in% colnames(df))) return(FALSE)
  for (col in cols) {
    vals = suppressWarnings(as.numeric(df[[col]]))
    if (any(!is.finite(vals))) return(FALSE)
  }
  TRUE
}

# ----------------------------------------------------------------------------
# Server registration.
# ----------------------------------------------------------------------------

#' Register the QC Data Upload tab: CSV parsing, column validation, tab
#' visibility, and the effective preprocess reactive returned to stat-analysis.
#' @noRd
register_qc_data_upload <- function(input, output, session, loadpage_input,
                                    app_template, get_data, preprocess_data,
                                    get_condition_metadata, turnover_ratios) {

  uploaded_feature_level = reactiveVal(NULL)
  uploaded_protein_level = reactiveVal(NULL)
  uploaded_turnover_ratios = reactiveVal(NULL)
  # TRUE only after an uploaded GROUP mapping passes every validation check;
  # required before turnover / chemo uploads are considered ready.
  mapping_valid = reactiveVal(FALSE)

  get_template = function() if (!is.null(app_template)) app_template() else NULL

  uploads_complete = reactive({
    qc_uploads_complete(get_template(),
                        !is.null(uploaded_feature_level()),
                        !is.null(uploaded_protein_level()),
                        has_turnover = !is.null(uploaded_turnover_ratios()),
                        has_mapping = isTRUE(mapping_valid()))
  })

  # ---- Parse + validate uploaded CSVs ----
  # fread returns a data.table by default (feedback_fread_default_type); columns
  # are validated by name, not position. GROUP is coerced to a factor so
  # stat-analysis can derive the condition list from levels(GROUP).

  observeEvent(input$upload_feature_level, {
    file = input$upload_feature_level
    req(file)
    parsed = tryCatch(
      data.table::fread(file$datapath),
      error = function(e) {
        showNotification(paste("Could not read FeatureLevelData:", conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(parsed)) {
      uploaded_feature_level(NULL)
      return()
    }
    missing = qc_missing_upload_columns(colnames(parsed),
                                        qc_required_feature_columns(get_template()))
    if (length(missing) > 0) {
      showNotification(
        paste0("FeatureLevelData is missing required column(s): ",
               paste(missing, collapse = ", ")),
        type = "error", duration = 10)
      uploaded_feature_level(NULL)
      return()
    }
    parsed$GROUP = factor(parsed$GROUP)
    parsed = as.data.frame(parsed)
    uploaded_feature_level(parsed)
    if (is.null(uploaded_protein_level())) {
      showNotification("FeatureLevelData uploaded. Please also upload ProteinLevelData.",
                       type = "message", duration = 6)
    }
  })

  observeEvent(input$upload_protein_level, {
    file = input$upload_protein_level
    req(file)
    parsed = tryCatch(
      data.table::fread(file$datapath),
      error = function(e) {
        showNotification(paste("Could not read ProteinLevelData:", conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(parsed)) {
      uploaded_protein_level(NULL)
      return()
    }
    missing = qc_missing_upload_columns(colnames(parsed),
                                        qc_required_protein_columns(get_template()))
    if (length(missing) > 0) {
      showNotification(
        paste0("ProteinLevelData is missing required column(s): ",
               paste(missing, collapse = ", ")),
        type = "error", duration = 10)
      uploaded_protein_level(NULL)
      return()
    }
    parsed$GROUP = factor(parsed$GROUP)
    parsed = as.data.frame(parsed)
    uploaded_protein_level(parsed)
    if (!is.null(get_template()) && get_template() == TEMPLATES$protein_turnover) {
      showNotification(paste("ProteinLevelData uploaded. Please also upload the turnover ratios",
                             "and the GROUP to TimeVal mapping."),
                       type = "message", duration = 6)
    } else if (is.null(uploaded_feature_level())) {
      showNotification("ProteinLevelData uploaded. Please also upload FeatureLevelData.",
                       type = "message", duration = 6)
    }
  })

  # ---- Mapping CSV: GROUP -> time/dose, written into condition_metadata ----
  # Turnover uses GROUP + TimeVal; chemo uses GROUP + DoseVal + DoseUnit +
  # DrugName. GROUP is renamed to Condition so downstream turnover / chemo code
  # consumes it unchanged.

  observeEvent(input$upload_condition_mapping, {
    file = input$upload_condition_mapping
    req(file)
    template = get_template()
    # Clear validity on every (re-)upload; only a fully validated mapping sets it.
    mapping_valid(FALSE)
    parsed = tryCatch(
      data.table::fread(file$datapath),
      error = function(e) {
        showNotification(paste("Could not read GROUP mapping:", conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(parsed)) return()

    missing = qc_missing_upload_columns(colnames(parsed),
                                        qc_required_mapping_columns(template))
    if (length(missing) > 0) {
      showNotification(
        paste0("GROUP mapping is missing required column(s): ",
               paste(missing, collapse = ", ")),
        type = "error", duration = 10)
      return()
    }

    # Value column must be numeric-coercible: blank / non-numeric entries become
    # NA downstream and fail the fit silently.
    value_col = if (!is.null(template) && template == TEMPLATES$protein_turnover) "TimeVal" else "DoseVal"
    if (any(is.na(suppressWarnings(as.numeric(parsed[[value_col]]))))) {
      showNotification(
        paste0("GROUP mapping ", value_col,
               " must be numeric for every row (found blank or non-numeric values)."),
        type = "error", duration = 10)
      return()
    }

    # Chemo dose units feed convert_dose_to_molar, which silently treats an
    # unrecognized unit as molar; reject unknown units up front.
    if (!is.null(template) && template == TEMPLATES$chemoproteomics &&
        !qc_dose_units_valid(parsed$DoseUnit)) {
      showNotification(
        "GROUP mapping DoseUnit must be one of nM, uM, mM, or M for every row.",
        type = "error", duration = 10)
      return()
    }

    # GROUP values must line up one-to-one with the uploaded ProteinLevelData;
    # unknown groups, missing groups, or duplicate rows all corrupt the join.
    pld = uploaded_protein_level()
    if (is.null(pld)) {
      showNotification("Please upload ProteinLevelData before the GROUP mapping.",
                       type = "error", duration = 10)
      return()
    }
    protein_groups = levels(pld$GROUP)
    if (is.null(protein_groups)) {
      protein_groups = unique(as.character(pld$GROUP))
    }
    group_errors = qc_mapping_group_errors(parsed$GROUP, protein_groups)
    if (length(group_errors) > 0) {
      showNotification(paste(group_errors, collapse = " "),
                       type = "error", duration = 10)
      return()
    }

    if (!is.null(get_condition_metadata)) {
      get_condition_metadata(qc_mapping_to_condition_metadata(parsed, template))
    }
    mapping_valid(TRUE)
    showNotification("GROUP mapping uploaded.", type = "message", duration = 6)
  })

  # ---- Turnover-ratios CSV (protein-turnover template) ----

  observeEvent(input$upload_turnover_ratios, {
    file = input$upload_turnover_ratios
    req(file)
    parsed = tryCatch(
      data.table::fread(file$datapath),
      error = function(e) {
        showNotification(paste("Could not read Turnover Ratios:", conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(parsed)) {
      uploaded_turnover_ratios(NULL)
      return()
    }
    missing = qc_missing_upload_columns(colnames(parsed), qc_required_ratios_columns())
    if (length(missing) > 0) {
      showNotification(
        paste0("Turnover Ratios is missing required column(s): ",
               paste(missing, collapse = ", ")),
        type = "error", duration = 10)
      uploaded_turnover_ratios(NULL)
      return()
    }
    if (!qc_values_numeric_finite(parsed, c("TimeVal", "H_frac", "L_frac"))) {
      showNotification(
        "Turnover Ratios TimeVal, H_frac, and L_frac must be numeric and finite for every row.",
        type = "error", duration = 10)
      uploaded_turnover_ratios(NULL)
      return()
    }
    uploaded_turnover_ratios(as.data.frame(parsed))
    if (is.null(uploaded_protein_level())) {
      showNotification(paste("Turnover Ratios uploaded. Please also upload ProteinLevelData",
                             "and the GROUP to TimeVal mapping."),
                       type = "message", duration = 6)
    }
  })

  # ---- Effective preprocess: uploaded tables when the load page was not used ----
  # Falls back to the raw preprocess_data() eventReactive whenever load-page data
  # exists or the uploads are incomplete, so existing flows are unchanged.

  effective_preprocess_data = reactive({
    loaded = tryCatch(get_data(), error = function(e) NULL)
    if (is.null(loaded) && uploads_complete()) {
      list(FeatureLevelData = uploaded_feature_level(),
           ProteinLevelData = uploaded_protein_level())
    } else {
      preprocess_data()
    }
  })

  # ---- Effective turnover ratios: uploaded table when the load page was unused ----
  # turnover_ratios is an eventReactive on input$run, which never fires on the
  # upload path, so return the uploaded ratios directly when present.

  effective_turnover_ratios = reactive({
    loaded = tryCatch(get_data(), error = function(e) NULL)
    if (is.null(loaded) && !is.null(uploaded_turnover_ratios())) {
      uploaded_turnover_ratios()
    } else {
      turnover_ratios()
    }
  })

  # ---- Tab visibility: show only until load-page data exists ----
  # Move a user off the tab before hiding it so they are not stranded on a
  # hidden pane.

  observeEvent(tryCatch(get_data(), error = function(e) NULL), {
    loaded = tryCatch(get_data(), error = function(e) NULL)
    if (is.null(loaded)) {
      showTab(inputId = "qc_tabs", target = "Data Upload", session = session)
    } else {
      updateTabsetPanel(session = session, inputId = "qc_tabs", selected = "Summarized Results")
      hideTab(inputId = "qc_tabs", target = "Data Upload", session = session)
    }
  }, ignoreNULL = FALSE)

  # ---- Disable the summarization Run button while uploads are in play ----

  observe({
    shinyjs::toggleState("run", is.null(uploaded_feature_level()) &&
                                  is.null(uploaded_protein_level()))
  })

  # ---- Template-gated upload panels: mapping (turnover + chemo), ratios (turnover) ----

  observeEvent(get_template(), {
    t = get_template()
    shinyjs::toggle(
      NAMESPACE_QC$data_upload_mapping_panel,
      condition = !is.null(t) && t %in% c(TEMPLATES$protein_turnover, TEMPLATES$chemoproteomics)
    )
    shinyjs::toggle(
      NAMESPACE_QC$data_upload_ratios_panel,
      condition = !is.null(t) && t == TEMPLATES$protein_turnover
    )
  }, ignoreNULL = FALSE)

  list(
    effective_preprocess_data = effective_preprocess_data,
    effective_turnover_ratios = effective_turnover_ratios,
    uploaded_feature_level = uploaded_feature_level,
    uploaded_protein_level = uploaded_protein_level
  )
}
