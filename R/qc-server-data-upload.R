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
#' channel) is only required for the protein-turnover template; commit 2 uses it.
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
#' Default and chemoproteomics templates require both FeatureLevelData and
#' ProteinLevelData. The protein-turnover branch is a stub for commit 2, which
#' will fold in the uploaded ratios / metadata; for now it mirrors the two-file
#' rule.
#' @noRd
qc_uploads_complete <- function(template, has_feature, has_protein, has_turnover) {
  if (!is.null(template) && template == TEMPLATES$protein_turnover) {
    has_feature && has_protein
  } else {
    has_feature && has_protein
  }
}

# ----------------------------------------------------------------------------
# Server registration.
# ----------------------------------------------------------------------------

#' Register the QC Data Upload tab: CSV parsing, column validation, tab
#' visibility, and the effective preprocess reactive returned to stat-analysis.
#' @noRd
register_qc_data_upload <- function(input, output, session, loadpage_input,
                                    app_template, get_data, preprocess_data) {

  uploaded_feature_level = reactiveVal(NULL)
  uploaded_protein_level = reactiveVal(NULL)

  get_template = function() if (!is.null(app_template)) app_template() else NULL

  uploads_complete = reactive({
    qc_uploads_complete(get_template(),
                        !is.null(uploaded_feature_level()),
                        !is.null(uploaded_protein_level()),
                        has_turnover = FALSE)
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
    uploaded_protein_level(parsed)
    if (is.null(uploaded_feature_level())) {
      showNotification("ProteinLevelData uploaded. Please also upload FeatureLevelData.",
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

  # ---- Tab visibility: show only until load-page data exists ----
  # Move a user off the tab before hiding it so they are not stranded on a
  # hidden pane.

  observeEvent(get_data(), {
    if (is.null(get_data())) {
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

  list(
    effective_preprocess_data = effective_preprocess_data,
    uploaded_feature_level = uploaded_feature_level,
    uploaded_protein_level = uploaded_protein_level
  )
}
