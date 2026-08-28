# QC Data Upload tab: parse and validate user-supplied FeatureLevelData /
# ProteinLevelData CSVs, show the tab only until load-page data exists, and swap
# the uploaded tables into an effective preprocess reactive that the
# stat-analysis page consumes in place of QC summarization output.

# ----------------------------------------------------------------------------
# Pure column / completeness helpers.
# ----------------------------------------------------------------------------

#' Required ProteinLevelData columns for an uploaded summarization table.
#'
#' Confirmed against MSstats::MSstatsSummarizationOutput. LABEL (heavy/light
#' channel) is only required for the protein-turnover template.
#' @noRd
get_qc_required_protein_columns <- function(template) {
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
#' placeholder for non-fragment workflows).
#' @noRd
get_qc_required_feature_columns <- function() {
  c("PROTEIN", "PEPTIDE", "FEATURE", "RUN", "GROUP", "LABEL", "INTENSITY")
}

#' Columns in `required_cols` that are absent from `present_cols`.
#' @noRd
get_missing_upload_columns <- function(present_cols, required_cols) {
  setdiff(required_cols, present_cols)
}

#' Whether the uploaded set is complete enough to bypass QC summarization.
#'
#' All templates require FeatureLevelData and ProteinLevelData. Chemoproteomics
#' and protein turnover additionally require a valid GROUP mapping; protein
#' turnover also requires the turnover-ratios table.
#' @noRd
qc_uploads_complete <- function(template, has_feature, has_protein, has_turnover,
                                has_mapping) {
  if (!is.null(template) && template == TEMPLATES$protein_turnover) {
    has_feature && has_protein && has_turnover && has_mapping
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
get_qc_required_mapping_columns <- function(template) {
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
get_qc_required_ratios_columns <- function() {
  c("Protein", "TimeVal", "H_frac", "L_frac")
}

#' Translate an uploaded GROUP mapping CSV into the condition_metadata format.
#'
#' The load page stores condition_metadata with a Condition column; the uploaded
#' CSV uses GROUP (identical values, user-facing name). Renames GROUP to
#' Condition and keeps the template-specific value columns, all coerced to
#' character to match the load-page format.
#'
#' Condition is trimmed because qc_mapping_group_errors() validates the TRIMMED
#' GROUP values against ProteinLevelData. Storing the raw value instead would
#' let a quoted "DMSO " cell (fread's strip.white does not touch quoted fields)
#' pass validation and then fail the ProteinLevelData join downstream, silently
#' dropping that condition's rows. What is stored must be what was checked.
#' @noRd
qc_mapping_to_condition_metadata <- function(parsed, template) {
  df = data.frame(Condition = trimws(as.character(parsed$GROUP)), stringsAsFactors = FALSE)
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

#' Errors for an upload that must have exactly one row per reference GROUP.
#'
#' Returns a character vector of messages (empty when valid): unknown groups,
#' reference groups missing from the upload, and duplicated rows. Missing or
#' duplicated groups otherwise corrupt the GROUP join.
#'
#' `subject` and `reference` name the two sides in the message. They are
#' parameters because more than one upload in this app is validated this way:
#' hardcoding "GROUP mapping" / "ProteinLevelData" would send a user who
#' mis-typed a condition in the tracer-constants file off to edit an unrelated
#' file. Defaults reproduce the GROUP-mapping wording verbatim.
#' @noRd
qc_mapping_group_errors <- function(mapping_groups, protein_groups,
                                    subject = "GROUP mapping",
                                    reference = "ProteinLevelData") {
  # Trimmed on both sides: condition names get no normalization when they are
  # read off the annotation, while fread applies strip.white = TRUE. So an
  # Excel-sourced "0h " could never be matched by any CSV cell, and the
  # load-page metadata table disables editing on the Condition column -- there
  # is no in-app fix for the user. Case is deliberately left strict.
  mapping_groups = trimws(as.character(mapping_groups))
  protein_groups = trimws(as.character(protein_groups))
  errors = character(0)
  unknown = setdiff(mapping_groups, protein_groups)
  if (length(unknown) > 0) {
    errors = c(errors, paste0(subject, " has GROUP value(s) not found in ", reference, ": ",
                              paste(unknown, collapse = ", "), "."))
  }
  missing_groups = setdiff(protein_groups, mapping_groups)
  if (length(missing_groups) > 0) {
    errors = c(errors, paste0(subject, " is missing row(s) for ", reference, " GROUP(s): ",
                              paste(missing_groups, collapse = ", "), "."))
  }
  duplicated_groups = unique(mapping_groups[duplicated(mapping_groups)])
  if (length(duplicated_groups) > 0) {
    errors = c(errors, paste0(subject, " has duplicate row(s) for GROUP(s): ",
                              paste(duplicated_groups, collapse = ", "), "."))
  }
  errors
}

#' Whether every value in the named columns is numeric-coercible and finite.
#'
#' With allow_na = TRUE, NA or blank entries pass but any present value must
#' still be finite numeric. Rejects turnover-ratios uploads that would fail the
#' fit silently.
#' @noRd
qc_values_numeric_finite <- function(df, cols, allow_na = FALSE) {
  if (!all(cols %in% colnames(df))) return(FALSE)
  for (col in cols) {
    raw = df[[col]]
    coerced = suppressWarnings(as.numeric(raw))
    if (allow_na) {
      blank = is.na(raw) | trimws(as.character(raw)) == ""
      if (any(!blank & !is.finite(coerced))) return(FALSE)
    } else {
      if (any(!is.finite(coerced))) return(FALSE)
    }
  }
  TRUE
}

# ----------------------------------------------------------------------------
# Tracer-constant upload helpers (protein turnover only).
# ----------------------------------------------------------------------------

#' Required columns for an uploaded tracer-constants CSV.
#'
#' GROUP matches the naming every other QC upload uses; TracerConstant is the
#' per-condition isotope enrichment fraction handed to
#' MSstatsResponse::calculateTurnoverRatios.
#' @noRd
get_qc_required_tracer_columns <- function() {
  c("GROUP", "TracerConstant")
}

#' Whether every tracer constant lies within CONSTANTS_QC$tracer_min/max.
#'
#' Takes df + col to match qc_values_numeric_finite's convention, and coerces
#' before comparing for the same reason it does: an uncoerced character column
#' compares lexicographically, so all(c("0.5", "0.5abc") > 0) is TRUE and the
#' bad value only turns into NA later, inside the analysis. Coercing first also
#' keeps NA out of the `all()`, which would otherwise return NA and throw
#' "missing value where TRUE/FALSE needed" from the calling `if`.
#'
#' Returns FALSE for an absent or empty column: all(logical(0)) is TRUE, so an
#' empty file would otherwise validate.
#' @noRd
qc_tracer_values_in_range <- function(df, col = "TracerConstant") {
  if (is.null(df) || length(col) != 1 || is.na(col)) return(FALSE)
  if (!(col %in% colnames(df))) return(FALSE)
  raw = df[[col]]
  if (length(raw) == 0) return(FALSE)
  # as.numeric on a factor reads the level codes, not the labels, so
  # factor("2.0") would validate as 2. fread does not produce factors, but this
  # helper is the single source of truth and takes an arbitrary data frame.
  if (is.factor(raw)) raw = as.character(raw)
  coerced = suppressWarnings(as.numeric(raw))
  all(is.finite(coerced) &
        coerced >= CONSTANTS_QC$tracer_min &
        coerced <= CONSTANTS_QC$tracer_max)
}

#' The neutral all-ones tracer-constant vector, named by condition.
#'
#' 1 is the identity for the divisor, so this reproduces uncorrected behaviour
#' exactly. Errors on zero conditions rather than returning a named numeric(0):
#' that is not NULL, so it would slip past calculateTurnoverRatios' own is.null
#' guard and make every H_frac NA.
#' @noRd
#' @importFrom stats setNames
qc_default_tracer_constants <- function(conditions) {
  conditions = as.character(conditions)
  if (length(conditions) == 0) {
    stop("Cannot build tracer constants: no experimental conditions are available.")
  }
  setNames(rep(1, length(conditions)), conditions)
}

#' Resolve the tracer-constant vector actually passed to the turnover fit.
#'
#' The single source of truth for both the analysis and the downloadable
#' script, so the two cannot disagree. `uploaded` is NULL when no file was
#' supplied, in which case every condition gets 1.
#'
#' The reindex is guarded rather than trusted: uploaded[conditions] returns NA
#' under an NA *name* for a condition the file does not cover -- no error -- and
#' H_frac / NA is NA, i.e. an entirely missing result set with no warning.
#' Names are re-applied from `conditions` because calculateTurnoverRatios re-keys
#' the vector with parse_timepoint(names(x)), so it must carry the raw condition
#' strings, not whatever the file happened to be sorted by.
#' @noRd
#' @importFrom stats setNames
qc_resolve_tracer_constants <- function(conditions, uploaded = NULL) {
  resolved = qc_default_tracer_constants(conditions)
  if (is.null(uploaded) || length(uploaded) == 0) return(resolved)

  conditions = names(resolved)
  # Matched on trimmed keys for the reason given in qc_mapping_group_errors,
  # but the RESULT keeps the raw condition strings as names: they are what
  # calculateTurnoverRatios re-keys through parse_timepoint.
  keys = trimws(conditions)
  upload_keys = trimws(as.character(names(uploaded)))

  duplicated_keys = unique(upload_keys[duplicated(upload_keys)])
  if (length(duplicated_keys) > 0) {
    stop("Tracer constants list condition(s) more than once: ",
         paste(duplicated_keys, collapse = ", "), ".")
  }

  matched = match(keys, upload_keys)
  if (anyNA(matched)) {
    stop("Tracer constants are missing for condition(s): ",
         paste(conditions[is.na(matched)], collapse = ", "), ".")
  }

  raw = uploaded
  if (is.factor(raw)) raw = as.character(raw)
  values = suppressWarnings(as.numeric(raw[matched]))
  if (anyNA(values)) {
    stop("Tracer constants could not be read for condition(s): ",
         paste(conditions[is.na(values)], collapse = ", "), ".")
  }
  setNames(values, conditions)
}

#' Hours a condition name resolves to, mirroring MSstatsResponse's parser.
#'
#' Deliberately duplicated rather than called through `:::`: parse_timepoint is
#' internal to MSstatsResponse and R CMD check flags ::: calls. It must stay in
#' step with it, because calculateTurnoverRatios re-keys the tracer vector with
#' names(x) <- parse_timepoint(names(x)) -- this is the function that decides
#' which constant a condition actually receives.
#'
#' Note the parser's two quirks, both reproduced here on purpose: the number is
#' anchored at the start of the string, and the day/week detectors are bare "d"
#' and "w" matched anywhere, so "6h_drug" resolves to 144 hours.
#' @noRd
#' @importFrom stringr str_extract str_detect
qc_tracer_timepoint_hours <- function(conditions) {
  conditions = as.character(conditions)
  numeric_part = suppressWarnings(as.numeric(str_extract(conditions, "^[0-9]+")))
  is_days = str_detect(conditions, "d|day")
  is_weeks = str_detect(conditions, "w|week")
  # str_detect returns NA for an NA condition, which would make the subscripted
  # assignments below error rather than fall through to the unparseable branch.
  is_days[is.na(is_days)] = FALSE
  is_weeks[is.na(is_weeks)] = FALSE

  hours = numeric_part
  hours[is_days] = numeric_part[is_days] * 24
  hours[is_weeks] = numeric_part[is_weeks] * 24 * 7
  hours
}

#' Condition names whose "d"/"w" is read as a unit but is not one.
#'
#' The parser's day and week detectors are the bare patterns "d|day" and
#' "w|week", matched ANYWHERE in the string, so "6h_drug" silently resolves to
#' 144 hours rather than 6 -- the "d" of "drug". Flagged only when the letter
#' fires the multiplier without the name actually ending in a day/week unit, so
#' ordinary suffixes that carry no "d" or "w" ("24h_rep1") are left alone.
#' @noRd
#' @importFrom stringr str_detect
qc_tracer_misleading_units <- function(conditions) {
  conditions = as.character(conditions)
  fires = !is.na(conditions) & str_detect(conditions, "d|day|w|week")
  genuine = !is.na(conditions) &
    str_detect(conditions, "^[0-9]+[[:space:]]*(d|days?|w|wks?|weeks?)$")
  fires & !genuine
}

#' Errors for condition names the turnover fit cannot key tracer constants by.
#'
#' Three failure modes, all silent data corruption today:
#'   - a name that resolves to NA drops every row of that condition, leaving an
#'     empty result set;
#'   - a stray "d" or "w" multiplies the timepoint (see
#'     qc_tracer_misleading_units);
#'   - two names that resolve to the SAME number collide, and the second
#'     silently takes the first's constant (e.g. "24h" and "1d" both give 24).
#'
#' The remedy has to be the annotation file: the load-page metadata table
#' disables editing on the Condition column, so there is no in-app fix.
#' @noRd
qc_tracer_timepoint_errors <- function(conditions) {
  conditions = unique(as.character(conditions))
  errors = character(0)
  if (length(conditions) == 0) return(errors)

  hours = qc_tracer_timepoint_hours(conditions)

  unparseable = conditions[is.na(hours)]
  if (length(unparseable) > 0) {
    errors = c(errors, paste0(
      "Condition name(s) must start with a number of hours, days or weeks ",
      "(for example 0h, 6h, 24h). These do not: ",
      paste(unparseable, collapse = ", "),
      ". Rename the conditions in the annotation file and reload the data."))
  }

  misleading = !is.na(hours) & qc_tracer_misleading_units(conditions)
  if (any(misleading)) {
    errors = c(errors, paste0(
      "Condition name(s) ", paste(conditions[misleading], collapse = ", "),
      " contain a \"d\" or \"w\" that is read as a day or week unit, so they ",
      "resolve to ", paste(hours[misleading], collapse = ", "),
      " hours. Rename them in the annotation file and reload the data."))
  }

  known = hours[!is.na(hours)]
  named = conditions[!is.na(hours)]
  for (collision in unique(known[duplicated(known)])) {
    errors = c(errors, paste0(
      "Condition name(s) ", paste(named[known == collision], collapse = ", "),
      " all resolve to the same timepoint (", collision,
      " hours), so they cannot be given different tracer constants. ",
      "Rename them in the annotation file and reload the data."))
  }
  errors
}

# ----------------------------------------------------------------------------
# Server registration.
# ----------------------------------------------------------------------------

#' Register the QC Data Upload tab: CSV parsing, column validation, tab
#' visibility, and the effective preprocess reactive returned to stat-analysis.
#' @noRd
register_qc_data_upload <- function(input, output, session, loadpage_input,
                                    app_template, get_data, preprocess_data,
                                    get_condition_metadata, turnover_ratios,
                                    tracer_upload = NULL) {

  uploaded_feature_level = reactiveVal(NULL)
  uploaded_protein_level = reactiveVal(NULL)
  uploaded_turnover_ratios = reactiveVal(NULL)
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
    missing = get_missing_upload_columns(colnames(parsed),
                                        get_qc_required_feature_columns())
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
    missing = get_missing_upload_columns(colnames(parsed),
                                        get_qc_required_protein_columns(get_template()))
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

  observeEvent(input$upload_condition_mapping, {
    file = input$upload_condition_mapping
    req(file)
    template = get_template()
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

    missing = get_missing_upload_columns(colnames(parsed),
                                        get_qc_required_mapping_columns(template))
    if (length(missing) > 0) {
      showNotification(
        paste0("GROUP mapping is missing required column(s): ",
               paste(missing, collapse = ", ")),
        type = "error", duration = 10)
      return()
    }

    value_col = if (!is.null(template) && template == TEMPLATES$protein_turnover) "TimeVal" else "DoseVal"
    if (any(is.na(suppressWarnings(as.numeric(parsed[[value_col]]))))) {
      showNotification(
        paste0("GROUP mapping ", value_col,
               " must be numeric for every row (found blank or non-numeric values)."),
        type = "error", duration = 10)
      return()
    }

    if (!is.null(template) && template == TEMPLATES$chemoproteomics &&
        !qc_dose_units_valid(parsed$DoseUnit)) {
      showNotification(
        "GROUP mapping DoseUnit must be one of nM, uM, mM, or M for every row.",
        type = "error", duration = 10)
      return()
    }

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
    missing = get_missing_upload_columns(colnames(parsed), get_qc_required_ratios_columns())
    if (length(missing) > 0) {
      showNotification(
        paste0("Turnover Ratios is missing required column(s): ",
               paste(missing, collapse = ", ")),
        type = "error", duration = 10)
      uploaded_turnover_ratios(NULL)
      return()
    }
    if (!qc_values_numeric_finite(parsed, "TimeVal")) {
      showNotification(
        "Turnover Ratios TimeVal must be numeric and finite for every row.",
        type = "error", duration = 10)
      uploaded_turnover_ratios(NULL)
      return()
    }
    if (!qc_values_numeric_finite(parsed, c("H_frac", "L_frac"), allow_na = TRUE)) {
      showNotification(
        "Turnover Ratios H_frac and L_frac must be numeric and finite, or left blank.",
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
  # One observer for all of them on purpose. A second toggleState("run", ...)
  # elsewhere would not AND with this one: each observer sends its own message
  # whenever its own dependencies invalidate, so the later write wins and the
  # button flips back on. Hence tracer_upload is passed in from
  # register_qc_turnover rather than gating Run from there.
  #
  # "pending" blocks Run for the same reason "rejected" does: the tracer file
  # picker sits ~20px above the Run button, so a click during the upload would
  # otherwise summarize with all-1s and no warning (plan Decision I).

  observe({
    tracer = if (is.null(tracer_upload)) NULL else tracer_upload()
    # Template-gated as well as state-gated. register_qc_turnover already
    # clears the state on a template switch; this term means a stale state
    # could not disable Run on a template whose sidebar has no tracer panel --
    # and so no Clear button -- to re-enable it from.
    tracer_blocks_run = identical(get_template(), TEMPLATES$protein_turnover) &&
      !is.null(tracer) && tracer$state %in% c("pending", "rejected")
    shinyjs::toggleState("run", is.null(uploaded_feature_level()) &&
                                  is.null(uploaded_protein_level()) &&
                                  !tracer_blocks_run)
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
