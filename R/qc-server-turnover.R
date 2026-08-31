# QC Turnover Ratios tab (protein-turnover template): the optional
# tracer-constants CSV upload, the ratio calculation, the results table, and
# the ratio CSV download.

#' Register the QC Turnover Ratios tab outputs.
#'
#' @return a list with three elements: `ratios` (the display reactive, which
#'   carries a req()), `tracer_upload` (the upload state reactiveVal, read by
#'   register_qc_data_upload to gate the Run button), and `tracer_constants`
#'   (the provenance record snapshotted at Run).
#' @noRd
register_qc_turnover <- function(input, output, session, app_template, get_data,
                                 get_condition_metadata, preprocess_data) {

  # ---- Tracer-constants CSV upload (optional; protein turnover only) ----
  #
  # Three states: "absent" (no file; every condition gets 1), "rejected"
  # (an uploaded file the app cannot honour; blocks Run rather than silently
  # falling back to all-1s), and "pending" (upload in flight; also blocks Run).
  tracer_upload <- reactiveVal(list(state = "absent", values = NULL, file = NULL))

  # The constants that actually produced the displayed ratios, snapshotted at
  # the moment of Run. NOT a live view of tracer_upload(): that would let the
  # ratios table and the downloadable script disagree about which constants
  # were used. NULL means the QC page has not been run in this session, which
  # is distinct from "run, and no file was supplied" (source = "none").
  tracer_constants_used <- reactiveVal(NULL)

  get_template <- function() if (is.null(app_template)) NULL else app_template()

  get_conditions <- function() {
    if (is.null(get_condition_metadata)) return(character(0))
    meta <- get_condition_metadata()
    if (is.null(meta) || is.null(meta$Condition)) return(character(0))
    as.character(meta$Condition)
  }

  # shinyjs::reset() sets the value client-side only and does not dispatch
  # "change", so clearing the upload cannot re-arm this handler.
  shinyjs::onevent("change", NAMESPACE_QC$tracer_constants_file, {
    tracer_upload(list(state = "pending", values = NULL, file = NULL))
  })

  # input[[tracer_constants_file]] keeps its old name/datapath after a
  # shinyjs::reset (client-side only), so tracer_upload() is the only
  # trustworthy source of the resolved values.
  observeEvent(input[[NAMESPACE_QC$tracer_constants_clear]], {
    shinyjs::reset(NAMESPACE_QC$tracer_constants_file)
    tracer_upload(list(state = "absent", values = NULL, file = NULL))
    showNotification(
      "Tracer constants cleared. Every condition will use 1 (no correction).",
      type = "message", duration = 6)
  }, ignoreInit = TRUE)

  # A blocking tracer state must not outlive the template it belongs to: the
  # tracer panel (and its Clear button) is only shown on protein turnover.
  observeEvent(get_template(), {
    if (identical(get_template(), TEMPLATES$protein_turnover)) return()

    tracer_constants_used(NULL)

    if (!identical(tracer_upload()$state, "absent")) {
      shinyjs::reset(NAMESPACE_QC$tracer_constants_file)
      tracer_upload(list(state = "absent", values = NULL, file = NULL))
    }
  }, ignoreNULL = FALSE)

  observeEvent(input[[NAMESPACE_QC$tracer_constants_file]], {
    file <- input[[NAMESPACE_QC$tracer_constants_file]]
    req(file)

    reject <- function(...) {
      tracer_upload(list(state = "rejected", values = NULL, file = file$name))
      showNotification(paste0(file$name, " was not accepted. ", ...),
                       type = "error", duration = 10)
    }
    tracer_upload(list(state = "rejected", values = NULL, file = file$name))

    conditions <- get_conditions()
    if (length(conditions) == 0) {
      reject("Load your data before uploading tracer constants: the app does ",
             "not yet know which experimental conditions to expect.")
      return()
    }

    # fread warns (rather than errors) on some malformed rows/files, so
    # warnings are captured for reporting but are not relied on for detection.
    fread_warnings <- character(0)
    parsed <- withCallingHandlers(
      tryCatch(
        data.table::fread(file$datapath,
                          # An all-numeric GROUP column is otherwise typed
                          # integer, so "0010" would read back as "10".
                          colClasses = list(character = "GROUP")),
        error = function(e) {
          reject("Could not read the tracer constants file: ", conditionMessage(e))
          NULL
        }
      ),
      warning = function(w) {
        fread_warnings <<- c(fread_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    if (is.null(parsed)) return()

    if (ncol(parsed) == 0 || nrow(parsed) == 0) {
      reject("The tracer constants file has no data rows. It needs a header ",
             "row of ", paste(get_qc_required_tracer_columns(), collapse = ", "),
             " and one row per condition.")
      return()
    }

    # fread keeps duplicate column names and `$` returns the first, so a
    # duplicated TracerConstant column would silently use the stale one.
    duplicated_columns <- unique(colnames(parsed)[duplicated(colnames(parsed))])
    if (length(duplicated_columns) > 0) {
      reject("The tracer constants file has more than one column named: ",
             paste(duplicated_columns, collapse = ", "),
             ". Delete the extra column(s) so it is unambiguous which values apply.")
      return()
    }

    missing <- get_missing_upload_columns(colnames(parsed),
                                          get_qc_required_tracer_columns())
    if (length(missing) > 0) {
      reject("The tracer constants file is missing required column(s): ",
             paste(missing, collapse = ", "),
             ". Required columns (case-sensitive): ",
             paste(get_qc_required_tracer_columns(), collapse = ", "), ".")
      return()
    }

    # A row fread cannot parse is silently dropped rather than erroring. Most
    # such cases are still caught below by the group-coverage check; comparing
    # counted source rows against parsed rows catches the rest (e.g. a
    # duplicate row for a condition another row already covers).
    source_rows <- tryCatch(
      sum(nzchar(trimws(readLines(file$datapath, warn = FALSE)))) - 1L,
      error = function(e) NA_integer_
    )
    if (!is.na(source_rows) && source_rows > nrow(parsed)) {
      reject(source_rows - nrow(parsed), " row(s) could not be read and were ",
             "skipped, so the file is not the one you think you uploaded. ",
             "Check for stray separators or decimal commas (write 0.42, not ",
             "0,42), then upload it again.")
      return()
    }

    if (length(fread_warnings) > 0) {
      showNotification(
        paste0(file$name, " parsed with warnings: ",
               paste(fread_warnings, collapse = " ")),
        type = "warning", duration = 10)
    }

    if (!qc_tracer_values_in_range(parsed)) {
      values <- suppressWarnings(as.numeric(as.character(parsed$TracerConstant)))
      out_of_range <- !is.finite(values) |
        values < CONSTANTS_QC$tracer_min | values > CONSTANTS_QC$tracer_max
      reject("TracerConstant must be a number between ", CONSTANTS_QC$tracer_min,
             " and ", CONSTANTS_QC$tracer_max, " (inclusive) for every row. ",
             "Check condition(s): ",
             paste(as.character(parsed$GROUP)[out_of_range], collapse = ", "), ".")
      return()
    }

    group_errors <- qc_mapping_group_errors(
      parsed$GROUP, conditions,
      subject = "The tracer constants file",
      reference = "the experimental conditions")
    if (length(group_errors) > 0) {
      reject(paste(group_errors, collapse = " "),
             " The file must have exactly one row per condition; clear the ",
             "upload to use 1 for every condition instead.")
      return()
    }

    timepoint_errors <- qc_tracer_timepoint_errors(conditions)
    if (length(timepoint_errors) > 0) {
      reject(paste(timepoint_errors, collapse = " "))
      return()
    }

    uploaded <- stats::setNames(parsed$TracerConstant, as.character(parsed$GROUP))
    resolved <- tryCatch(
      qc_resolve_tracer_constants(conditions, uploaded),
      error = function(e) {
        reject(conditionMessage(e))
        NULL
      }
    )
    if (is.null(resolved)) return()

    tracer_upload(list(state = "valid", values = resolved, file = file$name))
    showNotification(paste0("Tracer constants loaded from ", file$name,
                            " (", length(resolved), " conditions)."),
                     type = "message", duration = 6)
  })

  # condition_metadata is a shared reactiveVal that ordinary actions (e.g.
  # re-clicking proceed, uploading a GROUP mapping) can rewrite after a
  # tracer upload; a valid upload whose conditions no longer match must be
  # invalidated rather than silently reused or left to error later.
  if (!is.null(get_condition_metadata)) {
    observeEvent(get_condition_metadata(), {
      current <- tracer_upload()
      if (!identical(current$state, "valid")) return()
      if (setequal(trimws(names(current$values)), trimws(get_conditions()))) return()

      shinyjs::reset(NAMESPACE_QC$tracer_constants_file)
      tracer_upload(list(state = "rejected", values = NULL, file = current$file))
      showNotification(
        paste0("The experimental conditions changed after ", current$file,
               " was uploaded, so its tracer constants no longer apply. ",
               "Upload a file matching the new conditions, or press Clear to ",
               "use 1 for every condition."),
        type = "warning", duration = 10)
    }, ignoreInit = TRUE)
  }

  # ---- Tracer panel visibility ----
  # Shown only on protein turnover, and only once data is loaded (or a tracer
  # state is already in play, so a bad-upload dead end can still be cleared).
  observe({
    loaded <- tryCatch(get_data(), error = function(e) NULL)
    shinyjs::toggle(
      NAMESPACE_QC$tracer_constants_panel,
      condition = identical(get_template(), TEMPLATES$protein_turnover) &&
        (!is.null(loaded) || !identical(tracer_upload()$state, "absent"))
    )
  })

  output[[NAMESPACE_QC$tracer_constants_status]] <- renderUI({
    current <- tracer_upload()
    switch(
      current$state,
      valid = span(class = "text-success",
                   paste0("Using tracer constants from ", current$file, ".")),
      pending = span(class = "text-muted",
                     "Reading tracer constants file... If this does not finish ",
                     "(for example the file is over the upload size limit), ",
                     "press Clear."),
      rejected = span(class = "text-danger",
                      "Tracer constants were not accepted. Fix the file and ",
                      "upload it again, or press Clear to use 1 for every condition."),
      span(class = "text-muted",
           "No file uploaded: every condition uses 1 (no tracer correction).")
    )
  })


  turnover_ratios <- eventReactive(input$run, {
    # Cleared up front so any early exit below leaves no stale snapshot.
    tracer_constants_used(NULL)

    req(!is.null(app_template) && !is.null(app_template()) &&
          app_template() == TEMPLATES$protein_turnover)
    req(preprocess_data())

    req(!is.null(get_condition_metadata) && !is.null(get_condition_metadata()))
    conditions <- as.character(get_condition_metadata()$Condition)

    # Snapshot the upload as it stands at Run; read from tracer_upload() and
    # never from input[[tracer_constants_file]] (see the reset note above).
    upload <- tracer_upload()

    if (upload$state %in% c("pending", "rejected")) {
      showNotification(
        paste0("Turnover ratios were not calculated: the tracer constants ",
               "file is still being checked or was not accepted. Wait for it ",
               "to finish, fix it, or press Clear to use 1 for every ",
               "condition."),
        type = "error", duration = 10)
      req(FALSE)
    }
    uploaded <- if (identical(upload$state, "valid")) upload$values else NULL

    tracer_consts <- tryCatch(
      qc_resolve_tracer_constants(conditions, uploaded),
      error = function(e) {
        showNotification(
          paste0("Turnover ratios were not calculated. ", conditionMessage(e),
                 " Upload a tracer constants file matching the current ",
                 "conditions, or press Clear to use 1 for every condition."),
          type = "error", duration = 10)
        NULL
      }
    )
    req(tracer_consts)

    # Use ProteinLevelData when any condition has more than one sample (run);
    # fall back to FeatureLevelData for purely single-replicate designs.
    pld <- preprocess_data()$ProteinLevelData
    samples_per_condition <- tapply(pld$RUN, pld$GROUP, function(x) length(unique(x)))
    use_protein_level <- any(samples_per_condition > 1)

    ratios <- if (use_protein_level) {
      calculateTurnoverRatios(
        pld,
        channel_col      = "LABEL",
        heavy_label      = "H",
        light_label      = "L",
        time_col         = "GROUP",
        peptide_col      = "Protein",
        protein_col      = "Protein",
        intensity_col    = "LogIntensities",
        run_col          = "RUN",
        peptide_selector = NULL,
        agg_function     = max,
        normalize_tracer = TRUE,
        tracer_constants = tracer_consts
      )
    } else {
      calculateTurnoverRatios(
        preprocess_data()$FeatureLevelData,
        channel_col      = "LABEL",
        heavy_label      = "H",
        light_label      = "L",
        time_col         = "GROUP",
        peptide_col      = "PEPTIDE",
        protein_col      = "PROTEIN",
        intensity_col    = "INTENSITY",
        run_col          = "RUN",
        peptide_selector = NULL,
        agg_function     = max,
        normalize_tracer = TRUE,
        tracer_constants = tracer_consts
      )
    }

    # Committed only once the fit has returned, so a run that fails leaves no
    # snapshot behind (matches the clear at the top of this reactive).
    tracer_constants_used(list(
      values = tracer_consts,
      source = if (is.null(uploaded)) CONSTANTS_QC$tracer_source_none
               else CONSTANTS_QC$tracer_source_upload,
      file   = if (is.null(uploaded)) NULL else upload$file
    ))

    ratios
  })

  observeEvent(input$run, {
    req(!is.null(app_template) && !is.null(app_template()) &&
          app_template() == TEMPLATES$protein_turnover)
    turnover_ratios()
  }, ignoreInit = TRUE)

  turnover_ratios_display <- reactive({
    ratios <- turnover_ratios()
    req(ratios)

    # The snapshot is the authority for "are there ratios on screen"; without
    # this guard, switching templates away and back could redraw the table
    # from a cached ratios value after its snapshot had been cleared.
    req(tracer_constants_used())
    if (isTRUE(input[[NAMESPACE_QC$assign_feature_weights]]) && nrow(ratios) > 0) {
      calculatePeptideWeights(ratios)
    } else {
      ratios
    }
  })

  output$turnover_ratios_panel <- renderUI({
    req(!is.null(app_template) && !is.null(app_template()) &&
          app_template() == TEMPLATES$protein_turnover)

    ns <- session$ns
    ratios <- tryCatch(turnover_ratios_display(), error = function(e) NULL)
    has_ratios <- !is.null(ratios) && NROW(ratios) > 0

    download_button <- downloadButton(ns("download_turnover_ratios"), "Download Ratios")
    if (!has_ratios) {
      download_button <- disabled(download_button)
    }

    tagList(
      tags$br(),
      p("Run protein summarization in the side panel to calculate turnover ratios."),
      if (has_ratios) dataTableOutput(ns("turnover_ratios_table")),
      tags$br(),
      download_button
    )
  })

  output$turnover_ratios_table <- renderDataTable({
    turnover_ratios_display()
  }, options = list(scrollX = TRUE))

  output$download_turnover_ratios <- downloadHandler(
    filename = function() {
      paste0("Turnover_Ratios-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(turnover_ratios_display(), file, row.names = FALSE)
    }
  )

  list(
    ratios = turnover_ratios_display,
    tracer_upload = tracer_upload,
    tracer_constants = tracer_constants_used
  )
}
