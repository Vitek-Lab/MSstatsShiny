# QC Turnover Ratios tab (protein-turnover template): the optional
# tracer-constants CSV upload, the ratio calculation, the results table, and
# the ratio CSV download.

#' Register the QC Turnover Ratios tab outputs.
#'
#' @return a list with three elements: `ratios` (the display reactive, which
#'   carries a req()), `tracer_upload` (the upload state reactiveVal, read by
#'   register_qc_data_upload to gate the Run button), and `tracer_constants`
#'   (the provenance record snapshotted at Run, which must never raise -- the
#'   Download-code path reads it on all four templates).
#' @noRd
register_qc_turnover <- function(input, output, session, app_template, get_data,
                                 get_condition_metadata, preprocess_data) {

  # ---- Tracer-constants CSV upload (optional; protein turnover only) ----
  #
  # Three states, deliberately not collapsed into a nullable vector (plan
  # Decision H / plan section 0.1). "absent" means the user declined the
  # correction and every condition gets 1; "rejected" means they supplied an
  # intent the app cannot honour, which must block Run rather than silently
  # fall back to all-1s -- that silent fallback is the bug this whole feature
  # exists to remove.
  # "pending" covers the browser-to-server upload window: the Run button sits
  # ~20px below the file picker, and clicking it mid-upload would otherwise
  # summarize with all-1s and no warning (Decision I).
  #
  # State and values live in ONE reactiveVal so they cannot disagree, and the
  # committed values are whatever qc_resolve_tracer_constants() returns -- the
  # same function the fit and the generated script call -- so the stored vector
  # is by construction the vector the analysis will use.
  tracer_upload <- reactiveVal(list(state = "absent", values = NULL, file = NULL))

  # The constants that actually produced the displayed ratios, snapshotted at
  # the moment of Run (Decision I). NOT a live view of tracer_upload(): a live
  # one diverges the instant the user uploads a file after running, so the
  # ratios table would show one set of numbers while the downloadable script
  # emitted another -- which is the silent-divergence bug this feature exists
  # to remove, reintroduced by its own fix.
  #
  # A reactiveVal rather than a second eventReactive, deliberately. Reading an
  # eventReactive that has not yet fired raises shiny.silent.error, and this
  # value is read from the Download-code path on ALL FOUR templates
  # (plan Decision E) -- a silent error escaping there kills the button
  # everywhere. Reading a reactiveVal cannot fail.
  #
  # NULL is therefore load-bearing: it is the third provenance state from
  # plan section 0.6, "the QC page was never run", which is genuinely
  # different from "the user ran it and declined the correction" (source =
  # "none"). The generated script distinguishes all three.
  tracer_constants_used <- reactiveVal(NULL)

  get_template <- function() if (is.null(app_template)) NULL else app_template()

  get_conditions <- function() {
    if (is.null(get_condition_metadata)) return(character(0))
    meta <- get_condition_metadata()
    if (is.null(meta) || is.null(meta$Condition)) return(character(0))
    as.character(meta$Condition)
  }

  # Fires on the client the moment a file is chosen, i.e. before the upload
  # completes and before observeEvent(input$...) can run. shinyjs::reset() sets
  # the value with jQuery .val(''), which does not dispatch "change", so
  # clearing the upload cannot re-arm this and strand Run disabled.
  shinyjs::onevent("change", NAMESPACE_QC$tracer_constants_file, {
    tracer_upload(list(state = "pending", values = NULL, file = NULL))
  })

  # shinyjs::reset clears the file input on the CLIENT only -- it empties the
  # widget and posts back an empty message, so input$tracer_constants_file
  # keeps its old name/datapath for the rest of the session. tracer_upload() is
  # therefore the only trustworthy source of the resolved values; anything
  # downstream (Step 4's resolve-at-run in particular) must read it and never
  # the file input, or a cleared upload comes back to life.
  observeEvent(input[[NAMESPACE_QC$tracer_constants_clear]], {
    shinyjs::reset(NAMESPACE_QC$tracer_constants_file)
    tracer_upload(list(state = "absent", values = NULL, file = NULL))
    showNotification(
      "Tracer constants cleared. Every condition will use 1 (no correction).",
      type = "message", duration = 6)
  }, ignoreInit = TRUE)

  # A blocking tracer state must not outlive the template it belongs to. The
  # template is a plain selectInput on the Home tab and can be changed at any
  # point in a live session; the tracer panel -- and with it the Clear button,
  # the only affordance that resets the state -- is gated on protein turnover.
  # So a rejected upload followed by a template switch would leave Run disabled
  # with nothing on screen to re-enable it, escapable only by restarting.
  observeEvent(get_template(), {
    if (identical(get_template(), TEMPLATES$protein_turnover)) return()

    # Unconditional, unlike the upload reset below: pressing Run on another
    # template makes turnover_ratios' own req() fail, and shiny CACHES that
    # shiny.silent.error, so the ratios table stays empty for the rest of the
    # session until a fresh turnover Run. A snapshot surviving that would have
    # the generated script cite tracer constants for a table showing nothing
    # -- the script/display divergence Decision I exists to prevent.
    tracer_constants_used(NULL)

    if (!identical(tracer_upload()$state, "absent")) {
      shinyjs::reset(NAMESPACE_QC$tracer_constants_file)
      tracer_upload(list(state = "absent", values = NULL, file = NULL))
    }
  }, ignoreNULL = FALSE)

  observeEvent(input[[NAMESPACE_QC$tracer_constants_file]], {
    file <- input[[NAMESPACE_QC$tracer_constants_file]]
    req(file)

    # Every early return below leaves "rejected", never the previous file's
    # values: replacing a good file with a bad one must not keep running the
    # good one's constants under the bad one's filename (QA 4.1).
    reject <- function(...) {
      tracer_upload(list(state = "rejected", values = NULL, file = file$name))
      # Named, because replacing a good file with a bad one otherwise produces
      # a red toast that does not say which of the two failed (QA 4.1).
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

    # fread's failures here are split across three channels, and only one of
    # them is an error. It ERRORS on a file it cannot parse at all; it WARNS
    # (returning a 0x0 table) on a 0-byte file; and on a row with the wrong
    # field count it warns without erroring, either stopping early or
    # discarding the row as a "footer". So warnings are captured rather than
    # discarded -- but they are not the primary defence against a dropped row;
    # the line count below is. See that check for why.
    fread_warnings <- character(0)
    parsed <- withCallingHandlers(
      tryCatch(
        data.table::fread(file$datapath,
                          # plan section 0.4: an all-numeric GROUP column is otherwise typed
                          # integer, so a condition genuinely named "0010" reads
                          # back as "10" and can never be matched.
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

    # A 0-byte file warns rather than erroring, and a header-only file parses
    # cleanly to 0 rows -- where all(logical(0)) is TRUE, so every downstream
    # check would pass on it.
    if (ncol(parsed) == 0 || nrow(parsed) == 0) {
      reject("The tracer constants file has no data rows. It needs a header ",
             "row of ", paste(get_qc_required_tracer_columns(), collapse = ", "),
             " and one row per condition.")
      return()
    }

    # fread keeps duplicate column names and `$` silently returns the FIRST, so
    # a spreadsheet carrying an old and a corrected TracerConstant column would
    # quietly use the stale one.
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

    # A row fread could not parse is DROPPED, not reported as an error. Where
    # the row sits decides which: mid-file it stops early, and last it is
    # discarded as a "single-line footer". Most such files are still caught
    # downstream, because dropping a row usually leaves a condition uncovered
    # and the group check demands complete coverage. The exception is a dropped
    # row for a condition another row already covers -- a second "0hr" line
    # typed as "0hr,0,42" instead of "0hr,0.42". That file passes every check
    # below while the value the user actually entered is silently gone, and it
    # would have been rejected as a duplicate had it parsed.
    #
    # Counted from the source rather than matched against fread's warning text:
    # the wording is not part of data.table's API, and fread also emits
    # unrelated warnings ("Previous fread() session was not cleaned up
    # properly") that must not reject a good file.
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

    # Anything else fread had to say. Reported only once the column check has
    # passed: before that, a malformed file is better explained by the
    # missing-column message than by fread's internals.
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

    # plan section 0.7: the labels matter. The default wording names "GROUP mapping" and
    # "ProteinLevelData", and the GROUP mapping is a DIFFERENT upload on this
    # same page -- a user who mis-typed a condition here would go and edit an
    # unrelated file.
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

    # plan section 0.3: calculateTurnoverRatios re-keys the vector through parse_timepoint,
    # so names that do not survive it drop rows or collide -- silently, and in
    # the collision case while the metadata table displays the non-colliding
    # number. Checked at upload rather than at Run so the message arrives while
    # the user is still looking at the file.
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

  # Decision K: condition_metadata is a shared reactiveVal that ordinary
  # actions rewrite AFTER a tracer upload -- re-clicking the load page's
  # proceed button and uploading a GROUP mapping both replace it wholesale.
  # Nothing else invalidates the upload, so without this the resolved vector
  # would either go stale or blow up later as a raw "Internal error" blaming
  # the software for the user's click order.
  #
  # Only a genuine change to the condition SET clears it: proceed1 rewrites the
  # table on every click, and dropping a valid upload each time would be its
  # own bug (QA 4.5). Cleared to "rejected", not "absent", so the app still
  # refuses to fall back to all-1s behind the user's back (plan section 0.1).
  # Registered conditionally: get_condition_metadata is documented as
  # "reactive (or NULL)" and qcServer defaults it to NULL, so an unguarded
  # get_condition_metadata() would be NULL() -- an error thrown on the first
  # reactive flush of the session, on every template. Every sibling read in
  # this module guards the same way.
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
  #
  # shinyjs::toggle on the statically mounted div, mirroring the
  # template-gated upload panels at R/qc-server-data-upload.R:659-671.
  # observe() rather than observeEvent(get_template()) because the gate has two
  # dependencies, and toggle() must re-run when either moves.
  #
  # get_data() is the second half of the gate, so the panel stays off the
  # "upload pre-summarized abundances" flow. That is INTENTIONAL, not an
  # oversight, and is inherited from the req(get_data()) in the renderUI this
  # replaces (plan section 0.5): on that flow the turnover ratios were computed
  # -- and any enrichment correction applied -- outside the app, so there is
  # nothing left here for a tracer constant to divide. The Upload Summarized
  # Abundances tab states this where a user on that flow can actually read it;
  # help text inside a hidden panel would not be visible to them.
  # The state term is not redundant with the data term. get_data is an
  # eventReactive on the load page's proceed button that returns NULL when a
  # load FAILS (R/loadpage-server-data-loaders.R:67-78), so: upload a bad
  # tracer file (Run now disabled), go back, re-click proceed, load fails ->
  # data goes NULL, this panel would hide, and the Clear button that is the
  # only way out of "rejected" would vanish while Run stayed disabled. The
  # template never changed, so the clearing observer above does not fire
  # either. That is exactly the dead end plan section 0.1 requires not to
  # exist, so the panel stays mounted for as long as its state can block Run.
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
    # Cleared before the guards below, not after them, so that every path which
    # fails to reach the commit further down leaves no snapshot at all. The
    # snapshot describes the constants that produced the ratios now on screen;
    # a run that produces no ratios must therefore leave nothing behind, or the
    # generated script would cite constants for a table the user cannot see.
    # Placing this after the req()s left a stale snapshot alive whenever a
    # second Run bailed out early -- e.g. summarization returning NULL, which
    # empties the table while the previous run's constants stayed on record.
    tracer_constants_used(NULL)

    req(!is.null(app_template) && !is.null(app_template()) &&
          app_template() == TEMPLATES$protein_turnover)
    req(preprocess_data())

    req(!is.null(get_condition_metadata) && !is.null(get_condition_metadata()))
    conditions <- as.character(get_condition_metadata()$Condition)

    # eventReactive isolates its body, so this read takes a snapshot of the
    # upload as it stands at Run and creates no dependency on it -- exactly
    # what Decision I requires. Read from tracer_upload() and never from
    # input[[tracer_constants_file]]: shinyjs::reset clears the widget on the
    # client only, so a cleared upload still has its old name and datapath in
    # the input, and reading it would resurrect constants the user deleted.
    upload <- tracer_upload()

    # plan section 0.1 forbids falling back to all-1s behind the user's back,
    # and the toggleState that enforces it is CLIENT-side. shinyjs::onevent
    # needs a server round trip before Run is disabled, so a click inside that
    # window arrives here with the state still "pending" -- the exact race
    # Decision I names. Refusing here makes the guarantee server-side; the
    # notification is what tells the user why the click did nothing.
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

    # The same helper the generated script resolves through, so the analysis
    # and the script cannot disagree. It THROWS rather than defaulting to 1s
    # when it cannot honour the user's intent; that is the point (Decision G),
    # but an uncaught stop() inside this eventReactive surfaces as a bare red
    # error and output$turnover_ratios_panel would tryCatch it into a silent
    # "no ratios". So: name the reason in a toast, then req() out, leaving the
    # snapshot cleared above rather than recording constants never applied.
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

    # Committed only once the fit has RETURNED. A reactiveVal write is not
    # rolled back when the body aborts, so committing before the fit left a
    # snapshot on record whenever the fit threw -- and the fit does throw on
    # plausible data, e.g. ProteinLevelData carrying no "H" rows. The script
    # would then cite constants, and name the uploaded file, for a table that
    # never drew. This is the same rule as the clear at the top of the body:
    # a run that produces no ratios leaves nothing behind.
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

    # The snapshot is the single authority for "are there ratios on screen".
    # turnover_ratios is an eventReactive, so nothing but a fresh Run can
    # invalidate its cache -- in particular the template observer above cannot.
    # Without this guard, switching the template away and back redrew this
    # table from that surviving cache while the snapshot taken with it had been
    # cleared, so the ratios on screen carried the uploaded correction while the
    # generated script emitted all-1s stamped "the page was not run in this
    # session". Gating the display on the snapshot keeps the pair inseparable:
    # no snapshot, no table, and the user is asked to run again.
    #
    # Checked AFTER turnover_ratios() rather than before it: reading that
    # eventReactive is what WRITES the snapshot, so an earlier req() would test
    # the previous value and blank the table for a cycle on the first Run.
    req(tracer_constants_used())
    if (isTRUE(input[[NAMESPACE_QC$assign_feature_weights]]) && nrow(ratios) > 0) {
      calculatePeptideWeights(ratios)
    } else {
      ratios
    }
  })

  # The table and the download button are rendered together, from one read of
  # the ratios, rather than the button being toggled with enable() from a
  # sibling output. This panel lives in a tabPanel, so its outputs are
  # suspended while another tab is active and both re-render on activation; a
  # one-off enable() message raced with that re-render and left the button
  # greyed out next to a populated table. `tryCatch` maps the pending
  # eventReactive (before the first "Run summarization") and any error in the
  # calculation to "no ratios", so neither blocks the panel from drawing.
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

  # A list, not the bare ratios reactive: register_qc_data_upload needs the
  # upload state to extend its existing toggleState("run", ...) observer, and a
  # SECOND toggleState observer on the same button would fight the first
  # (last writer wins on each flush).
  # tracer_constants is exposed separately from ratios rather than bundled into
  # it: ratios (turnover_ratios_display) carries a req(), and Decision E is
  # explicit that the reactive reaching the Download-code path must not, or the
  # silent error takes the button out on every template.
  list(
    ratios = turnover_ratios_display,
    tracer_upload = tracer_upload,
    tracer_constants = tracer_constants_used
  )
}
