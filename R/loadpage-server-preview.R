# Loadpage preview cluster: first-100-row preview, DIANN version auto-detection, and the Metamorpheus modification-ID UI.


#' Register the loadpage preview cluster.
#'
#' @param input   the Shiny module's `input` object
#' @param output  the Shiny module's `output` object
#' @param session the Shiny module's `session`
#' @return       `preview_data` reactiveVal (invisibly)
#' @noRd
register_loadpage_preview <- function(input, output, session) {
  preview_data <- reactiveVal(NULL)
  last_detected_diann_format <- reactiveVal(NULL)

  # Determine the main data file based on current selections
  # TODO: Add preview mappings for remaining PTM file types (PD, spec, sky, maxq)
  # once preview-based UI features are extended beyond Metamorpheus.
  main_data_file <- reactive({
    req(input$filetype)
    if (input$BIO == "PTM") {
      switch(input$filetype,
        "meta" = input$ptm_input,
        # TODO: "maxq" = input$ptm_input,
        # TODO: "PD" = input$ptm_input,
        # TODO: "spec" = input$ptm_input,
        # TODO: "sky" = input$ptm_input,
        # TODO: "phil" = input$ptmdata,
        # TODO: "msstats" = input$msstatsptmdata,
        NULL
      )
    } else {
      switch(input$filetype,
        # TODO: Map remaining non-PTM file types when preview features are needed
        "prog" =, "PD" =, "open" =, "openms" =, "spmin" =, "phil" =, "meta" = input$data,
        "msstats" = input$msstatsdata,
        "sky" = input$skylinedata,
        "spec" = input$specdata,
        "diann" = input$dianndata,
        "maxq" = input$evidence,
        NULL
      )
    }
  })

  # Read first 100 rows for preview-based UI features.
  # Supported: Metamorpheus PTM (modification ID dropdown), DIANN (version auto-detection).
  # TODO: Extend to other input formats (Spectronaut, MaxQuant) as needed.
  observe({
    should_preview <- (isTRUE(input$filetype == "meta") && isTRUE(input$BIO == "PTM")) ||
                     (isTRUE(input$filetype == "diann") && isTRUE(input$BIO != "PTM"))
    if (should_preview) {
      file_info <- main_data_file()
      if (!is.null(file_info)) {
        # Reset DIANN detection tracker so a new file re-triggers the notification
        last_detected_diann_format(NULL)
        preview <- .read_preview(file_info$datapath, file_info$name)
        if (is.null(preview)) {
          showNotification("Could not preview file. Please verify the file format.",
                           type = "warning", duration = 5)
        }
        preview_data(preview)
      } else {
        preview_data(NULL)
      }
    } else {
      preview_data(NULL)
    }
  })

  # Auto-toggle DIANN 2.0+ checkbox based on detected file format
  observe({
    req(input$filetype == "diann", input$BIO != "PTM")
    preview <- preview_data()
    if (is.null(preview)) return()

    is_2plus <- .is_diann_2plus(preview)
    previous <- last_detected_diann_format()
    # Only update and notify when the detected state actually changes
    if (is.null(previous) || previous != is_2plus) {
      updateCheckboxInput(session, "diann_2plus", value = is_2plus)
      if (is_2plus) {
        showNotification("Detected DIANN 2.0+ format (per-fragment columns).",
                         type = "message", duration = 5)
      } else {
        showNotification("Detected DIANN 1.x format (legacy fragment column).",
                         type = "message", duration = 5)
      }
      last_detected_diann_format(is_2plus)
    }
  })

  # Warn user if they manually set DIANN 2.0+ checkbox to a value that conflicts with detected format
  observeEvent(input$diann_2plus, {
    req(input$filetype == "diann", input$BIO != "PTM")
    preview <- preview_data()
    if (is.null(preview)) return()
    detected_2plus <- .is_diann_2plus(preview)
    if (isTRUE(input$diann_2plus) != detected_2plus) {
      showNotification(
        paste0("Warning: You've ",
               if (isTRUE(input$diann_2plus)) "checked" else "unchecked",
               " DIANN 2.0+, but the uploaded file appears to be ",
               if (detected_2plus) "DIANN 2.0+ format" else "DIANN 1.x format",
               ". This mismatch may cause upload to fail."),
        type = "warning", duration = 10)
    }
  }, ignoreInit = TRUE)

  # ========= METAMORPHEUS PTM: Dynamic modification ID dropdown =========
  output$mod_id_meta_ui <- renderUI({
    ns <- session$ns
    req(input$filetype == "meta", input$BIO == "PTM")
    mods <- .extract_mod_ids_from_preview(preview_data())
    create_meta_mod_id_selector(ns, mods)
  })

  # Show manual text input when "Other" is selected (replaces conditionalPanel)
  output$mod_id_meta_other_input <- renderUI({
    req(input$mod_id_meta_select == "__other__")
    textInput(session$ns("mod_id_meta_custom"),
              label = h5("Enter modification ID (e.g. [Common Biological:Phosphorylation on S])"),
              value = "")
  })

  invisible(preview_data)
}
