# QC Turnover Ratios tab (protein-turnover template): tracer-constant inputs,
# the ratio calculation, the results table, and the ratio CSV download.

#' Register the QC Turnover Ratios tab outputs and return the ratios reactive.
#' @noRd
register_qc_turnover <- function(input, output, session, app_template, get_data,
                                 get_condition_metadata, preprocess_data) {

  output$turnover_ratios_sidebar <- renderUI({
    req(!is.null(app_template) && !is.null(app_template()) &&
          app_template() == TEMPLATES$protein_turnover)
    req(get_data())

    req(!is.null(get_condition_metadata) && !is.null(get_condition_metadata()))
    ns <- session$ns
    conditions <- as.character(get_condition_metadata()$Condition)

    tracer_inputs <- lapply(conditions, function(cond) {
      input_id <- ns(paste0("tracer_", make.names(cond)))
      fluidRow(
        column(6, p(strong(cond))),
        column(6, numericInput(input_id, NULL, value = 1.0, min = 0, max = 1, step = 0.001))
      )
    })

    tagList(
      tags$hr(),
      h4("Turnover Ratio Calculation"),
      p("Enter tracer constants (0 to 1) for each condition:"),
      tagList(tracer_inputs)
    )
  })

  turnover_ratios <- eventReactive(input$run, {
    req(!is.null(app_template) && !is.null(app_template()) &&
          app_template() == TEMPLATES$protein_turnover)
    req(preprocess_data())

    req(!is.null(get_condition_metadata) && !is.null(get_condition_metadata()))
    conditions <- as.character(get_condition_metadata()$Condition)
    tracer_consts <- sapply(conditions, function(cond) {
      val <- input[[paste0("tracer_", make.names(cond))]]
      if (is.null(val)) 1.0 else as.numeric(val)
    })
    names(tracer_consts) <- conditions

    # Use ProteinLevelData when any condition has more than one sample (run);
    # fall back to FeatureLevelData for purely single-replicate designs.
    pld <- preprocess_data()$ProteinLevelData
    samples_per_condition <- tapply(pld$RUN, pld$GROUP, function(x) length(unique(x)))
    use_protein_level <- any(samples_per_condition > 1)

    if (use_protein_level) {
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
  })

  observeEvent(input$run, {
    req(!is.null(app_template) && !is.null(app_template()) &&
          app_template() == TEMPLATES$protein_turnover)
    turnover_ratios()
  }, ignoreInit = TRUE)

  output$turnover_ratios_panel <- renderUI({
    req(!is.null(app_template) && !is.null(app_template()) &&
          app_template() == TEMPLATES$protein_turnover)

    ns <- session$ns
    tagList(
      tags$br(),
      p("Run protein summarization after filling in tracer constants in the side panel."),
      uiOutput(ns("turnover_ratios_table_ui")),
      tags$br(),
      disabled(downloadButton(ns("download_turnover_ratios"), "Download Ratios"))
    )
  })

  output$turnover_ratios_table_ui <- renderUI({
    req(turnover_ratios())
    ns <- session$ns
    enable("download_turnover_ratios")
    dataTableOutput(ns("turnover_ratios_table"))
  })

  output$turnover_ratios_table <- renderDataTable({
    turnover_ratios()
  })

  output$download_turnover_ratios <- downloadHandler(
    filename = function() {
      paste0("Turnover_Ratios-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(turnover_ratios(), file, row.names = FALSE)
    }
  )

  turnover_ratios
}
