# Loadpage condition-metadata DT editor + the post-`proceed1` summary outputs and experimental-design summary statistics (number of conditions, number of replicates, etc.).


#' Register the loadpage post-`proceed1` summary cluster.
#'
#' @param input              the Shiny module's `input` object
#' @param output             the Shiny module's `output` object
#' @param session            the Shiny module's `session`
#' @param parent_session     the parent module's session (for the tablist
#'                           switch in `onclick("proceed2")`)
#' @param app_template       reactive (or NULL) returning the active template
#' @param data_reactives     named list from `register_loadpage_data_loaders`;
#'                           the helper consumes `get_data`, `get_annot`
#' @param condition_metadata `reactiveVal` owned by the orchestrator
#' @noRd
register_loadpage_summary <- function(input, output, session, parent_session,
                                       app_template = NULL,
                                       data_reactives,
                                       condition_metadata) {

  get_data    <- data_reactives$get_data
  get_annot   <- data_reactives$get_annot

  get_summary1 <- eventReactive(input$proceed1, {
    getSummary1(input, get_data(), get_annot())
  })

  get_summary2 <- eventReactive(input$proceed1, {
    getSummary2(input, get_data())
  })

  # Handle edits to the condition metadata DT table
  observeEvent(input$condition_metadata_table_cell_edit, {
    info <- input$condition_metadata_table_cell_edit
    current <- condition_metadata()
    if (is.null(current)) return()
    if (info$col == 1) {
      value_col <- if ("TimeVal" %in% colnames(current)) "TimeVal" else "DoseVal"
      current[[value_col]][info$row] <- info$value
      condition_metadata(current)
    } else if (info$col == 2 && "DrugName" %in% colnames(current)) {
      current[["DrugName"]][info$row] <- as.character(info$value)
      condition_metadata(current)
    } else if (info$col == 3 && "DoseUnit" %in% colnames(current)) {
      current[["DoseUnit"]][info$row] <- as.character(info$value)
      condition_metadata(current)
    }
  })

  # Render the editable condition metadata table
  output$condition_metadata_table <- DT::renderDT({
    req(!is.null(condition_metadata()))
    meta <- condition_metadata()
    caption_text <- "Click any cell to edit. Cells showing '?' could not be
      parsed and must be filled in before running analysis."
    DT::datatable(
      meta,
      editable = list(target = "cell", disable = list(columns = c(0))),
      rownames = FALSE,
      selection = "none",
      options = list(dom = 't', paging = FALSE),
      caption = caption_text
    )
  })

  onclick("proceed1", {
    get_data()
    get_annot()
    shinyjs::show("summary_tables")

    condition_metadata(NULL)
    # Initialize condition metadata for protein turnover and chemoproteomics templates
    if (!is.null(app_template) && app_template() == TEMPLATES$protein_turnover) {
      tryCatch({
        data <- get_data()
        if (!is.null(data) && "Condition" %in% colnames(data)) {
          conditions <- unique(as.character(data$Condition))
          time_vals <- as.character(autofill_condition_value(conditions))
          time_vals[is.na(time_vals) | time_vals == "NA"] <- "?"
          meta_df <- data.frame(Condition = conditions,
                                TimeVal = time_vals,
                                stringsAsFactors = FALSE)
          condition_metadata(meta_df)
        }
      }, error = function(e) {})
    } else if (!is.null(app_template) && app_template() == TEMPLATES$chemoproteomics) {
      tryCatch({
        data <- get_data()
        if (!is.null(data) && "Condition" %in% colnames(data)) {
          conditions <- unique(as.character(data$Condition))
          is_ctrl <- grepl("^(dmso|control|vehicle)$", tolower(trimws(conditions)))
          parsed_drug <- parse_drug_name_from_conditions(conditions)
          dose_vals <- as.character(autofill_condition_value(conditions))
          dose_vals[is.na(dose_vals) | dose_vals == "NA"] <- "?"
          meta_df <- data.frame(Condition = conditions,
                                DoseVal = dose_vals,
                                DrugName = ifelse(is_ctrl, conditions, parsed_drug),
                                DoseUnit = parse_dose_unit_from_conditions(conditions),
                                stringsAsFactors = FALSE)
          condition_metadata(meta_df)
        }
      }, error = function(e) {
          condition_metadata(NULL)
          showNotification(
              paste("Could not initialize condition metadata:", conditionMessage(e)),
              type = "warning",
              duration = 6
          )
      })
    }

    ### outputs ###
    get_summary <- reactive({
      if (is.null(get_data())) {
        return(NULL)
      }
      data1 <- get_data()
      data_summary <- describe(data1)
    })

    output$summary = renderTable(
      {
        d = get_data()
        if (!is.null(app_template) && app_template() == TEMPLATES$metabolomics)
          head(metabolomics_preview_view(d)) else head(d)
      }, bordered = TRUE
    )
    output$summary_ptm <- renderTable(
      {
        head(get_data()$PTM)
      }, bordered = TRUE
    )
    output$summary_prot <- renderTable(
      {
        head(get_data()$PROTEIN)
      }, bordered = TRUE
    )


    output$summary1 <- renderTable(
      {
        req(get_data())
        get_summary1()

      }, colnames = FALSE, bordered = TRUE
    )

    output$summary2 <- renderTable(
      {
        req(get_data())
        get_summary2()

      }, colnames = FALSE, bordered = TRUE, align = 'lr'
    )

    onclick("proceed2", {
      updateTabsetPanel(session = parent_session, inputId = "tablist",
                        selected = "DataProcessing")
    })
    output$summary_tables <- renderUI({
      ns <- session$ns
      is_turnover <- !is.null(app_template) && app_template() == TEMPLATES$protein_turnover
      is_chemo   <- !is.null(app_template) && app_template() == TEMPLATES$chemoproteomics
      tagList(
        tags$head(
          tags$style(HTML('#loadpage-proceed2{background-color:orange}'))
        ),
        actionButton(inputId = ns("proceed2"), label = "Next step"),
        if (is_turnover) tagList(
          tags$hr(),
          h4("Condition time points"),
          p("Time values are auto-filled from condition names. Correct any values as needed before running the analysis."),
          DT::dataTableOutput(ns("condition_metadata_table")),
          tags$br()
        ) else if (is_chemo) tagList(
          tags$hr(),
          h4("Condition doses"),
          p("Dose values are auto-filled from condition names. Correct any values as needed before running the analysis."),
          DT::dataTableOutput(ns("condition_metadata_table")),
          tags$br()
        ),
        h4("Summary of experimental design"),
        tableOutput(ns('summary1')),
        tags$br(),
        h4("Summary of dataset"),
        tableOutput(ns("summary2")),
        tags$br(),
        shinyjs::hidden(div(id = ns(NAMESPACE_LOADPAGE$summary_nonptm_panel),
                         h4("Top 6 rows of the dataset"),
                         div(style = "overflow-x: auto;", tableOutput(ns("summary")))
        )),
        shinyjs::hidden(div(id = ns(NAMESPACE_LOADPAGE$summary_ptm_panel),
                         h4("Top 6 rows of the PTM dataset"),
                         div(style = "overflow-x: auto;", tableOutput(ns("summary_ptm"))),
                         tags$br(),
                         h4("Top 6 rows of the unmodified protein dataset"),
                         div(style = "overflow-x: auto;", tableOutput(ns("summary_prot")))
        ))
      )
    })

  })

  invisible(NULL)
}
