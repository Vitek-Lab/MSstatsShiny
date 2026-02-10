# ============================================================================
# Modeling Results Table Section
# ============================================================================

#' Create the results display section
#' @param ns namespace function
#' @noRd
create_results_section <- function(ns) {
  tagList(
    fluidRow(
      uiOutput(ns('code.button')),
      column(7, offset = 10,
             disabled(actionButton(inputId = ns("Design"), label = "Next Step")),
             tags$br(),
             tags$br()
      )
    ),
    uiOutput(ns("matrix")), # rename to something else to capture response curve mappings
    create_results_tables(ns), # should include tables for isotonic regression & ic50 calculations
    tags$br(),
    uiOutput(ns(NAMESPACE_STATMODEL$visualization_plot_output)) # rename to model_plots_output to encapsulate all models
  )
}

#' Create results tables with conditional display for PTM vs non-PTM
#' @noRd
create_results_tables <- function(ns) {
  tagList(
    conditionalPanel(
      condition = "input['loadpage-BIO']=='PTM'",
      tabsetPanel(
        tabPanel("Adjusted PTM Results", uiOutput(ns("adj_table_results"))),
        tabPanel("Unadjusted PTM Results", uiOutput(ns("unadj_table_results"))),
        tabPanel("Protein Results", uiOutput(ns("prot_table_results")))
      )
    ),
    conditionalPanel(
      condition = "input['loadpage-BIO']!=='PTM'",
      uiOutput(ns("table_results"))
    )
  )
}