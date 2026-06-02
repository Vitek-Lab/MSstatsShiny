# ============================================================================
# Style and Header Functions
# ============================================================================

#' Create custom CSS styles for buttons
#' @noRd
create_custom_styles <- function() {
  tags$head(
    tags$style(HTML(
      paste0('#statmodel-', 
             NAMESPACE_STATMODEL$comparisons_submit, 
             '{background-color:orange}')
    )),
    tags$style(HTML(
      paste0('#statmodel-', 
             NAMESPACE_STATMODEL$comparisons_clear, 
             '{background-color:orange}')
    )),
    tags$style(HTML(
      paste0('#statmodel-', 
             NAMESPACE_STATMODEL$modeling_start, 
             '{background-color:orange}')
    )),
    tags$style(HTML(
      paste0('#statmodel-', 
             NAMESPACE_STATMODEL$visualization_view_results, 
             '{background-color:orange}')
    )),
    tags$style(HTML(
      paste0('#statmodel-', 
             NAMESPACE_STATMODEL$visualization_download_plot_results, 
             '{background-color:orange}')
    )),
    tags$link(rel = "stylesheet", type = "text/css", href = "assets/style.css")
  )
}

#' Create header section with instructions
#' @noRd
create_header_section <- function() {
  tagList(
    headerPanel("Statistical modeling and inference"),
    p("In this tab, build your statistical model in three steps:"),
    p(id = "statmodel_workflow_bullet_default",
      "(i) Create a contrast matrix for a group comparison or set up a configuration for a response curve analysis,"),
    shinyjs::hidden(
      p(id = "statmodel_workflow_bullet_response_curve",
        "(i) Configure the response curve analysis,")
    ),
    p("(ii) generate the model and "),
    p("(iii) view result plots."),
    p("More info ", a("here", href="https://www.rdocumentation.org/packages/MSstats/versions/3.4.0/topics/groupComparisonPlots"))
  )
}