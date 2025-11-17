# ============================================================================
# Modeling Setup
# ============================================================================

#' Create the contrast matrix definition section
#' @param ns namespace function
#' @noRd
create_contrast_section <- function(ns) {
  fluidRow(
    create_contrast_radio_buttons(ns),
    tags$br(),
    create_custom_pairwise_panel(ns),
    create_all_vs_one_panel(ns),
    create_all_pairwise_panel(ns),
    create_custom_nonpairwise_panel(ns),
    create_response_matrix_panel(ns),
    tags$hr()
  )
}

#' Create radio buttons for contrast type selection
#' @noRd
create_contrast_radio_buttons <- function(ns) {
  radioButtons(
    ns("contrast_mode"), 
    label = h4(
      "1. Define comparisons - contrast matrix",
      class = "icon-wrapper",
      icon("question-circle", lib = "font-awesome"),
      div("Define what conditions you want to compare here.", class = "icon-tooltip")
    ),
    c(
      "All possible pairwise comparisons" = "all_pair", 
      "Compare all against one" = "all_one", 
      "Create custom pairwise comparisons" = "custom",
      "Create custom non-pairwise comparisons" = "custom_np",
      "Create response curves" = "response_curve"
    ), 
    selected = character(0)
  )
}

#' Create UI for custom pairwise comparisons
#' @noRd
create_custom_pairwise_panel <- function(ns) {
  conditionalPanel(
    condition = "input['statmodel-contrast_mode'] == 'custom'",
    uiOutput(ns('choice1')),
    h6("vs"),
    uiOutput(ns("choice2")),
    actionButton(ns("submit"), "Add"),
    actionButton(ns("clear"), "Clear matrix")
  )
}

#' Create UI for all vs one comparisons
#' @noRd
create_all_vs_one_panel <- function(ns) {
  conditionalPanel(
    condition = "input['statmodel-contrast_mode'] == 'all_one'",
    h5("Compare all groups against:"),
    uiOutput(ns("choice3")),
    actionButton(ns("submit1"), "Submit"),
    actionButton(ns("clear1"), "Clear matrix")
  )
}

#' Create UI for all pairwise comparisons
#' @noRd
create_all_pairwise_panel <- function(ns) {
  conditionalPanel(
    condition = "input['statmodel-contrast_mode'] == 'all_pair'",
    actionButton(ns("submit2"), "Submit"),
    actionButton(ns("clear2"), "Clear matrix")
  )
}

#' Create UI for custom non-pairwise comparisons
#' @noRd
create_custom_nonpairwise_panel <- function(ns) {
  conditionalPanel(
    condition = "input['statmodel-contrast_mode'] == 'custom_np'",
    h5("Non-pairwise Comparison:"),
    uiOutput(ns('comp_name')),
    uiOutput(ns('weights')),
    actionButton(ns("submit3"), "Add"),
    actionButton(ns("clear3"), "Clear matrix")
  )
}

#' Create panel for configuring response curve metadata
#' @noRd
create_response_matrix_panel <- function(ns) {
  conditionalPanel(
    condition = "input['statmodel-contrast_mode'] == 'response_curve'",
    h5("Add Condition to Response Mapping:"),
    uiOutput(ns("choice3")),
    textInput(ns("response_curve_xaxis"), "X-Axis Label:", placeholder = "e.g., Dosage, Time"),
    numericInput(ns("response_curve_amount"), "Response:", value = NULL, step = 0.1),
    actionButton(ns("submit4"), "Add Entry"),
    actionButton(ns("clear4"), "Clear All Data")
  )
}