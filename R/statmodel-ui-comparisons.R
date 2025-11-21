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
    uiOutput(ns(NAMESPACE_STATMODEL$comparisons_conditional_panel)),
    tags$hr()
  )
}

#' Create radio buttons for contrast type selection
#' @noRd
create_contrast_radio_buttons <- function(ns) {
  radioButtons(
    ns(NAMESPACE_STATMODEL$comparison_mode), 
    label = h4(
      "1. Define comparisons - contrast matrix",
      class = "icon-wrapper",
      icon("question-circle", lib = "font-awesome"),
      div("Define what conditions you want to compare here", class = "icon-tooltip")
    ),
    c(
      "All possible pairwise comparisons" = CONSTANTS_STATMODEL$comparison_mode_all_pairwise, 
      "Compare all against one" = CONSTANTS_STATMODEL$comparison_mode_all_vs_one, 
      "Create custom pairwise comparisons" = CONSTANTS_STATMODEL$comparison_mode_custom_pairwise,
      "Create custom non-pairwise comparisons" = CONSTANTS_STATMODEL$comparison_mode_custom_nonpairwise,
      "Create response curves" = CONSTANTS_STATMODEL$comparison_mode_response_curve
    ),
    selected = character(0)
  )
}

#' Create UI for custom pairwise comparisons
#' @noRd
build_custom_pairwise_panel <- function(ns) {
  tagList(
    uiOutput(ns(NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1)),
    h6("vs"),
    uiOutput(ns(NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2)),
    actionButton(ns(NAMESPACE_STATMODEL$comparisons_submit), "Add"),
    actionButton(ns(NAMESPACE_STATMODEL$comparisons_clear), "Clear matrix")
  )
}

#' Create UI for all vs one comparisons
#' @noRd
build_all_vs_one_panel <- function(ns) {
  tagList(
    h5("Compare all groups against:"),
    uiOutput(ns(NAMESPACE_STATMODEL$comparisons_all_vs_one_choice)),
    actionButton(ns(NAMESPACE_STATMODEL$comparisons_submit), "Submit"),
    actionButton(ns(NAMESPACE_STATMODEL$comparisons_clear), "Clear matrix")
  )
}

#' Create UI for all pairwise comparisons
#' @noRd
build_all_pairwise_panel <- function(ns) {
  tagList(
    actionButton(ns(NAMESPACE_STATMODEL$comparisons_submit), "Submit"),
    actionButton(ns(NAMESPACE_STATMODEL$comparisons_clear), "Clear matrix")
  )
}

#' Create UI for custom non-pairwise comparisons
#' @noRd
build_custom_nonpairwise_panel <- function(ns) {
  tagList(
    h5("Non-pairwise Comparison:"),
    textInput(ns(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_name), 
              label = "Comparison Name", value = ""),
    uiOutput(ns(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights)),
    actionButton(ns(NAMESPACE_STATMODEL$comparisons_submit), "Add"),
    actionButton(ns(NAMESPACE_STATMODEL$comparisons_clear), "Clear matrix")
  )
}

#' Create panel for configuring response curve metadata
#' @noRd
build_response_curve_panel <- function(ns) {
  tagList(
    h5("Set up response curve configuration:"),
    uiOutput(ns(NAMESPACE_STATMODEL$comparisons_response_curve_choice)),
    textInput(ns(NAMESPACE_STATMODEL$comparisons_response_curve_xaxis), "X-Axis Label:", placeholder = "e.g., Dosage, Time"),
    numericInput(ns(NAMESPACE_STATMODEL$comparisons_response_curve_amount), "Response:", value = NULL, step = 0.1),
    actionButton(ns(NAMESPACE_STATMODEL$comparisons_submit), "Add Entry"),
    actionButton(ns(NAMESPACE_STATMODEL$comparisons_clear), "Clear All Data")
  )
}