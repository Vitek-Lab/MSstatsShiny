# ============================================================================
# Group Comparison / Modeling
# ============================================================================

#' Create the modeling section
#' @param ns namespace function
#' @noRd
create_modeling_section <- function(ns) {
  tagList(
    h4("2. Group comparison"),
    p("Please add a comparison matrix before modeling."),
    disabled(actionButton(ns(NAMESPACE_STATMODEL$modeling_start), "Start")),
    tags$hr(),
    uiOutput(ns(NAMESPACE_STATMODEL$modeling_tmt_moderation)),
    create_significance_slider(ns),
    # need option for increasing or decreasing trend for dose response
    tags$br()
  )
}

create_moderation_radio_buttons <- function(ns) {
  radioButtons(
    ns(NAMESPACE_STATMODEL$modeling_tmt_moderation), 
    label = h4(
      "Empirical Bayes moderation",
      class = "icon-wrapper",
      icon("question-circle", lib = "font-awesome"),
      div("TRUE will moderate t statistic; FALSE (default) uses ordinary t statistic.", class = "icon-tooltip")
    ), 
    c(True = TRUE, False = FALSE)
  )
}

#' Create significance level slider
#' @noRd
create_significance_slider <- function(ns) {
  sliderInput(
    ns(NAMESPACE_STATMODEL$modeling_significance_level),
    label = h5(
      "Significance level",
      class = "icon-wrapper",
      icon("question-circle", lib = "font-awesome"),
      div("The alpha used to determine significant results, i.e. the probability of type I error", class = "icon-tooltip")
    ), 
    0, 1, 0.05
  )
}