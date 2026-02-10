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
    uiOutput(ns(NAMESPACE_STATMODEL$modeling_response_curve_fitting_options)),
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

create_response_curve_log_xaxis_checkbox <- function(ns) {
  checkboxInput(
    ns(NAMESPACE_STATMODEL$modeling_response_curve_log_xaxis), 
    label = tags$div("Log scale for treatment values",
                     class = "icon-wrapper",
                     icon("question-circle", lib = "font-awesome"),
                     div("Check this box to use a log scale for the x-axis of dose response curves. 
                          This is typically used when doses are in a log scale (e.g. 0.1, 1, 10, 100).  
                          But if your scale is linear, e.g. time 1, 2, 3 hours, then we recommend unchecking this box", 
                         class = "icon-tooltip")
                     ),
    value = TRUE
  )
}

create_response_curve_increasing_trend_checkbox <- function(ns) {
  checkboxInput(
    ns(NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend), 
    label = tags$div("Increasing trend for dose response curves",
                     class = "icon-wrapper",
                     icon("question-circle", lib = "font-awesome"),
                     div("Check this box if you expect an increasing trend in your dose response curve, e.g. higher doses lead to higher protein abundance. 
                          Uncheck if you expect a decreasing trend, e.g. higher doses lead to lower protein abundance.", 
                         class = "icon-tooltip")
    ),
    value = FALSE
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