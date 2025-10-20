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
    disabled(actionButton(ns("calculate"), "Start")),
    tags$hr(),
    create_moderation_option(ns),
    create_significance_slider(ns),
    # need option for increasing or decreasing trend for dose response
    tags$br()
  )
}

#' Create empirical Bayes moderation option (TMT-specific)
#' @noRd
create_moderation_option <- function(ns) {
  conditionalPanel(
    condition = "input['loadpage-DDA_DIA'] == 'TMT' || (input['loadpage-BIO'] == 'PTM' && input['loadpage-DDA_DIA'] == 'TMT')",
    radioButtons(
      ns("moderated"), 
      label = h4(
        "Empirical Bayes moderation",
        class = "icon-wrapper",
        icon("question-circle", lib = "font-awesome"),
        div("TRUE will moderate t statistic; FALSE (default) uses ordinary t statistic.", class = "icon-tooltip")
      ), 
      c(True = TRUE, False = FALSE)
    )
  )
}

#' Create significance level slider
#' @noRd
create_significance_slider <- function(ns) {
  sliderInput(
    ns("signif"),
    label = h5(
      "Significance level",
      class = "icon-wrapper",
      icon("question-circle", lib = "font-awesome"),
      div("The alpha used to determine significant results. IE the probability of type I error)", class = "icon-tooltip")
    ), 
    0, 1, 0.05
  )
}