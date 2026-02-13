# ============================================================================
# Modeling Options Functions
# ============================================================================

#' Get TMT moderation radio button conditioned on if experiment is TMT
#' @noRd
get_tmt_moderation_radio_button <- function(loadpage_input, ns) {
  if (loadpage_input$DDA_DIA == "TMT") {
    create_moderation_radio_buttons(ns)
  }
}

#' Get response curve fitting options conditioned on if contrast mode is response curve
#' @noRd
get_response_curve_fitting_options <- function(mode, ns) {
  if (!is.null(mode) && mode == CONSTANTS_STATMODEL$comparison_mode_response_curve) {
    tagList(
      create_response_curve_log_xaxis_checkbox(ns),
      create_response_curve_increasing_trend_checkbox(ns)
    )
  }
}


