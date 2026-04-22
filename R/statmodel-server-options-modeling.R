# ============================================================================
# Modeling Options Functions
# ============================================================================

#' Get TMT moderation radio button conditioned on if experiment is TMT
#' @noRd
get_tmt_moderation_radio_button = function(loadpage_input, ns) {
  if (loadpage_input$DDA_DIA == "TMT") {
    create_moderation_radio_buttons(ns)
  }
}

#' Get modeling section header based on comparison mode and template
#'
#' @param mode Character. The current comparison mode.
#' @param template Character. The active template name.
#' @return A tagList with the appropriate heading and description.
#' @noRd
get_modeling_section_header <- function(mode, template = TEMPLATES$default) {
  if (isTRUE(mode == CONSTANTS_STATMODEL$comparison_mode_response_curve)) {
    if (isTRUE(template == TEMPLATES$protein_turnover)) {
      tagList(
        h4("2. Turnover analysis"),
        p("Please configure the mapping between experimental conditions and time points.")
      )
    } else {
      tagList(
        h4("2. Dose-response analysis"),
        p("Please configure the mapping between experimental groups and treatment concentrations.")
      )
    }
  } else {
    tagList(
      h4("2. Group comparison"),
      p("Please add a comparison matrix before modeling.")
    )
  }
}

#' Get response curve fitting options conditioned on if contrast mode is response curve
#' @noRd
get_response_curve_fitting_options = function(mode, ns, template = TEMPLATES$default) {
  if (!is.null(mode) && mode == CONSTANTS_STATMODEL$comparison_mode_response_curve) {
    is_turnover <- isTRUE(template == TEMPLATES$protein_turnover)
    label_text <- if (is_turnover) {
      "Increasing heavy-isotope incorporation over time"
    } else {
      "Increasing trend for dose-response curves"
    }
    tooltip_text <- if (is_turnover) {
      "Check this box if you expect the heavy-isotope fraction to increase over time (typical for pulse-chase turnover experiments)."
    } else {
      "Check this box if you expect an increasing trend in your dose-response curve, e.g. higher doses lead to higher protein abundance. Uncheck if you expect a decreasing trend, e.g. higher doses lead to lower protein abundance."
    }
    tagList(
      create_response_curve_increasing_trend_checkbox(
        ns,
        value = template != TEMPLATES$chemoproteomics,
        label_text = label_text,
        tooltip_text = tooltip_text
      )
    )
  }
}


