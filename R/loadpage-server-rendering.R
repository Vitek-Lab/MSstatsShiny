# ============================================================================
# Loadpage — server-side visibility predicates and observer registration
# ============================================================================
#
# Phase 1 scope: DIANN converter cluster only. These predicates and the
# observer-registration helper replace the JavaScript `conditionalPanel`
# expressions that previously gated four DIANN-related UI sections:
#
#   1. DIANN LType options block (diann_2plus toggle + intensity-column override)
#   2. Q-value filter block (shared with Skyline / Spectronaut) including the
#      DIANN-specific MBR sub-panel
#   3. DIANN regular-path anomaly-scoring toggle + run-order fileInput
#   4. DIANN big-file-path anomaly-scoring run-order fileInput
#
# All four panels migrate to `shinyjs::hidden(div(id = ns(...), ...))` in the
# UI plus an `observe({ shinyjs::toggle(...) })` block here. Reason: panels
# contain inputs whose state must persist across visibility flips (an
# intensity-column text, a Q-value numeric, a run-order file, etc.). `renderUI`
# would rebuild the inputs at defaults whenever the driving reactive changed,
# which is a behavior change vs the original `conditionalPanel` (which kept
# the DOM mounted) and would also cause `getData(input)` in R/utils.R to read
# `NULL` for hidden inputs at proceed time.
#
# All helpers are internal (`@noRd`); kept pure (no Shiny reactivity) so they
# can be exercised with truth-table tests.


#' Should the DIANN LType options block be visible?
#'
#' Mirrors:
#'   filetype == 'diann' && DDA_DIA == 'LType' && !big_file_diann
#'
#' @param filetype       value of `input$filetype`
#' @param dda_dia        value of `input$DDA_DIA`
#' @param big_file_diann value of `input$big_file_diann`
#' @noRd
loadpage_show_diann_lf_options <- function(filetype, dda_dia, big_file_diann) {
  isTRUE(filetype == "diann") &&
    isTRUE(dda_dia == "LType") &&
    !isTRUE(big_file_diann)
}

#' Should the DIANN intensity-column override be visible?
#'
#' Shown only when the user has indicated DIANN < 2.0 by leaving the
#' `diann_2plus` checkbox unchecked. Parent panel
#' (`loadpage_show_diann_lf_options`) must already be visible — when it is
#' hidden the CSS cascade hides this sub-panel regardless of this predicate.
#'
#' @param diann_2plus value of `input$diann_2plus`
#' @noRd
loadpage_show_diann_intensity_column <- function(diann_2plus) {
  !isTRUE(diann_2plus)
}

#' Should the Q-value filter section be visible?
#'
#' Shared across Skyline, Spectronaut, and DIANN (regular path).
#'
#' Mirrors:
#'   filetype == 'sky' || filetype == 'spec' ||
#'     (filetype == 'diann' && !big_file_diann)
#'
#' @param filetype       value of `input$filetype`
#' @param big_file_diann value of `input$big_file_diann`
#' @noRd
loadpage_show_qval_filter <- function(filetype, big_file_diann) {
  isTRUE(filetype == "sky") ||
    isTRUE(filetype == "spec") ||
    (isTRUE(filetype == "diann") && !isTRUE(big_file_diann))
}

#' Should the Q-value cutoff + MBR sub-section be visible?
#'
#' Inside the Q-value filter section; gated by the `q_val` checkbox.
#'
#' @param q_val value of `input$q_val`
#' @noRd
loadpage_show_qval_cutoff <- function(q_val) {
  isTRUE(q_val)
}

#' Should the DIANN MBR checkbox be visible?
#'
#' Only relevant for DIANN once the user has enabled Q-value filtering.
#'
#' @param q_val    value of `input$q_val`
#' @param filetype value of `input$filetype`
#' @noRd
loadpage_show_diann_mbr <- function(q_val, filetype) {
  isTRUE(q_val) && isTRUE(filetype == "diann")
}

#' Should the DIANN regular-path anomaly-scoring section be visible?
#'
#' Mirrors:
#'   filetype == 'diann' && !big_file_diann
#'
#' @param filetype       value of `input$filetype`
#' @param big_file_diann value of `input$big_file_diann`
#' @noRd
loadpage_show_diann_anomaly <- function(filetype, big_file_diann) {
  isTRUE(filetype == "diann") && !isTRUE(big_file_diann)
}

#' Should the DIANN regular-path anomaly run-order fileInput be visible?
#'
#' Inside the regular-path anomaly section; gated by the anomaly checkbox.
#'
#' @param diann_calculate_anomaly_scores value of `input$diann_calculate_anomaly_scores`
#' @noRd
loadpage_show_diann_anomaly_run_order <- function(diann_calculate_anomaly_scores) {
  isTRUE(diann_calculate_anomaly_scores)
}

#' Should the DIANN big-file-path anomaly run-order fileInput be visible?
#'
#' Inside the big-file annotation block (itself only rendered when
#' `big_file_diann` is TRUE and `is_web_server` is FALSE); gated by the
#' big-file anomaly checkbox.
#'
#' @param big_diann_calculate_anomaly_scores value of
#'   `input$big_diann_calculate_anomaly_scores`
#' @noRd
loadpage_show_big_diann_anomaly_run_order <- function(big_diann_calculate_anomaly_scores) {
  isTRUE(big_diann_calculate_anomaly_scores)
}


#' Register DIANN-cluster visibility observers inside the loadpage module
#'
#' Adds one `observe({ shinyjs::toggle(...) })` block per migrated panel.
#' Call once from `loadpageServer`'s `moduleServer` scope so the observers
#' inherit the module session and `shinyjs` uses raw (unnamespaced) IDs.
#'
#' @param input   the Shiny module's `input` object
#' @param session the Shiny module's `session` (unused for now but kept for
#'   future-proofing — `shinyjs::toggle` already uses the current reactive
#'   domain)
#' @noRd
register_diann_visibility_observers <- function(input, session) {
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$diann_lf_options_panel,
      condition = loadpage_show_diann_lf_options(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$dda_dia]],
        input[[NAMESPACE_LOADPAGE$big_file_diann]]
      )
    )
  })

  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$diann_intensity_column_panel,
      condition = loadpage_show_diann_intensity_column(
        input[[NAMESPACE_LOADPAGE$diann_2plus]]
      )
    )
  })

  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$qval_filter_panel,
      condition = loadpage_show_qval_filter(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$big_file_diann]]
      )
    )
  })

  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$qval_cutoff_panel,
      condition = loadpage_show_qval_cutoff(
        input[[NAMESPACE_LOADPAGE$q_val]]
      )
    )
  })

  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$qval_mbr_panel,
      condition = loadpage_show_diann_mbr(
        input[[NAMESPACE_LOADPAGE$q_val]],
        input[[NAMESPACE_LOADPAGE$filetype]]
      )
    )
  })

  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$diann_anomaly_panel,
      condition = loadpage_show_diann_anomaly(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$big_file_diann]]
      )
    )
  })

  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$diann_anomaly_run_order_panel,
      condition = loadpage_show_diann_anomaly_run_order(
        input[[NAMESPACE_LOADPAGE$diann_calculate_anomaly_scores]]
      )
    )
  })

  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$big_diann_anomaly_run_order_panel,
      condition = loadpage_show_big_diann_anomaly_run_order(
        input[[NAMESPACE_LOADPAGE$big_diann_calculate_anomaly_scores]]
      )
    )
  })

  invisible(NULL)
}
