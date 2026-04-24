# ============================================================================
# Modeling Visualization Options
# ============================================================================

#' Create the visualization section
#' @param ns namespace function
#' @noRd
create_visualization_section <- function(ns) {
  fluidRow(
    column(12,
           create_plot_type_selector(ns),
           uiOutput(ns(NAMESPACE_STATMODEL$visualization_plot_options_conditional_panel)),
           create_plot_action_buttons(ns)
    )
  )
}
#' Create plot type selector dropdown
#' @noRd
create_plot_type_selector <- function(ns) {
  fluidRow(
    selectInput(
      ns(NAMESPACE_STATMODEL$visualization_plot_type), 
      label = h4("3. Visualization - select plot type"), 
      c(
        "Volcano Plot" = CONSTANTS_STATMODEL$plot_type_volcano_plot, 
        "Heatmap" = CONSTANTS_STATMODEL$plot_type_heatmap, 
        "Comparison Plot" = CONSTANTS_STATMODEL$plot_type_comparison_plot,
        "Response Curve" = CONSTANTS_STATMODEL$plot_type_response_curve
      )
    )
  )
}
#' Create volcano plot specific options
#' @noRd
create_volcano_plot_options <- function(ns) {
  tagList(
    uiOutput(ns(NAMESPACE_STATMODEL$visualization_which_comparison)),
    selectInput(
      ns(NAMESPACE_STATMODEL$visualization_logp_base), 
      label = h5("Log transformation of adjusted p-value"),
      c("base 2" = "2", "base 10" = "10"), 
      selected = "10"
    ),
    sliderInput(
      ns(NAMESPACE_STATMODEL$visualization_volcano_significance_cutoff), 
      label = h5(
        "Adjusted p-value cutoff",
        class = "icon-wrapper",
        icon("question-circle", lib = "font-awesome"),
        div("The cutoff used to determine significant results.", class = "icon-tooltip")
      ),
      0, 1, 0.05
    ),
    create_fold_change_options(ns),
    tags$br()
  )
}
#' Create comparison plot specific options
#' @noRd
create_comparison_plot_options <- function(ns) {
  tagList(
    uiOutput(ns(NAMESPACE_STATMODEL$visualization_which_protein))
  )
}
#' Create heatmap specific options
#' @noRd
create_heatmap_options <- function(ns) {
  tagList(
    h4("Note: Only one page will be shown in browser. To view all proteins please view this plot as a pdf. Heatmaps require at least two comparisons."),
    selectInput(
      ns(NAMESPACE_STATMODEL$visualization_logp_base), 
      label = h5("Log transformation of adjusted p-value"),
      c("base 2" = "2", "base 10" = "10"), 
      selected = "10"
    ),
    create_fold_change_options(ns),
    numericInput(
      ns(NAMESPACE_STATMODEL$visualization_heatmap_number_proteins), 
      "Number of proteins to display", 
      100, 1, 180, 1
    ),
    selectInput(
      ns(NAMESPACE_STATMODEL$visualization_heatmap_cluster_option), 
      label = h5(
        "Cluster analysis",
        class = "icon-wrapper",
        icon("question-circle", lib = "font-awesome"),
        div("Determines how to order proteins and comparisons. protein means, comparison means, or both", class = "icon-tooltip")
      ), 
      c(
        "protein dendrogram" = "protein", 
        "comparison dendrogram" = "comparison", 
        "protein and comparison dendrograms" = "both"
      )
    )
  )
}

#' Create response curve specific options
#' @noRd
create_response_curve_options <- function(ns, template = NULL) {
  is_turnover <- isTRUE(template == TEMPLATES$protein_turnover)
  tagList(
    uiOutput(ns(NAMESPACE_STATMODEL$visualization_which_protein)),
    uiOutput(ns(NAMESPACE_STATMODEL$visualization_response_curve_which_drug)),
    if (!is_turnover) {
      checkboxInput(
        ns(NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale),
        label = span(
          "Use ratio scale",
          class = "icon-wrapper",
          icon("question-circle", lib = "font-awesome"),
          div("When enabled, protein abundances are shown relative to the control (control = 1.0). Useful for chemoproteomic experiments to display fold-change vs. DMSO.",
              class = "icon-tooltip",
              style = "max-width: 280px; width: max-content; white-space: normal; line-height: 1.4; text-align: left;")
        ),
        value = TRUE
      )
    }
  )
}

#' Create fold change cutoff options (shared between plot types)
#' @noRd
create_fold_change_options <- function(ns) {
  tagList(
    checkboxInput(
      ns(NAMESPACE_STATMODEL$visualization_fold_change_checkbox), 
      label = p("Apply specific fold change cutoff for significance")
    ),
    uiOutput(ns(NAMESPACE_STATMODEL$visualization_fold_change_input))
  )
}
#' Create plot action buttons
#' @noRd
create_plot_action_buttons <- function(ns) {
  tagList(
    p("Please note if you want to plot more than one Volcano Plot comparison, you must save the results as a HTML."),
    actionButton(ns(NAMESPACE_STATMODEL$visualization_view_results), "View plot in browser (only for one comparison/protein)"),
    downloadButton(ns(NAMESPACE_STATMODEL$visualization_download_plot_results), "Save plot results as Zip")
  )
}
