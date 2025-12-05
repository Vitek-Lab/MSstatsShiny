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
           uiOutput(ns("plot_specific_options")),
           create_plot_action_buttons(ns)
    )
  )
}

#' Create plot type selector dropdown
#' @noRd
create_plot_type_selector <- function(ns) {
  fluidRow(
    selectInput(
      ns("typeplot"), 
      label = h4("3. Visualization - select plot type"), 
      c(
        "Volcano Plot" = "VolcanoPlot", 
        "Heatmap" = "Heatmap", 
        "Comparison Plot" = "ComparisonPlot"
      )
    )
  )
}

#' Create volcano plot specific options
#' @noRd
create_volcano_plot_options <- function(ns, show_protein_name = TRUE) {
  tagList(
    uiOutput(ns("WhichComp")),
    if (show_protein_name) {
      checkboxInput(ns("pname"), label = p("display protein name"))
    },
    selectInput(
      ns("logp"), 
      label = h5("Log transformation of adjusted p-value"),
      c("base 2" = "2", "base 10" = "10"), 
      selected = "10"
    ),
    sliderInput(
      ns("sig"), 
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
    uiOutput(ns("WhichProt")),
    uiOutput(ns("WhichComp1"))
  )
}

#' Create heatmap specific options
#' @noRd
create_heatmap_options <- function(ns) {
  tagList(
    h4("Note: Only one page will be shown in browser. To view all proteins please view this plot as a pdf. Heatmaps require at least two comparisons."),
    selectInput(
      ns("logp"), 
      label = h5("Log transformation of adjusted p-value"),
      c("base 2" = "2", "base 10" = "10"), 
      selected = "10"
    ),
    create_fold_change_options(ns),
    numericInput(
      ns("nump"), 
      "Number of proteins to display", 
      100, 1, 180, 1
    ),
    selectInput(
      ns("cluster"), 
      label = h5(
        "Cluster analysis",
        class = "icon-wrapper",
        icon("question-circle", lib = "font-awesome"),
        div("Determines how to order proteins and comparisons. protein means, comparison means, or both", class = "icon-tooltip")
      ), 
      c(
        "protein dendogram" = "protein", 
        "comparison dendogram" = "comparison", 
        "protein and comparison dendograms" = "both"
      )
    )
  )
}

#' Create fold change cutoff options (shared between plot types)
#' @noRd
create_fold_change_options <- function(ns) {
  tagList(
    checkboxInput(
      ns("FC1"), 
      label = p("Apply specific fold change cutoff for significance")
    ),
    uiOutput(ns("fold_change_input"))
  )
}

#' Create plot action buttons
#' @noRd
create_plot_action_buttons <- function(ns) {
  tagList(
    p("Please note if you want to plot more than one Volcano Plot comparison, you must save the results as a HTML."),
    uiOutput(ns("view_button")),
    downloadButton(ns("plotresults"), "Save plot results as Zip")
  )
}