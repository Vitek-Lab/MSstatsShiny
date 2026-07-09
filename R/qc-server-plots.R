# QC Summarization Plots tab: plot-type and protein selectors, the
# profile/QC/quality-metric plot builders, and the plot output.

#' Register the QC Summarization Plots tab outputs.
#' @noRd
register_qc_plots <- function(input, output, session, loadpage_input, get_data,
                              preprocess_data, ordered_preprocess_data) {

  output$plotTypeUI <- renderUI({
    ns <- session$ns

    choices <- c("Quality Control Plots" = "QCPlot",
                 "Profile Plots"         = "ProfilePlot")

    if (.anomaly_scores_enabled(loadpage_input())) {
      choices <- c(choices, "Quality Metrics Plots" = "QualityMetricsPlot")
    }

    selectInput(ns("qc_page_plot_type"),
                label = h5("Select plot type",
                           class = "icon-wrapper",
                           icon("question-circle", lib = "font-awesome"),
                           div("For details on plotting options please see the Help tab.",
                               class = "icon-tooltip")),
                choices = choices)
  })

  output$qualityMetricSelector <- renderUI({
    ns <- session$ns
    req(get_data())

    std_cols <- c("ProteinName", "PeptideSequence", "PeptideModifiedSequence",
                  "PrecursorCharge", "FragmentIon", "ProductCharge",
                  "IsotopeLabelType", "Condition", "BioReplicate", "Run",
                  "TechReplicate", "StandardType", "Fraction",
                  "DetectionQValue", "Intensity")
    data_cols   <- colnames(get_data())
    metric_cols <- setdiff(data_cols, std_cols)

    if ("AnomalyScores" %in% metric_cols) {
      metric_cols <- c("AnomalyScores", setdiff(metric_cols, "AnomalyScores"))
    }

    if (length(metric_cols) == 0) {
      return(p("No quality metric columns found in the data."))
    }

    protein_choices <- unique(get_data()$ProteinName)

    tagList(
      selectInput(ns("quality_metric"),
                  label    = h5("Quality metric"),
                  choices  = metric_cols,
                  selected = metric_cols[1]),
      selectizeInput(ns("qm_protein"),
                     label   = h5("Show plot for"),
                     choices = c("", protein_choices))
    )
  })

  # which protein to plot (will add "all" for QCPlot)
  output$which_protein_for_data_process_plots_ui = renderUI({
    ns <- session$ns
    req(input$qc_page_plot_type)
    if (input$qc_page_plot_type == "QualityMetricsPlot") {
      return(NULL)
    }
    if ((loadpage_input()$BIO!="PTM" && input$qc_page_plot_type == "QCPlot")) {
      selectizeInput(ns("which_protein_for_data_process_plots"), "Show plot for",
                     choices = c("", "ALL PROTEINS" = "allonly",
                                 unique(get_data()$ProteinName)))
    } else if (loadpage_input()$BIO == "PTM"){
      if (input$qc_page_plot_type == "QCPlot"){
        selectizeInput(ns("which_protein_for_data_process_plots"), "Show plot for",
                       choices = c("", "ALL PROTEINS" = "allonly",
                                   unique(get_data()$PTM$ProteinName)))
      } else {
        selectizeInput(ns("which_protein_for_data_process_plots"), "Show plot for",
                       choices = c("", unique(get_data()$PTM$ProteinName)))
      }
    } else {
      selectizeInput(ns("which_protein_for_data_process_plots"), "Show plot for",
                     choices = c("", unique(get_data()$ProteinName)))
    }
  })

  callDataProcessPlots = function(protein, summary, original) {
    if (input$which_protein_for_data_process_plots != "") {
      if (loadpage_input()$BIO == "PTM"){
        plot = dataProcessPlotsPTM(preprocess_data(),
                            type=input$qc_page_plot_type,
                            which.PTM = protein,
                            originalPlot = original,
                            summaryPlot = input$summ,
                            address = FALSE,
                            isPlotly = TRUE
        )[[1]]
        return(plot)
      } else if(loadpage_input()$DDA_DIA == "TMT"){
        plot <- dataProcessPlotsTMT(preprocess_data(),
                            type=input$qc_page_plot_type,
                            featureName = input$fname,
                            ylimUp = FALSE,
                            ylimDown = FALSE,
                            which.Protein = protein,
                            originalPlot = original,
                            summaryPlot = input$summ,
                            address = FALSE, isPlotly = TRUE
        )[[1]]
        return(plot)
      } else {
        plot <- dataProcessPlots(data = ordered_preprocess_data(),
                         type=input$qc_page_plot_type,
                         featureName = input$fname,
                         ylimUp = FALSE,
                         ylimDown = FALSE,
                         which.Protein = protein,
                         originalPlot = original,
                         summaryPlot = input$summ,
                         save_condition_plot_result = FALSE,
                         address = FALSE,
                         isPlotly = TRUE

        )[[1]]
        return(plot)
      }
    }
    else {
      return(NULL)
    }
  }

  theplot = reactive({
    if (!is.null(input$qc_page_plot_type) && input$qc_page_plot_type == "QualityMetricsPlot") {
      req(get_data())
      req(input$quality_metric)
      req(input$qm_protein != "")
      return(MSstats::MSstatsQualityMetricsPlot(
        get_data(),
        metric        = input$quality_metric,
        which.Protein = input$qm_protein,
        isPlotly      = TRUE
      ))
    }
    if (input$summ == FALSE) {
      output = callDataProcessPlots(input$which_protein_for_data_process_plots, FALSE, TRUE)
    }
    else if (input$summ == TRUE) {
      output = callDataProcessPlots(input$which_protein_for_data_process_plots, TRUE, FALSE)
    }
    return(output)
  })

  output$showplot = renderUI({
    ns<- session$ns
    output$theplot = renderPlotly(theplot())
    op <- div(
      style = "overflow-x: auto; width: 100%;",
      div(
        style = "min-width: 1400px;",
        plotlyOutput(ns("theplot"), width = "100%")
      )
    )
    tagList(
      op,
      tags$br()
    )
  })
}
