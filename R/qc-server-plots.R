# QC Summarization Plots tab: plot-type and protein selectors, the
# profile/QC/quality-metric plot builders, and the plot + per-condition
# statistics outputs.

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

    selectInput(ns("type1"),
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
  output$Which = renderUI({
    ns <- session$ns
    req(input$type1)
    if (input$type1 == "QualityMetricsPlot") {
      return(NULL)
    }
    if ((loadpage_input()$BIO!="PTM" && input$type1 == "QCPlot")) {
      selectizeInput(ns("which"), "Show plot for",
                     choices = c("", "ALL PROTEINS" = "allonly",
                                 unique(get_data()$ProteinName)))
    } else if (loadpage_input()$BIO == "PTM"){
      if (input$type1 == "QCPlot"){
        selectizeInput(ns("which"), "Show plot for",
                       choices = c("", "ALL PROTEINS" = "allonly",
                                   unique(get_data()$PTM[1])))
      } else {
        selectizeInput(ns("which"), "Show plot for",
                       choices = c("", unique(get_data()$PTM[1])))
      }
    } else {
      selectizeInput(ns("which"), "Show plot for",
                     choices = c("", unique(get_data()$ProteinName)))
    }
  })

  plotresult = function(saveFile, protein, summary, original, file) {
    if (input$which != "") {
      if (loadpage_input()$BIO == "PTM"){
        plot = dataProcessPlotsPTM(preprocess_data(),
                            type=input$type1,
                            which.PTM = protein,
                            originalPlot = original,
                            summaryPlot = input$summ,
                            address = file,
                            isPlotly = TRUE
        )[[1]]
        return(plot)
      } else if(loadpage_input()$DDA_DIA == "TMT"){
        plot <- dataProcessPlotsTMT(preprocess_data(),
                            type=input$type1,
                            featureName = input$fname,
                            ylimUp = FALSE,
                            ylimDown = FALSE,
                            which.Protein = protein,
                            originalPlot = original,
                            summaryPlot = input$summ,
                            address = file, isPlotly = TRUE
        )[[1]]
        return(plot)
      } else {
        plot <- dataProcessPlots(data = ordered_preprocess_data(),
                         type=input$type1,
                         featureName = input$fname,
                         ylimUp = FALSE,
                         ylimDown = FALSE,
                         scale = input$cond_scale,
                         interval = input$interval,
                         which.Protein = protein,
                         originalPlot = original,
                         summaryPlot = input$summ,
                         save_condition_plot_result = FALSE,
                         address = file,
                         isPlotly = TRUE

        )[[1]]
        return(plot)
      }
    }
    else {
      return(NULL)
    }
  }

  # per-condition statistics for the ConditionPlot summary table
  statistics = reactive({
    sub = preprocess_data()$ProteinLevelData[which(preprocess_data()$ProteinLevelData$Protein == input$which),]
    len = aggregate(sub$LogIntensities~sub$GROUP_ORIGINAL, length, data = sub)
    colnames(len)[colnames(len)=="sub$LogIntensities"] = "Number_of_Measurements"
    sd = aggregate(sub$LogIntensities~sub$GROUP_ORIGINAL, sd, data = sub)
    colnames(sd)[colnames(sd)=="sub$LogIntensities"] = "Standard_Deviation"
    mean = aggregate(sub$LogIntensities~sub$GROUP_ORIGINAL, mean, data = sub)
    colnames(mean)[colnames(mean)=="sub$LogIntensities"] = "Mean"
    tab = merge(len, sd, by="sub$GROUP_ORIGINAL")
    tab = merge(mean, tab, by="sub$GROUP_ORIGINAL")
    colnames(tab)[colnames(tab)=="sub$GROUP_ORIGINAL"] = "Condition"
    SE = tab$Standard_Deviation/sqrt(tab$Number_of_Measurements)
    tab$CI_width = qt(.975, df=tab$Number_of_Measurement)*SE
    CI_Limits = c(tab$Mean-tab$CI, tab$Mean+tab$CI)

    return(tab)
  })

  theplot = reactive({
    if (!is.null(input$type1) && input$type1 == "QualityMetricsPlot") {
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
      output = plotresult(FALSE, input$which, FALSE, TRUE, FALSE)
    }
    else if (input$summ == TRUE) {
      output = plotresult(FALSE, input$which, TRUE, FALSE, FALSE)
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
      conditionalPanel(condition = "input['qc-type'] == 'ConditionPlot' && input['qc-which'] != ''",
                       tableOutput(ns("stats"))),
      tags$br(),
      enable("saveplot")
    )
  })

  output$saveplot = downloadHandler(
    filename = function() {
      paste("SummaryPlot-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      pdf(file)
      plotresult(TRUE, input$which, FALSE, TRUE, FALSE)
      dev.off()
    }
  )

  output$stats = renderTable(statistics())
}
