#' QC Server module for data processing
#'
#' This function sets up the QC server to process data based on user
#' selected inputs
#'
#' @param input input object to capture different ui element values
#' @param output to render and create elements
#' @param session session current module
#' @param parent_session session of the main calling module
#' @param loadpage_input input object from loadpage UI
#' @param get_data stored function that returns the data from loadpage
#' 
#' @return input object with user selected options
#'
#' @export
#' @examples
#' NA
#' 
qcServer <- function(input, output, session, parent_session, loadpage_input, get_data,
                     app_template = NULL, get_condition_metadata = NULL) {

  output$Names = renderUI({
    ns <- session$ns

    if (!is.null(app_template) && !is.null(app_template()) &&
        app_template() == TEMPLATES$protein_turnover) {
      return(selectizeInput(ns("names"), "Standard name",
                            choices = "unlabeled", selected = "unlabeled",
                            multiple = TRUE))
    }

    if (input$standards == "Proteins") {
      selectizeInput(ns("names"), "choose standard", unique(get_data()$ProteinName), multiple = TRUE)
    }
    else if (input$standards == "Peptides") {
      selectizeInput(ns("names"), "choose standard", unique(get_data()$PeptideSequence), multiple = TRUE)
    }

  })

  observeEvent(app_template(), {
    req(!is.null(app_template))
    if (app_template() == TEMPLATES$protein_turnover) {
      showTab(inputId = "qc_tabs", target = "Turnover Ratios", session = session)
      shinyjs::hide("log_section")
      shinyjs::hide("censoring_section")
      shinyjs::hide("standards_type_section")
      updateSelectInput(session, "norm",
                        choices = c("none" = "FALSE", "global standards" = "globalStandards"),
                        selected = "FALSE")
      updateRadioButtons(session, "features_used",
                         choices = c("Use all features" = "all"),
                         selected = "all")
    } else {
      hideTab(inputId = "qc_tabs", target = "Turnover Ratios", session = session)
      shinyjs::show("log_section")
      shinyjs::show("censoring_section")
      shinyjs::show("standards_type_section")
      updateSelectInput(session, "norm",
                        choices = c("none" = "FALSE", "equalize medians" = "equalizeMedians",
                                    "quantile" = "quantile", "global standards" = "globalStandards"),
                        selected = "equalizeMedians")
      updateRadioButtons(session, "features_used",
                         choices = c("Use all features" = "all", "Use top N features" = "topN",
                                     "Remove uninformative features & outliers" = "highQuality"),
                         selected = "all")
    }
  }, ignoreNULL = TRUE)
  
  # toggle censoring input based on type of experiment
  
  observe({
    if(!is.null(loadpage_input()$filetype)) {
      runjs("$('[type=radio][name=censInt]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
      enable("censInt")
      if (loadpage_input()$filetype == "sky" || loadpage_input()$filetype == "prog" || loadpage_input()$filetype == "spec") {
        disable(selector = "[type=radio][value=NA]")
        runjs("$.each($('[type=radio][name=censInt]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
      }
      else if (loadpage_input()$filetype == "maxq" || loadpage_input()$filetype == "PD" || loadpage_input()$filetype == "open") {
        disable(selector = "[type=radio][value=0]")
        runjs("$.each($('[type=radio][name=censInt]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
      }
    }
  })
  
  observe ({
    shinyjs::toggleState("maxQC", input$null == FALSE)
  })
  
  
  # features
  
  output$features = renderUI({
    ns <- session$ns
    req(get_data())
    max_feat = reactive({
      ## Old code for only 20 features. Meena thought this should be all uniques
      ## TODO: Need to fix this bc hard to be specific with slider.
      # if (nrow(unique(get_data()[1])) < 20) {
      #   m_feat = nrow(unique(get_data()[1]))
      # }
      # else
      # {
      #   m_feat = 20
      #   }
      
      if (loadpage_input()$BIO =="PTM"){
        m_feat = nrow(unique(get_data()$PTM[1]))  
      } else {
        m_feat = nrow(unique(get_data()[1]))
      }
      
      return(m_feat)
    })
    sliderInput(ns("n_feat"), "Number of top features to use", 1, 
                as.numeric(max_feat()), 1)
  })
  
  observe ({
    toggleState("n_feat", input$all_feat == FALSE)
  })
  
  # which protein to plot (will add "all" for QCPlot)
  
  output$Which = renderUI({
    ns <- session$ns
    req(input$type1)
    if (input$type1 == "QualityMetricsPlot") {
      return(NULL)
    }
    if ((loadpage_input()$BIO!="PTM" && input$type1 == "QCPlot")) {
      # if((loadpage_input()$DDA_DIA=="LType" && loadpage_input()$filetype=="sky") || (loadpage_input()$DDA_DIA=="LType" && loadpage_input()$filetype=="ump")){
      #   selectizeInput(ns("which"), "Show plot for", 
      #                  choices = c("", "ALL PROTEINS" = "allonly", 
      #                              unique(get_data()[2])))
      # } else {
      selectizeInput(ns("which"), "Show plot for", 
                     choices = c("", "ALL PROTEINS" = "allonly", 
                                 unique(get_data()$ProteinName)))
      # }
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
  
  output$plotTypeUI <- renderUI({
    ns <- session$ns

    choices <- c("Quality Control Plots" = "QCPlot",
                 "Profile Plots"         = "ProfilePlot")

    if (isTRUE(loadpage_input()$calculate_anomaly_scores)) {
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
    data_cols  <- colnames(get_data())
    metric_cols <- setdiff(data_cols, std_cols)

    # Put AnomalyScores first when present
    if ("AnomalyScores" %in% metric_cols) {
      metric_cols <- c("AnomalyScores", setdiff(metric_cols, "AnomalyScores"))
    }

    if (length(metric_cols) == 0) {
      return(p("No quality metric columns found in the data."))
    }

    selectInput(ns("quality_metric"),
                label = h5("Quality metric"),
                choices = metric_cols,
                selected = metric_cols[1])
  })

  output$summaryMethodUI <- renderUI({
    ns <- session$ns
    
    # Default choices
    choices <- c("TMP" = "TMP")
    tooltip_text <- "Run-level summarization method. TMP is Tukey's Median Polish. "
    selected <- "TMP"
    
    # Conditionally add MSstats+ if anomaly score calculation is checked
    if (isTRUE(loadpage_input()$calculate_anomaly_scores)) {
      choices <- c(choices, "MSstats+" = "linear")
      tooltip_text <- paste0(tooltip_text, "MSstats+ uses a weighted linear model.")
      selected = "linear"
    }
    
    radioButtons(
      ns("summaryMethod"),
      label = h4(
        "6. Summarization",
        class = "icon-wrapper",
        icon("question-circle", lib = "font-awesome"),
        div(tooltip_text, class = "icon-tooltip")
      ),
      choices = choices,
      selected = selected
    )
  })
  
  # preprocess data
  preprocess_data = eventReactive(input$run, {

   qc_input <- reactive({
      input
    })
    preprocessData(qc_input(),loadpage_input(),get_data())
  })

  # For protein turnover, re-level GROUP factor using TimeVal ordering from loadpage
  ordered_preprocess_data <- reactive({
    data <- preprocess_data()
    if (is.null(data)) return(data)
    if (!is.null(get_condition_metadata) && !is.null(get_condition_metadata())) {
      meta <- get_condition_metadata()
      meta_with_time <- meta[!is.na(meta$TimeVal), ]
      if (nrow(meta_with_time) > 0) {
        ordered_conditions <- meta_with_time$Condition[order(as.numeric(meta_with_time$TimeVal))]
        all_groups <- unique(as.character(data$FeatureLevelData$GROUP))
        remaining <- setdiff(all_groups, ordered_conditions)
        final_levels <- c(ordered_conditions, remaining)
        final_levels <- final_levels[final_levels %in% all_groups]
        data$FeatureLevelData$GROUP <- factor(data$FeatureLevelData$GROUP, levels = final_levels)
        if (!is.null(data$ProteinLevelData)) {
          data$ProteinLevelData$GROUP <- factor(data$ProteinLevelData$GROUP, levels = final_levels)
        }
      }
    }
    data
  })

  preprocess_data_code <- eventReactive(input$calculate, {
    qc_input <- reactive({
      input
    })
    preprocessDataCode(qc_input(),loadpage_input())
  })
  
  plotresult = function(saveFile, protein, summary, original, file) {
    if (input$which != "") {
      # id = as.character(UUIDgenerate(FALSE))
      # id_address = paste("tmp/",id, sep = "")
      # path = function()  {
      #   if (saveFile) {
      #     path_id = paste("www/", id_address, sep = "")
      #   } 
      #   else {
      #     path_id = FALSE
      #   }
      #   return (path_id)
      # }
      
      if(loadpage_input()$DDA_DIA == "TMT"){
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
        
      } else if (loadpage_input()$BIO == "PTM"){
        
        dataProcessPlotsPTM(preprocess_data(),
                            type=input$type1,
                            which.PTM = protein,
                            summaryPlot = input$summ,
                            address = file)
        
      } else{
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
  
  # statistics (for ConditionPlot)
  
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
  
  
  cap = eventReactive(input$run, {
    text_output = "Protein abundance have been estimated, use the tabs below to download and plot the results."
  })
  
  observeEvent(input$run, {
    output$submit.button = renderUI({
      ns <- session$ns
      actionButton(inputId = ns("proceed6"),label = "Next step")
    })
    
      
    })
  
  output$caption = renderText({
    cap()
  })
  
  observeEvent(input$run,{
    
    if(loadpage_input()$BIO=="PTM"){
      enable("prepr_csv_ptm")
      enable("summ_csv_ptm")
      enable("prepr_csv_prot")
      enable("summ_csv_prot")
    } else {
      enable("prepr_csv")
      enable("summ_csv")
    }
    
  })
  
  # download preprocessed data
  
  output$prepr_csv = downloadHandler(
    filename = function() {
      paste("Feature_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      if(loadpage_input()$DDA_DIA=='TMT'){
        
        write.csv(preprocess_data()$FeatureLevelData, file, row.names = FALSE)
        
      }
      else{
        
        write.csv(preprocess_data()$FeatureLevelData, file, row.names = FALSE)
      }
      
    }
  )
  
  output$prepr_csv_ptm = downloadHandler(
    filename = function() {
      paste("PTM_Feature_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$PTM$FeatureLevelData, file, row.names = FALSE)
    }
  )
  
  output$prepr_csv_prot = downloadHandler(
    filename = function() {
      paste("Protein_Feature_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$PROTEIN$FeatureLevelData, file, row.names = FALSE)
    }
  )
  
  output$summ_csv = downloadHandler(
    filename = function() {
      paste("Protein_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$ProteinLevelData, file, row.names = FALSE)
    }
  )
  
  output$summ_csv_ptm = downloadHandler(
    filename = function() {
      paste("PTM_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$PTM$ProteinLevelData, file, row.names = FALSE)
    }
  )
  
  output$summ_csv_prot = downloadHandler(
    filename = function() {
      paste("Protein_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$PROTEIN$ProteinLevelData, file, row.names = FALSE)
    }
  )
  
  # download/view plots
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
  
  # observeEvent(input$saveone, {
  #   path = plotresult(TRUE, input$which, FALSE, TRUE)
  #   if (input$type1 == "ProfilePlot" || input$type1 == "ProfilePlot") {
  #     js = paste("window.open('", path, "ProfilePlot.pdf')", sep="")
  #     runjs(js);
  #   }
  #   else if (input$type1 == "ConditionPlot") {
  #     js = paste("window.open('", path, "ConditionPlot.pdf')", sep="")
  #     runjs(js);
  #   }
  #   else if (input$type1 == "QCPlot" || input$type1 == "QCPlot") {
  #     js = paste("window.open('", path, "QCPlot.pdf')", sep="")
  #     runjs(js);
  #   }
  # })
  
  # observeEvent(input$saveall, {
  #   path = plotresult(TRUE, "all", FALSE, TRUE)
  #   if (input$type1 == "ProfilePlot" || input$type1 == "ProfilePlot") {
  #     js = paste("window.open('", path, "ProfilePlot.pdf')", sep="")
  #     runjs(js);
  #   }
  #   else if (input$type1 == "ConditionPlot") {
  #     js = paste("window.open('", path, "ConditionPlot.pdf')", sep="")
  #     runjs(js);
  #   }
  #   else if (input$type1 == "QCPlot" || input$type1 == "QCPlot") {
  #     js = paste("window.open('", path, "QCPlot.pdf')", sep="")
  #     runjs(js);
  #   }
  # })
  
  output$showplot = renderUI({
    ns<- session$ns

    # PTM plotly plots are still under development
    if (loadpage_input()$BIO == "PTM") {
      output$theplot = renderPlot(theplot())
      op <- plotOutput(ns("theplot"))
    } else {
      output$theplot = renderPlotly(theplot())
      op <- plotlyOutput(ns("theplot"))
    }

    tagList(
      op,
      conditionalPanel(condition = "input['qc-type'] == 'ConditionPlot' && input['qc-which'] != ''",
                       tableOutput(ns("stats"))),
      tags$br(),
      enable("saveplot")
    )
  })
  
  theplot = reactive({
    if (!is.null(input$type1) && input$type1 == "QualityMetricsPlot") {
      req(get_data())
      req(input$quality_metric)
      return(MSstats::MSstatsQualityMetricsPlot(
        get_data(),
        metric    = input$quality_metric,
        isPlotly  = TRUE
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
  
  # quantification
  
  abundant = reactiveValues()
  
  observeEvent(loadpage_input()$proceed1, {
    abundant$results = NULL
  })
  
  abundance = eventReactive(input$update_results, {
    validate(need(preprocess_data(),
                  message = "PLEASE COMPLETE DATA PROCESSING"))
    
    if (loadpage_input()$DDA_DIA == "TMT"){
      temp = copy(preprocess_data())
      setnames(temp$ProteinLevelData, 
               c("Abundance", "Condition", "BioReplicate"), 
               c("LogIntensities", "GROUP", "SUBJECT"))
      abundant$results = quantification(temp,
                                        type = input$typequant,
                                        format = input$format,
                                        use_log_file = FALSE)
    } else if (loadpage_input()$BIO == "PTM" & ((loadpage_input()$BIO == "PTM" & loadpage_input()$DDA_DIA == "TMT") | loadpage_input()$filetype=='phil')){
      temp = copy(preprocess_data())
      setnames(temp$PTM$ProteinLevelData, 
               c("Abundance", "Condition", "BioReplicate"), 
               c("LogIntensities", "GROUP", "SUBJECT"))
      abundant$results = quantification(temp$PTM,
                                        type = input$typequant,
                                        format = input$format,
                                        use_log_file = FALSE)
    } else if (loadpage_input()$BIO == "PTM" & (loadpage_input()$BIO == "PTM" & loadpage_input()$DDA_DIA != "TMT")){
      temp = copy(preprocess_data())
      abundant$results =quantification(temp$PTM,
                                       type = input$typequant,
                                       format = input$format,
                                       use_log_file = FALSE)
    } else if (!is.null(app_template) && !is.null(app_template()) &&
               app_template() == TEMPLATES$protein_turnover) {
      # TODO: Refactor quantification function to handle LABEL column
      abundant$results <- preprocess_data()$ProteinLevelData
    } else{
      temp = copy(preprocess_data())
      abundant$results =quantification(temp,
                                       type = input$typequant,
                                       format = input$format,
                                       use_log_file = FALSE)
    }
    
    return(abundant$results)
  })
  
  output$stats = renderTable(statistics())
  
  output$abundance = renderUI({
    ns <- session$ns
    req(abundance())
    if (is.null(abundant$results)) {
      tagList(
        tags$br())
    } else {
      tagList(
        dataTableOutput(ns("abundanceTable")) )
    }
  })
  output$abundanceTable = renderDataTable(abundance())
  
  
  enable("proceed6")
  observeEvent(preprocess_data(),{
    enable("proceed6")
  })
  
  onclick("proceed6", {
    updateTabsetPanel(session = parent_session, inputId = "tablist", selected = "StatsModel")
  })
  
  # downloads
  
  output$download_summary = downloadHandler(
    filename = function() {
      paste("Abundance-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(abundance(), file)
    }
  )
  
  # not used
  observeEvent(input$proceed4, {
    updateTabsetPanel(session = parent_session, inputId = "tablist", selected = "StatsModel")
  })
  # ---- Protein Turnover: tracer constants form and ratio calculation ----

  output$turnover_ratios_sidebar <- renderUI({
    req(!is.null(app_template) && !is.null(app_template()) &&
          app_template() == TEMPLATES$protein_turnover)
    req(get_data())

    req(!is.null(get_condition_metadata) && !is.null(get_condition_metadata()))
    ns <- session$ns
    conditions <- as.character(get_condition_metadata()$Condition)

    tracer_inputs <- lapply(conditions, function(cond) {
      input_id <- ns(paste0("tracer_", make.names(cond)))
      fluidRow(
        column(6, p(strong(cond))),
        column(6, numericInput(input_id, NULL, value = 1.0, min = 0, max = 1, step = 0.001))
      )
    })

    tagList(
      tags$hr(),
      h4("Turnover Ratio Calculation"),
      p("Enter tracer constants (0 to 1) for each condition:"),
      tagList(tracer_inputs)
    )
  })

  turnover_ratios <- eventReactive(input$run, {
    req(!is.null(app_template) && !is.null(app_template()) &&
          app_template() == TEMPLATES$protein_turnover)
    req(preprocess_data())

    req(!is.null(get_condition_metadata) && !is.null(get_condition_metadata()))
    conditions <- as.character(get_condition_metadata()$Condition)
    tracer_consts <- sapply(conditions, function(cond) {
      val <- input[[paste0("tracer_", make.names(cond))]]
      if (is.null(val)) 1.0 else as.numeric(val)
    })
    names(tracer_consts) <- conditions

    # Use ProteinLevelData when any condition has more than one sample (run);
    # fall back to FeatureLevelData for purely single-replicate designs.
    pld <- preprocess_data()$ProteinLevelData
    samples_per_condition <- tapply(pld$RUN, pld$GROUP, function(x) length(unique(x)))
    use_protein_level <- any(samples_per_condition > 1)

    if (use_protein_level) {
      calculateTurnoverRatios(
        pld,
        channel_col      = "LABEL",
        heavy_label      = "H",
        light_label      = "L",
        time_col         = "GROUP",
        peptide_col      = "Protein",
        protein_col      = "Protein",
        intensity_col    = "LogIntensities",
        run_col          = "RUN",
        peptide_selector = NULL,
        agg_function     = max,
        normalize_tracer = TRUE,
        tracer_constants = tracer_consts
      )
    } else {
      calculateTurnoverRatios(
        preprocess_data()$FeatureLevelData,
        channel_col      = "LABEL",
        heavy_label      = "H",
        light_label      = "L",
        time_col         = "GROUP",
        peptide_col      = "PEPTIDE",
        protein_col      = "PROTEIN",
        intensity_col    = "INTENSITY",
        run_col          = "RUN",
        peptide_selector = NULL,
        agg_function     = max,
        normalize_tracer = TRUE,
        tracer_constants = tracer_consts
      )
    }
  })

  observeEvent(input$run, {
    req(!is.null(app_template) && !is.null(app_template()) &&
          app_template() == TEMPLATES$protein_turnover)
    turnover_ratios()
  }, ignoreInit = TRUE)

  output$turnover_ratios_panel <- renderUI({
    req(!is.null(app_template) && !is.null(app_template()) &&
          app_template() == TEMPLATES$protein_turnover)

    ns <- session$ns
    tagList(
      tags$br(),
      p("Run protein summarization after filling in tracer constants in the side panel."),
      uiOutput(ns("turnover_ratios_table_ui")),
      tags$br(),
      disabled(downloadButton(ns("download_turnover_ratios"), "Download Ratios"))
    )
  })

  output$turnover_ratios_table_ui <- renderUI({
    req(turnover_ratios())
    ns <- session$ns
    enable("download_turnover_ratios")
    dataTableOutput(ns("turnover_ratios_table"))
  })

  output$turnover_ratios_table <- renderDataTable({
    turnover_ratios()
  })

  output$download_turnover_ratios <- downloadHandler(
    filename = function() {
      paste0("Turnover_Ratios-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(turnover_ratios(), file, row.names = FALSE)
    }
  )

  return(
    list(
      input = input,
      preprocessData = preprocess_data,
      turnoverRatios = turnover_ratios
    )
  )
}
