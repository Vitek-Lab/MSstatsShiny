
######## UI ########

# standards name

output$Names <- renderUI({
  if (input$standards == "Proteins") {
    if((input$DDA_DIA=="SRM_PRM" && input$filetype=="sky")||(input$DDA_DIA=="DIA" && input$filetype=="ump")){
      selectizeInput("names", "choose standard", unique(get_data()[2]), multiple = T)
    }
    else{
      selectizeInput("names", "choose standard", unique(get_data()[1]), multiple = T)
    }
    
  }
  else if (input$standards == "Peptides") {
    selectizeInput("names", "choose standard", unique(get_data()[2]), multiple = T)
  }
})

# toggle censoring input based on type of experiment

observe({
  if(!is.null(input$filetype)) {
    shinyjs::runjs("$('[type=radio][name=censInt]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
    shinyjs::enable("censInt")
    if (input$filetype == "sky" || input$filetype == "prog" || input$filetype == "spec") {
      shinyjs::disable(selector = "[type=radio][value=NA]")
      shinyjs::runjs("$.each($('[type=radio][name=censInt]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
    }
    else if (input$filetype == "maxq" || input$filetype == "PD" || input$filetype == "open") {
      shinyjs::disable(selector = "[type=radio][value=0]")
      shinyjs::runjs("$.each($('[type=radio][name=censInt]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
    }
  }
})

observe ({
  shinyjs::toggleState("maxQC", input$null == FALSE)
})

quantile <- function() {
  if (input$null == TRUE || input$null1 == TRUE) {
    maxQC <- NULL
  }
  else {
    if(input$DDA_DIA=="TMT"){
      maxQC <- input$maxQC1
    }
    else{
      maxQC <- input$maxQC
    }
    
  }
  return(maxQC)
}

# features

output$features <- renderUI({
  req(get_data())
  max_feat <- reactive ({
    if (nrow(unique(get_data()[1])) < 20) {
      m_feat <- nrow(unique(get_data()[1]))
    }
    else
    {
      m_feat <- 20
      }
    return(m_feat)
  })
  sliderInput("n_feat", "Number of top features to use", 1, as.numeric(max_feat()), 3)
})

observe ({
  shinyjs::toggleState("n_feat", input$all_feat == FALSE)
})

# features <- function() {
#   if (input$all_feat == FALSE) {
#     n_feat <- "topN"
#   }
#   else {
#     n_feat <- "all"
#   }
#   return(n_feat)
# }

features <- function() {
  if (input$features_used == "all_feat") {
    n_feat <- "n_feat"
  }
  else {
    n_feat <- "all_feat"
  }
  return(n_feat)
}

# which protein to plot (will add "all" for QCPlot)

output$Which <- renderUI({
  if ((input$DDA_DIA!="TMT" && input$type2 == "QCPlot") || (input$DDA_DIA=="TMT" && input$type1 == "QCPlot")) {
    if((input$DDA_DIA=="SRM_PRM" && input$filetype=="sky")||(input$DDA_DIA=="DIA" && input$filetype=="ump")){
      selectizeInput("which", "Show plot for", choices = c("", "ALL PROTEINS" = "allonly", unique(get_data()$PTM[2])))
    }
    else{
      selectizeInput("which", "Show plot for", choices = c("", "ALL PROTEINS" = "allonly", unique(get_data()$PTM[1])))
    }
    
  }
  else {
    selectizeInput("which", "Show plot for", choices = c("", unique(get_data()$PTM[1])))
  }
})

######### functions ########

lf_summarization_loop = function(data, busy_indicator = TRUE){
  print("here1")
  proteins = as.character(unique(data[, 'ProteinName']))
  
  if (busy_indicator){
    show_modal_progress_line() # show the modal window
    
    ## Setup progress bar
    update_val = 1/length(proteins)
    counter = 0
  }
  print("here2")
  ## Prepare MSstats for summarization
  peptides_dict = makePeptidesDictionary(as.data.table(unclass(data)), 
                                         toupper(input$norm))
  print("here3")
  prep_input = MSstatsPrepareForDataProcess(data, as.numeric(input$log), NULL)
  print("here4")
  prep_input = MSstatsNormalize(prep_input, input$norm, peptides_dict, input$names)
  print("here5")
  prep_input = MSstatsMergeFractions(prep_input)
  print("here6")
  prep_input = MSstatsHandleMissing(prep_input, "TMP", input$MBi,
                                    "NA", quantile())
  print("here7")
  prep_input = MSstatsSelectFeatures(prep_input, "all", input$n_feat, 2)
  print("here8")
  processed = getProcessed(prep_input)
  print("here9")
  prep_input = MSstatsPrepareForSummarization(prep_input, "TMP", input$MBi, 
                                              input$censInt, FALSE)
  print("here10")
  input_split = split(prep_input, prep_input$PROTEIN)
  summarized_results = vector("list", length(proteins))
  print("here11")
  ## Loop over proteins
  for (i in seq_along(proteins)){
    print("here12")
    temp_data = input_split[[i]]
    print(head(temp_data))
    print(nrow(temp_data))
    summarized_results[[i]] = MSstatsSummarizeSingleTMP(temp_data,
                                                        input$MBi, input$censInt, 
                                                        input$remove50)
    print("here13")
    ## Update progress bar
    if (busy_indicator){
      counter = counter + update_val
      update_modal_progress(counter)
    }
    print("here14")
  }
  
  ## Summarization output
  preprocessed <- MSstatsSummarizationOutput(prep_input, summarized_results, 
                                             processed, "TMP", input$MBi, 
                                             input$censInt)
  
  if (busy_indicator){
    remove_modal_progress() # remove it when done
  }
  return(preprocessed)
  
}

tmt_summarization_loop = function(data){
  MBimpute = FALSE ## Add option for MBimpute to server..
  
  MSstatsConvert::MSstatsLogsSettings(FALSE,
                                      pkg_name = "MSstatsTMT")
  
  ## Prep functions
  prep_input = MSstatsTMT:::MSstatsPrepareForSummarizationTMT(
    data, input$summarization, input$global_norm, input$reference_norm,
    input$remove_norm_channel, TRUE, MBimpute, quantile() 
  )
  prep_input = MSstatsTMT:::MSstatsNormalizeTMT(prep_input, "peptides", 
                                                input$global_norm)
  
  ## Go inside summarization loop to track progress
  log2Intensity = NULL
  annotation = unique(prep_input[!is.na(log2Intensity),
                                 c("Run", "Channel", "BioReplicate", "Condition",
                                   "Mixture", "TechRepMixture", "RunChannel"),
                                 with = FALSE])
  
  ## Current implementatin only keeps track of msstats progress
  ## Other functions are vectorized and should be faster (?)
  if (input$summarization == "msstats") {
    MSRun = FragmentIon = ProductCharge = IsotopeLabelType = ProteinName = 
      PeptideSequence = PrecursorCharge = Run = Condition = BioReplicate =
      Intensity = PSM = RunChannel = NULL
    
    runs = na.omit(unique(annotation$Run))
    num_runs = length(runs)
    
    data.table::setnames(prep_input, c("Run", "RunChannel", "Charge"),
                         c("MSRun", "Run", "PrecursorCharge"))  
    prep_input[, FragmentIon := NA]
    prep_input[, ProductCharge := NA]
    prep_input[, IsotopeLabelType := "L"]
    
    processed_data = vector("list", num_runs)
    summarized_results = vector("list", num_runs)
    
    ## Setup progress bar
    show_modal_progress_line() # show the modal window
    update_val = 1/num_runs
    counter = 0
    for (i in seq_len(num_runs)) {
      
      single_run = prep_input[MSRun == runs[i],
                              list(ProteinName, PeptideSequence, PrecursorCharge,
                                   FragmentIon, ProductCharge, Run, Condition,
                                   BioReplicate, Intensity, IsotopeLabelType,
                                   Fraction = 1)]
      single_run = new("MSstatsValidated", single_run)

      ## Make LF flow into a function and replace it here
      msstats_summary = lf_summarization_loop(single_run, FALSE)
      
      feature_level_data = msstats_summary$FeatureLevelData
      msstats_cols = c("PROTEIN", "PEPTIDE", "originalRUN", "censored",
                       "predicted", "newABUNDANCE")
      msstats_cols = intersect(msstats_cols, colnames(feature_level_data))
      feature_level_data = feature_level_data[, msstats_cols]
      processed_data[[i]] = feature_level_data
      
      protein_level_data = msstats_summary$ProteinLevelData
      protein_level_data = protein_level_data[, c("Protein", "LogIntensities",
                                                  "originalRUN")]
      summarized_results[[i]] = protein_level_data
      
      ## Update progress bar
      counter = counter + update_val
      update_modal_progress(counter)
    }
    
    processed = data.table::rbindlist(processed_data)
    summarized_results = data.table::rbindlist(summarized_results)
    
    data.table::setnames(summarized_results,
                         c("LogIntensities", "originalRUN"),
                         c("Abundance", "RunChannel"))
    summarized_results = merge(summarized_results, annotation,
                               by = "RunChannel", all.x = TRUE)
    summarized_results = summarized_results[, colnames(summarized_results) != "RunChannel",
                                            with = FALSE]
    data.table::setnames(processed, 
                         c("PROTEIN", "PEPTIDE",
                           "originalRUN", "newABUNDANCE"),
                         c("ProteinName", "PSM", 
                           "RunChannel", "log2Intensity"))
    processed = merge(processed, annotation,
                      by = "RunChannel", all.x = TRUE)
    processed[, c("PeptideSequence", "Charge") := tstrsplit(PSM, "_", fixed=TRUE)]
    processed[, RunChannel := NULL]
    summarized = list(summarized_results, processed)
    
  } else if (input$summarization == "MedianPolish") {
    summarized = MSstatsTMT:::.summarizeTMP(prep_input, annotation)
  } else if (input$summarization == "LogSum") {
    summarized = MSstatsTMT:::.summarizeSimpleStat(prep_input, annotation, 
                                                   .logSum)
  } else if (input$summarization == "Median") {
    summarized = MSstatsTMT:::.summarizeSimpleStat(prep_input, annotation, median)
  }
  
  ## Output functions
  processed = MSstatsTMT:::getProcessedTMT(summarized, prep_input)
  summarized = MSstatsTMT:::getSummarizedTMT(summarized)
  summarized = MSstatsTMT:::MSstatsNormalizeTMT(summarized, "proteins", 
                                                input$reference_norm)
  preprocessed = MSstatsTMT:::MSstatsSummarizationOutputTMT(summarized,
                                                            processed, TRUE,
                                                            input$remove_norm_channel)
  
  remove_modal_progress() # remove it when done
  
  return(preprocessed)
}

# preprocess data
preprocess_data = eventReactive(input$run, {
  
  validate(need(get_data(), 
                message = "PLEASE UPLOAD DATASET OR SELECT SAMPLE"))
  
  ## Preprocess input for loop
  input_data = get_data()
  preprocess_list = list()
  
  MSstatsConvert::MSstatsLogsSettings(FALSE)
  
  ## Here we run the underlying functions for MSstats and MSstatsTMT 
  ## summarization. Done so we can loop over proteins and create a progress bar
  if (input$DDA_DIA == "PTM" & input$PTMTMT == "No"){
    # preprocessed =  MSstatsPTM::dataSummarizationPTM(input_data, 
    #                                  logTrans = input$log,
    #                                  normalization = input$norm,
    #                                  normalization.PTM = input$norm,
    #                                  n_top_feature = input$features_used,
    #                                  n_top_feature.PTM = input$features_used,
    #                                  MBimpute = input$censInt,
    #                                  MBimpute.PTM = input$censInt)
    preprocessed_ptm = lf_summarization_loop(input_data$PTM)
    preprocessed_unmod = lf_summarization_loop(input_data$PROTEIN)
    preprocessed = list(PTM = preprocessed_ptm, PROTEIN = preprocessed_unmod)
    
  } else if(input$DDA_DIA == "PTM" & input$PTMTMT == "Yes"){
    print(head(input_data$PTM))
    print(head(input_data$PROTEIN))
    # preprocessed = MSstatsPTM::dataSummarizationPTM_TMT(input_data, 
    #                                  method = input$summarization,
    #                                  reference_norm = input$reference_norm,
    #                                  reference_norm.PTM = input$reference_norm,
    #                                  remove_norm_channel = input$remove_norm_channel,
    #                                  remove_empty_channel = input$remove_norm_channel
    #                                  )
    preprocessed_ptm = tmt_summarization_loop(input_data$PTM)
    preprocessed_unmod = tmt_summarization_loop(input_data$PROTEIN)
    preprocessed = list(PTM = preprocessed_ptm, PROTEIN = preprocessed_unmod)
    
  } else if(input$DDA_DIA == "TMT"){
    
    ## Run MSstatsTMT summarization
    preprocessed = tmt_summarization_loop(input_data)

  } else {
    
    ## Run LF MSstats summarization
    preprocessed = lf_summarization_loop(input_data)

  }

  return(preprocessed)
  })

preprocess_data_code <- eventReactive(input$calculate, { 
  
  codes <- get_data_code()
  
  if(input$DDA_DIA == "TMT"){
    
    codes <- paste(codes, "\n# use MSstats for protein summarization\n", sep = "")
    codes <- paste(codes, "summarized <- MSstatsTMT:::proteinSummarization(data, \'",input$summarization,"\',", 
                   input$global_norm,",", input$reference_norm,",",
                   input$remove_norm_channel,",", "TRUE, FALSE,",input$maxQC1,")\n", sep = "")
    codes <- paste(codes, "\n# use to create data summarization plots\n", sep = "")
    codes <- paste(codes, "dataProcessPlotsTMT(summarized,
                            type= \"Enter ProfilePlot or QCPlot Here\",
                            ylimUp = FALSE,
                            ylimDown = FALSE,
                            which.Protein = \"Enter Protein to Plot Here\",
                            originalPlot = TRUE,
                            summaryPlot =", input$summ,",\t\t\t\t   
                            address = FALSE)\n", sep="")
  }
  else{
    codes <- paste(codes, "\n# use MSstats for protein summarization\n", sep = "")
    codes <- paste(codes, "summarized <- MSstats:::dataProcess(data,
                               normalization = \'", input$norm,"\',\t\t\t\t   
                               logTrans = ", as.numeric(input$log),",\t\t\t\t   
                               nameStandards = ", input$name, ",\t\t\t\t   
                               summaryMethod=\"TMP\",
                               censoredInt=\'", input$censInt, "\',\t\t\t\t   
                               MBimpute=", input$MBi, ",\t\t\t\t   
                               remove50missing=", input$remove50, ",\t\t\t\t   
                               maxQuantileforCensored=", input$maxQC, ")\n", sep = "")
    
    codes <- paste(codes, "dataProcessPlots(data=summarized,
                           type=\"Enter ProfilePlot or QCPlot Here\",
                           ylimUp = F,
                           ylimDown = F,
                           which.Protein = \"Enter Protein to Plot Here\",
                           summaryPlot = TRUE,
                           address = FALSE)\n", sep="")
  }
  
  return(codes)
})



# plot data
# onclick("run", {
#   preprocess_data()
# })


plotresult <- function(saveFile, protein, summary, original) {
  if (input$which != "") {
    id <- as.character(UUIDgenerate(FALSE))
    id_address <- paste("tmp/",id, sep = "")
    path <- function()  {
      if (saveFile) {
        path_id = paste("www/", id_address, sep = "")
      } 
      else {
        path_id = FALSE
      }
      return (path_id)
    }
    
    if(input$DDA_DIA == "TMT"){
      
      dataProcessPlotsTMT(preprocess_data(),
                          type=input$type1,
                          ylimUp = FALSE,
                          ylimDown = FALSE,
                          # x.axis.size = 10,
                          # y.axis.size = 10,
                          # text.size = 4,
                          # text.angle = 90,
                          # legend.size = 7,
                          # dot.size.profile = 2,
                          # ncol.guide = 5,
                          # width = 10,
                          # height = 10,
                          which.Protein = protein,
                          originalPlot = TRUE,
                          summaryPlot = input$summ,
                          address = path()
      )
      
    } else if (input$DDA_DIA == "PTM"){
      
      plot = dataProcessPlotsPTM(preprocess_data(),
                                 type=input$type1,
                                 which.PTM = protein,
                                 summaryPlot = input$summ,
                                 address = path()
      )
      
    } else{
      
      plot = dataProcessPlots(data = preprocess_data(),
                               type=input$type2,
                               featureName = input$fname,
                               ylimUp = F,
                               ylimDown = F,
                               scale = input$cond_scale,
                               interval = input$interval,
                               #              x.axis.size = input_xsize,
                               #              y.axis.size = input_ysize,
                               #              t.axis.size = input_tsize,
                               #              text.angle = input_tangle,
                               #              legend.size = input_legend,
                               #              dot.size.profile = input_dot_prof,
                               #              dot.size.condition = input_dot_cond,
                               #              width = input_width,
                               #              height = input_height,
                               which.Protein = protein,
                               originalPlot = original,
                               summaryPlot = input$summ,
                               save_condition_plot_result = FALSE,
                               address = path()
      )
      
    }
    
    
    if (saveFile) {
      return(id_address)
    } 
    else {
      return (plot)
    }
  }
  else {
    return(NULL)
  }
}

# statistics (for ConditionPlot)

statistics <- reactive({
  sub <- preprocess_data()$ProteinLevelData[which(preprocess_data()$ProteinLevelData$Protein == input$which),]
  len <- aggregate(sub$LogIntensities~sub$GROUP_ORIGINAL, length, data = sub)
  colnames(len)[colnames(len)=="sub$LogIntensities"] <- "Number_of_Measurements"
  sd <- aggregate(sub$LogIntensities~sub$GROUP_ORIGINAL, sd, data = sub)
  colnames(sd)[colnames(sd)=="sub$LogIntensities"] <- "Standard_Deviation"
  mean <- aggregate(sub$LogIntensities~sub$GROUP_ORIGINAL, mean, data = sub)
  colnames(mean)[colnames(mean)=="sub$LogIntensities"] <- "Mean"
  tab <- merge(len, sd, by="sub$GROUP_ORIGINAL")
  tab <- merge(mean, tab, by="sub$GROUP_ORIGINAL")
  colnames(tab)[colnames(tab)=="sub$GROUP_ORIGINAL"] <- "Condition"
  SE <- tab$Standard_Deviation/sqrt(tab$Number_of_Measurements)
  tab$CI_width <- qt(.975, df=tab$Number_of_Measurement)*SE
  CI_Limits <- c(tab$Mean-tab$CI, tab$Mean+tab$CI)
  
  return(tab)
})


cap <- eventReactive(input$run, {
  text_output <- "Protein abundance have been estimated, use the tabs below to download and plot the results."
})

observeEvent(input$run, {output$submit.button <- renderUI(actionButton(inputId = "proceed6", label = "Next step"))})

output$caption <- renderText({
  cap()
})

observeEvent(input$run,{
  
  if(input$DDA_DIA=="PTM"){
    shinyjs::enable("prepr_csv_ptm")
    shinyjs::enable("summ_csv_ptm")
    shinyjs::enable("prepr_csv_prot")
    shinyjs::enable("summ_csv_prot")
  } else {
    shinyjs::enable("prepr_csv")
    shinyjs::enable("summ_csv")
  }
  
})
# output preprocessed data

# observeEvent(input$run, {
#   output$effect <- renderPrint(
#     str(preprocess_data())
#   )
#   insertUI(selector = "#download_buttons",
#            where = "afterEnd",
#            ui= tags$div(tags$br(),
#                         downloadButton("prepr_csv","Download .csv of preprocessed data"),
#                         conditionalPanel(condition = "input.DDA_DIA !== 'TMT'",
#                                          downloadButton("summ_csv","Download .csv of summarised data")
#                                          )
# 
#            )
#   )
# })

# download preprocessed data

output$prepr_csv <- downloadHandler(
  filename = function() {
    paste("Feature_level_data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    if(input$DDA_DIA=='TMT'){
      
      write.csv(preprocess_data()$FeatureLevelData, file, row.names = F)
      
    }
    else{
      
      write.csv(preprocess_data()$FeatureLevelData, file, row.names = F)
    }
    
  }
)

output$prepr_csv_ptm <- downloadHandler(
  filename = function() {
    paste("PTM_Feature_level_data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(preprocess_data()$PTM$FeatureLevelData, file, row.names = F)
  }
)

output$prepr_csv_prot <- downloadHandler(
  filename = function() {
    paste("Protein_Feature_level_data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(preprocess_data()$PROTEIN$FeatureLevelData, file, row.names = F)
  }
)

output$summ_csv <- downloadHandler(
  filename = function() {
    paste("Protein_level_data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(preprocess_data()$ProteinLevelData, file, row.names = F)
  }
)

output$summ_csv_ptm <- downloadHandler(
  filename = function() {
    paste("PTM_level_data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(preprocess_data()$PTM$ProteinLevelData, file, row.names = F)
  }
)

output$summ_csv_prot <- downloadHandler(
  filename = function() {
    paste("Protein_level_data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(preprocess_data()$PROTEIN$ProteinLevelData, file, row.names = F)
  }
)

# download/view plots

 observeEvent(input$saveone, {
   path <- plotresult(TRUE, input$which, FALSE, TRUE)
   if (input$type1 == "ProfilePlot" || input$type2 == "ProfilePlot") {
     js <- paste("window.open('", path, "ProfilePlot.pdf')", sep="")
     shinyjs::runjs(js);
   }
   else if (input$type2 == "ConditionPlot") {
     js <- paste("window.open('", path, "ConditionPlot.pdf')", sep="")
     shinyjs::runjs(js);
   }
   else if (input$type1 == "QCPlot" || input$type2 == "QCPlot") {
     js <- paste("window.open('", path, "QCPlot.pdf')", sep="")
     shinyjs::runjs(js);
   }
 })
   
 observeEvent(input$saveall, {
   path <- plotresult(TRUE, "all", FALSE, TRUE)
   if (input$type1 == "ProfilePlot" || input$type2 == "ProfilePlot") {
     js <- paste("window.open('", path, "ProfilePlot.pdf')", sep="")
     shinyjs::runjs(js);
   }
   else if (input$type2 == "ConditionPlot") {
     js <- paste("window.open('", path, "ConditionPlot.pdf')", sep="")
     shinyjs::runjs(js);
   }
   else if (input$type1 == "QCPlot" || input$type2 == "QCPlot") {
     js <- paste("window.open('", path, "QCPlot.pdf')", sep="")
     shinyjs::runjs(js);
   }
 })
 
output$showplot <- renderUI({
  tagList(
    plotOutput("theplot"),
    conditionalPanel(condition = "input.type == 'ConditionPlot' && input.which != ''",
                     tableOutput("stats")),
    tags$br(),
    conditionalPanel(condition = "input.which != ''",
                     actionButton("saveone", "Save this plot"),
                     bsTooltip(id = "saveone", title = "Open plot as pdf. \
                               Popups must be enabled", placement = "bottom", 
                               trigger = "hover")
                     )
    )
})

theplot <- reactive({
  if (input$summ == FALSE) {
    output <- plotresult(FALSE, input$which, FALSE, TRUE)
  }
  else if (input$summ == TRUE) {
    output <- plotresult(FALSE, input$which, TRUE, FALSE)
  } 
  return (output)
})

# quantification

abundant <- reactiveValues()

observeEvent(input$proceed1, {
  abundant$results <- NULL
})

abundance <- eventReactive(input$update_results, {
  validate(need(preprocess_data(),
                message = "PLEASE COMPLETE DATA PROCESSING"))
  
  if (input$DDA_DIA == "TMT"){
    temp <- copy(preprocess_data())
    setnames(temp$ProteinLevelData, 
             c("Abundance", "Condition", "BioReplicate"), 
             c("LogIntensities", "GROUP", "SUBJECT"))
    abundant$results <- quantification(temp,
                   type = input$typequant,
                   format = input$format,
                   use_log_file = FALSE)
  } else if (input$DDA_DIA == "PTM" & input$PTMTMT == "Yes"){
    temp <- copy(preprocess_data())
    setnames(temp$PTM$ProteinLevelData, 
             c("Abundance", "Condition", "BioReplicate"), 
             c("LogIntensities", "GROUP", "SUBJECT"))
    abundant$results <- quantification(temp$PTM,
                                       type = input$typequant,
                                       format = input$format,
                                       use_log_file = FALSE)
  } else if (input$DDA_DIA == "PTM" & input$PTMTMT == "Yes"){
    temp <- copy(preprocess_data())
    abundant$results <-quantification(temp$PTM,
                                      type = input$typequant,
                                      format = input$format,
                                      use_log_file = FALSE)
  } else{
    temp <- copy(preprocess_data())
    abundant$results <-quantification(temp,
                   type = input$typequant,
                   format = input$format,
                   use_log_file = FALSE)
  }
  
  return(abundant$results)
})

output$theplot <- renderPlot(theplot())

output$stats <- renderTable(statistics())

output$abundance <- renderUI({
  req(abundance())
  if (is.null(abundant$results)) {
    
    tagList(
      tags$br())
  } else {
    tagList(
      dataTableOutput("abundanceTable") )
    }
})
output$abundanceTable <- renderDataTable(abundance())


shinyjs::enable("proceed6")
observeEvent(preprocess_data(),{
  shinyjs::enable("proceed6")
})

onclick("proceed6", {
  updateTabsetPanel(session = session, inputId = "tablist", selected = "StatsModel")
})



# downloads

output$download_summary <- downloadHandler(
  filename = function() {
    paste("Abundance-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(abundance(), file)
  }
)


observeEvent(input$proceed4, {
  updateTabsetPanel(session = session, inputId = "tablist", selected = "StatsModel")
})

