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


# features

output$features <- renderUI({
  req(get_data())
  max_feat <- reactive ({
    ## Old code for only 20 features. Meena thought this should be all uniques
    ## TODO: Need to fix this bc hard to be specific with slider.
    # if (nrow(unique(get_data()[1])) < 20) {
    #   m_feat <- nrow(unique(get_data()[1]))
    # }
    # else
    # {
    #   m_feat <- 20
    #   }
    m_feat <- nrow(unique(get_data()[1]))
    return(m_feat)
  })
  sliderInput("n_feat", "Number of top features to use", 1, as.numeric(max_feat()), 1)
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

# features <- function() {
#   if (input$features_used == "all_feat") {
#     n_feat = "all_feat"
#     code_feat = "all"
#   }
#   else {
#     n_feat = "n_feat"
#     code_feat = "topN"
#   }
#   return(n_feat)
# }

# which protein to plot (will add "all" for QCPlot)

output$Which <- renderUI({
  if ((input$DDA_DIA!="TMT" && input$type2 == "QCPlot") || (input$DDA_DIA=="TMT" && input$type1 == "QCPlot")) {
    if (input$DDA_DIA == "PTM"){
      if((input$DDA_DIA=="SRM_PRM" && input$filetype=="sky")||(input$DDA_DIA=="DIA" && input$filetype=="ump")){
        selectizeInput("which", "Show plot for", choices = c("", "ALL PROTEINS" = "allonly", unique(get_data()$PTM[2])))
      }
      else{
        selectizeInput("which", "Show plot for", choices = c("", "ALL PROTEINS" = "allonly", unique(get_data()$PTM[1])))
      }
    } else {
      if((input$DDA_DIA=="SRM_PRM" && input$filetype=="sky")||(input$DDA_DIA=="DIA" && input$filetype=="ump")){
        selectizeInput("which", "Show plot for", choices = c("", "ALL PROTEINS" = "allonly", unique(get_data()[2])))
      }
      else{
        selectizeInput("which", "Show plot for", choices = c("", "ALL PROTEINS" = "allonly", unique(get_data()[1])))
      }
    }
  } else {
    if (input$DDA_DIA == "PTM"){
      selectizeInput("which", "Show plot for", choices = c("", unique(get_data()$PTM[1])))
    } else {
      selectizeInput("which", "Show plot for", choices = c("", unique(get_data()[1])))
    }
  }
})

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

    preprocessed_ptm = lf_summarization_loop(input_data$PTM)
    preprocessed_unmod = lf_summarization_loop(input_data$PROTEIN)
    preprocessed = list(PTM = preprocessed_ptm, PROTEIN = preprocessed_unmod)
    
  } else if(input$DDA_DIA == "PTM" & input$PTMTMT == "Yes"){

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
    codes <- paste(codes, "summarized <- MSstatsTMT:::proteinSummarization(data, 
                   method = '",input$summarization,"\',\t\t\t\t
                   global_norm = ", input$global_norm,",\t\t\t\t 
                   reference_norm = ", input$reference_norm,",\t\t\t\t
                   remove_norm_channel  = ", input$remove_norm_channel,",\t\t\t\t
                   remove_empty_channel = TRUE, \t\t\t\t 
                   MBimpute = FALSE, \t\t\t\t
                   maxQuantileforCensored = ", input$maxQC1,")\n", sep = "")
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
    if (input$features_used == "all"){
      code_n_feat = 'NULL'
    } else if (input$features_used == "topN") {
      code_n_feat = input$n_feat
    } else {
      code_n_feat = 'NULL'
    }
    
    codes <- paste(codes, "\n# use MSstats for protein summarization\n", sep = "")
    codes <- paste(codes, "summarized <- MSstats:::dataProcess(data,
                               normalization = \'", input$norm,"\',\t\t\t\t   
                               logTrans = ", as.numeric(input$log),",\t\t\t\t   
                               nameStandards = ", paste0("c('", paste(input$names, collapse = "', '"), "')"), ",\t\t\t\t  
                               featureSubset = \'", input$features_used, "\',\t\t\t\t  
                               n_top_feature = ", code_n_feat, ",\t\t\t\t  
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
  } else if (input$DDA_DIA == "PTM" & input$PTMTMT == "No"){
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
