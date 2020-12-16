######### UI #########

# choices of groups for contrast matrix

choices <- reactive({
  if(input$DDA_DIA=="TMT"){
    levels(preprocess_data()$Condition)
    
  }
  else{
    levels(preprocess_data()$ProcessedData$GROUP_ORIGINAL)
  }
  
})
row <- reactive({rep(0, length(choices()))})
contrast <- reactiveValues()
comp_list <- reactiveValues()

output$choice1 <- renderUI({
  selectInput("group1", "Group 1", choices())
})

output$choice2 <- renderUI({
  selectInput("group2", "Group 2", choices())
})

output$choice3 <- renderUI({
  selectInput("group3", "", choices())
})

# rownames for matrix

Rownames <- reactive({
  rownames(matrix_build())
})

# choices of comparisons/proteins to plot

output$WhichComp <- renderUI ({
  selectInput("whichComp", 
              label = h5("Select comparison to plot"), c("all", Rownames()), selected = "all")
})

output$WhichProt <- renderUI ({
  selectizeInput("whichProt",
                 label = h4("which protein to plot"), unique(get_data()[1]))
})

output$WhichProt1 <- renderUI ({
  selectizeInput("whichProt1",
                 label = h4("which protein to plot"), c("", unique(get_data()[1])))
})


########## functions ########

# build matrix

observeEvent(input$def_comp, {
  contrast$matrix <- NULL
  comp_list$dList <- NULL
})

matrix_build <- eventReactive(input$submit | input$submit1 | input$submit2, {
  req(input$def_comp)
  if(input$def_comp == "custom") {
    validate(
      need(input$group1 != input$group2, "Please select different groups")
    )
    index1 <- reactive({which(choices() == input$group1)})
    index2 <- reactive({which(choices() == input$group2)})
    comp_list$dList <- c(isolate(comp_list$dList), paste(input$group1, "vs", input$group2, sep = " "))
    contrast$row <- matrix(row(), nrow=1)
    contrast$row[index1()] = 1
    contrast$row[index2()] = -1
    if (is.null(contrast$matrix)) {
      contrast$matrix <- contrast$row 
    } 
    else {
      contrast$matrix <- rbind(contrast$matrix, contrast$row)
      contrast$matrix <- rbind(contrast$matrix[!duplicated(contrast$matrix),])
    }
    rownames(contrast$matrix) <- comp_list$dList
    colnames(contrast$matrix) <- choices()
  }
  else if (input$def_comp == "all_one") {
    for (index in 1:length(choices())) {
      index3 <- reactive({which(choices() == input$group3)})
      if(index == index3()) next
      if(input$DDA_DIA=="TMT"){
        comp_list$dList <- c(isolate(comp_list$dList), paste(choices()[index], " vs ", input$group3, sep = ""))
      } else{
        comp_list$dList <- c(isolate(comp_list$dList), paste("C",index, " vs ", input$group3, sep = ""))
      }
      
      contrast$row <- matrix(row(), nrow=1)
      contrast$row[index] = 1
      contrast$row[index3()] = -1
      if (is.null(contrast$matrix)) {
        contrast$matrix <- contrast$row 
      } 
      else {
        contrast$matrix <- rbind(contrast$matrix, contrast$row)
      }
      rownames(contrast$matrix) <- comp_list$dList
      colnames(contrast$matrix) <- choices()
    }
  }
  else if (input$def_comp == "all_pair") {
    contrast$matrix <- NULL
    for (index in 1:length(choices())) {
      for (index1 in 1:length(choices())) {
        if (index == index1) next
        if (index < index1) {
          if(input$DDA_DIA=="TMT"){
            comp_list$dList <- c(isolate(comp_list$dList), paste(choices()[index], " vs ", choices()[index1], sep = ""))
          } else{
            comp_list$dList <- c(isolate(comp_list$dList), paste("C",index, " vs ", "C",index1, sep = ""))
          }
          
          contrast$row <- matrix(row(), nrow=1)
          contrast$row[index] = 1
          contrast$row[index1] = -1
          if (is.null(contrast$matrix)) {
            contrast$matrix <- contrast$row 
          } 
          else {
            contrast$matrix <- rbind(contrast$matrix, contrast$row)
            contrast$matrix <- rbind(contrast$matrix[!duplicated(contrast$matrix),])
          }
          rownames(contrast$matrix) <- comp_list$dList
          colnames(contrast$matrix) <- choices()
        }
      }
    }
  }
  
  return(contrast$matrix)
})

# clear matrix

observeEvent({input$clear
  input$clear1
  input$clear2},  {
    comp_list$dList <- NULL
    contrast$matrix <- NULL
  })

# compare data

data_comparison <- eventReactive(input$calculate, {
  if(input$DDA_DIA=="TMT"){
    groupComparisonTMT(contrast.matrix = matrix_build(), data = preprocess_data())
  }
  else{
    groupComparison(contrast.matrix = matrix_build(), data = preprocess_data())
  }
  
})

round_df <- function(df) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = 4)
  
  (df)
}

SignificantProteins <- reactive({
  if(input$DDA_DIA=="TMT"){
    data_comp <- data_comparison()
    with(data_comp,round_df(data_comp[data_comp$adj.pvalue < input$signif, ]))
    
  } else {
    with(data_comparison(),round_df(ComparisonResult[ComparisonResult$adj.pvalue < input$signif, ]))
  }
  
})
  



# comparison plots

# observeEvent(input$plotresults, {
#   if(input$typeplot != "ComparisonPlot") {
#     group_comparison(TRUE)
#   }
#   else {
#     group_comparison(TRUE)
#   }
# })
# 
# observeEvent(input$viewresults, {
#   if(input$typeplot != "ComparisonPlot") {
#     group_comparison(TRUE)
#   }
#   else {
#     group_comparison(TRUE)
#   } 
# })

group_comparison <- function(saveFile1, pdf) {
  id1 <- as.character(UUIDgenerate(FALSE))
  id_address1 <- paste("tmp/",id1, sep = "")
  path1 <- function() {
    if (saveFile1) {
      path1_id = paste("www/", id_address1, sep = "")
    }
    else {
      path1_id = FALSE
    }
    return(path1_id)
  }
  
  if(input$DDA_DIA=="TMT"){
    
    plot1 <- groupComparisonPlots2(data=data_comparison(),
                                   type=input$typeplot,
                                   sig=input$sig,
                                   FCcutoff=input$FC,
                                   logBase.pvalue=input$logp,
                                   ProteinName=input$pname,
                                   numProtein=input$nump, 
                                   clustering=input$cluster, 
                                   which.Comparison=input$whichComp,
                                   which.Protein = input$whichProt,
                                   address=path1(),
                                   savePDF=pdf
    )
    
  } else{
    
    plot1 <- groupComparisonPlots2(data=data_comparison()$ComparisonResult,
                                   type=input$typeplot,
                                   sig=input$sig,
                                   FCcutoff=input$FC,
                                   logBase.pvalue=input$logp,
                                   ProteinName=input$pname,
                                   numProtein=input$nump, 
                                   clustering=input$cluster, 
                                   which.Comparison=input$whichComp,
                                   which.Protein = input$whichProt,
                                   address=path1(),
                                   savePDF=pdf
    )
    
  }
  
  if(saveFile1) {
    return(id_address1)
  }
  else {
    return(plot1)
  }
}

# model assumptions plots


assumptions1 <- function(saveFile3, protein) {
  if (input$whichProt1 != "") {
    id2 <- as.character(UUIDgenerate(FALSE))
    id_address2 <- paste("tmp/",id2, sep = "")
    path2 <- function()  {
      if (saveFile3) {
        path_id2 = paste("www/", id_address2, sep = "")
      } 
      else {
        path_id2 = FALSE
      }
      return (path_id2)
    }
    
    plots <- modelBasedQCPlots(data=data_comparison(), type=input$assum_type, which.Protein = protein, address = path2())
    
    if(saveFile3) {
      return(id_address2)
    }
    else {
      return(plots)
    }
  }
  else {
    return(NULL)
  }
}



########## output ##########

# download comparison data

output$compar <- downloadHandler(
  filename = function() {
    paste("comparison-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(data_comparison()$ComparisonResult, file)
  })

output$model_QC <- downloadHandler(
  filename = function() {
    paste("ModelQC-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(data_comparison()$ModelQC, file)
  })

output$fitted_v <- downloadHandler(
  filename = function() {
    paste("model_summary-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(capture.output(data_comparison()$fittedmodel), file)
  })

# matrix

output$matrix <- renderUI({
  tagList(
    h5("Comparison matrix"),
    if (is.null(contrast$matrix)) {
      ""
    } else {
      dataTableOutput("table") 
    }
  )
})

output$table <-  renderDataTable({
  matrix_build()
}, rownames = T)

# table of significant proteins

output$table_results <- renderUI({
  req(data_comparison())
  tagList(
    tags$br(),
    h5("Results"),
    h5("There are ",textOutput("number", inline = TRUE),"significant proteins"),
    dataTableOutput("significant", width = "100%"),
    tags$br(),
    downloadButton("download_compar", "Download full table of comparison"),
    downloadButton("download_signif", "Download table of significant proteins")
  )
})

output$significant <- renderDataTable({
  SignificantProteins()
}, rownames = F
)

# number of significant proteins

output$number <- renderText({
  nrow(SignificantProteins())
})

# plot in browser 

observeEvent(input$typeplot, {
  updateSelectInput(session, "whichComp", selected = "all")
})

observeEvent(input$viewresults, {
  insertUI(
    selector = "#comparison_plots",
    ui=tags$div(
      plotOutput("comp_plots", height = "100%", click = "click1"),
      conditionalPanel(condition = "input.typeplot == 'VolcanoPlot' && input.DDA_DIA!='TMT'",
                       h5("Click on plot for details"),
                       verbatimTextOutput("info2")),
      conditionalPanel(condition = "input.typeplot == 'Heatmap'",
                       sliderInput("height", "Plot height", value = 500, min = 200, max = 1300, post = "px"))
    )
  )
}
)



observe ({output$comp_plots <- renderPlot({
  group_comparison(FALSE, FALSE)}, height = input$height
)
})

plotset <- reactive({
  
  if(input$DDA_DIA=="TMT"){
    data_comp <- data_comparison()
    v1 <- data_comp[,1]
    v2 <- round(data_comp[,3], 10)
    v3 <- round(data_comp[,8], 10)
    v4 <- data_comp[,2]
    
  } else{
    v1 <- data_comparison()$ComparisonResult[,1]
    v2 <- round(data_comparison()$ComparisonResult[,3], 10)
    v3 <- round(data_comparison()$ComparisonResult[,8], 10)
    v4 <- data_comparison()$ComparisonResult[,2]
    
  }
  
  if (input$logp == "2") {
    v3 <- -log2(v3)
  }
  else if (input$logp == "10") {
    v3 <- - log10(v3)
  }
  
  df <- data.frame(v1,v2,v3,v4)
  df <- df[df$v4 == input$whichComp,]
  colnames(df) <- c("Protein", "logFC", "logadj.pvalue", "comparison")
  return(df)
})

output$info2 <- renderPrint({
  print(nearPoints(plotset(), input$click1, xvar = "logFC", yvar = "logadj.pvalue"))
})

# Assumption plots in browser

output$verify <- renderUI ({
  tagList(
    plotOutput("assum_plots", width = "800px", height = "600px"),
    conditionalPanel(condition = "input.whichProt1 != ''",
                     actionButton("saveone1", "Save this plot"),
                     bsTooltip(id = "saveone1", title = "Open plot as pdf.  Popups must be enabled", placement = "bottom", trigger = "hover"),
                     actionButton("saveall1", "Save all plots"),
                     bsTooltip(id = "saveall1", title = "Open pdf of all plots.  Popups must be enabled", placement = "bottom", trigger = "hover")
    )
  )
})

output$assum_plots <- renderPlot({
  assumptions1(FALSE, input$whichProt1)})


# downloads
observeEvent(input$saveone1, {
  path <- assumptions1(TRUE, input$whichProt1)
  if (input$assum_type == "QQPlots") {
    js <- paste("window.open('", path, "QQPlot.pdf')", sep="")
    shinyjs::runjs(js);
  }
  else if (input$type == "ResidualPlots") {
    js <- paste("window.open('", path, "ResidualPlots.pdf')", sep="")
    shinyjs::runjs(js);
  }
})

observeEvent(input$saveall1, {
  path <- assumptions1(TRUE, "all")
  if (input$assum_type == "QQPlots") {
    js <- paste("window.open('", path, "QQPlot.pdf')", sep="")
    shinyjs::runjs(js);
  }
  else if (input$type == "ResidualPlots") {
    js <- paste("window.open('", path, "ResidualPlots.pdf')", sep="")
    shinyjs::runjs(js);
  }
})


output$download_compar <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    if(input$DDA_DIA=="TMT") {
      write.csv(data_comparison(), file)
    } else {
      write.csv(data_comparison()$ComparisonResult, file)
    }
    
  }
)

output$download_signif <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(SignificantProteins(), file)
  }
)

observeEvent(input$plotresults, {
  insertUI(
    selector = "#comparison_plots",
    ui=tags$div(
      if (input$typeplot == "VolcanoPlot") {
        js <- paste("window.open('", group_comparison(TRUE, TRUE), "VolcanoPlot.pdf')", sep="")
        shinyjs::runjs(js);
      }
      else if (input$typeplot == "Heatmap") {
        js <- paste("window.open('", group_comparison(TRUE, TRUE), "Heatmap.pdf')", sep="")
        shinyjs::runjs(js);
      }
      else if (input$typeplot == "ComparisonPlot") {
        js <- paste("window.open('", group_comparison(TRUE, TRUE), "ComparisonPlot.pdf')", sep="")
        shinyjs::runjs(js);
      }
    )
  )
})



