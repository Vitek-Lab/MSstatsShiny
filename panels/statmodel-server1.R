######### UI #########

# choices of groups for contrast matrix

choices <- reactive({levels(preprocess_data()$ProcessedData$GROUP_ORIGINAL)})
row <- reactive({rep(0, length(choices()))})
contrast <- reactiveValues()
comp_list <- reactiveValues()

output$choice1 <- renderUI({
  selectInput("group1", "Group 1", choices())
})

output$choice2 <- renderUI({
  selectInput("group2", "Group 2", choices())
})

# rownames for matrix

Rownames <- reactive({
  rownames(matrix_build())
})

# choices of comparisons to plot

output$WhichComp <- renderUI ({
  selectInput("whichComp", 
              label = h4("which comparison to plot"), c("all", Rownames()))
})

output$WhichProt <- renderUI ({
  selectInput("whichProt",
              label = h4("which protein to plot"), c("all", unique(get_data()[1])))
})


########## functions ########

# build matrix

matrix_build <- eventReactive(input$submit, {
  validate(
    need(input$group1 != input$group2, "Please select different groups")
  )
  index1 <- reactive({which(choices() == input$group1)})
  index2 <- reactive({which(choices() == input$group2)})
  comp_list$dList <- c(isolate(comp_list$dList), c(input$group1, "vs", input$group2, ", "))
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
  row.names(contrast$matrix) <- seq(1, nrow(contrast$matrix), 1)
  return(contrast$matrix)
})

# clear matrix

observeEvent(input$clear, {
  comp_list$dList <- ""
  contrast$matrix <- NULL
})

# compare data

data_comparison <- eventReactive(input$calculate, {
  groupComparison(contrast.matrix = matrix_build(), data = preprocess_data())
})

SignificantProteins <- reactive({with(data_comparison(),
                                      ComparisonResult[ComparisonResult$adj.pvalue < input$signif, ])
})

# comparison plots

observeEvent(input$plotresults, {
  group_comparison(TRUE)  
})

observeEvent(input$viewresults, {
  group_comparison(FALSE)  
})

group_comparison <- function(saveFile1) {
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
  
  plot1 <- groupComparisonPlots(data=data_comparison()$ComparisonResult,
                                type=input$typeplot,
                                sig=input$sig,
                                FCcutoff=input$FC,
                                logBase.pvalue=input$logp,
                                #                     ylimUp=input_ylup,
                                #                     ylimDown=input_yldown,
                                #                     xlimUp=input_xlimUp,
                                #                     x.axis.size=input_xax,
                                #                     y.axis.size=input_yax,
                                #                     dot.size=input_dot,
                                #                     text.size=input_text,
                                #                     legend.size=input_legend,
                                ProteinName=input$pname,
                                numProtein=input$nump, 
                                clustering=input$cluster, 
                                #                     height=input_h, 
                                #                     width=input_w, 
                                which.Comparison=input$whichComp,
                                which.Protein=input$whichProt,
                                address=path1()
  )
  if(saveFile1) {
    return(id_address1)
  }
  else {
    return(plot1)
  }
}

# model assumptions plots

assumptions1 <- eventReactive(input$plot_assumptions, {
  # normal quantile-quantile plots
  id2 <- as.character(UUIDgenerate(FALSE))
  id_address2 <- paste("tmp/",id2, sep = "")
  path2 = paste("www/", id_address2, sep = "")
  QQ <- modelBasedQCPlots(data=data_comparison(), type="QQPlots", address = path2)
  return(id_address2)
})
assumptions2 <- eventReactive(input$plot_assumptions, {
  # residual plots
  id3 <- as.character(UUIDgenerate(FALSE))
  id_address3 <- paste("tmp/",id3, sep = "")
  path3 = paste("www/", id_address3, sep = "")
  RES <- modelBasedQCPlots(data=data_comparison(), type="ResidualPlots", address = path3)
  return(id_address3)
})

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

# list of comparisons

output$comparisons <- renderText({
  comp_list$dList
})

# matrix

output$matrix <- renderUI({
  tagList(
    h4("Selected comparisons"),
    textOutput("comparisons"),
    h5("Comparison matrix"),
    if (is.null(contrast$matrix)) {
      ""
    } else {
      tableOutput("table") 
    }
  )
})

output$table <-  renderTable({
  matrix_build()
}, rownames = T)

# table of significant proteins

output$table_results <- renderUI({
  req(data_comparison())
  tagList(
    h4("Results"),
    tags$br(),
    h4("There are ",textOutput("number", inline = TRUE),"significant proteins"),
    tableOutput("significant"),
    tags$br(),
    downloadButton("download_compar", "Download full table of comparison"),
    downloadButton("download_signif", "Download table of significant proteins")
  )
})

output$significant <- renderTable({
  head(SignificantProteins())
})

# number of significant proteins

output$number <- renderText({
  nrow(SignificantProteins())
})

# plot in browser 

observeEvent(input$viewresults, {
  insertUI(
    selector = "#comparison_plots",
    ui=tags$div(
      plotOutput("comp_plots", hover = "hover2"),
      verbatimTextOutput("info2"))
  )
}
)

output$comp_plots <- renderPlot(group_comparison(FALSE))

output$info2 <- renderText({
  paste0(
    "hover: ", xy_str(input$hover2)
  )
})

# downloads

output$verify <- renderUI({
  assumptions1()
  assumptions2()
  tagList(
    a("Open QQ-Plot", href=paste(assumptions1(),"QQPlot.pdf", sep = ""), target="_blank"),
    tags$br(),
    a("Open Residual Plot", href=paste(assumptions2(),"ResidualPlot.pdf", sep = ""), target="_blank")
  )
}
)

output$download_compar <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(data_comparison(), file)
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
        a("Open Volcano Plot", href=paste(group_comparison(TRUE),"VolcanoPlot.pdf", sep = ""), target="_blank")
      }
      else if (input$typeplot == "Heatmap") {
        a("Open Heatmap", href=paste(group_comparison(TRUE),"Heatmap.pdf", sep = ""), target="_blank")
      }
      else if (input$typeplot == "ComparisonPlot") {
        a("Open Comparison Plot", href=paste(group_comparison(TRUE),"ComparisonPlot.pdf", sep = ""), target="_blank")
      }
    )
  )
})




