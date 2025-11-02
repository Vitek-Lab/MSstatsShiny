# ============================================================================
# UI Helper Functions
# ============================================================================

get_condition_choices = function(loadpage_input, preprocess_data) {
  if (loadpage_input$BIO == "PTM" & 
      ((loadpage_input$BIO == "PTM" & loadpage_input$DDA_DIA == "TMT") | 
       loadpage_input$filetype == 'phil')) {
    levels(preprocess_data$PTM$ProteinLevelData$Condition)
  } else if (loadpage_input$BIO == "PTM" & 
             (loadpage_input$BIO == "PTM" & loadpage_input$DDA_DIA != "TMT")) {
    levels(preprocess_data$PTM$ProteinLevelData$GROUP)
  } else if (loadpage_input$DDA_DIA == "TMT") {
    levels(preprocess_data$ProteinLevelData$Condition)
  } else {
    levels(preprocess_data$ProteinLevelData$GROUP)
  }
}

render_group_selectors = function(output, session, choices) {
  ns = session$ns
  
  output$choice1 = renderUI({
    selectInput(ns("group1"), "Group 1", choices())
  })
  
  output$choice2 = renderUI({
    selectInput(ns("group2"), "Group 2", choices())
  })
  
  output$choice3 = renderUI({
    selectInput(ns("group3"), "", choices())
  })
}

render_comparison_inputs = function(output, session, choices) {
  ns = session$ns
  
  output$comp_name = renderUI({
    textInput(ns("comp_name"), label = "Comparison Name", value = "")
  })
  
  output$weights = renderUI({
    lapply(1:length(choices()), function(i) {
      list(numericInput(ns(paste0("weight", i)), 
                        label = choices()[i], value = 0))
    })
  })
}

render_plot_selectors = function(output, session, rownames_func, get_data) {
  ns = session$ns
  
  output$WhichComp = renderUI({
    selectInput(ns("whichComp"),
                label = h5("Select comparison to plot"), 
                c("all", rownames_func()), selected = "all")
  })
  
  output$WhichProt = renderUI({
    selectInput(ns("whichProt"),
                label = h4("which protein to plot"), 
                unique(get_data()[[1]]))
  })
  
  output$WhichProt1 = renderUI({
    selectizeInput(ns("whichProt1"),
                   label = h4("which protein to plot"), 
                   c("", unique(get_data()[[1]])))
  })
}

# ============================================================================
# Contrast Matrix Building Functions
# ============================================================================

validate_contrast_inputs = function(input, def_comp, choices) {
  if (def_comp == "custom") {
    validate(
      need(input$group1 != input$group2, "Please select different groups")
    )
  } else if (def_comp == "custom_np") {
    wt_sum = sum(sapply(1:length(choices), function(i) {
      input[[paste0("weight", i)]]
    }))
    
    validate(
      need(wt_sum == 0, "The contrast weights should sum up to 0")
    )
  }
}

build_custom_contrast = function(input, choices, contrast, comp_list, row) {
  if (input$group1 == input$group2) {
    return(contrast$matrix)
  }
  
  index1 = which(choices == input$group1)
  index2 = which(choices == input$group2)
  
  comp_list$dList = unique(c(isolate(comp_list$dList), 
                             paste(input$group1, "vs", input$group2, sep = " ")))
  
  contrast$row = matrix(row, nrow = 1)
  contrast$row[index1] = 1
  contrast$row[index2] = -1
  
  if (is.null(contrast$matrix)) {
    contrast$matrix = contrast$row
  } else {
    contrast$matrix = rbind(contrast$matrix, contrast$row)
    contrast$matrix = rbind(contrast$matrix[!duplicated(contrast$matrix),])
  }
  
  rownames(contrast$matrix) = comp_list$dList
  colnames(contrast$matrix) = choices
  
  return(contrast$matrix)
}

build_custom_np_contrast = function(input, choices, contrast, comp_list, row) {
  wt_sum = sum(sapply(1:length(choices), function(i) {
    input[[paste0("weight", i)]]
  }))
  
  if (wt_sum != 0) {
    return(contrast$matrix)
  }
  
  comp_list$dList = unique(c(isolate(comp_list$dList), input$comp_name))
  contrast$row = matrix(row, nrow = 1)
  
  for (index in 1:length(choices)) {
    contrast$row[index] = input[[paste0("weight", index)]]
  }
  
  if (is.null(contrast$matrix)) {
    contrast$matrix = contrast$row
  } else {
    contrast$matrix = rbind(contrast$matrix, contrast$row)
    contrast$matrix = rbind(contrast$matrix[!duplicated(contrast$matrix),])
  }
  
  rownames(contrast$matrix) = comp_list$dList
  colnames(contrast$matrix) = choices
  
  return(contrast$matrix)
}

build_all_one_contrast = function(input, choices, contrast, comp_list, row, loadpage_input) {
  index3 = which(choices == input$group3)
  
  for (index in 1:length(choices)) {
    if (index == index3) next
    
    comp_list$dList = c(isolate(comp_list$dList),
                        paste(choices[index], "vs", input$group3, sep = " "))
    
    contrast$row = matrix(row, nrow = 1)
    contrast$row[index] = 1
    contrast$row[index3] = -1
    
    if (is.null(contrast$matrix)) {
      contrast$matrix = contrast$row
    } else {
      contrast$matrix = rbind(contrast$matrix, contrast$row)
    }
  }
  
  rownames(contrast$matrix) = comp_list$dList
  colnames(contrast$matrix) = choices
  
  return(contrast$matrix)
}

build_all_pair_contrast = function(input, choices, contrast, comp_list, row, loadpage_input) {
  contrast$matrix = NULL
  
  for (index in 1:length(choices)) {
    for (index1 in 1:length(choices)) {
      if (index == index1) next
      if (index < index1) {
        comp_list$dList = c(isolate(comp_list$dList),
                            paste(choices[index], "vs", choices[index1], sep = " "))
        
        contrast$row = matrix(row, nrow = 1)
        contrast$row[index] = 1
        contrast$row[index1] = -1
        
        if (is.null(contrast$matrix)) {
          contrast$matrix = contrast$row
        } else {
          contrast$matrix = rbind(contrast$matrix, contrast$row)
          contrast$matrix = rbind(contrast$matrix[!duplicated(contrast$matrix),])
        }
        
        rownames(contrast$matrix) = comp_list$dList
        colnames(contrast$matrix) = choices
      }
    }
  }
  
  return(contrast$matrix)
}

# ============================================================================
# Analysis and Plotting Functions
# ============================================================================

round_df = function(df) {
  nums = vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[, nums] = round(df[, nums], digits = 4)
  return(df)
}

extract_significant_proteins = function(data_comp, loadpage_input, signif_threshold) {
  if (loadpage_input$BIO == "PTM") {
    sig_unadj = data_comp$PTM.Model[data_comp$PTM.Model$adj.pvalue < signif_threshold,]
    sig_prot = data_comp$PROTEIN.Model[data_comp$PROTEIN.Model$adj.pvalue < signif_threshold,]
    sig_adj = data_comp$ADJUSTED.Model[data_comp$ADJUSTED.Model$adj.pvalue < signif_threshold,]
    
    list(PTM.Model = sig_unadj,
         PROTEIN.Model = sig_prot,
         ADJUSTED.Model = sig_adj)
  } else if (loadpage_input$DDA_DIA == "TMT") {
    data_comp$ComparisonResult[data_comp$ComparisonResult$adj.pvalue < signif_threshold,]
  } else {
    data_comp$ComparisonResult[which(data_comp$ComparisonResult$adj.pvalue < signif_threshold),]
  }
}

create_group_comparison_plot = function(input, loadpage_input, data_comparison) {
  show_modal_spinner()
  
  tryCatch({
    if (loadpage_input$BIO == "PTM") {
      plot_result = groupComparisonPlotsPTM(
        data_comparison,
        input$typeplot,
        sig = input$sig,
        FCcutoff = input$FC,
        logBase.pvalue = as.integer(input$logp),
        ProteinName = input$pname,
        which.Comparison = input$whichComp,
        address = FALSE
      )
    } else if (loadpage_input$DDA_DIA == "TMT") {
      if (toupper(input$typeplot) == "VOLCANOPLOT" && input$whichComp == "all") {
        remove_modal_spinner()
        stop('** Cannot generate multiple plots in a screen. Please refine selection or save to a pdf. **')
      }
      
      plot_result = groupComparisonPlots(
        data = data_comparison$ComparisonResult,
        type = input$typeplot,
        sig = input$sig,
        FCcutoff = input$FC,
        logBase.pvalue = as.numeric(input$logp),
        ProteinName = input$pname,
        numProtein = input$nump,
        clustering = input$cluster,
        which.Comparison = input$whichComp,
        which.Protein = input$whichProt,
        height = input$height,
        address = "Ex_",
        isPlotly = TRUE
      )[[1]]
    } else {
      if (toupper(input$typeplot) == "VOLCANOPLOT" && input$whichComp == "all") {
        remove_modal_spinner()
        stop('** Cannot generate multiple plots in a screen. Please refine selection or save to a pdf.**')
      }
      
      plot_result = groupComparisonPlots(
        data = data_comparison$ComparisonResult,
        type = input$typeplot,
        sig = input$sig,
        FCcutoff = input$FC,
        logBase.pvalue = as.numeric(input$logp),
        ProteinName = input$pname,
        numProtein = input$nump,
        clustering = input$cluster,
        which.Comparison = input$whichComp,
        which.Protein = input$whichProt,
        height = input$height,
        address = "Ex_",
        isPlotly = TRUE
      )[[1]]
    }
    
    remove_modal_spinner()
    return(plot_result)
  }, error = function(e) {
    remove_modal_spinner()
    message("An error occurred: ", conditionMessage(e))
    stop('** Cannot generate multiple plots in a screen. Please refine selection or save to a pdf.**')
  })
}

create_assumption_plots = function(input, data_comparison, protein, saveFile) {
  if (input$whichProt1 == "") {
    return(NULL)
  }
  
  id2 = as.character(UUIDgenerate(FALSE))
  id_address2 = paste("tmp/", id2, sep = "")
  
  path2 = if (saveFile) {
    paste("www/", id_address2, sep = "")
  } else {
    FALSE
  }
  
  plots = modelBasedQCPlots(
    data = data_comparison,
    type = input$assum_type,
    which.Protein = protein,
    address = path2
  )
  
  if (saveFile) {
    return(path2)
  } else {
    return(plots)
  }
}

prepare_plotset_data = function(data_comparison, loadpage_input, input) {
  if (loadpage_input$DDA_DIA == "TMT") {
    data_comp = data_comparison$ComparisonResult
    v1 = data_comp[, 1]
    v2 = round(data_comp[, 3], 10)
    v3 = round(data_comp[, 8], 10)
    v4 = data_comp[, 2]
  } else {
    v1 = data_comparison$ComparisonResult[, 1]
    v2 = round(data_comparison$ComparisonResult[, 3], 10)
    v3 = round(data_comparison$ComparisonResult[, 8], 10)
    v4 = data_comparison$ComparisonResult[, 2]
  }
  
  if (input$logp == "2") {
    v3 = -log2(v3)
  } else if (input$logp == "10") {
    v3 = -log10(v3)
  }
  
  df = data.frame(v1, v2, v3, v4)
  df = df[df$v4 == input$whichComp,]
  colnames(df) = c("Protein", "logFC", "logadj.pvalue", "comparison")
  
  return(df)
}

# ============================================================================
# Code Generation Functions
# ============================================================================

generate_analysis_code = function(qc_input, loadpage_input, comp_mat, input) {
  codes = preprocessDataCode(qc_input, loadpage_input)
  
  codes = paste(codes, "\n# Create the contrast matrix\n", sep = "")
  codes = paste(codes, "contrast.matrix = NULL\n", sep = "")
  
  for (i in 1:nrow(comp_mat)) {
    codes = paste(codes, "comparison = matrix(c(", 
                  toString(comp_mat[i,]), "),nrow=1)\n", sep = "")
    codes = paste(codes, "contrast.matrix = rbind(contrast.matrix, comparison)\n", sep = "")
  }
  
  codes = paste(codes, "row.names(contrast.matrix)=c(\"", 
                paste(row.names(comp_mat), collapse = '","'), "\")\n", sep = "")
  codes = paste(codes, "colnames(contrast.matrix)=c(\"", 
                paste(colnames(comp_mat), collapse = '","'), "\")\n", sep = "")
  
  if (loadpage_input$DDA_DIA == "TMT") {
    codes = paste(codes, "\n# Model-based comparison\n", sep = "")
    codes = paste(codes, "model = MSstatsTMT::groupComparisonTMT(summarized,
                       contrast.matrix = contrast.matrix,
                       moderated = ", input$moderated, ",\t\t\t\t
                       adj.method = \"BH\",
                       remove_norm_channel = TRUE,
                       remove_empty_channel = TRUE
                       )\n", sep = "")
  } else if (loadpage_input$BIO == "PTM") {
    dt = if ((loadpage_input$BIO == "PTM" & loadpage_input$DDA_DIA == "TMT") | 
             loadpage_input$filetype == 'phil') "TMT" else "LabelFree"
    
    codes = paste(codes, "\n# Model-based comparison\n", sep = "")
    codes = paste(codes, "model = MSstatsPTM::groupComparisonPTM(summarized, '",
                  dt, "', \t\t\t\t
                      contrast.matrix = contrast.matrix)\n", sep = "")
  } else {
    codes = paste(codes, "\n# Model-based comparison\n", sep = "")
    codes = paste(codes, "model = MSstats::groupComparison(contrast.matrix, summarized)\n", sep = "")
  }
  
  if (loadpage_input$BIO == "PTM") {
    codes = paste(codes, "groupComparisonPlotsPTM(data=model,
                               type=\"Enter VolcanoPlot, Heatmap, or ComparisonPlot\",
                               which.Comparison=\"all\",
                               which.PTM=\"all\",
                               address=\"\")\n", sep = "")
  } else {
    codes = paste(codes, "groupComparisonPlots(data=model$ComparisonResult,
                               type=\"Enter VolcanoPlot, Heatmap, or ComparisonPlot\",
                               which.Comparison=\"all\",
                               which.Protein=\"all\",isPlotly=FALSE,
                               address=\"\")\n", sep = "")
  }
  
  return(codes)
}

# ============================================================================
# Download Handler Functions
# ============================================================================

create_download_handlers = function(output, data_comparison, SignificantProteins, 
                                    data_comparison_code) {
  output$plotresults = downloadHandler(
    filename = function() paste("SummaryPlot-", Sys.Date(), ".zip", sep = ""),
    content = function(file) {
      files = list.files(getwd(), pattern = "^Ex_", full.names = TRUE)
      file_info = file.info(files)
      latest_file = files[which.max(file_info$mtime)]
      file.copy(latest_file, file)
    }
  )
  
  output$download_compar = downloadHandler(
    filename = function() paste("test_result-", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(data_comparison()$ComparisonResult, file)
  )
  
  output$download_code = downloadHandler(
    filename = function() paste("mstats-code-", Sys.Date(), ".R", sep = ""),
    content = function(file) writeLines(data_comparison_code(), file)
  )
  
  output$download_signif = downloadHandler(
    filename = function() paste("data-", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(SignificantProteins(), file)
  )
}

# ============================================================================
# UI Rendering Functions
# ============================================================================

render_results_table = function(output, session, data_comparison, SignificantProteins) {
  ns = session$ns
  
  output$table_results = renderUI({
    req(data_comparison())
    req(SignificantProteins())
    
    if (is.null(SignificantProteins())) {
      tagList(tags$br())
    } else {
      tagList(
        tags$br(),
        h2("Results"),
        h5("There are ", textOutput(ns("number"), inline = TRUE), "significant proteins"),
        tags$br(),
        dataTableOutput(ns("significant")),
        downloadButton(ns("download_compar"), "Download all modeling results"),
        downloadButton(ns("download_signif"), "Download significant proteins")
      )
    }
  })
  
  output$significant = renderDataTable({ SignificantProteins() })
  output$number = renderText({ nrow(SignificantProteins()) })
}

# ============================================================================
# Main Server Function
# ============================================================================

#' Statmodel Server module for stat inference
#'
#' This function sets up the Statmodel server to process data based on user
#' selected inputs
#'
#' @param input input object to capture different ui element values
#' @param output to render and create elements
#' @param session session current module
#' @param parent_session session of the main calling module
#' @param loadpage_input input object from loadpage UI
#' @param qc_input input object from QC UI
#' @param get_data stored function that returns the data from loadpage
#' @param preprocess_data stored function that returns preprocessed data
#' 
#' @return list object with user selected options and matrix build
#'
#' @export
#' @examples
#' NA
#'
statmodelServer = function(id, parent_session, loadpage_input, qc_input, 
                           get_data, preprocess_data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Initialize reactive values
      choices = reactive({ 
        get_condition_choices(loadpage_input(), preprocess_data()) 
      })
      row = reactive({ rep(0, length(choices())) })
      contrast = reactiveValues(matrix = NULL, row = NULL)
      comp_list = reactiveValues(dList = NULL)
      significant = reactiveValues(result = NULL)
      
      # UI visibility
      observe({
        if (loadpage_input()$DDA_DIA == "TMT" | loadpage_input()$BIO == "PTM") {
          hide("Design")
        } else {
          shinyjs::show("Design")
        }
      })
      
      # Render UI elements
      render_group_selectors(output, session, choices)
      render_comparison_inputs(output, session, choices)
      
      Rownames = eventReactive(input$submit | input$submit1 | input$submit2 | input$submit3, {
        req(input$def_comp)
        req(loadpage_input()$DDA_DIA)
        tryCatch({ rownames(matrix_build()) }, error = function(e) {})
      })
      
      render_plot_selectors(output, session, Rownames, get_data)
      
      # Reset on configuration change
      observeEvent(c(input$def_comp, loadpage_input()$proceed1), {
        contrast$matrix = NULL
        comp_list$dList = NULL
        significant$result = NULL
      })
      
      # Validate contrast inputs
      check_cond = eventReactive(
        input$submit | input$submit1 | input$submit2 | input$submit3, {
          req(input$def_comp)
          req(loadpage_input()$DDA_DIA)
          validate_contrast_inputs(input, input$def_comp, choices())
        })
      
      # Build contrast matrix
      matrix_build = eventReactive(
        input$submit | input$submit1 | input$submit2 | input$submit3, {
          req(input$def_comp)
          req(loadpage_input()$DDA_DIA)
          
          if (input$def_comp == "custom") {
            contrast$matrix = build_custom_contrast(
              input, choices(), contrast, comp_list, row())
          } else if (input$def_comp == "custom_np") {
            contrast$matrix = build_custom_np_contrast(
              input, choices(), contrast, comp_list, row())
          } else if (input$def_comp == "all_one") {
            contrast$matrix = build_all_one_contrast(
              input, choices(), contrast, comp_list, row(), loadpage_input())
          } else if (input$def_comp == "all_pair") {
            contrast$matrix = build_all_pair_contrast(
              input, choices(), contrast, comp_list, row(), loadpage_input())
          }
          
          enable("calculate")
          return(contrast$matrix)
        })
      
      # Clear matrix
      observeEvent(input$clear | input$clear1 | input$clear2 | input$clear3, {
        disable("calculate")
        comp_list$dList = NULL
        contrast$matrix = NULL
      })
      
      # Run analysis
      data_comparison = eventReactive(input$calculate, {
        matrix = matrix_build()
        dataComparison(input, qc_input(), loadpage_input(), matrix, preprocess_data())
      })
      
      data_comparison_code = eventReactive(input$calculate, {
        comp_mat = matrix_build()
        generate_analysis_code(qc_input(), loadpage_input(), comp_mat, input)
      })
      
      SignificantProteins = eventReactive(input$calculate, {
        data_comp = data_comparison()
        extract_significant_proteins(data_comp, loadpage_input(), input$signif)
      })
      
      # Plotting
      group_comparison = function(saveFile1, pdf) {
        create_group_comparison_plot(input, loadpage_input(), data_comparison())
      }
      
      assumptions1 = function(saveFile3, protein) {
        create_assumption_plots(input, data_comparison(), protein, saveFile3)
      }
      
      plotset = reactive({
        prepare_plotset_data(data_comparison(), loadpage_input(), input)
      })
      
      # Matrix output
      output$message = renderText({ check_cond() })
      output$table = renderDataTable({ matrix_build() })
      
      output$matrix = renderUI({
        ns = session$ns
        tagList(
          h2("Comparison matrix"),
          br(),
          textOutput(ns("message")),
          br(),
          if (is.null(contrast$matrix)) "" else dataTableOutput(ns("table"))
        )
      })
      
      # Results rendering
      render_results_table(output, session, data_comparison, SignificantProteins)
      
      # Download handlers
      create_download_handlers(output, data_comparison, SignificantProteins, 
                               data_comparison_code)
      
      # Plot rendering
      observeEvent(input$viewresults, {
        ns = session$ns
        if (loadpage_input()$BIO == "PTM") {
          output$comp_plots = renderPlot({ group_comparison(FALSE, FALSE) })
          op = plotOutput(ns("comp_plots"))
        } else {
          output$comp_plots = renderPlotly({ group_comparison(FALSE, FALSE) })
          op = plotlyOutput(ns("comp_plots"), height = input$height)
        }
        
        insertUI(
          selector = paste0("#", ns("comparison_plots")),
          ui = tags$div(
            op,
            conditionalPanel(
              condition = "input['statmodel-typeplot'] == 'VolcanoPlot' && input['loadpage-DDA_DIA']!='TMT'",
              h5("Click on plot for details"),
              verbatimTextOutput(ns("info2"))
            ),
            conditionalPanel(
              condition = "input['statmodel-typeplot'] == 'Heatmap'",
              sliderInput(ns("height"), "Plot height", 
                          value = 500, min = 200, max = 1300, post = "px")
            )
          )
        )
      })
      
      output$info2 = renderPrint({
        nearPoints(plotset(), input$click1, xvar = "logFC", yvar = "logadj.pvalue")
      })
      
      # Assumption plots
      output$verify = renderUI({
        ns = session$ns
        tagList(
          plotOutput(ns("assum_plots"), width = "800px", height = "600px"),
          conditionalPanel(
            condition = "input['statmodel-whichProt1'] != ''",
            actionButton(ns("saveone1"), "Save this plot"),
            actionButton(ns("saveall1"), "Save all plots")
          )
        )
      })
      
      output$assum_plots = renderPlot({
        assumptions1(FALSE, input$whichProt1)
      })
      
      # Enable controls after calculation
      observeEvent(input$calculate, {
        enable("Design")
        enable("typeplot")
        enable("WhichComp")
        enable("download_code")
        
        output$code.button = renderUI({
          ns = session$ns
          downloadButton(ns("download_code"), "Download analysis code", 
                         icon("download"),
                         style = "color: #000000; background-color: #75ba82; border-color: #000000")
        })
      })
      
      return(list(
        input = input,
        dataComparison = data_comparison
      ))
    }
  )
}