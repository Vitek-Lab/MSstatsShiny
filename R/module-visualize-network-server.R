# =============================================================================
# UI RENDERING FUNCTIONS (SHINY-SPECIFIC)
# These functions stay in your Shiny application
# =============================================================================

#' Generate Cytoscape JavaScript with Shiny event handling
#' 
#' This function wraps the package's generateCytoscapeConfig() function
#' and adds Shiny-specific event handling for table highlighting
#' 
#' @param node_elements Node elements from package
#' @param edge_elements Edge elements from package  
#' @param display_label_type Column to display the label from the node table
#' @param container_id Network Visualization Container ID (default: 'network-cy')
#' @param module_id Module ID for Shiny Application (default: 'network')
#' @importFrom MSstatsBioNet generateCytoscapeConfig
#' @return JavaScript code string with Shiny event handlers
generateCytoscapeJSForShiny <- function(node_elements, edge_elements, 
                                        display_label_type = "id",
                                        container_id = "network-cy", module_id = "network") {
  
  # Define Shiny-specific event handlers for both edges and nodes
  shiny_event_handlers <- list(
    edge_click = paste0("function(evt) {
        var edge = evt.target;
        const edgeId = edge.id();
        Shiny.setInputValue('", module_id, "-edgeClicked', {
            source: edge.data('source'),
            target: edge.data('target'),
            interaction: edge.data('interaction'),
            edge_type: edge.data('edge_type'),
            category: edge.data('category'),
            evidenceLink: edge.data('evidenceLink')
        });
    }"),
    node_click = paste0("function(evt) {
        var node = evt.target;
        const nodeId = node.id();
        Shiny.setInputValue('", module_id, "-nodeClicked', { 
            id: node.data('id'),
            label: node.data('label'),
            color: node.data('color')
        });
    }")
  )
  
  # Use the package function to generate configuration
  config <- generateCytoscapeConfig(
    nodes = node_elements,
    edges = edge_elements,
    display_label_type = display_label_type,
    container_id = container_id,
    event_handlers = shiny_event_handlers
  )
  
  # Return the JavaScript code
  return(config$js_code)
}

renderDataTables <- function(output, nodes_table, edges_table) {
  output$nodesTable <- renderDT({
    datatable(nodes_table, 
              options = list(pageLength = 10, 
                             searchable = TRUE,
                             scrollX = TRUE,
                             autoWidth = TRUE))
  })
  
  output$edgesTable <- renderDT({
    datatable(edges_table, 
              options = list(pageLength = 10, 
                             searchable = TRUE,
                             scrollX = TRUE,
                             autoWidth = TRUE), 
              selection = 'single')
  })
}

highlightNodeInTable <- function(output, node_data, nodes_table) {
  node_id <- node_data$id
  
  # Find matching row based on node ID
  row_index <- which(nodes_table$id == node_id)
  
  if (length(row_index) > 0) {
    # Create a filtered table with just the clicked node
    filtered_table <- nodes_table[row_index, , drop = FALSE]
    
    output$nodesTable <- renderDT({
      datatable(filtered_table, 
                options = list(pageLength = 10, 
                               searchable = TRUE,
                               scrollX = TRUE,
                               autoWidth = TRUE), 
                selection = list(mode = 'single', selected = 1))
    })
  }
}

highlightEdgeInTable <- function(output, edge_data, edges_table) {
  req(edge_data$source, edge_data$target, edge_data$interaction)
  source <- edge_data$source
  target <- edge_data$target
  interaction <- gsub(" \\(bidirectional\\)", "", edge_data$interaction)
  edge_type <- edge_data$edge_type
  
  # Find matching rows
  if (edge_type %in% c("undirected", "bidirectional")) {
    
    # For undirected or bidirectional edges, match source and target regardless of order
    row_indices <- which(((edges_table$source == source & edges_table$target == target) |
                            (edges_table$source == target & edges_table$target == source)) &
                           (edges_table$interaction == interaction))
  } else {
    # For directed edges, match exactly
    row_indices <- which(edges_table$source == source & 
                           edges_table$target == target & 
                           edges_table$interaction == interaction)
  }
  
  if (length(row_indices) > 0) {
    # Bring the highlighted row to the top
    reordered_table <- edges_table[row_indices, ]
    
    output$edgesTable <- renderDT({
      datatable(reordered_table, 
                options = list(pageLength = 10, 
                               searchable = TRUE,
                               scrollX = TRUE,
                               autoWidth = TRUE), 
                selection = list(mode = 'single', selected = 1))
    })
  }
}

# Open evidence link in new tab
openEvidenceLink <- function(session, evidence_link) {
  if (!is.null(evidence_link) && evidence_link != "" && !is.na(evidence_link)) {
    # Send JavaScript command to open link in new tab
    session$sendCustomMessage(
      type = 'openLinkInNewTab',
      message = list(url = evidence_link)
    )
  }
}

# =============================================================================
# UPDATED SERVER CODE - Using the decoupled architecture
# =============================================================================

# Load your cytoscape visualization package
# library(YourCytoscapePackage)

# Source the UI rendering functions (or include in your Shiny app)
# source("ui_rendering_functions.R")

# =============================================================================
# HELPER FUNCTIONS - Data Management (unchanged)
# =============================================================================

loadCsvData <- function(input, dataComparison) {
  if (is.null(input$dataUpload) && !is.null(dataComparison()$ComparisonResult)) {
    df <- dataComparison()$ComparisonResult
    if (!is.null(df) && "Protein" %in% names(df)) {
      df$Protein <- as.character(df$Protein)
      return(df)
    }
  }
  req(input$dataUpload)
  tryCatch({
    read.csv(input$dataUpload$datapath)
  }, error = function(e) {
    showNotification(paste("Error reading file:", e$message), type = "error")
    return(NULL)
  })
}

# Helper function to update the label dropdown choices
updateLabelChoices <- function(session, df) {
  if (!is.null(df) && "Label" %in% names(df)) {
    unique_labels <- unique(df$Label)
    # Remove any NA values
    unique_labels <- unique_labels[!is.na(unique_labels)]
    
    updateSelectInput(session, "selectedLabel",
                      choices = unique_labels)
  } else {
    # If no Label column exists, disable the dropdown
    updateSelectInput(session, "selectedLabel",
                      choices = c("No Label column found" = "none"),
                      selected = "none")
  }
}

# Updated helper function to update the protein choices dropdown
updateProteinChoices <- function(session, df) {
  if (!is.null(df) && "Protein" %in% names(df)) {
    updateSelectizeInput(
      session,
      "selectedProteins",
      choices = unique(df$Protein),
      server  = TRUE
    )
  } else {
    # If no Protein column exists, clear the dropdown
    updateSelectizeInput(
      session,
      "selectedProteins",
      choices = NULL,
      server  = TRUE
    )
  }
}

getInputParameters <- function(input) {
  # Require that both filters have at least one selection
  req(input$statementTypes, input$sources)
  
  # Handle "all" selections for statement type
  statementTypes <- if("all" %in% req(input$statementTypes)) {
    NULL
  } else {
    input$statementTypes
  }
  
  # Handle "all" selections for sources
  sources <- if("all" %in% input$sources) {
    NULL
  } else {
    input$sources
  }
  
  # Handle protein selection (NULL if nothing selected)
  selectedProteins <- if(is.null(input$selectedProteins) || length(input$selectedProteins) == 0) {
    NULL
  } else {
    input$selectedProteins
  }
  
  list(
    proteinIdType = req(input$proteinIdType),
    pValue = as.numeric(req(input$pValue)),
    evidence = as.numeric(req(input$evidence)),
    absLogFC = as.numeric(req(input$absLogFC)),
    statementTypes = statementTypes,
    sources = sources,
    selectedLabel = req(input$selectedLabel),
    selectedProteins = selectedProteins
  )
}

# =============================================================================
# HELPER FUNCTIONS - Data Processing (unchanged)
# =============================================================================

# Helper function to filter data by selected label
filterDataByLabel <- function(df, selectedLabel) {
  if ("Label" %in% names(df)) {
    filtered_df <- df[df$Label == selectedLabel & !is.na(df$Label), ]
    return(filtered_df)
  } else {
    return(df)
  }
}

annotateProteinData <- function(df, proteinIdType) {
  tryCatch({
    annotateProteinInfoFromIndra(df, proteinIdType)
  }, error = function(e) {
    showNotification(paste("Error in annotation:", e$message), type = "error")
    return(NULL)
  })
}

extractSubnetwork <- function(annotated_df, pValue, evidence, statementTypes, 
                              sources, absLogFC, selectedProteins) {
  tryCatch({
    getSubnetworkFromIndra(annotated_df, 
                           pvalueCutoff = pValue, 
                           evidence_count_cutoff = evidence,
                           statement_types = statementTypes,
                           sources_filter = sources,
                           logfc_cutoff = absLogFC,
                           force_include_proteins = selectedProteins)
  }, error = function(e) {
    showNotification(paste("Error in subnetwork extraction:", e$message), type = "error")
    print(e$message)
    return(NULL)
  })
}

# =============================================================================
# MAIN SERVER FUNCTION - Updated to use decoupled architecture
# =============================================================================

#' Server logic for network visualization module
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param parent_session Parent Shiny session
#' @param dataComparison Reactive expression containing comparison data
#'
#' @return None (side effects only)
#' 
#' @importFrom MSstatsBioNet annotateProteinInfoFromIndra getSubnetworkFromIndra
#' @importFrom DT renderDT datatable
#' @importFrom shiny updateSelectizeInput showNotification outputOptions
visualizeNetworkServer <- function(input, output, session, parent_session, dataComparison) {
  
  # Output to control conditional panels
  output$hasValidDataComparison <- reactive({
    !is.null(dataComparison()) &&
      !is.null(dataComparison()$ComparisonResult) && 
      !is.null(dataComparison()$ComparisonResult$Protein)
  })
  outputOptions(output, "hasValidDataComparison", suspendWhenHidden = FALSE)
  
  # Main reactive expressions
  df <- reactive({
    loadCsvData(input, dataComparison)
  })
  
  # Create a reactive expression to generate the network data
  renderNetwork <- reactive({
    params <- getInputParameters(input)
    
    # Get the original data
    original_df <- df()
    if (is.null(original_df)) return(NULL)
    
    # Filter by selected label first
    filtered_df <- filterDataByLabel(original_df, params$selectedLabel)
    if (nrow(filtered_df) == 0) {
      showNotification("No data found for selected label", type = "warning")
      return(NULL)
    }
    
    # Annotate protein info and filter the subnetwork
    annotated_df <- annotateProteinData(filtered_df, params$proteinIdType)
    if (is.null(annotated_df)) return(NULL)
    
    subnetwork <- extractSubnetwork(annotated_df, params$pValue, params$evidence, 
                                    params$statementTypes, params$sources,
                                    params$absLogFC, params$selectedProteins)
    if (is.null(subnetwork)) return(NULL)
    
    return(list(
      nodes_table = subnetwork$nodes,
      edges_table = subnetwork$edges
    ))
  })
  
  networkVisualization <- reactive({
    network_data <- renderNetwork()
    if (is.null(network_data)) return(NULL)
    
    # Generate JavaScript code with Shiny-specific event handling
    js_code <- generateCytoscapeJSForShiny(
      network_data$nodes_table, 
      network_data$edges_table,
      display_label_type = input$displayLabelType,
      container_id = session$ns("cy"),
      module_id = session$ns(NULL)
    )
    
    return(list(
      js_code = js_code,
      edges_table = network_data$edges_table,
      nodes_table = network_data$nodes_table
    ))
  })
  
  generate_network_code <- eventReactive(input$showNetwork, {
    params <- getInputParameters(input)
    
    codes <- ""
    codes <- paste(codes, "\n# Load Required Packages\n", sep = "")
    codes <- paste(codes, "library(MSstatsBioNet)\nlibrary(dplyr)\n\n", sep = "")
    
    codes <- paste(codes, "# Read data\n", sep = "")
    codes <- paste(codes, "df <- read.csv(\"path/to/your/data.csv\")\n\n", sep = "")
    
    # Add label filtering if not default
    if (params$selectedLabel != "" && !is.null(params$selectedLabel)) {
      codes <- paste(codes, "# Filter by selected comparison\n", sep = "")
      codes <- paste(codes, "filtered_df <- df[df$Label == \"", params$selectedLabel, "\" & !is.na(df$Label), ]\n\n", sep = "")
    } else {
      codes <- paste(codes, "filtered_df <- df\n\n", sep = "")
    }
    
    codes <- paste(codes, "# Annotate protein information\n", sep = "")
    codes <- paste(codes, "annotated_df <- annotateProteinInfoFromIndra(filtered_df, \"", params$proteinIdType, "\")\n\n", sep = "")
    
    codes <- paste(codes, "# Extract subnetwork with filtering parameters\n", sep = "")
    codes <- paste(codes, "subnetwork <- getSubnetworkFromIndra(\n", sep = "")
    codes <- paste(codes, "  annotated_df,\n", sep = "")
    codes <- paste(codes, "  pvalueCutoff = ", params$pValue, ",\n", sep = "")
    codes <- paste(codes, "  evidence_count_cutoff = ", params$evidence, ",\n", sep = "")
    
    # Handle statement types
    if (is.null(params$statementTypes)) {
      codes <- paste(codes, "  statement_types = NULL,\n", sep = "")
    } else {
      statement_types_str <- paste0("c(\"", paste(params$statementTypes, collapse = "\", \""), "\")")
      codes <- paste(codes, "  statement_types = ", statement_types_str, ",\n", sep = "")
    }
    
    # Handle sources
    if (is.null(params$sources)) {
      codes <- paste(codes, "  sources_filter = NULL,\n", sep = "")
    } else {
      sources_str <- paste0("c(\"", paste(params$sources, collapse = "\", \""), "\")")
      codes <- paste(codes, "  sources_filter = ", sources_str, ",\n", sep = "")
    }
    
    codes <- paste(codes, "  logfc_cutoff = ", params$absLogFC, sep = "")
    
    # Handle selected proteins
    if (!is.null(params$selectedProteins) && length(params$selectedProteins) > 0) {
      selected_proteins_str <- paste0("c(\"", paste(params$selectedProteins, collapse = "\", \""), "\")")
      codes <- paste(codes, ",\n  force_include_proteins = ", selected_proteins_str, "\n", sep = "")
    } else {
      codes <- paste(codes, ",\n  force_include_proteins = NULL\n", sep = "")
    }
    
    codes <- paste(codes, ")\n\n", sep = "")
    
    codes <- paste(codes, "# View network components\n", sep = "")
    codes <- paste(codes, "print(\"Nodes in network:\")\n", sep = "")
    codes <- paste(codes, "print(subnetwork$nodes)\n\n", sep = "")
    codes <- paste(codes, "print(\"Edges in network:\")\n", sep = "")
    codes <- paste(codes, "print(subnetwork$edges)\n\n", sep = "")
    
    codes <- paste(codes, "# Save results\n", sep = "")
    codes <- paste(codes, "write.csv(subnetwork$nodes, \"network_nodes.csv\", row.names = FALSE)\n", sep = "")
    codes <- paste(codes, "write.csv(subnetwork$edges, \"network_edges.csv\", row.names = FALSE)\n", sep = "")
    codes <- paste(codes, "# Visualize network on web browser and export as an HTML file\n", sep = "")
    displayLabelTypeStr <- paste0("\"", paste(input$displayLabelType, collapse = "\", \""), "\"")
    codes <- paste(codes, "previewNetworkInBrowser(subnetwork$nodes, subnetwork$edges, displayLabelType=", displayLabelTypeStr, ")\n", sep = "")
    codes <- paste(codes, "exportNetworkToHTML(subnetwork$nodes, subnetwork$edges, displayLabelType=", displayLabelTypeStr, ")\n", sep = "")
    
    return(codes)
  })
  
  # Event observers
  observeEvent(input$showNetwork, {
    req(df(), getInputParameters(input))
    
    # Show loading indicator
    shinyjs::show("loadingIndicator")
    
    # Disable the button during processing
    shinyjs::disable("showNetwork")
    
    render_data <- networkVisualization()
    if (is.null(render_data)) {
      # Hide loading indicator and re-enable button if there's an error
      shinyjs::hide("loadingIndicator")
      shinyjs::enable("showNetwork")
      return()
    }
    
    # Send JavaScript code to frontend
    session$sendCustomMessage(type = 'runCytoscape', message = render_data$js_code)
    
    # Render data tables
    renderDataTables(output, render_data$nodes_table, render_data$edges_table)
    
    # Hide loading indicator and re-enable button when done
    shinyjs::hide("loadingIndicator")
    shinyjs::enable("showNetwork")
    
    output$network.code.button <- renderUI({
      ns <- session$ns
      downloadButton(ns("network_download_code"), "Download analysis code", icon("download"),
                     style="color: #000000; background-color: #75ba82; border-color: #000000")
    })
  })
  
  output$network_download_code <- downloadHandler(
    filename = function() {
      paste("network-analysis-code-", Sys.Date(), ".R", sep = "")
    },
    content = function(file) {
      tryCatch({
        code_content <- generate_network_code()
        if (is.null(code_content) || length(code_content) == 0) {
          stop("No code generated. Please ensure network is displayed first.")
        }
        writeLines(code_content, file)
      }, error = function(e) {
        showNotification(
          paste("Error downloading code:", e$message),
          type = "error"
        )
      })
    }
  )
  
  # Observe edge click events
  observeEvent(input$edgeClicked, {
    edge_data <- input$edgeClicked
    network_data <- renderNetwork()
    req(network_data)
    edges_table <- network_data$edges_table
    
    highlightEdgeInTable(output, edge_data, edges_table)
    openEvidenceLink(session, edge_data$evidenceLink)
  })
  
  # Observe node click events
  observeEvent(input$nodeClicked, {
    node_data <- input$nodeClicked
    network_data <- renderNetwork()
    req(network_data)
    nodes_table <- network_data$nodes_table
    
    highlightNodeInTable(output, node_data, nodes_table)
  })
  
  observeEvent(df(), {
    current_df <- df()
    updateLabelChoices(session, current_df)
    updateProteinChoices(session, current_df)
  })
}