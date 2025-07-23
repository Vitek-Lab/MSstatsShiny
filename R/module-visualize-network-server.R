
# =============================================================================
# HELPER FUNCTIONS - Data Management
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
                      choices = c(setNames(unique_labels, unique_labels)))
  } else {
    # If no Label column exists, disable the dropdown
    updateSelectInput(session, "selectedLabel",
                      choices = c("No Label column found" = "none"),
                      selected = "none")
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
  
  list(
    proteinIdType = req(input$proteinIdType),
    pValue = as.numeric(req(input$pValue)),
    evidence = as.numeric(req(input$evidence)),
    statementTypes = statementTypes,
    sources = sources,
    selectedLabel = req(input$selectedLabel)
  )
}


# =============================================================================
# HELPER FUNCTIONS - Data Processing
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

extractSubnetwork <- function(annotated_df, pValue, evidence, statementTypes, sources) {
  tryCatch({
    getSubnetworkFromIndra(annotated_df, 
                           pvalueCutoff = pValue, 
                           evidence_count_cutoff = evidence,
                           statement_types = statementTypes,
                           sources_filter = sources)
  }, error = function(e) {
    showNotification(paste("Error in subnetwork extraction:", e$message), type = "error")
    print(e$message)
    return(NULL)
  })
}

# =============================================================================
# HELPER FUNCTIONS - Cytoscape Visualization
# =============================================================================

createNodeElements <- function(nodes, displayLabelType = "id") {
  # Map logFC to colors if logFC column exists
  if ("logFC" %in% names(nodes)) {
    node_colors <- mapLogFCToColor(nodes$logFC)
  } else {
    node_colors <- rep("#D3D3D3", nrow(nodes))  # Default color
  }
  
  # Determine which column to use for labels
  label_column <- if(displayLabelType == "hgncName" && "hgncName" %in% names(nodes)) {
    "hgncName"
  } else {
    "id"
  }
  
  apply(cbind(nodes, color = node_colors), 1, function(row) {
    # Use the appropriate label, fallback to id if hgncName is missing/empty
    display_label <- if(label_column == "hgncName" && !is.na(row['hgncName']) && row['hgncName'] != "") {
      row['hgncName']
    } else {
      row['id']
    }
    
    paste0("{ data: { id: '", row['id'], "', label: '", display_label, "', color: '", row['color'], "' } }")
  })
}

createEdgeElements <- function(edges) {
  edge_elements <- list()
  
  for (i in 1:nrow(edges)) {
    row <- edges[i,]
    edge_key <- paste(row['source'], row['target'], row['interaction'], sep = "-")
    edge_elements[[edge_key]] <- paste0("{ data: { source: '", row['source'], 
                                        "', target: '", row['target'], 
                                        "', id: '", edge_key, 
                                        "', interaction: '", row['interaction'], "' } }")
  }
  
  return(edge_elements)
}

# Helper function to map logFC values to colors
mapLogFCToColor <- function(logFC_values) {
  # Define the color palette
  colors <- c("#ADD8E6", "#ADD8E6", "#D3D3D3", "#FFA590", "#FFA590")
  
  # Handle case where all values are the same or missing
  if (all(is.na(logFC_values)) || length(unique(logFC_values[!is.na(logFC_values)])) <= 1) {
    return(rep("#D3D3D3", length(logFC_values)))
  }
  
  # Get range of logFC values
  min_logFC <- min(logFC_values, na.rm = TRUE)
  max_logFC <- max(logFC_values, na.rm = TRUE)
  
  # Create color mapping function
  color_map <- colorRamp(colors)
  
  # Normalize logFC values to [0, 1] range
  normalized_values <- (logFC_values - min_logFC) / (max_logFC - min_logFC)
  
  # Handle NA values
  normalized_values[is.na(normalized_values)] <- 0.5  # Default to middle color
  
  # Get RGB colors and convert to hex
  rgb_colors <- color_map(normalized_values)
  hex_colors <- rgb(rgb_colors[,1], rgb_colors[,2], rgb_colors[,3], maxColorValue = 255)
  
  return(hex_colors)
}

generateCytoscapeJS <- function(node_elements, edge_elements) {
  elements <- c(node_elements, edge_elements)
  
  paste0("
    cytoscape.use(cytoscapeDagre);
    var cy = cytoscape({
        container: document.getElementById('network-cy'),
        elements: [", paste(elements, collapse = ", "), "],
        style: [
            {
                selector: 'node',
                style: {
                    'background-color': 'data(color)',
                    'label': 'data(label)',
                    'width': function(ele) {
                        // Calculate width based on label length, with minimum and maximum sizes
                        var label = ele.data('label') || '';
                        var labelLength = label.length;
                        return Math.max(60, Math.min(labelLength * 8 + 20, 150));
                    },
                    'height': function(ele) {
                        // Calculate height based on label length, with minimum size
                        var label = ele.data('label') || '';
                        var labelLength = label.length;
                        return Math.max(40, Math.min(labelLength * 2 + 30, 60));
                    },
                    'shape': 'round-rectangle',
                    'font-size': '11px',
                    'font-weight': 'bold',
                    'color': '#000',
                    'text-valign': 'center',
                    'text-halign': 'center',
                    'text-wrap': 'wrap',
                    'text-max-width': function(ele) {
                        // Ensure text doesn't exceed node width
                        var label = ele.data('label') || '';
                        var labelLength = label.length;
                        return Math.max(50, Math.min(labelLength * 8 + 10, 140));
                    },
                    'border-width': 2,
                    'border-color': '#333',
                    'padding': '5px'
                }
            },
            {
                selector: 'edge',
                style: {
                    'width': 3,
                    'line-color': '#ccc',
                    'label': 'data(interaction)',
                    'curve-style': 'bezier',
                    'target-arrow-shape': 'triangle',
                    'target-arrow-color': '#234',
                    'edge-offset': 10,
                    'text-margin-y': -10,
                    'text-halign': 'center',
                    'edge-text-rotation': 'autorotate',
                    'font-size': '10px'
                }
            }
        ],
        layout: {
            name: 'dagre',
            rankDir: 'TB',
            animate: true,
            fit: true,
            padding: 30,
            spacingFactor: 1.25
        }
    });
    
    // Capture the event when an edge is clicked
    cy.on('tap', 'edge', function(evt) {
        var edge = evt.target;
        const edgeId = edge.id();
        Shiny.setInputValue('network-edgeClicked', { 
            source: edge.data('source'),
            target: edge.data('target'),
            interaction: edge.data('interaction')
        });
    });
    ")
}

# =============================================================================
# HELPER FUNCTIONS - UI Rendering
# =============================================================================

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

highlightEdgeInTable <- function(output, edge_data, edges_table) {
  source <- edge_data$source
  target <- edge_data$target
  interaction <- edge_data$interaction
  
  # Find matching row
  row_index <- which(edges_table$source == source & 
                       edges_table$target == target & 
                       edges_table$interaction == interaction)
  
  if (length(row_index) > 0) {
    # Bring the highlighted row to the top
    reordered_table <- edges_table[c(row_index, setdiff(1:nrow(edges_table), row_index)), ]
    
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

# =============================================================================
# MAIN SERVER FUNCTION
# =============================================================================

#' @importFrom MSstatsBioNet annotateProteinInfoFromIndra getSubnetworkFromIndra
#' @importFrom DT renderDT datatable
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
                                    params$statementTypes, params$sources)
    if (is.null(subnetwork)) return(NULL)
    
    return(list(
      nodes_table = subnetwork$nodes,
      edges_table = subnetwork$edges
    ))
  })
  
  networkVisualization <- reactive({
    network_data <- renderNetwork()
    if (is.null(network_data)) return(NULL)
    
    # Create Cytoscape elements with current display label setting
    node_elements <- createNodeElements(network_data$nodes_table, input$displayLabelType)
    edge_elements <- createEdgeElements(network_data$edges_table)
    
    # Generate JavaScript code
    js_code <- generateCytoscapeJS(node_elements, edge_elements)
    
    return(list(
      js_code = js_code,
      edges_table = network_data$edges_table,
      nodes_table = network_data$nodes_table
    ))
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
  })
  
  # Observe edge click events
  observeEvent(input$edgeClicked, {
    edge_data <- input$edgeClicked
    edges_table <- renderNetwork()$edges_table
    
    highlightEdgeInTable(output, edge_data, edges_table)
  })
  
  observeEvent(df(), {
    current_df <- df()
    updateLabelChoices(session, current_df)
  })
}