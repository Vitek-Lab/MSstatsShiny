
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
    sources = sources
  )
}


# =============================================================================
# HELPER FUNCTIONS - Data Processing
# =============================================================================

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

createNodeElements <- function(nodes) {
  apply(nodes, 1, function(row) {
    paste0("{ data: { id: '", row['id'], "', label: '", row['id'], "' } }")
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
                    'background-color': '#66ccff',
                    'label': 'data(label)'
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
                    'edge-text-rotation': 'autorotate'
                }
            }
        ],
        layout: {
            name: 'dagre',
            rankDir: 'TB',
            animate: true,
            fit: true
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
  
  # Main reactive expressions
  df <- reactive({
    loadCsvData(input, dataComparison)
  })
  
  # Create a reactive expression to generate the network data
  renderNetwork <- reactive({
    params <- getInputParameters(input)
    
    # Annotate protein info and filter the subnetwork
    annotated_df <- annotateProteinData(df(), params$proteinIdType)
    if (is.null(annotated_df)) return(NULL)
    
    subnetwork <- extractSubnetwork(annotated_df, params$pValue, params$evidence, 
                                    params$statementTypes, params$sources)
    if (is.null(subnetwork)) return(NULL)
    
    # Create Cytoscape elements
    node_elements <- createNodeElements(subnetwork$nodes)
    edge_elements <- createEdgeElements(subnetwork$edges)
    
    # Generate JavaScript code
    js_code <- generateCytoscapeJS(node_elements, edge_elements)
    
    return(list(
      js_code = js_code,
      edges_table = subnetwork$edges,
      nodes_table = subnetwork$nodes
    ))
  })
  
  # Event observers
  observeEvent(input$showNetwork, {
    req(df(), getInputParameters(input))
    
    # Show loading indicator
    shinyjs::show("loadingIndicator")
    
    # Disable the button during processing
    shinyjs::disable("showNetwork")
    
    render_data <- renderNetwork()
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
}