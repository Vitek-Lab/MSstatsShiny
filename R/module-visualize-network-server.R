
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
  if (nrow(edges) == 0) return(list())
  
  # First consolidate edges
  consolidated_edges <- consolidateEdges(edges)
  
  edge_elements <- list()
  
  for (i in 1:nrow(consolidated_edges)) {
    row <- consolidated_edges[i,]
    edge_key <- paste(row$source, row$target, row$interaction, sep = "-")
    
    # Get styling for this edge
    style <- getEdgeStyle(row$interaction, row$category, row$edge_type)
    
    # Create edge data with styling information
    edge_data <- paste0("{ data: { source: '", row$source, 
                        "', target: '", row$target, 
                        "', id: '", edge_key,
                        "', interaction: '", row$interaction,
                        "', edge_type: '", row$edge_type,
                        "', category: '", row$category,
                        "', color: '", style$color,
                        "', line_style: '", style$style,
                        "', arrow_shape: '", style$arrow,
                        "', width: ", style$width, " } }")
    
    edge_elements[[edge_key]] <- edge_data
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
  default_max <- 2
  max_logFC <- max(c(abs(logFC_values), default_max))
  min_logFC <- -1 * max_logFC
  
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

# Define relationship categories and their properties
getRelationshipProperties <- function() {
  list(
    complex = list(
      types = c("Complex"),
      color = "#8B4513",        # Brown
      style = "solid",
      arrow = "none",           # Undirected
      width = 4,
      consolidate = "undirected"
    ),
    regulatory = list(
      types = c("Inhibit", "Activate", "IncreaseAmount", "DecreaseAmount"),
      colors = list(
        "Inhibit" = "#FF4444",           # Red
        "Activate" = "#44AA44",          # Green  
        "IncreaseAmount" = "#4488FF",    # Blue
        "DecreaseAmount" = "#FF8844"     # Orange
      ),
      style = "solid",
      arrow = "triangle",
      width = 3,
      consolidate = "bidirectional"
    ),
    ptm = list(
      types = c("Phosphorylation"),
      color = "#9932CC",        # Purple
      style = "dashed",
      arrow = "triangle",
      width = 2,
      consolidate = "directed"
    ),
    other = list(
      color = "#666666",        # Gray
      style = "dotted",
      arrow = "triangle", 
      width = 2,
      consolidate = "directed"
    )
  )
}

# Consolidate bidirectional edges based on relationship type
consolidateEdges <- function(edges) {
  if (nrow(edges) == 0) return(edges)
  
  relationship_props <- getRelationshipProperties()
  consolidated_edges <- list()
  processed_pairs <- c()
  
  for (i in 1:nrow(edges)) {
    edge <- edges[i, ]
    pair_key <- paste(sort(c(edge$source, edge$target)), edge$interaction, collapse = "-")
    reverse_key <- paste(sort(c(edge$source, edge$target), decreasing = TRUE), edge$interaction, sep = "-")
    
    # Skip if we've already processed this pair
    if (pair_key %in% processed_pairs) next
    
    # Determine relationship category
    interaction_type <- edge$interaction
    category <- "other"
    for (cat_name in names(relationship_props)) {
      if (interaction_type %in% relationship_props[[cat_name]]$types) {
        category <- cat_name
        break
      }
    }
    
    # Find reverse edge if it exists
    reverse_edges <- edges[edges$source == edge$target & 
                             edges$target == edge$source & 
                             edges$interaction == edge$interaction, ]
    
    consolidation_type <- relationship_props[[category]]$consolidate
    
    if (nrow(reverse_edges) > 0 && consolidation_type %in% c("undirected", "bidirectional")) {
      # Create consolidated edge
      if (consolidation_type == "undirected") {
        # For complex relationships - create undirected edge
        consolidated_edge <- data.frame(
          source = edge$source,
          target = edge$target,
          interaction = edge$interaction,
          edge_type = "undirected",
          category = category,
          stringsAsFactors = FALSE
        )
      } else {
        # For regulatory relationships - create bidirectional edge
        consolidated_edge <- data.frame(
          source = edge$source,
          target = edge$target,
          interaction = paste(edge$interaction, "(bidirectional)"),
          edge_type = "bidirectional", 
          category = category,
          stringsAsFactors = FALSE
        )
      }
      
      # Copy any additional columns from original edge
      other_cols <- setdiff(names(edge), c("source", "target", "interaction"))
      for (col in other_cols) {
        consolidated_edge[[col]] <- edge[[col]]
      }
      
      edge_key <- paste(edge$source, edge$target, consolidated_edge$interaction, sep = "-")
      consolidated_edges[[edge_key]] <- consolidated_edge
      
      # Mark both directions as processed
      processed_pairs <- c(processed_pairs, pair_key)
      
    } else {
      # Keep as directed edge
      directed_edge <- edge
      directed_edge$edge_type <- "directed"
      directed_edge$category <- category
      
      edge_key <- paste(edge$source, edge$target, edge$interaction, sep = "-")
      consolidated_edges[[edge_key]] <- directed_edge
    }
  }
  
  # Convert list back to data frame
  if (length(consolidated_edges) > 0) {
    result <- do.call(rbind, consolidated_edges)
    rownames(result) <- NULL
    return(result)
  } else {
    return(edges[0, ])  # Return empty data frame with same structure
  }
}

# Get edge styling properties based on category and interaction type
getEdgeStyle <- function(interaction, category, edge_type) {
  relationship_props <- getRelationshipProperties()
  
  if (category %in% names(relationship_props)) {
    props <- relationship_props[[category]]
    
    # Handle regulatory relationships with specific colors
    if (category == "regulatory" && "colors" %in% names(props)) {
      base_interaction <- gsub(" \\(bidirectional\\)", "", interaction)
      color <- if (base_interaction %in% names(props$colors)) {
        props$colors[[base_interaction]]
      } else {
        "#666666"  # Default gray
      }
    } else {
      color <- props$color
    }
    
    # Adjust arrow type based on edge type
    arrow <- if (edge_type == "undirected") {
      "none"
    } else if (edge_type == "bidirectional") {
      "triangle"  # Will be handled specially in CSS
    } else {
      props$arrow
    }
    
    return(list(
      color = color,
      style = props$style,
      arrow = arrow,
      width = props$width
    ))
  } else {
    # Default styling for unknown relationships
    return(relationship_props$other)
  }
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
                        var label = ele.data('label') || '';
                        var labelLength = label.length;
                        return Math.max(60, Math.min(labelLength * 8 + 20, 150));
                    },
                    'height': function(ele) {
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
                    'width': 'data(width)',
                    'line-color': 'data(color)',
                    'line-style': 'data(line_style)',
                    'label': 'data(interaction)',
                    'curve-style': 'bezier',
                    'target-arrow-shape': 'data(arrow_shape)',
                    'target-arrow-color': 'data(color)',
                    'source-arrow-shape': function(ele) {
                        return ele.data('edge_type') === 'bidirectional' ? 'triangle' : 'none';
                    },
                    'source-arrow-color': 'data(color)',
                    'edge-text-rotation': 'autorotate',
                    'text-margin-y': -12,
                    'text-halign': 'center',
                    'font-size': '9px',
                    'font-weight': 'bold',
                    'color': 'data(color)',
                    'text-background-color': '#ffffff',
                    'text-background-opacity': 0.8,
                    'text-background-padding': '2px'
                }
            },
            // Special styling for different edge categories
            {
                selector: 'edge[category = \"complex\"]',
                style: {
                    'line-style': 'solid',
                    'target-arrow-shape': 'none',
                    'source-arrow-shape': 'none'
                }
            },
            {
                selector: 'edge[category = \"ptm\"]',
                style: {
                    'line-style': 'dashed',
                    'width': 2
                }
            },
            {
                selector: 'edge[edge_type = \"bidirectional\"]',
                style: {
                    'source-arrow-shape': 'triangle',
                    'target-arrow-shape': 'triangle'
                }
            }
        ],
        layout: {
            name: 'dagre',
            rankDir: 'TB',
            animate: true,
            fit: true,
            padding: 30,
            spacingFactor: 1.5,
            // Adjust layout parameters for better edge visibility
            nodeSep: 50,
            edgeSep: 20,
            rankSep: 80
        }
    });
    
    // Add legend for edge types
    var legend = document.getElementById('network-legend');
    if (!legend) {
        legend = document.createElement('div');
        legend.id = 'network-legend';
        legend.style.cssText = `
            position: absolute;
            top: 10px;
            right: 10px;
            background: rgba(255,255,255,0.9);
            border: 1px solid #ccc;
            border-radius: 5px;
            padding: 10px;
            font-size: 12px;
            font-family: Arial, sans-serif;
            z-index: 1000;
            max-width: 200px;
        `;
        legend.innerHTML = `
            <div style='font-weight: bold; margin-bottom: 8px;'>Edge Types</div>
            <div style='margin: 3px 0;'><span style='color: #8B4513; font-weight: bold;'>━━</span> Complex</div>
            <div style='margin: 3px 0;'><span style='color: #44AA44; font-weight: bold;'>━▶</span> Activate</div>
            <div style='margin: 3px 0;'><span style='color: #FF4444; font-weight: bold;'>━▶</span> Inhibit</div>
            <div style='margin: 3px 0;'><span style='color: #4488FF; font-weight: bold;'>━▶</span> Increase Amount</div>
            <div style='margin: 3px 0;'><span style='color: #FF8844; font-weight: bold;'>━▶</span> Decrease Amount</div>
            <div style='margin: 3px 0;'><span style='color: #9932CC; font-weight: bold;'>┅▶</span> PTM</div>
            <div style='margin: 3px 0;'><span style='color: #666666; font-weight: bold;'>┄▶</span> Other</div>
            <div style='margin-top: 8px; font-size: 10px; color: #666;'>
                ◀━▶ = Bidirectional<br/>
                ━━ = Undirected
            </div>
        `;
        document.getElementById('network-cy').appendChild(legend);
    }
    
    // Capture the event when an edge is clicked
    cy.on('tap', 'edge', function(evt) {
        var edge = evt.target;
        const edgeId = edge.id();
        Shiny.setInputValue('network-edgeClicked', { 
            source: edge.data('source'),
            target: edge.data('target'),
            interaction: edge.data('interaction'),
            edge_type: edge.data('edge_type'),
            category: edge.data('category')
        });
    });
    
    // Add double-click to fit view
    cy.on('dblclick', function(evt) {
        if (evt.target === cy) {
            cy.fit();
        }
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
  
  # Show consolidated edges in the table
  consolidated_edges <- consolidateEdges(edges_table)
  
  output$edgesTable <- renderDT({
    datatable(consolidated_edges, 
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
  
  # Work with consolidated edges
  consolidated_edges <- consolidateEdges(edges_table)
  
  # Find matching row
  row_index <- which(consolidated_edges$source == source & 
                       consolidated_edges$target == target & 
                       consolidated_edges$interaction == interaction)
  
  if (length(row_index) > 0) {
    # Bring the highlighted row to the top
    reordered_table <- consolidated_edges[c(row_index, setdiff(1:nrow(consolidated_edges), row_index)), ]
    
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
    updateProteinChoices(session, current_df)
    
  })
}