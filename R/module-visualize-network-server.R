renderDataTables <- function(output, nodes_table, edges_table) {
  nodes_table <- as.data.frame(lapply(nodes_table, function(x) {
    if (is.numeric(x) && any(is.infinite(x))) as.character(x) else x
  }))
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
  nodes_table <- as.data.frame(lapply(nodes_table, function(x) {
    if (is.numeric(x) && any(is.infinite(x))) as.character(x) else x
  }))
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
  interaction <- edge_data$interaction
  edge_type <- edge_data$edge_type
  
  # Find matching rows
  if (edge_type == "undirected") {
    
    # For undirected edges, match source and target regardless of order
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

# Returns the comparison data frame from the reactive, handling both PTM
# (ADJUSTED.Model) and non-PTM (ComparisonResult) return structures.
extractComparisonResult <- function(dataComparison) {
  dc <- dataComparison()
  if (!is.null(dc$ADJUSTED.Model)) dc$ADJUSTED.Model else dc$ComparisonResult
}

loadCsvData <- function(input, dataComparison) {
  if (is.null(input$dataUpload) && !is.null(extractComparisonResult(dataComparison))) {
    df <- extractComparisonResult(dataComparison)
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

getInputParameters <- function(input, selectedProteins) {
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
  
  selectedProteins <- if(is.null(selectedProteins) || length(selectedProteins) == 0) {
    NULL
  } else {
    selectedProteins
  }
  
  filterByCuration <- if(is.null(input$filterByCuration)) {
    FALSE  # Default to FALSE if somehow NULL 
  } else {
    as.logical(input$filterByCuration)
  }
  
  list(
    proteinIdType = req(input$proteinIdType),
    pValue = as.numeric(req(input$pValue)),
    evidence = as.numeric(req(input$evidence)),
    absLogFC = as.numeric(req(input$absLogFC)),
    statementTypes = statementTypes,
    sources = sources,
    selectedLabel = req(input$selectedLabel),
    selectedProteins = selectedProteins,
    filterByCuration = filterByCuration,
    filter_by_ptm_site = input$filter_by_ptm_site,
    include_infinite_fc = input$include_infinite_fc,
    direction = input$direction
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
                              sources, absLogFC, selectedProteins, filterByCuration,
                              filter_by_ptm_site, include_infinite_fc, direction) {
  tryCatch({
    getSubnetworkFromIndra(annotated_df, 
                           pvalueCutoff = pValue, 
                           evidence_count_cutoff = evidence,
                           statement_types = statementTypes,
                           sources_filter = sources,
                           logfc_cutoff = absLogFC,
                           force_include_other = selectedProteins,
                           filter_by_curation = filterByCuration,
                           filter_by_ptm_site = filter_by_ptm_site,
                           include_infinite_fc = include_infinite_fc,
                           direction = direction
                           )
  }, error = function(e) {
    showNotification(paste("Error in subnetwork extraction:", e$message), type = "error")
    print(e$message)
    return(NULL)
  })
}

export_network_html <- function(render_data, displayLabelType, file) {
  if (is.null(render_data)) {
    stop("No network to export. Please ensure network is displayed first.")
  }
  
  tmp_file <- file.path(tempdir(), paste0("network-", Sys.Date(), ".html"))
  
  exportNetworkToHTML(
    nodes            = render_data$nodes_table,
    edges            = render_data$edges_table,
    nodeFontSize     = 12,
    displayLabelType = displayLabelType,
    filename         = tmp_file
  )
  
  copied <- file.copy(tmp_file, file, overwrite = TRUE)
  if (!copied) {
    stop("Failed to prepare HTML download file.")
  }
}

# =============================================================================
# MAIN SERVER FUNCTION - Updated to use decoupled architecture
# =============================================================================

#' Server logic for network visualization module
#'
#' @param id Module ID string
#' @param parent_session Parent Shiny session
#' @param dataComparison Reactive expression containing comparison data
#'
#' @return None (side effects only)
#' 
#' @importFrom MSstatsBioNet annotateProteinInfoFromIndra getSubnetworkFromIndra
#' @importFrom DT renderDT datatable
#' @importFrom shiny moduleServer updateSelectizeInput showNotification outputOptions
#' @importFrom httr POST content_type_json accept_json content status_code
visualizeNetworkServer <- function(id, parent_session, dataComparison) {
  moduleServer(id, function(input, output, session) {
  
  # Output to control conditional panels
  output$hasValidDataComparison <- reactive({
    !is.null(dataComparison()) &&
      !is.null(extractComparisonResult(dataComparison)) &&
      !is.null(extractComparisonResult(dataComparison)$Protein)
  })
  outputOptions(output, "hasValidDataComparison", suspendWhenHidden = FALSE)
  
  # Reactive value to store selected proteins
  selectedProteinsReactive <- reactiveVal(character(0))

  # Reactive value to store search results
  proteinSearchResults <- reactiveVal(NULL)

  # Reactive value to accumulate edges deleted interactively in the visualization
  deletedEdges <- reactiveVal(list())

  # Reactive value tracking the live edges table (updated as edges are deleted)
  currentEdgesTable <- reactiveVal(NULL)
  
  # Render selected proteins as tags
  output$selectedProteinsTags <- renderUI({
    proteins <- selectedProteinsReactive()
    if (length(proteins) == 0) {
      return(div(style = "color: #999; font-style: italic;", "No proteins selected"))
    }
    
    ns <- session$ns
    tagList(
      lapply(seq_along(proteins), function(i) {
        protein <- proteins[i]
        tags$span(
          style = "display: inline-block; background-color: #337ab7; color: white; 
                   padding: 5px 10px; margin: 2px; border-radius: 3px;",
          protein,
          tags$span(
            style = "margin-left: 8px; cursor: pointer; font-weight: bold;",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", 
                              ns("removeProtein"), i),
            "x"
          )
        )
      })
    )
  })
  
  # Handle protein search
  observeEvent(input$proteinSearchButton, {
    req(input$proteinSearchInput)
    search_text <- trimws(input$proteinSearchInput)
    
    if (nchar(search_text) == 0) {
      proteinSearchResults(NULL)
      return()
    }
    
    # Show loading notification
    showNotification("Searching for protein...", type = "message", duration = 2)
    
    # Call INDRA grounding API
    tryCatch({
      response <- httr::POST(
        url = "https://grounding.indra.bio/ground",
        body = list(
          text = search_text,
          context = "",
          organisms = list("9606")
        ),
        encode = "json",
        httr::content_type_json(),
        httr::accept_json()
      )
      
      if (httr::status_code(response) == 200) {
        results <- httr::content(response, as = "parsed")
        
        if (length(results) > 0) {
          # Extract relevant information
          formatted_results <- lapply(results, function(r) {
            db <- r$term$db
            id <- r$term$id
            
            # Check if ID already starts with the database prefix
            # e.g., if db is "CHEBI" and id is "CHEBI:4911"
            if (grepl(paste0("^", db, ":"), id, ignore.case = TRUE)) {
              # ID already contains the prefix, use as-is
              full_id <- id
            } else {
              # Concatenate db and id
              full_id <- paste0(db, ":", id)
            }
            
            list(
              display = sprintf("%s (%s)", 
                                r$term$text,
                                full_id),
              text = r$term$text,
              db = db,
              id = id,
              full_id = full_id,  # Store the properly formatted ID
              score = r$score
            )
          })
          
          # Sort by score (descending)
          formatted_results <- formatted_results[order(sapply(formatted_results, function(x) x$score), 
                                                       decreasing = TRUE)]
          
          proteinSearchResults(formatted_results)
          showNotification(sprintf("Found %d result(s)", length(formatted_results)), 
                           type = "message", duration = 2)
        } else {
          proteinSearchResults(NULL)
          showNotification("No results found", type = "warning", duration = 3)
        }
      } else {
        proteinSearchResults(NULL)
        showNotification("Error searching protein database", type = "error", duration = 3)
      }
    }, error = function(e) {
      proteinSearchResults(NULL)
      showNotification(paste("Search error:", e$message), type = "error", duration = 3)
    })
  })
  
  observeEvent(input$toggle_adv, {
    ns <- session$ns
    toggle(id = "adv_panel", anim = TRUE)
  })
  
  # Render search results
  output$proteinSearchResults <- renderUI({
    results <- proteinSearchResults()
    
    if (is.null(results)) {
      return(NULL)
    }
    
    ns <- session$ns
    
    div(
      style = "margin-top: 10px; border: 1px solid #ddd; border-radius: 4px; 
               max-height: 300px; overflow-y: auto; background-color: white;",
      lapply(seq_along(results), function(i) {
        result <- results[[i]]
        div(
          style = "padding: 10px; border-bottom: 1px solid #eee; cursor: pointer;
                   transition: background-color 0.2s;",
          onmouseover = "this.style.backgroundColor='#f5f5f5'",
          onmouseout = "this.style.backgroundColor='white'",
          onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                            ns("selectProteinResult"), result$display),
          tags$strong(result$display),
          tags$br(),
          tags$small(
            style = "color: #666;",
            sprintf("Score: %.2f | Source: %s", result$score, result$db)
          )
        )
      })
    )
  })
  
  # Handle protein selection from results
  observeEvent(input$selectProteinResult, {
    selected <- input$selectProteinResult
    current <- selectedProteinsReactive()
    
    # Extract the identifier (e.g., "hgnc:1925" from "CHEK1 (hgnc:1925)")
    identifier <- sub(".*\\((.*)\\).*", "\\1", selected)
    
    # Check if already selected
    if (!identifier %in% current) {
      selectedProteinsReactive(c(current, identifier))
      showNotification(sprintf("Added: %s", selected), type = "message", duration = 2)
    } else {
      showNotification("Protein already selected", type = "warning", duration = 2)
    }
    
    # Clear search
    updateTextInput(session, "proteinSearchInput", value = "")
    proteinSearchResults(NULL)
  })
  
  # Handle protein removal
  observeEvent(input$removeProtein, {
    index <- input$removeProtein
    current <- selectedProteinsReactive()
    if (index > 0 && index <= length(current)) {
      selectedProteinsReactive(current[-index])
      showNotification("Protein removed", type = "message", duration = 2)
    }
  })
  
  # Main reactive expressions
  df <- reactive({
    loadCsvData(input, dataComparison)
  })
  
  # Create a reactive expression to generate the network data
  renderNetwork <- reactive({
    params <- getInputParameters(input, selectedProteinsReactive())
    
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
                                    params$absLogFC, params$selectedProteins,
                                    params$filterByCuration, params$filter_by_ptm_site, 
                                    params$include_infinite_fc, params$direction)
    if (is.null(subnetwork)) return(NULL)
    
    return(list(
      nodes_table = subnetwork$nodes,
      edges_table = subnetwork$edges
    ))
  })
  
  networkVisualization <- reactive({
    network_data <- renderNetwork()
    if (is.null(network_data)) return(NULL)
    return(list(
      edges_table = network_data$edges_table,
      nodes_table = network_data$nodes_table
    ))
  })
  
  generate_network_code <- eventReactive(input$showNetwork, {
    params <- getInputParameters(input, selectedProteinsReactive())
    
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
      codes <- paste(codes, ",\n  force_include_other = ", selected_proteins_str, "\n", sep = "")
    } else {
      codes <- paste(codes, ",\n  force_include_other = NULL\n", sep = "")
    }
    
    codes <- paste(codes, ",\n  filter_by_curation = ", params$filterByCuration, "\n", sep = "")
    codes <- paste(codes, ",\n  filter_by_ptm_site = ", params$filter_by_ptm_site, "\n", sep = "")
    codes <- paste(codes, ",\n  include_infinite_fc = ", params$include_infinite_fc, "\n", sep = "")
    codes <- paste(codes, ",\n  direction = \"", params$direction, "\"\n", sep = "")
    
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
    codes <- paste(codes, "cytoscapeNetwork(subnetwork$nodes, subnetwork$edges, displayLabelType=", displayLabelTypeStr, ")\n", sep = "")
    codes <- paste(codes, "exportNetworkToHTML(subnetwork$nodes, subnetwork$edges, displayLabelType=", displayLabelTypeStr, ")\n", sep = "")
    
    return(codes)
  })
  
  # Event observers
  observeEvent(input$showNetwork, {
    req(df(), getInputParameters(input, selectedProteinsReactive()))

    # Reset deleted edges whenever a fresh network is rendered
    deletedEdges(list())

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
    
    output$network <- MSstatsBioNet::renderCytoscapeNetwork({
      MSstatsBioNet::cytoscapeNetwork(
        nodes        = render_data$nodes_table,
        edges        = render_data$edges_table,
        nodeFontSize = 12,
        displayLabelType = input$displayLabelType
      )
    })
    
    # Render data tables and seed the live edges state
    renderDataTables(output, render_data$nodes_table, render_data$edges_table)
    currentEdgesTable(render_data$edges_table)
    
    # Hide loading indicator and re-enable button when done
    shinyjs::hide("loadingIndicator")
    shinyjs::enable("showNetwork")
    
    output$network.code.button <- renderUI({
      ns <- session$ns
      downloadButton(ns("network_download_code"), "Download analysis code", icon("download"),
                     style="color: #000000; background-color: #75ba82; border-color: #000000")
    })
    
    output$network.html.button <- renderUI({
      ns <- session$ns
      downloadButton(ns("network_html_code"), "Download HTML", icon("download"),
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

        deleted <- deletedEdges()
        if (length(deleted) > 0) {
          deletion_lines <- vapply(deleted, function(e) {
            sprintf(
              'subnetwork$edges <- MSstatsBioNet::deleteEdgeFromNetwork(subnetwork$edges, "%s", "%s", "%s")',
              e$source, e$target, e$interaction
            )
          }, character(1))
          edge_deletion_section <- paste0(
            "\n# Delete edges removed interactively\n",
            paste(deletion_lines, collapse = "\n"),
            "\n"
          )
          code_content <- sub(
            "# Visualize network",
            paste0(edge_deletion_section, "# Visualize network"),
            code_content,
            fixed = TRUE
          )
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
  
  output$network_html_code <- downloadHandler(
    filename = function() {
      paste("network-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      tryCatch({
        render_data <- networkVisualization()
        export_network_html(render_data, input$displayLabelType, file)
      }, error = function(e) {
        showNotification(paste("Error downloading HTML:", e$message), type = "error")
      })
    }
  )
  
  # Observe edge deletion events from the visualization
  observeEvent(input$network_edge_deleted, {
    edge_data <- input$network_edge_deleted

    new_deletions <- list(edge_data)
    updated_edges <- MSstatsBioNet::deleteEdgeFromNetwork(
      currentEdgesTable(),
      edge_data$source,
      edge_data$target,
      edge_data$interaction
    )

    # Complex edges are stored bidirectionally, so also remove the reverse direction
    if (identical(edge_data$interaction, "Complex")) {
      reverse_edge_data <- list(
        source = edge_data$target,
        target = edge_data$source,
        interaction = edge_data$interaction
      )
      new_deletions <- c(new_deletions, list(reverse_edge_data))
      updated_edges <- MSstatsBioNet::deleteEdgeFromNetwork(
        updated_edges,
        edge_data$target,
        edge_data$source,
        edge_data$interaction
      )
    }

    deletedEdges(c(deletedEdges(), new_deletions))
    currentEdgesTable(updated_edges)

    output$edgesTable <- DT::renderDT({
      DT::datatable(
        updated_edges,
        options = list(pageLength = 10, searchable = TRUE,
                       scrollX = TRUE, autoWidth = TRUE),
        selection = "single"
      )
    })
  })

  # Observe edge click events
  observeEvent(input$network_edge_clicked, {
    edge_data <- input$network_edge_clicked
    edges_table <- currentEdgesTable()
    req(edges_table)
    highlightEdgeInTable(output, edge_data, edges_table)
  })
  
  # Observe node click events
  observeEvent(input$network_node_clicked, {
    node_data <- input$network_node_clicked
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
  }) # end moduleServer
}
