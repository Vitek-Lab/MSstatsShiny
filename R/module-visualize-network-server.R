networkServer <- function(input, output, session, parent_session, significant) {

    # Reactive for uploaded CSV data
    df <- reactive({
        if (is.null(input$dataUpload) && !is.null(significant())) {
            df <- significant()
            if (!is.null(df) && "Protein" %in% names(df)) {
                df$Protein <- as.character(significant()$Protein)
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
    })

    # Reactive values for input fields with default values
    proteinIdType <- reactive({
        req(input$proteinIdType)
        input$proteinIdType
    })

    pValue <- reactive({
        req(input$pValue)
        as.numeric(input$pValue)
    })

    logFC <- reactive({
        req(input$logFC)
        as.numeric(input$logFC)
    })

    # Create a reactive expression to generate the JavaScript code for Cytoscape
    renderNetwork <- reactive({
        # Annotate protein info and filter the subnetwork based on p-value

        annotated_df <- annotateProteinInfoFromIndra(df(), proteinIdType())
        subnetwork <- getSubnetworkFromIndra(annotated_df, pValue())

        # Create the JavaScript code to initialize Cytoscape.js
        node_elements <- apply(subnetwork$nodes, 1, function(row) {
            paste0("{ data: { id: '", row['id'], "', label: '", row['id'], "' } }")
        })

        edge_elements <- list()

        for (i in 1:nrow(subnetwork$edges)) {
            row <- subnetwork$edges[i,]
            edge_key <- paste(row['source'], row['target'], row['interaction'], sep = "-")
            # Add edge with unique ID based on interaction type
            edge_elements[[edge_key]] <- paste0("{ data: { source: '", row['source'], "', target: '", row['target'], 
                                                 "', id: '", edge_key, "', interaction: '", row['interaction'], "' } }")
        }

        # Combine the node and edge elements into a single list
        elements <- c(node_elements, edge_elements)

        # Generate the Cytoscape.js initialization code
        js_code <- paste0("
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
                        'text-margin-y': -10,  // Adjust the vertical distance of edge labels
                        'text-halign': 'center',  // Horizontally align labels to the center
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
        ")
        # Return the JavaScript code to be executed
        return(list(js_code = js_code,
            edges_table = subnetwork$edges,
            nodes_table = subnetwork$nodes
        ))
    })

    # Observe the button click to trigger network rendering and table display
    observeEvent(input$showNetwork, {
        # Ensure that all required inputs are valid before rendering the network
        req(df(), proteinIdType(), pValue(), logFC())
        
        # When the "Display Network" button is clicked, run the network rendering logic
        render_data <- renderNetwork()
        js_code <- render_data$js_code
        edges_table <- render_data$edges_table
        nodes_table <- render_data$nodes_table
        
        # Send the JavaScript code to the frontend to render Cytoscape.js
        session$sendCustomMessage(type = 'runCytoscape', message = js_code)

        output$nodesTable <- renderDT({
            datatable(nodes_table, options = list(pageLength = 10, searchable = TRUE))
        })
        
        # Render the table of edges
        output$edgesTable <- renderDT({
            datatable(edges_table, options = list(pageLength = 10, searchable = TRUE))
        })
    })
}
