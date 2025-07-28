# =============================================================================
# UNIT TESTS FOR NETWORK VISUALIZATION MODULE
# =============================================================================

# Load required libraries
library(testthat)
library(mockery)
library(shiny)
library(DT)

# =============================================================================
# MOCK DATA SETUP
# =============================================================================

# Mock data for testing
create_mock_input_data <- function() {
  data.frame(
    Protein = c("P53_HUMAN", "MDM2_HUMAN", "ATM_HUMAN", "BRCA1_HUMAN"),
    log2FC = c(2.5, -1.8, 1.2, -2.1),
    adj.pvalue = c(0.001, 0.02, 0.03, 0.005),
    Label = rep("Treatment_vs_Control", 4),
    stringsAsFactors = FALSE
  )
}

create_mock_annotated_data <- function() {
  data.frame(
    Protein = c("P53_HUMAN", "MDM2_HUMAN", "ATM_HUMAN", "BRCA1_HUMAN"),
    log2FC = c(2.5, -1.8, 1.2, -2.1),
    adj.pvalue = c(0.001, 0.02, 0.03, 0.005),
    Label = rep("Treatment_vs_Control", 4),
    HgncId = c("101", "102", "103", "104"),
    HgncName = c("TP53", "MDM2", "ATM", "BRCA1"),
    stringsAsFactors = FALSE
  )
}

create_mock_subnetwork_nodes <- function() {
  data.frame(
    id = c("P53_HUMAN", "MDM2_HUMAN", "ATM_HUMAN", "BRCA1_HUMAN"),
    logFC = c(2.5, -1.8, 1.2, -2.1),
    pvalue = c(0.001, 0.02, 0.03, 0.005),
    hgncName = c("TP53", "MDM2", "ATM", "BRCA1"),
    stringsAsFactors = FALSE
  )
}

create_mock_subnetwork_edges <- function() {
  data.frame(
    source = c("TP53", "MDM2", "ATM", "TP53"),
    target = c("MDM2", "TP53", "TP53", "BRCA1"),
    interaction = c("Inhibition", "Activation", "Phosphorylation", "Complex"),
    evidenceCount = c(15, 8, 12, 5),
    evidenceLink = c("link1", "link2", "link3", "link4"),
    source_counts = c("{reach:10, signor:5}", "{reach:5,biopax:3}", "{reach:8,phosphoelm:4}", "{biopax:5}"),
    stringsAsFactors = FALSE
  )
}

create_mock_subnetwork <- function() {
  list(
    nodes = create_mock_subnetwork_nodes(),
    edges = create_mock_subnetwork_edges()
  )
}

# =============================================================================
# TESTS FOR COLOR MAPPING FUNCTION
# =============================================================================

test_that("mapLogFCToColor handles various input scenarios", {
  
  # Test normal case with varied logFC values
  logFC_values <- c(-2, -1, 0, 1, 2)
  colors <- mapLogFCToColor(logFC_values)
  expect_equal(length(colors), 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors))) # Valid hex colors
  
  # Test case with all NA values
  na_values <- c(NA, NA, NA)
  na_colors <- mapLogFCToColor(na_values)
  expect_equal(length(na_colors), 3)
  expect_true(all(na_colors == "#D3D3D3"))
  
  # Test case with all same values
  same_values <- c(1, 1, 1)
  same_colors <- mapLogFCToColor(same_values)
  expect_equal(length(same_colors), 3)
  expect_true(all(same_colors == "#D3D3D3"))
  
  # Test empty input
  empty_colors <- mapLogFCToColor(numeric(0))
  expect_equal(length(empty_colors), 0)
})

# =============================================================================
# TESTS FOR RELATIONSHIP PROPERTIES
# =============================================================================

test_that("getRelationshipProperties returns correct structure", {
  props <- getRelationshipProperties()
  
  expect_type(props, "list")
  expect_true("complex" %in% names(props))
  expect_true("regulatory" %in% names(props))
  expect_true("phosphorylation" %in% names(props))
  expect_true("other" %in% names(props))
  
  # Test complex properties
  complex_props <- props$complex
  expect_true("types" %in% names(complex_props))
  expect_true("Complex" %in% complex_props$types)
  expect_equal(complex_props$consolidate, "undirected")
  
  # Test regulatory properties
  reg_props <- props$regulatory
  expect_true("colors" %in% names(reg_props))
  expect_true("Inhibition" %in% names(reg_props$colors))
  expect_equal(reg_props$consolidate, "bidirectional")
})

# =============================================================================
# TESTS FOR EDGE CONSOLIDATION
# =============================================================================

test_that("consolidateEdges properly consolidates bidirectional relationships", {
  
  # Create test edges with bidirectional regulatory relationships
  test_edges <- data.frame(
    source = c("TP53", "MDM2", "ATM", "BRCA1"),
    target = c("MDM2", "TP53", "TP53", "ATM"),
    interaction = c("Inhibition", "Inhibition", "Phosphorylation", "Complex"),
    stringsAsFactors = FALSE
  )
  
  consolidated <- consolidateEdges(test_edges)
  
  expect_s3_class(consolidated, "data.frame")
  expect_true("edge_type" %in% names(consolidated))
  expect_true("category" %in% names(consolidated))
  
  # Should have fewer edges than original due to consolidation
  expect_lt(nrow(consolidated), nrow(test_edges))
  
  # Check that bidirectional inhibition was consolidated
  inhibition_edges <- consolidated[grepl("Inhibition", consolidated$interaction), ]
  expect_equal(nrow(inhibition_edges), 1)
  expect_equal(inhibition_edges$edge_type, "bidirectional")
})

test_that("consolidateEdges handles empty input", {
  empty_edges <- data.frame(
    source = character(0),
    target = character(0),
    interaction = character(0),
    stringsAsFactors = FALSE
  )
  
  result <- consolidateEdges(empty_edges)
  expect_equal(nrow(result), 0)
})

# =============================================================================
# TESTS FOR EDGE STYLING
# =============================================================================

test_that("getEdgeStyle returns appropriate styling", {
  
  # Test regulatory relationship styling
  style <- getEdgeStyle("Inhibition", "regulatory", "directed")
  expect_type(style, "list")
  expect_true("color" %in% names(style))
  expect_equal(style$color, "#FF4444") # Red for inhibition
  
  # Test complex relationship styling
  complex_style <- getEdgeStyle("Complex", "complex", "undirected")
  expect_equal(complex_style$arrow, "none")
  expect_equal(complex_style$color, "#8B4513")
  
  # Test unknown relationship
  unknown_style <- getEdgeStyle("Unknown", "other", "directed")
  expect_equal(unknown_style$color, "#666666")
})

# =============================================================================
# TESTS FOR NODE ELEMENT CREATION
# =============================================================================

test_that("createNodeElements creates proper node structures", {
  nodes <- create_mock_subnetwork_nodes()
  
  # Test with default label type (id)
  node_elements <- createNodeElements(nodes, "id")
  expect_equal(length(node_elements), nrow(nodes))
  expect_true(all(grepl("data:", node_elements)))
  expect_true(all(grepl("id:", node_elements)))
  expect_true(all(grepl("label:", node_elements)))
  
  # Test with hgncName label type
  node_elements_hgnc <- createNodeElements(nodes, "hgncName")
  expect_equal(length(node_elements_hgnc), nrow(nodes))
  
  # Test nodes without logFC column
  nodes_no_logfc <- nodes[, !names(nodes) %in% "logFC"]
  node_elements_no_logfc <- createNodeElements(nodes_no_logfc, "id")
  expect_equal(length(node_elements_no_logfc), nrow(nodes_no_logfc))
})

# =============================================================================
# TESTS FOR EDGE ELEMENT CREATION
# =============================================================================

test_that("createEdgeElements creates proper edge structures", {
  edges <- create_mock_subnetwork_edges()
  
  edge_elements <- createEdgeElements(edges)
  expect_type(edge_elements, "list")
  expect_gt(length(edge_elements), 0)
  
  # Check that all elements contain required fields
  expect_true(all(sapply(edge_elements, function(x) grepl("source:", x))))
  expect_true(all(sapply(edge_elements, function(x) grepl("target:", x))))
  
  # Test empty edges
  empty_edges <- data.frame(
    source = character(0),
    target = character(0),
    interaction = character(0),
    stringsAsFactors = FALSE
  )
  empty_elements <- createEdgeElements(empty_edges)
  expect_equal(length(empty_elements), 0)
})

# =============================================================================
# TESTS FOR CYTOSCAPE CONFIG GENERATION
# =============================================================================

test_that("generateCytoscapeConfig creates complete configuration", {
  nodes <- create_mock_subnetwork_nodes()
  edges <- create_mock_subnetwork_edges()
  
  node_elements <- createNodeElements(nodes, "id")
  edge_elements <- createEdgeElements(edges)
  
  config <- generateCytoscapeConfig(node_elements, edge_elements)
  
  expect_type(config, "list")
  expect_true("elements" %in% names(config))
  expect_true("style" %in% names(config))
  expect_true("layout" %in% names(config))
  expect_true("container_id" %in% names(config))
  expect_true("js_code" %in% names(config))
  
  expect_equal(config$container_id, "network-cy")
  expect_type(config$js_code, "character")
  expect_gt(nchar(config$js_code), 100)
})

test_that("generateCytoscapeConfig accepts custom parameters", {
  nodes <- create_mock_subnetwork_nodes()
  edges <- create_mock_subnetwork_edges()
  
  node_elements <- createNodeElements(nodes, "id")
  edge_elements <- createEdgeElements(edges)
  
  custom_layout <- list(name = "grid", fit = FALSE)
  custom_handlers <- list(edge_click = "function() { console.log('test'); }")
  
  config <- generateCytoscapeConfig(
    node_elements, 
    edge_elements,
    container_id = "custom-container",
    event_handlers = custom_handlers,
    layout_options = custom_layout
  )
  
  expect_equal(config$container_id, "custom-container")
  expect_equal(config$layout$name, "grid")
  expect_false(config$layout$fit)
  expect_true(grepl("console.log", config$js_code))
})

# =============================================================================
# TESTS FOR SHINY-SPECIFIC FUNCTIONS
# =============================================================================

test_that("generateCytoscapeJSForShiny includes Shiny event handlers", {
  nodes <- create_mock_subnetwork_nodes()
  edges <- create_mock_subnetwork_edges()
  
  node_elements <- createNodeElements(nodes, "id")
  edge_elements <- createEdgeElements(edges)
  
  js_code <- generateCytoscapeJSForShiny(node_elements, edge_elements)
  
  expect_type(js_code, "character")
  expect_true(grepl("Shiny.setInputValue", js_code))
  expect_true(grepl("network-edgeClicked", js_code))
  expect_true(grepl("network-nodeClicked", js_code))
})

# =============================================================================
# TESTS FOR DATA PROCESSING HELPER FUNCTIONS
# =============================================================================

test_that("filterDataByLabel filters data correctly", {
  df <- create_mock_input_data()
  
  # Test filtering by existing label
  filtered <- filterDataByLabel(df, "Treatment_vs_Control")
  expect_equal(nrow(filtered), 4)
  
  # Test filtering by non-existing label
  filtered_empty <- filterDataByLabel(df, "NonExistent")
  expect_equal(nrow(filtered_empty), 0)
  
  # Test data frame without Label column
  df_no_label <- df[, !names(df) %in% "Label"]
  filtered_no_label <- filterDataByLabel(df_no_label, "Treatment_vs_Control")
  expect_equal(nrow(filtered_no_label), nrow(df_no_label))
})

test_that("getInputParameters processes inputs correctly", {
  
  # Mock input object
  mock_input <- list(
    statementTypes = c("Inhibition", "Activation"),
    sources = c("reach", "signor"),
    proteinIdType = "Uniprot",
    pValue = 0.05,
    evidence = 5,
    absLogFC = 0.5,
    selectedLabel = "Treatment_vs_Control",
    selectedProteins = c("TP53", "MDM2")
  )
  
  # Create a simple mock req function
  if(!exists("req")) {
    req <- function(x, y = NULL) x
  }
  
  params <- getInputParameters(mock_input)
  
  expect_equal(params$statementTypes, c("Inhibition", "Activation"))
  expect_equal(params$sources, c("reach", "signor"))
  expect_equal(params$proteinIdType, "Uniprot")
  expect_equal(params$pValue, 0.05)
  expect_equal(params$selectedProteins, c("TP53", "MDM2"))
})

test_that("getInputParameters handles 'all' selections", {
  
  # Mock input with "all" selections
  mock_input_all <- list(
    statementTypes = "all",
    sources = "all",
    proteinIdType = "Uniprot",
    pValue = 0.05,
    evidence = 5,
    absLogFC = 0.5,
    selectedLabel = "Treatment_vs_Control",
    selectedProteins = NULL
  )
  
  # Create a simple mock req function
  if(!exists("req")) {
    req <- function(x, y = NULL) x
  }
  
  params <- getInputParameters(mock_input_all)
  
  expect_null(params$statementTypes)
  expect_null(params$sources)
  expect_null(params$selectedProteins)
})

# =============================================================================
# TESTS WITH MOCKED MSSTATSBIONET FUNCTIONS
# =============================================================================

test_that("annotateProteinData works with mocked MSstatsBioNet function", {
  
  input_df <- create_mock_input_data()
  
  # Create a mock function that mimics the expected behavior
  mock_annotate_func <- function(df, id_type) {
    df$HgncId <- c("1101", "1102", "1103", "1104")
    df$HgncName <- c("TP53", "MDM2", "ATM", "BRCA1")
    return(df)
  }
  
  # Use mockery to stub the function call
  stub(annotateProteinData, "annotateProteinInfoFromIndra", mock_annotate_func)
  
  result <- annotateProteinData(input_df, "Uniprot")
  
  expect_s3_class(result, "data.frame")
  expect_true("HgncId" %in% names(result))
  expect_true("HgncName" %in% names(result))
  expect_equal(nrow(result), nrow(input_df))
  expect_equal(result$HgncName, c("TP53", "MDM2", "ATM", "BRCA1"))
})

test_that("extractSubnetwork works with mocked MSstatsBioNet function", {
  
  annotated_df <- create_mock_annotated_data()
  expected_subnetwork <- create_mock_subnetwork()
  
  # Create a mock function that returns the expected subnetwork
  mock_extract_func <- function(df, pvalueCutoff, evidence_count_cutoff, 
                                statement_types, sources_filter, 
                                logfc_cutoff, force_include_proteins) {
    return(expected_subnetwork)
  }
  
  # Use mockery to stub the function call
  stub(extractSubnetwork, "getSubnetworkFromIndra", mock_extract_func)
  
  result <- extractSubnetwork(
    annotated_df, 
    pValue = 0.05, 
    evidence = 5, 
    statementTypes = NULL,
    sources = NULL, 
    absLogFC = 0.5, 
    selectedProteins = NULL
  )
  
  expect_type(result, "list")
  expect_true("nodes" %in% names(result))
  expect_true("edges" %in% names(result))
  expect_equal(names(result$nodes), c("id", "logFC", "pvalue", "hgncName"))
  expect_equal(names(result$edges), c("source", "target", "interaction", "evidenceCount", "evidenceLink", "source_counts"))
  expect_equal(nrow(result$nodes), 4)
  expect_equal(nrow(result$edges), 4)
})

# =============================================================================
# TESTS FOR UI HELPER FUNCTIONS
# =============================================================================

test_that("createNodeElements handles different label types", {
  nodes <- create_mock_subnetwork_nodes()
  
  # Test with id labels
  elements_id <- createNodeElements(nodes, "id")
  expect_true(all(grepl("P53_HUMAN|MDM2_HUMAN|ATM_HUMAN|BRCA1_HUMAN", elements_id)))
  
  # Test with hgncName labels
  elements_hgnc <- createNodeElements(nodes, "hgncName")
  expect_true(all(grepl("TP53|MDM2|ATM|BRCA1", elements_hgnc)))
  
  # Test with nodes missing hgncName
  nodes_no_hgnc <- nodes
  nodes_no_hgnc$hgncName <- NA
  elements_fallback <- createNodeElements(nodes_no_hgnc, "hgncName")
  expect_true(all(grepl("P53_HUMAN|MDM2_HUMAN|ATM_HUMAN|BRCA1_HUMAN", elements_fallback)))
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("Full pipeline works with mocked functions", {
  
  input_df <- create_mock_input_data()
  subnetwork <- create_mock_subnetwork()
  
  # Mock the annotation function
  mock_annotate_func <- function(df, id_type) {
    df$HgncId <- c("TP53", "MDM2", "ATM", "BRCA1")
    df$HgncName <- c("TP53", "MDM2", "ATM", "BRCA1")
    return(df)
  }
  
  # Mock the extraction function
  mock_extract_func <- function(...) {
    return(subnetwork)
  }
  
  # Use mockery to stub the function calls
  stub(annotateProteinData, "annotateProteinInfoFromIndra", mock_annotate_func)
  stub(extractSubnetwork, "getSubnetworkFromIndra", mock_extract_func)
  
  # Test the full pipeline
  filtered_df <- filterDataByLabel(input_df, "Treatment_vs_Control")
  annotated <- annotateProteinData(filtered_df, "Uniprot")
  subnet <- extractSubnetwork(annotated, 0.05, 5, NULL, NULL, 0.5, NULL)
  
  # Create visualization elements
  node_elements <- createNodeElements(subnet$nodes, "id")
  edge_elements <- createEdgeElements(subnet$edges)
  
  # Generate configuration
  config <- generateCytoscapeConfig(node_elements, edge_elements)
  
  expect_type(config, "list")
  expect_true(nchar(config$js_code) > 0)
  expect_gt(length(config$elements), 0)
})

# =============================================================================
# TESTS FOR ERROR HANDLING
# =============================================================================

test_that("Functions handle errors gracefully", {
  
  # Create mock functions that throw errors
  mock_error_func <- function(...) {
    stop("Simulated error")
  }
  
  # Create a mock showNotification function that does nothing
  mock_show_notification <- function(...) {}
  
  # Test annotateProteinData with error
  stub(annotateProteinData, "annotateProteinInfoFromIndra", mock_error_func)
  stub(annotateProteinData, "showNotification", mock_show_notification)
  result1 <- annotateProteinData(create_mock_input_data(), "Uniprot")
  expect_null(result1)
  
  # Test extractSubnetwork with error
  stub(extractSubnetwork, "getSubnetworkFromIndra", mock_error_func)
  stub(extractSubnetwork, "showNotification", mock_show_notification)
  result2 <- extractSubnetwork(create_mock_annotated_data(), 0.05, 5, NULL, NULL, 0.5, NULL)
  expect_null(result2)
})

# =============================================================================
# TESTS FOR STYLE CONVERSION FUNCTIONS
# =============================================================================

test_that("convertStyleToJS creates valid JavaScript", {
  style_list <- list(
    list(
      selector = "node",
      style = list(
        `background-color` = "data(color)",
        width = "60px"
      )
    )
  )
  
  js_style <- convertStyleToJS(style_list)
  expect_type(js_style, "character")
  expect_true(grepl("selector", js_style))
  expect_true(grepl("background-color", js_style))
  expect_true(grepl("data\\(color\\)", js_style))
})

test_that("convertLayoutToJS creates valid JavaScript", {
  layout_list <- list(
    name = "dagre",
    fit = TRUE,
    padding = 30
  )
  
  js_layout <- convertLayoutToJS(layout_list)
  expect_type(js_layout, "character")
  expect_true(grepl("\"name\": \"dagre\"", js_layout))
  expect_true(grepl("\"fit\": true", js_layout))
  expect_true(grepl("\"padding\": 30", js_layout))
})

# =============================================================================
# TESTS FOR UI COMPONENT FUNCTIONS
# =============================================================================

test_that("UI component functions create proper HTML structure", {
  
  ns <- NS("test")
  
  # Test file upload input
  upload_input <- createFileUploadInput(ns)
  expect_s3_class(upload_input, "shiny.tag")
  
  # Test parameter sliders
  if(exists("createParameterSliders")) {
    sliders <- createParameterSliders(ns)
    expect_s3_class(sliders, "shiny.tag.list")
  }
  
  # Test filter dropdowns
  if(exists("createFilterDropdowns")) {
    dropdowns <- createFilterDropdowns(ns)
    expect_s3_class(dropdowns, "shiny.tag.list")
  }
  
  # Test network visualization box
  if(exists("createNetworkVisualizationBox")) {
    viz_box <- createNetworkVisualizationBox(ns)
    expect_s3_class(viz_box, "shiny.tag")
  }
})

# =============================================================================
# MOCK SESSION TESTS
# =============================================================================

test_that("updateLabelChoices updates selectInput correctly", {
  
  df_with_labels <- data.frame(
    Protein = c("A", "B"),
    Label = c("Label1", "Label2"),
    stringsAsFactors = FALSE
  )
  
  mock_session <- list()
  
  # Mock updateSelectInput function
  mock_update <- mock()
  stub(updateLabelChoices, "updateSelectInput", mock_update)
  
  updateLabelChoices(mock_session, df_with_labels)
  
  # Verify the mock was called
  expect_called(mock_update, 1)
})

test_that("updateProteinChoices updates selectizeInput correctly", {
  
  df_with_proteins <- data.frame(
    Protein = c("TP53", "MDM2", "ATM"),
    stringsAsFactors = FALSE
  )
  
  mock_session <- list()
  
  # Mock updateSelectizeInput function
  mock_update <- mock()
  stub(updateProteinChoices, "updateSelectizeInput", mock_update)
  
  updateProteinChoices(mock_session, df_with_proteins)
  
  # Verify the mock was called
  expect_called(mock_update, 1)
})