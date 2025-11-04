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
# TESTS FOR SHINY-SPECIFIC FUNCTIONS
# =============================================================================

test_that("generateCytoscapeJSForShiny includes Shiny event handlers", {
  nodes = create_mock_subnetwork_nodes()
  edges = create_mock_subnetwork_edges()
  
  js_code <- generateCytoscapeJSForShiny(nodes, edges)
  
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
  
  params <- getInputParameters(mock_input, c("HGNC:1234", "CHEBI:5321"))
  
  expect_equal(params$statementTypes, c("Inhibition", "Activation"))
  expect_equal(params$sources, c("reach", "signor"))
  expect_equal(params$proteinIdType, "Uniprot")
  expect_equal(params$pValue, 0.05)
  expect_equal(params$selectedProteins, c("HGNC:1234", "CHEBI:5321"))
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
    selectedLabel = "Treatment_vs_Control"
  )
  
  # Create a simple mock req function
  if(!exists("req")) {
    req <- function(x, y = NULL) x
  }
  
  params <- getInputParameters(mock_input_all, NULL)
  
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
                                logfc_cutoff, force_include_other) {
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
  
  # Generate configuration
  js_code <- generateCytoscapeJSForShiny(subnet$nodes, subnet$edges)
  
  expect_type(js_code, "character")
  expect_true(nchar(js_code) > 0)
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