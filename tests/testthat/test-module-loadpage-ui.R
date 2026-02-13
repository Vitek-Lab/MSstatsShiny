test_that("loadpageUI returns a valid tagList with fluidPage structure", {
  # Test basic function execution and structure
  result <- loadpageUI("test")
  
  # Should return a tagList
  expect_s3_class(result, "shiny.tag.list")
  
  result_html = as.character(result)
  expect_true(grepl("div", result_html))
  
  # Should not be NULL or empty
  expect_true(length(result) > 0)
})

test_that("loadpageUI generates correct namespaced input IDs", {
  # Test that all input elements use proper namespacing
  result <- loadpageUI("testmodule")
  html_output <- as.character(result)
  
  # Check for key namespaced input IDs that should be present
  expected_ids <- c(
    "testmodule-BIO",           # Biological question radio buttons
    "testmodule-DDA_DIA",       # Label type radio buttons  
    "testmodule-filetype",      # File type radio buttons
    "testmodule-proceed1"       # Upload button
  )
  
  for(id in expected_ids) {
    expect_true(
      grepl(id, html_output, fixed = TRUE),
      info = paste("Missing namespaced ID:", id)
    )
  }
})

test_that("loadpageUI contains all required radio button choices", {
  # Test that essential radio button options are present
  result <- loadpageUI("test")
  html_output <- as.character(result)
  
  # Check biological question options
  bio_options <- c("Protein", "Peptide", "PTM")
  for(option in bio_options) {
    expect_true(grepl(option, html_output), 
                info = paste("Missing biological option:", option))
  }
  
  # Check label type options
  label_options <- c("Label-Free", "TMT")
  for(option in label_options) {
    expect_true(grepl(option, html_output),
                info = paste("Missing label option:", option))
  }
  
  # Check file type options (sample a few key ones)
  file_options <- c("MaxQuant", "Skyline", "MSstats Format")
  for(option in file_options) {
    expect_true(grepl(option, html_output),
                info = paste("Missing file type option:", option))
  }
})

test_that("loadpageUI includes required conditional panels for different workflows", {
  # Test that key conditional panels exist for different analysis types
  result <- loadpageUI("test")
  html_output <- as.character(result)
  
  # Check for conditional panel conditions that handle different workflows
  # Note: HTML entities encode single quotes as &#39;
  expected_conditions <- c(
    "input[&#39;loadpage-filetype&#39;] == &#39;sample&#39;",     # Sample data panels
    "input[&#39;loadpage-BIO&#39;] != &#39;PTM&#39;",             # Non-PTM workflows
    "input[&#39;loadpage-filetype&#39;] == &#39;maxq&#39;",       # MaxQuant workflow
    "input[&#39;loadpage-DDA_DIA&#39;] == &#39;TMT&#39;",         # TMT labeling
    "input[&#39;loadpage-filetype&#39;] == &#39;sky&#39;"        # Skyline workflow
  )
  
  for(condition in expected_conditions) {
    expect_true(grepl(condition, html_output, fixed = TRUE),
                info = paste("Missing conditional panel for:", condition))
  }
})

test_that("loadpageUI properly handles file input elements and validation", {
  # Test that file inputs are properly configured
  result <- loadpageUI("test")
  html_output <- as.character(result)
  
  # Should contain file input elements
  expect_true(grepl('type="file"', html_output),
              "No file input elements found")
  
  # Upload button should be disabled initially (using shinyjs)
  expect_true(grepl("proceed1", html_output),
              "Upload button not found")
  
  # Check for separator radio buttons for file parsing
  expect_true(grepl("sep_data", html_output) || grepl("Column separator", html_output),
              "File separator options not found")
  
  # Should include help text and external links
  expect_true(grepl("User Guide", html_output),
              "Help documentation links not found")
  
  # Should include file size warnings
  expect_true(grepl("250 MB", html_output),
              "File size limit warning not found")
})

# Test suite for loadpage UI module
test_that("loadpageUI creates proper structure", {
  # Test the main UI structure
  ui_output <- loadpageUI("test")
  
  expect_s3_class(ui_output, "shiny.tag.list")
  expect_true(any(grepl("container-fluid", as.character(ui_output))))
  expect_true(any(grepl("Upload data", as.character(ui_output))))
})

# Tests for create_header_content()
test_that("create_header_content includes required elements", {
  header <- create_header_content()
  header_html <- as.character(header)
  
  # Check for key text content
  expect_true(grepl("MSstats Pipeline", header_html))
  expect_true(grepl("User Guide", header_html))
  expect_true(grepl("MSstatsPTM", header_html))
  expect_true(grepl("CSV/TSV format", header_html))
  expect_true(grepl("250 MB", header_html))
  
  # Check for external links
  expect_true(grepl("msstats.org", header_html))
  expect_true(grepl("bioconductor.org", header_html))
  expect_true(grepl('target="_blank"', header_html))
})

# Tests for create_sample_dataset_descriptions()
test_that("create_sample_dataset_descriptions creates conditional panels", {
  descriptions <- create_sample_dataset_descriptions()
  descriptions_html <- as.character(descriptions)
  
  # Check for conditional panels
  expect_true(grepl("shiny-panel-conditional", descriptions_html))
  
  # Check for specific dataset references
  expect_true(grepl("DDA acquisition", descriptions_html))
  expect_true(grepl("DIA acquisition", descriptions_html))
  expect_true(grepl("SRM/PRM acquisition", descriptions_html))
  
  # Check for publication links
  expect_true(grepl("Choi, M. et al", descriptions_html))
  expect_true(grepl("Selevsek, N. et al", descriptions_html))
  expect_true(grepl("Picotti, P. et al", descriptions_html))
})

# Tests for create_css_styling()
test_that("create_css_styling includes required CSS", {
  css <- create_css_styling()
  
  # Check that it's a proper tag structure (not character conversion)
  expect_s3_class(css, "shiny.tag")
  expect_equal(css$name, "head")
  
  # Check the children elements contain the expected content
  css_children <- css$children
  expect_true(length(css_children) >= 3)  # Should have 3 children (2 styles + 1 link)
  
  # Convert children to string to check content
  children_html <- paste(css_children, collapse = " ")
  expect_true(grepl("background-color:orange", children_html))
  expect_true(grepl("proceed1", children_html))
  expect_true(grepl("reset1", children_html))
  expect_true(grepl("style.css", children_html))
  
  # Check that it's a head tag with proper structure
  expect_true(any(sapply(css_children, function(x) {
    if (is.list(x) && !is.null(x$name)) {
      return(x$name == "style" || x$name == "link")
    }
    FALSE
  })))
})

# Tests for create_main_selection_controls()
test_that("create_main_selection_controls creates proper radio buttons", {
  controls <- create_main_selection_controls(NS("test"))
  controls_html <- as.character(controls)
  
  # Check for biological question options
  expect_true(grepl("Biological Question", controls_html))
  expect_true(grepl("Protein", controls_html))
  expect_true(grepl("Peptide", controls_html))
  expect_true(grepl("PTM", controls_html))
  
  # Check for label type options
  expect_true(grepl("Label Type", controls_html))
  expect_true(grepl("Label-Free", controls_html))
  expect_true(grepl("TMT", controls_html))
  
  # Check for file type options
  expect_true(grepl("Type of File", controls_html))
  expect_true(grepl("MSstats Format", controls_html))
  expect_true(grepl("Skyline", controls_html))
  expect_true(grepl("MaxQuant", controls_html))
  expect_true(grepl("FragPipe", controls_html))
  expect_true(grepl("DIANN", controls_html))
  
  # Check for proper namespace
  expect_true(grepl("test-BIO", controls_html))
  expect_true(grepl("test-DDA_DIA", controls_html))
  expect_true(grepl("test-filetype", controls_html))
})

# Tests for create_label_free_type_selection()
test_that("create_label_free_type_selection creates conditional panel", {
  selection <- create_label_free_type_selection(NS("test"))
  selection_html <- as.character(selection)
  
  expect_true(grepl("shiny-panel-conditional", selection_html))
  expect_true(grepl("Type of Label-Free type", selection_html))
  expect_true(grepl("DDA", selection_html))
  expect_true(grepl("DIA", selection_html))
  expect_true(grepl("SRM/PRM", selection_html))
  
  # Check conditional logic
  expect_true(grepl("loadpage-BIO", selection_html))
  expect_true(grepl("loadpage-filetype", selection_html))
})

# Tests for create_standard_uploads()
test_that("create_standard_uploads creates file input with conditions", {
  uploads <- create_standard_uploads(NS("test"))
  uploads_html <- as.character(uploads)
  
  expect_true(grepl("Upload quantification dataset", uploads_html))
  expect_true(grepl("shiny-input-file", uploads_html))
  expect_true(grepl("test-data", uploads_html))
  
  # Check conditional logic for multiple file types
  expect_true(grepl("loadpage-filetype", uploads_html))
  expect_true(grepl("prog", uploads_html))
  expect_true(grepl("PD", uploads_html))
  expect_true(grepl("phil", uploads_html))
})

# Tests for create_msstats_uploads()
test_that("create_msstats_uploads creates different inputs for regular and PTM", {
  uploads <- create_msstats_uploads(NS("test"))
  uploads_html <- as.character(uploads)
  
  # Check for regular MSstats format
  expect_true(grepl("Upload data in MSstats Format", uploads_html))
  expect_true(grepl("test-msstatsdata", uploads_html))
  
  # Check for PTM MSstats format
  expect_true(grepl("Upload PTM data in MSstats Format", uploads_html))
  expect_true(grepl("test-msstatsptmdata", uploads_html))
  expect_true(grepl("Upload unmodified data in MSstats Format", uploads_html))
  expect_true(grepl("test-unmod", uploads_html))
})

# Tests for create_ptm_fragpipe_uploads()
test_that("create_ptm_fragpipe_uploads creates comprehensive PTM options", {
  uploads <- create_ptm_fragpipe_uploads(NS("test"))
  uploads_html <- as.character(uploads)
  
  # Check for required uploads
  expect_true(grepl("Upload PTM msstats dataset", uploads_html))
  expect_true(grepl("Upload PTM annotation file", uploads_html))
  expect_true(grepl("Upload global profiling msstats dataset", uploads_html))
  expect_true(grepl("Upload global profiling annotation file", uploads_html))
  
  # Check for processing options
  expect_true(grepl("modification id column", uploads_html))
  expect_true(grepl("localization_cutoff", uploads_html))
  expect_true(grepl("Remove unlocalized peptides", uploads_html))
  
  # Check default values
  expect_true(grepl("STY", uploads_html))
  expect_true(grepl("\\.75", uploads_html))
})

# Tests for create_maxquant_uploads()
test_that("create_maxquant_uploads creates proper file inputs", {
  uploads <- create_maxquant_uploads(NS("test"))
  uploads_html <- as.character(uploads)
  
  expect_true(grepl("Upload evidence.txt File", uploads_html))
  expect_true(grepl("Upload proteinGroups.txt File", uploads_html))
  expect_true(grepl("Upload annotation File", uploads_html))
  
  expect_true(grepl("test-evidence", uploads_html))
  expect_true(grepl("test-pGroup", uploads_html))
  expect_true(grepl("test-annot1", uploads_html))
})

# Tests for create_ptm_uploads()
test_that("create_ptm_uploads creates PTM-specific inputs", {
  uploads <- create_ptm_uploads(NS("test"))
  uploads_html <- as.character(uploads)
  
  # Check for PTM specific uploads
  expect_true(grepl("Upload PTM input.txt File", uploads_html))
  expect_true(grepl("Upload fasta File", uploads_html))
  expect_true(grepl("Upload Unmodified Protein input.txt File", uploads_html))
  
  # Check for modification labels
  expect_true(grepl("Modification Label", uploads_html))
  expect_true(grepl("FASTA file column name", uploads_html))
  
  # Check default values
  expect_true(grepl("uniprot_iso", uploads_html))
})

# Tests for create_processing_options()
test_that("create_processing_options creates TMT and label-free options", {
  options <- create_processing_options(NS("test"))
  options_html <- as.character(options)
  
  # Check for processing options structure
  expect_true(grepl("Select the options for pre-processing", options_html))
  expect_true(grepl("Use unique peptides", options_html))
  expect_true(grepl("Remove proteins with 1", options_html))
})

# Tests for create_quality_filtering_options()
test_that("create_quality_filtering_options creates filtering controls", {
  options <- create_quality_filtering_options(NS("test"))
  options_html <- as.character(options)
  
  expect_true(grepl("Filter with Q-value", options_html))
  expect_true(grepl("Filter with M-score", options_html))
  expect_true(grepl("Q-value cutoff", options_html))
  expect_true(grepl("M-score cutoff", options_html))
  expect_true(grepl("MBR Enabled", options_html))
})

# Tests for create_separator_buttons()
test_that("create_separator_buttons creates proper radio buttons", {
  buttons <- create_separator_buttons(NS("test"), "sep_test")
  buttons_html <- as.character(buttons)
  
  expect_true(grepl("Column separator", buttons_html))
  expect_true(grepl("Comma", buttons_html))
  expect_true(grepl("Semicolon", buttons_html))
  expect_true(grepl("Tab", buttons_html))
  expect_true(grepl("Pipe", buttons_html))
  
  # Check for inline display
  expect_true(grepl("radio-inline", buttons_html))
})

# Test order preservation in main selection controls
test_that("main selection controls maintain proper order", {
  controls <- create_main_selection_controls(NS("test"))
  controls_html <- as.character(controls)
  
  # Find positions of each section
  bio_pos <- regexpr("Biological Question", controls_html)
  label_pos <- regexpr("Label Type", controls_html)
  file_pos <- regexpr("Type of File", controls_html)
  
  # Verify correct order
  expect_true(bio_pos < label_pos)
  expect_true(label_pos < file_pos)
  
  # Verify numbered headings are in order
  expect_true(grepl("1\\. Biological Question", controls_html))
  expect_true(grepl("2\\. Label Type", controls_html))
  expect_true(grepl("3\\. Type of File", controls_html))
})

# Test tooltip content is preserved
test_that("tooltips contain proper explanatory text", {
  controls <- create_main_selection_controls(NS("test"))
  controls_html <- as.character(controls)
  
  # Check for tooltip explanations
  expect_true(grepl("Select the biological question of interest", controls_html))
  expect_true(grepl("Label-free will process all label-free acquisitions", controls_html))
  expect_true(grepl("Choose the spectral processing tool used", controls_html))
  
  # Check for icon-wrapper and icon-tooltip classes
  expect_true(grepl("icon-wrapper", controls_html))
  expect_true(grepl("icon-tooltip", controls_html))
})

# Test file input configurations
test_that("file inputs have proper accept attributes", {
  # Test CSV-only inputs
  annot_input <- create_standard_annotation_uploads(NS("test"))
  annot_html <- as.character(annot_input)
  expect_true(grepl('accept=.*csv', annot_html))
})

# Tests for Spectronaut specific UI components
test_that("create_spectronaut_uploads creates UI outputs", {
  uploads <- create_spectronaut_uploads(NS("test"))
  uploads_html <- as.character(uploads)
  
  expect_true(grepl("spectronaut_header_ui", uploads_html))
  expect_true(grepl("spectronaut_file_selection_ui", uploads_html))
  expect_true(grepl("spectronaut_options_ui", uploads_html))
})

test_that("Spectronaut helper functions create correct UI elements", {
  # Header
  header <- create_spectronaut_header()
  expect_true(grepl("Upload MSstats scheme output from Spectronaut", as.character(header)))
  
  # Mode selector
  mode_sel <- create_spectronaut_mode_selector(NS("test"))
  expect_true(grepl("Large file mode", as.character(mode_sel)))
  expect_true(grepl("checkbox", as.character(mode_sel)))
  
  # Standard UI
  std_ui <- create_spectronaut_standard_ui(NS("test"))
  expect_true(grepl("file", as.character(std_ui)))
  expect_true(grepl("specdata", as.character(std_ui)))
  
  # Large file UI
  large_ui <- create_spectronaut_large_file_ui(NS("test"))
  large_ui_html <- as.character(large_ui)
  expect_true(grepl("Browse for local file", large_ui_html))
  expect_true(grepl("specdata_big_path", large_ui_html))
  
  # Filter options
  filter_opts <- create_spectronaut_large_filter_options(NS("test"))
  opts_html <- as.character(filter_opts)
  expect_true(grepl("Filter by excluded", opts_html))
  expect_true(grepl("Filter by identified", opts_html))
  expect_true(grepl("Filter by q-value", opts_html))
  
  # Q-value cutoff
  qval_ui <- create_spectronaut_qvalue_cutoff_ui(NS("test"))
  expect_true(grepl("Q-value cutoff", as.character(qval_ui)))
  expect_true(grepl("0.01", as.character(qval_ui)))
  
  # Bottom UI
  bottom_ui <- create_spectronaut_large_bottom_ui(NS("test"))
  bottom_html <- as.character(bottom_ui)
  expect_true(grepl("Max feature count", bottom_html))
  expect_true(grepl("Use unique peptides", bottom_html))
  expect_true(grepl("Aggregate PSMs", bottom_html))
  expect_true(grepl("Filter features with few observations", bottom_html))
})

# Tests for Metamorpheus specific UI components
test_that("Metamorpheus converter option exists in filetype choices", {
  result <- loadpageUI("test")
  html_output <- as.character(result)

  # Check that the display label "Metamorpheus" is present
  expect_true(grepl("Metamorpheus", html_output),
              info = "Metamorpheus label not found in UI")

  # Check that the radio button value "meta" is present
  expect_true(grepl("value=\"meta\"", html_output),
              info = "Metamorpheus radio button value 'meta' not found in UI")
})