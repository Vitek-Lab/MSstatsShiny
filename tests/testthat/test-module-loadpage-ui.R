test_that("loadpageUI returns a valid tagList with fluidPage structure", {
  # Test basic function execution and structure
  result <- loadpageUI("test")
  
  # Should return a tagList
  expect_s3_class(result, "shiny.tag.list")
  
  # Should contain a fluidPage as the main structure
  expect_equal(result[[1]][[4]][[1]]$name, "div")
  expect_true("container-fluid" %in% result[[1]][[4]][[1]]$attribs$class)
  
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