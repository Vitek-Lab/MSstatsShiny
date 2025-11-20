# Helper function to create a test UI
create_test_ui <- function() {
  MSstatsShiny::statmodelUI("statmodel")
}

# ============================================================================
# 1. COMPONENT ORDER TESTS
# ============================================================================

test_that("UI components appear in correct order", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  # Extract positions of key elements
  pos_contrast <- regexpr("1\\. Define comparisons", ui_html)
  pos_group_comp <- regexpr("2\\. Group comparison", ui_html)
  pos_viz <- regexpr("3\\. Visualization", ui_html)
  
  # Verify order
  expect_true(pos_contrast < pos_group_comp,
              info = "Contrast matrix section should appear before group comparison")
  expect_true(pos_group_comp < pos_viz,
              info = "Group comparison should appear before visualization")
})

test_that("Side panel contains all three main sections", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  expect_true(grepl("1\\. Define comparisons", ui_html))
  expect_true(grepl("2\\. Group comparison", ui_html))
  expect_true(grepl("3\\. Visualization", ui_html))
})

test_that("Header and instructions appear before interactive elements", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  pos_header <- regexpr("Statistical modeling and inference", ui_html)
  pos_radio <- regexpr("contrast_mode", ui_html)
  
  expect_true(pos_header < pos_radio,
              info = "Header should appear before interactive elements")
})

# ============================================================================
# 2. CONDITIONAL LOGIC TESTS - UI Structure
# ============================================================================

test_that("All comparison type radio buttons are present", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  expect_true(grepl("all_pair", ui_html), 
              info = "All pairwise option should be present")
  expect_true(grepl("all_one", ui_html),
              info = "All against one option should be present")
  expect_true(grepl("custom", ui_html),
              info = "Custom pairwise option should be present")
  expect_true(grepl("custom_np", ui_html),
              info = "Custom non-pairwise option should be present")
})

test_that("Plot type conditional panels exist", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html

  expect_true(grepl("input[&#39;statmodel-typeplot&#39;] == &#39;VolcanoPlot", 
                    ui_html, fixed = TRUE),
              info = "Volcano plot conditional panel should exist")
  expect_true(grepl("input[&#39;statmodel-typeplot&#39;] == &#39;ComparisonPlot", 
                    ui_html, fixed = TRUE),
              info = "Comparison plot conditional panel should exist")
  expect_true(grepl("input[&#39;statmodel-typeplot&#39;] == &#39;Heatmap", 
                    ui_html, fixed = TRUE),
              info = "Heatmap conditional panel should exist")
})

# ============================================================================
# 3. BUTTON AND CONTROL TESTS
# ============================================================================

test_that("All action buttons are present with correct IDs", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  # Main action buttons
  expect_true(grepl('id="statmodel-calculate"', ui_html),
              info = "Calculate button should exist")
  expect_true(grepl('id="statmodel-viewresults"', ui_html),
              info = "View results button should exist")
  expect_true(grepl('id="statmodel-plotresults"', ui_html),
              info = "Plot results button should exist")
})

test_that("Calculate button is initially disabled", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  # Look for disabled attribute on calculate button
  expect_true(grepl('disabled.*id="statmodel-calculate"', ui_html) ||
                grepl('id="statmodel-calculate".*disabled', ui_html),
              info = "Calculate button should be initially disabled")
})

test_that("Design (Next Step) button is initially disabled", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  expect_true(grepl('disabled.*id="statmodel-Design"', ui_html) ||
                grepl('id="statmodel-Design".*disabled', ui_html),
              info = "Design button should be initially disabled")
})

# ============================================================================
# 4. INPUT CONTROL TESTS
# ============================================================================

test_that("All required input controls are present", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  # Radio buttons
  expect_true(grepl('id="statmodel-contrast_mode"', ui_html),
              info = "Comparison type radio buttons should exist")
  expect_true(grepl('id="statmodel-moderated"', ui_html),
              info = "Moderated radio buttons should exist")
  
  # Sliders
  expect_true(grepl('id="statmodel-signif"', ui_html),
              info = "Significance level slider should exist")
  expect_true(grepl('id="statmodel-sig"', ui_html),
              info = "Adjusted p-value cutoff slider should exist")
  
  # Select inputs
  expect_true(grepl('id="statmodel-typeplot"', ui_html),
              info = "Plot type select should exist")
  expect_true(grepl('id="statmodel-logp"', ui_html),
              info = "Log transformation select should exist")
  expect_true(grepl('id="statmodel-cluster"', ui_html),
              info = "Cluster analysis select should exist")
  
  # Checkboxes
  expect_true(grepl('id="statmodel-pname"', ui_html),
              info = "Protein name checkbox should exist")
  expect_true(grepl('id="statmodel-FC1"', ui_html),
              info = "Fold change cutoff checkbox should exist")
  
  # Numeric inputs
  expect_true(grepl('id="statmodel-FC"', ui_html),
              info = "Fold change cutoff numeric input should exist")
  expect_true(grepl('id="statmodel-nump"', ui_html),
              info = "Number of proteins numeric input should exist")
})

test_that("Select inputs have correct options", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  # Plot type options
  expect_true(grepl("VolcanoPlot", ui_html))
  expect_true(grepl("Heatmap", ui_html))
  expect_true(grepl("ComparisonPlot", ui_html))
  
  # Log transformation options
  expect_true(grepl('value="2"', ui_html), info = "Base 2 log option should exist")
  expect_true(grepl('value="10"', ui_html), info = "Base 10 log option should exist")
  
  # Cluster options
  expect_true(grepl("protein dendogram", ui_html))
  expect_true(grepl("comparison dendogram", ui_html))
  expect_true(grepl("both", ui_html))
})

# ============================================================================
# 5. STYLING TESTS
# ============================================================================

test_that("Busy spinner is included", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  expect_true(grepl("fading-circle", ui_html),
              info = "Busy spinner should be configured")
})

# ============================================================================
# 6. HELP TEXT AND TOOLTIPS
# ============================================================================

test_that("All sections have help tooltips", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  # Check for specific tooltip divs
  expect_true(grepl("icon-tooltip", ui_html),
              info = "Tooltip divs should be present")
})

test_that("Instructions and help links are present", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  expect_true(grepl("Create a contrast matrix", ui_html),
              info = "Step 1 instruction should be present")
  expect_true(grepl("generate the model", ui_html),
              info = "Step 2 instruction should be present")
  expect_true(grepl("view result plots", ui_html),
              info = "Step 3 instruction should be present")
  
  # Check for documentation link
  expect_true(grepl("rdocumentation.org", ui_html),
              info = "Documentation link should be present")
})

test_that("Output UI elements are present", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  expect_true(grepl('id="statmodel-WhichComp"', ui_html),
              info = "WhichComp UI output should exist")
  expect_true(grepl('id="statmodel-WhichProt"', ui_html),
              info = "WhichProt UI output should exist")
  expect_true(grepl('id="statmodel-matrix"', ui_html),
              info = "Matrix UI output should exist")
  expect_true(grepl('id="statmodel-table_results"', ui_html),
              info = "Table results UI output should exist")
})

# ============================================================================
# 7. DOWNLOAD HANDLER TESTS
# ============================================================================

test_that("Download button is present", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  expect_true(grepl('id="statmodel-plotresults"', ui_html),
              info = "Download button for plot results should exist")
  expect_true(grepl("Save plot results as Zip", ui_html),
              info = "Download button should have correct label")
})

# ============================================================================
# 8. TABSET PANEL TESTS
# ============================================================================

test_that("PTM results tabset structure exists", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  expect_true(grepl("Adjusted PTM Results", ui_html),
              info = "Adjusted PTM Results tab should exist")
  expect_true(grepl("Unadjusted PTM Results", ui_html),
              info = "Unadjusted PTM Results tab should exist")
  expect_true(grepl("Protein Results", ui_html),
              info = "Protein Results tab should exist")
})

# ============================================================================
# 9. NAMESPACE TESTS
# ============================================================================

test_that("All inputs use correct namespace", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  # All IDs should be prefixed with "statmodel-"
  input_ids <- c("contrast_mode", "calculate", "moderated",
                 "signif", "typeplot", "pname", "logp", "sig", "FC1", "FC",
                 "nump", "cluster", "viewresults", "plotresults")
  
  for (input_id in input_ids) {
    pattern <- paste0('id="statmodel-', input_id, '"')
    expect_true(grepl(pattern, ui_html),
                info = paste("Input", input_id, "should use correct namespace"))
  }
})

# ============================================================================
# 10. FOLD CHANGE CUTOFF CONDITIONAL TESTS
# ============================================================================

test_that("Fold change cutoff appears conditionally", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  # Check that FC numeric input is inside conditional panel
  expect_true(grepl("statmodel-FC1.*==.*true", ui_html),
              info = "Fold change numeric input should be conditional on FC1 checkbox")
})

# ============================================================================
# 11. VALIDATION MESSAGES
# ============================================================================

test_that("Informative messages are present", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  expect_true(grepl("Please add a comparison matrix before modeling", ui_html),
              info = "Matrix requirement message should be present")
  expect_true(grepl("Heatmaps require at least two comparisons", ui_html),
              info = "Heatmap requirement message should be present")
})