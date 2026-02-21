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
  pos_radio <- regexpr(NAMESPACE_STATMODEL$comparison_mode, ui_html)
  
  expect_true(pos_header < pos_radio,
              info = "Header should appear before interactive elements")
})

# ============================================================================
# 2. CONDITIONAL LOGIC TESTS - UI Structure
# ============================================================================

test_that("All comparison type radio buttons are present", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  expect_true(grepl(CONSTANTS_STATMODEL$comparison_mode_all_vs_one, ui_html), 
              info = "All against one option should be present")
  expect_true(grepl(CONSTANTS_STATMODEL$comparison_mode_all_pairwise, ui_html),
              info = "All pairwise option should be present")
  expect_true(grepl(CONSTANTS_STATMODEL$comparison_mode_custom_pairwise, ui_html),
              info = "Custom pairwise option should be present")
  expect_true(grepl(CONSTANTS_STATMODEL$comparison_mode_custom_nonpairwise, ui_html),
              info = "Custom non-pairwise option should be present")
})

# ============================================================================
# 3. BUTTON AND CONTROL TESTS
# ============================================================================

test_that("All action buttons are present with correct IDs", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  # Main action buttons
  expect_true(grepl(NAMESPACE_STATMODEL$modeling_start, ui_html),
              info = "Calculate button should exist")
  expect_true(grepl(NAMESPACE_STATMODEL$visualization_view_results, ui_html),
              info = "View results button should exist")
  expect_true(grepl(NAMESPACE_STATMODEL$visualization_download_plot_results, ui_html),
              info = "Plot results button should exist")
})

test_that("Calculate button is initially disabled", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  # Look for disabled attribute on calculate button
  expect_true(grepl(paste0('disabled.*id="statmodel-', NAMESPACE_STATMODEL$modeling_start), ui_html) ||
                grepl(paste0(NAMESPACE_STATMODEL$modeling_start, '".*disabled'), ui_html),
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
  expect_true(grepl(NAMESPACE_STATMODEL$comparison_mode, ui_html),
              info = "Comparison mode radio buttons should exist")

})

test_that("Select inputs have correct options", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  # Plot type options
  expect_true(grepl("VolcanoPlot", ui_html))
  expect_true(grepl("Heatmap", ui_html))
  expect_true(grepl("ComparisonPlot", ui_html))
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
  
  expect_true(grepl(NAMESPACE_STATMODEL$visualization_download_plot_results, ui_html),
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
# 11. VALIDATION MESSAGES
# ============================================================================

test_that("Informative messages are present", {
  ui <- create_test_ui()
  ui_html <- htmltools::renderTags(ui)$html
  
  expect_true(grepl("Please add a comparison matrix before modeling", ui_html),
              info = "Matrix requirement message should be present")
})

# ============================================================================
# 12. RESPONSE CURVE RATIO SCALE CHECKBOX TESTS
# ============================================================================

test_that("Response curve ratio scale checkbox is present in response curve options", {
  ui <- MSstatsShiny:::create_response_curve_options(shiny::NS("test"))
  ui_html <- htmltools::renderTags(ui)$html
  
  expect_true(grepl("Use ratio scale", ui_html),
              info = "Ratio scale checkbox label should be present")
  expect_true(grepl(MSstatsShiny:::NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale, ui_html),
              info = "Ratio scale checkbox input ID should be present")
})

test_that("Response curve ratio scale checkbox has tooltip", {
  ui <- MSstatsShiny:::create_response_curve_options(shiny::NS("test"))
  ui_html <- htmltools::renderTags(ui)$html
  
  expect_true(grepl("icon-tooltip", ui_html),
              info = "Tooltip class should be present")
  expect_true(grepl("fa-circle-question|question-circle", ui_html),
              info = "Question mark icon should be present")
  expect_true(grepl("protein abundances are shown relative to the control", ui_html),
              info = "Tooltip text should describe ratio scale")
})

test_that("Response curve ratio scale checkbox is checked by default", {
  ui <- MSstatsShiny:::create_response_curve_options(shiny::NS("test"))
  ui_html <- htmltools::renderTags(ui)$html
  
  # Check for the value = TRUE in the checkbox definition
  expect_true(grepl("value = TRUE", ui_html) || grepl('checked["\']', ui_html),
              info = "Ratio scale checkbox should be checked by default")
})