test_that("build_custom_pairwise_panel creates correct UI structure", {
  ns <- function(id) paste0("statmodel-", id)
  ui <- build_custom_pairwise_panel(ns)
  html <- as.character(ui)
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1, html, fixed = TRUE))
  expect_true(grepl("<h6>vs</h6>", html, fixed = TRUE))
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2, html, fixed = TRUE))
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_submit, html, fixed = TRUE))
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_clear, html, fixed = TRUE))
  expect_true(grepl(">Add<", html, fixed = TRUE))
  expect_true(grepl(">Clear matrix<", html, fixed = TRUE))
  expect_s3_class(ui, "shiny.tag.list")
  expect_length(ui, 5)
})

test_that("build_all_vs_one_panel creates correct UI structure", {
  ns <- function(id) paste0("statmodel-", id)
  ui <- build_all_vs_one_panel(ns)
  html <- as.character(ui)
  expect_true(grepl("<h5>Compare all groups against:</h5>", html, fixed = TRUE))
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_all_vs_one_choice, html, fixed = TRUE))
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_submit, html, fixed = TRUE))
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_clear, html, fixed = TRUE))
  expect_true(grepl(">Submit<", html, fixed = TRUE))
  expect_true(grepl(">Clear matrix<", html, fixed = TRUE))
  expect_s3_class(ui, "shiny.tag.list")
  expect_length(ui, 4)
})

test_that("build_all_pairwise_panel creates correct UI structure", {
  ns <- function(id) paste0("statmodel-", id)
  ui <- build_all_pairwise_panel(ns)
  html <- as.character(ui)
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_submit, html, fixed = TRUE))
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_clear, html, fixed = TRUE))
  expect_true(grepl(">Submit<", html, fixed = TRUE))
  expect_true(grepl(">Clear matrix<", html, fixed = TRUE))
  expect_s3_class(ui, "shiny.tag.list")
  expect_length(ui, 2)
})

test_that("build_custom_nonpairwise_panel creates correct UI structure", {
  ns <- function(id) paste0("statmodel-", id)
  ui <- build_custom_nonpairwise_panel(ns)
  html <- as.character(ui)
  expect_true(grepl("<h5>Non-pairwise Comparison:</h5>", html, fixed = TRUE))
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_name, html, fixed = TRUE))
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, html, fixed = TRUE))
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_submit, html, fixed = TRUE))
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_clear, html, fixed = TRUE))
  expect_true(grepl(">Add<", html, fixed = TRUE))
  expect_true(grepl(">Clear matrix<", html, fixed = TRUE))
  expect_s3_class(ui, "shiny.tag.list")
  expect_length(ui, 5)
})

test_that("build_response_curve_panel creates correct UI structure", {
  ns <- function(id) paste0("statmodel-", id)
  ui <- build_response_curve_panel(ns)
  html <- as.character(ui)
  # Setup Metadata button removed. Metadata auto-builds on radio selection
  expect_false(grepl(NAMESPACE_STATMODEL$comparisons_submit, html, fixed = TRUE),
               info = "Submit contrast matrix button should not be present")
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_clear, html, fixed = TRUE))
  expect_s3_class(ui, "shiny.tag.list")
  expect_length(ui, 1)
})

# ============================================================================
# DYNAMIC HEADING AND DROPDOWN TESTS
# ============================================================================

test_that("modeling section header placeholder exists in UI", {
  ui <- MSstatsShiny::statmodelUI("statmodel")
  ui_html <- htmltools::renderTags(ui)$html

  expect_true(grepl(NAMESPACE_STATMODEL$modeling_section_header, ui_html),
              info = "Modeling section header uiOutput placeholder should be present")
})

# ============================================================================
# MODELING SECTION HEADER TESTS
# ============================================================================

test_that("get_modeling_section_header returns dose response heading for response curve mode", {
  result <- MSstatsShiny:::get_modeling_section_header(
    CONSTANTS_STATMODEL$comparison_mode_response_curve
  )
  html <- as.character(result)
  expect_true(grepl("Dose response analysis", html),
              info = "Should show dose response heading")
  expect_true(grepl("configure the mapping", html),
              info = "Should show dose response description")
  expect_false(grepl("Group comparison", html),
               info = "Should not show group comparison heading")
})

test_that("get_modeling_section_header returns group comparison heading for other modes", {
  for (mode in c(
    CONSTANTS_STATMODEL$comparison_mode_all_pairwise,
    CONSTANTS_STATMODEL$comparison_mode_all_vs_one,
    CONSTANTS_STATMODEL$comparison_mode_custom_pairwise,
    CONSTANTS_STATMODEL$comparison_mode_custom_nonpairwise
  )) {
    result <- MSstatsShiny:::get_modeling_section_header(mode)
    html <- as.character(result)
    expect_true(grepl("Group comparison", html),
                info = paste("Should show group comparison for mode:", mode))
    expect_false(grepl("Dose response", html),
                 info = paste("Should not show dose response for mode:", mode))
  }
})

test_that("get_modeling_section_header handles NULL mode", {
  result <- MSstatsShiny:::get_modeling_section_header(NULL)
  html <- as.character(result)
  expect_true(grepl("Group comparison", html),
              info = "NULL mode should default to group comparison")
})