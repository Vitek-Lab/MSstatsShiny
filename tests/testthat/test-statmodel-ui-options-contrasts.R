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
  # Setup Metadata button removed — metadata auto-builds on radio selection
  expect_false(grepl(NAMESPACE_STATMODEL$comparisons_submit, html, fixed = TRUE),
               info = "Setup Metadata button should not be present")
  expect_true(grepl(NAMESPACE_STATMODEL$comparisons_clear, html, fixed = TRUE))
  expect_s3_class(ui, "shiny.tag.list")
  expect_length(ui, 1)
})

# ============================================================================
# DYNAMIC HEADING AND DROPDOWN TESTS
# ============================================================================

test_that("modeling section shows conditional headings in UI", {
  ui <- MSstatsShiny::statmodelUI("statmodel")
  ui_html <- htmltools::renderTags(ui)$html

  expect_true(grepl("Dose response analysis", ui_html),
              info = "Dose response heading should be in conditional panel")
  expect_true(grepl("Group comparison", ui_html),
              info = "Group comparison heading should be in conditional panel")
  expect_true(grepl("configure the mapping", ui_html),
              info = "Dose response description should be present")
  expect_true(grepl("add a comparison matrix", ui_html),
              info = "Group comparison description should be present")
})