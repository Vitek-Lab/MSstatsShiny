test_that("Correct namespaces are present in create_fold_change_options", {
  ns <- NS("test_module")
  result <- create_fold_change_options(ns)
  ui_html <- htmltools::renderTags(result)$html
  expect_true(grepl(NAMESPACE_STATMODEL$visualization_fold_change_checkbox, ui_html),
              info = "Fold change checkbox should be present")
  expect_true(grepl(NAMESPACE_STATMODEL$visualization_fold_change_input, ui_html),
              info = "Fold change input should be present")
})

test_that("Correct elements are present in create_heatmap_options", {
  ns <- NS("test_module")
  result <- create_heatmap_options(ns)
  ui_html <- htmltools::renderTags(result)$html
  expect_true(grepl(NAMESPACE_STATMODEL$visualization_logp_base, ui_html),
              info = "Log p-value base namespace should be present")
  expect_true(grepl(NAMESPACE_STATMODEL$visualization_heatmap_number_proteins, ui_html),
              info = "Number of proteins input namespace should be present")
  expect_true(grepl(NAMESPACE_STATMODEL$visualization_heatmap_cluster_option, ui_html),
              info = "Cluster option select input namespace should be present")
  expect_true(grepl('value="2"', ui_html), info = "Base 2 log option should exist")
  expect_true(grepl('value="10"', ui_html), info = "Base 10 log option should exist")
  expect_true(grepl("protein dendrogram", ui_html))
  expect_true(grepl("comparison dendrogram", ui_html))
  expect_true(grepl("protein and comparison dendrograms", ui_html))
})

test_that("Correct elements are present in create_comparison_plot_options", {
  ns <- NS("test_module")
  result <- create_comparison_plot_options(ns)
  ui_html <- htmltools::renderTags(result)$html
  expect_true(grepl(NAMESPACE_STATMODEL$visualization_which_protein, ui_html),
              info = "Which protein namespace should be present")
})

test_that("Correct elements are present in create_volcano_plot_options", {
  ns <- NS("test_module")
  result <- create_volcano_plot_options(ns)
  ui_html <- htmltools::renderTags(result)$html
  expect_true(grepl(NAMESPACE_STATMODEL$visualization_which_comparison, ui_html),
              info = "Which comparison namespace should be present")
  expect_false(grepl(NAMESPACE_STATMODEL$visualization_volcano_display_protein_name, ui_html),
               info = "Display protein name checkbox should be removed")
  expect_true(grepl(NAMESPACE_STATMODEL$visualization_logp_base, ui_html),
              info = "Log p-value base namespace should be present")
  expect_true(grepl(NAMESPACE_STATMODEL$visualization_volcano_significance_cutoff, ui_html),
              info = "Significance cutoff namespace should be present")
})

test_that("All possible options in create_plot_type_selector", {
  ns <- NS("test_module")
  result <- create_plot_type_selector(ns)
  ui_html <- htmltools::renderTags(result)$html
  expect_true(grepl("Volcano Plot", ui_html),
              info = "Volcano Plot option should be present")
  expect_true(grepl("Heatmap", ui_html),
              info = "Heatmap option should be present")
  expect_true(grepl("Comparison Plot", ui_html),
              info = "Comparison Plot should be present")
  
})