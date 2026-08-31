# Tests for the QC UI module: namespaced input ids, the server-toggled
# visibility containers, and the single (collapsed) normalization select.

render_qc_ui_html <- function(id = "test") {
  ui <- MSstatsShiny::qcUI(id)
  htmltools::renderTags(ui)$html
}

count_occurrences <- function(haystack, needle) {
  matches <- gregexpr(needle, haystack, fixed = TRUE)[[1]]
  if (length(matches) == 1 && matches[1] == -1) 0L else length(matches)
}

test_that("qcUI returns a tagList", {
  ui <- MSstatsShiny::qcUI("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("qcUI renders the namespaced processing-option input ids", {
  html <- render_qc_ui_html("test")
  input_ids <- c("global_norm", "log", "summarization", "null", "maxQC", "norm",
                 "standards", "reference_norm", "remove_norm_channel", "features_used",
                 "censInt", "null1", "maxQC1", "MBi", "remove50",
                 "typequant", "format", "summ", "fname", "run", "update_results",
                 # Addressed server-side through NAMESPACE_QC, so a renamed or
                 # un-namespaced id leaves the panel and Clear button inert.
                 "tracer_constants_file", "tracer_constants_clear")
  for (id in input_ids) {
    expect_true(grepl(paste0('id="test-', id, '"'), html, fixed = TRUE),
                info = paste("Missing input id:", id))
  }
})

test_that("qcUI renders every server-toggled visibility container", {
  html <- render_qc_ui_html("test")
  container_ids <- c("global_norm_panel", "log_section", "summarization_panel",
                     "maxqc_msstats_panel", "norm_panel", "standards_panel",
                     "standards_type_section", "reference_norm_panel", "lf_options_panel",
                     "features_topn_panel", "censoring_section", "mbi_panel",
                     "profileplot_options_panel", "qualitymetrics_options_panel",
                     "nonptm_downloads_panel", "ptm_downloads_panel",
                     "data_upload_mapping_panel", "data_upload_ratios_panel",
                     "tracer_constants_panel")
  for (id in container_ids) {
    expect_true(grepl(paste0('id="test-', id, '"'), html, fixed = TRUE),
                info = paste("Missing container id:", id))
  }
})

test_that("qcUI declares the normalization select exactly once (duplicate-id collapse)", {
  html <- render_qc_ui_html("test")
  expect_equal(count_occurrences(html, 'id="test-norm"'), 1)
})

test_that("the collapsed normalization select keeps all label-free choices", {
  html <- render_qc_ui_html("test")
  for (choice in c("equalizeMedians", "quantile", "globalStandards")) {
    expect_true(grepl(choice, html, fixed = TRUE),
                info = paste("Missing normalization choice:", choice))
  }
})

test_that("qcUI mounts the tracer-constants status output", {
  html <- render_qc_ui_html("test")
  # uiOutput, not an input: without it the absent/pending/rejected/valid states
  # have nowhere to render.
  expect_true(grepl('id="test-tracer_constants_status"', html, fixed = TRUE))
})

test_that("the tracer-constants help text quotes CONSTANTS_QC, not a literal", {
  html <- render_qc_ui_html("test")
  # Fails if the bounds are revised in CONSTANTS_QC while the help text keeps a
  # hardcoded number.
  expect_true(grepl(paste0("between ", CONSTANTS_QC$tracer_min, " and ",
                           CONSTANTS_QC$tracer_max), html, fixed = TRUE))

  # Scoped to the panel's own markup: page-wide, "GROUP" appears in unrelated
  # uploads and would still match with this panel deleted.
  panel_start <- regexpr("test-tracer_constants_panel", html, fixed = TRUE)
  expect_gt(panel_start, 0)
  panel <- substr(html, panel_start, panel_start + 3000L)
  for (column in get_qc_required_tracer_columns()) {
    expect_true(grepl(column, panel, fixed = TRUE), info = column)
  }
})
