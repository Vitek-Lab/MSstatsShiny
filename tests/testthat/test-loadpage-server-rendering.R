# Truth-table tests for the DIANN-cluster visibility predicates extracted in
# Phase 1 of the loadpage refactor. Each predicate is a pure transform of a
# few Shiny input values — these tests pin the JS-condition semantics that
# previously lived in conditionalPanel(condition = "...") expressions in
# module-loadpage-ui.R.

test_that("loadpage_show_diann_lf_options is TRUE only when diann + LType + small-file", {
  expect_true(
    MSstatsShiny:::loadpage_show_diann_lf_options("diann", "LType", FALSE)
  )
  expect_true(
    MSstatsShiny:::loadpage_show_diann_lf_options("diann", "LType", NULL)
  )

  # wrong converter
  expect_false(
    MSstatsShiny:::loadpage_show_diann_lf_options("sky", "LType", FALSE)
  )
  expect_false(
    MSstatsShiny:::loadpage_show_diann_lf_options("spec", "LType", FALSE)
  )
  # wrong label type
  expect_false(
    MSstatsShiny:::loadpage_show_diann_lf_options("diann", "TMT", FALSE)
  )
  # big-file mode active
  expect_false(
    MSstatsShiny:::loadpage_show_diann_lf_options("diann", "LType", TRUE)
  )
})

test_that("loadpage_show_diann_intensity_column inverts diann_2plus", {
  expect_true(MSstatsShiny:::loadpage_show_diann_intensity_column(FALSE))
  expect_true(MSstatsShiny:::loadpage_show_diann_intensity_column(NULL))
  expect_false(MSstatsShiny:::loadpage_show_diann_intensity_column(TRUE))
})

test_that("loadpage_show_qval_filter is TRUE for sky/spec and diann-small-file", {
  expect_true(MSstatsShiny:::loadpage_show_qval_filter("sky", FALSE))
  expect_true(MSstatsShiny:::loadpage_show_qval_filter("sky", TRUE))   # sky doesn't read big_file_diann
  expect_true(MSstatsShiny:::loadpage_show_qval_filter("spec", FALSE))
  expect_true(MSstatsShiny:::loadpage_show_qval_filter("spec", TRUE))  # spec ignores big_file_diann

  expect_true(MSstatsShiny:::loadpage_show_qval_filter("diann", FALSE))
  expect_false(MSstatsShiny:::loadpage_show_qval_filter("diann", TRUE))

  expect_false(MSstatsShiny:::loadpage_show_qval_filter("maxq", FALSE))
  expect_false(MSstatsShiny:::loadpage_show_qval_filter("PD", FALSE))
  expect_false(MSstatsShiny:::loadpage_show_qval_filter("open", FALSE))
  expect_false(MSstatsShiny:::loadpage_show_qval_filter(NULL, FALSE))
})

test_that("loadpage_show_qval_cutoff is the q_val checkbox", {
  expect_true(MSstatsShiny:::loadpage_show_qval_cutoff(TRUE))
  expect_false(MSstatsShiny:::loadpage_show_qval_cutoff(FALSE))
  expect_false(MSstatsShiny:::loadpage_show_qval_cutoff(NULL))
})

test_that("loadpage_show_diann_mbr requires both q_val and diann", {
  expect_true(MSstatsShiny:::loadpage_show_diann_mbr(TRUE, "diann"))

  expect_false(MSstatsShiny:::loadpage_show_diann_mbr(FALSE, "diann"))
  expect_false(MSstatsShiny:::loadpage_show_diann_mbr(NULL, "diann"))
  expect_false(MSstatsShiny:::loadpage_show_diann_mbr(TRUE, "sky"))
  expect_false(MSstatsShiny:::loadpage_show_diann_mbr(TRUE, "spec"))
  expect_false(MSstatsShiny:::loadpage_show_diann_mbr(TRUE, NULL))
})

test_that("loadpage_show_diann_anomaly is TRUE for diann + small-file only", {
  expect_true(MSstatsShiny:::loadpage_show_diann_anomaly("diann", FALSE))
  expect_true(MSstatsShiny:::loadpage_show_diann_anomaly("diann", NULL))

  expect_false(MSstatsShiny:::loadpage_show_diann_anomaly("diann", TRUE))
  expect_false(MSstatsShiny:::loadpage_show_diann_anomaly("sky", FALSE))
  expect_false(MSstatsShiny:::loadpage_show_diann_anomaly("spec", FALSE))
  expect_false(MSstatsShiny:::loadpage_show_diann_anomaly(NULL, FALSE))
})

test_that("loadpage_show_diann_anomaly_run_order is the anomaly checkbox", {
  expect_true(MSstatsShiny:::loadpage_show_diann_anomaly_run_order(TRUE))
  expect_false(MSstatsShiny:::loadpage_show_diann_anomaly_run_order(FALSE))
  expect_false(MSstatsShiny:::loadpage_show_diann_anomaly_run_order(NULL))
})

test_that("loadpage_show_big_diann_anomaly_run_order is the big-file anomaly checkbox", {
  expect_true(MSstatsShiny:::loadpage_show_big_diann_anomaly_run_order(TRUE))
  expect_false(MSstatsShiny:::loadpage_show_big_diann_anomaly_run_order(FALSE))
  expect_false(MSstatsShiny:::loadpage_show_big_diann_anomaly_run_order(NULL))
})

test_that("NAMESPACE_LOADPAGE retains literal string values (no renames in Phase 1)", {
  expect_equal(NAMESPACE_LOADPAGE$bio, "BIO")
  expect_equal(NAMESPACE_LOADPAGE$dda_dia, "DDA_DIA")
  expect_equal(NAMESPACE_LOADPAGE$filetype, "filetype")
  expect_equal(NAMESPACE_LOADPAGE$proceed1, "proceed1")
  expect_equal(NAMESPACE_LOADPAGE$q_val, "q_val")
  expect_equal(NAMESPACE_LOADPAGE$q_cutoff, "q_cutoff")
  expect_equal(NAMESPACE_LOADPAGE$mbr, "MBR")
  expect_equal(NAMESPACE_LOADPAGE$intensity_column, "intensity_column")
  expect_equal(NAMESPACE_LOADPAGE$diann_2plus, "diann_2plus")
  expect_equal(NAMESPACE_LOADPAGE$diann_calculate_anomaly_scores,
               "diann_calculate_anomaly_scores")
  expect_equal(NAMESPACE_LOADPAGE$diann_run_order_file, "diann_run_order_file")
  expect_equal(NAMESPACE_LOADPAGE$big_file_diann, "big_file_diann")
  expect_equal(NAMESPACE_LOADPAGE$big_diann_calculate_anomaly_scores,
               "big_diann_calculate_anomaly_scores")
  expect_equal(NAMESPACE_LOADPAGE$big_diann_run_order_file,
               "big_diann_run_order_file")
})
