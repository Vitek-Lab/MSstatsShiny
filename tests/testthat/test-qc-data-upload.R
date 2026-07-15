# Truth-table tests for the QC Data Upload column-validation and
# upload-completeness helpers (pure functions, no Shiny session required).

test_that("qc_missing_upload_columns returns the required columns that are absent", {
  # All required columns present -> nothing missing.
  expect_equal(
    MSstatsShiny:::qc_missing_upload_columns(c("A", "B", "C"), c("A", "B")),
    character(0)
  )
  # Missing columns are reported in required order.
  expect_equal(
    MSstatsShiny:::qc_missing_upload_columns(c("A", "C"), c("A", "B", "D")),
    c("B", "D")
  )
  # Column order and extra present columns do not matter (name-based, not position).
  expect_equal(
    MSstatsShiny:::qc_missing_upload_columns(c("C", "B", "A", "Z"), c("A", "B", "C")),
    character(0)
  )
})

test_that("qc_required_protein_columns matches the summarization output keys", {
  expect_equal(
    MSstatsShiny:::qc_required_protein_columns(TEMPLATES$default),
    c("Protein", "GROUP", "RUN", "LogIntensities")
  )
  expect_equal(
    MSstatsShiny:::qc_required_protein_columns(TEMPLATES$chemoproteomics),
    c("Protein", "GROUP", "RUN", "LogIntensities")
  )
  # Turnover adds the heavy/light channel column (consumed in commit 2).
  expect_equal(
    MSstatsShiny:::qc_required_protein_columns(TEMPLATES$protein_turnover),
    c("Protein", "GROUP", "RUN", "LogIntensities", "LABEL")
  )
  # A NULL template (module called without app_template) falls back to the base set.
  expect_equal(
    MSstatsShiny:::qc_required_protein_columns(NULL),
    c("Protein", "GROUP", "RUN", "LogIntensities")
  )
})

test_that("qc_required_feature_columns is the MSstats feature-level key set", {
  expected = c("PROTEIN", "PEPTIDE", "FEATURE", "RUN", "GROUP", "LABEL", "INTENSITY")
  expect_equal(MSstatsShiny:::qc_required_feature_columns(TEMPLATES$default), expected)
  expect_equal(MSstatsShiny:::qc_required_feature_columns(TEMPLATES$chemoproteomics), expected)
  expect_equal(MSstatsShiny:::qc_required_feature_columns(TEMPLATES$protein_turnover), expected)
  expect_equal(MSstatsShiny:::qc_required_feature_columns(NULL), expected)
})

test_that("qc_uploads_complete needs both feature and protein tables (default/chemo)", {
  expect_true(MSstatsShiny:::qc_uploads_complete(TEMPLATES$default, TRUE, TRUE, FALSE))
  expect_true(MSstatsShiny:::qc_uploads_complete(TEMPLATES$chemoproteomics, TRUE, TRUE, FALSE))

  # Any single missing file leaves the upload incomplete.
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$default, TRUE, FALSE, FALSE))
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$default, FALSE, TRUE, FALSE))
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$default, FALSE, FALSE, FALSE))
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$chemoproteomics, FALSE, TRUE, FALSE))
})

test_that("qc_uploads_complete turnover branch is currently a two-file stub", {
  # Commit 2 will fold in uploaded ratios / metadata; for now it mirrors the
  # default rule and ignores has_turnover.
  expect_true(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, TRUE, TRUE))
  expect_true(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, TRUE, FALSE))
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, FALSE, TRUE))
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, FALSE, TRUE, TRUE))
})
