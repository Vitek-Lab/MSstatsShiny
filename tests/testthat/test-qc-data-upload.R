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

test_that("qc_uploads_complete turnover needs protein + ratios (feature optional)", {
  # Turnover consumes the uploaded ratios table directly; FeatureLevelData is not
  # used on the response-curve path, so protein + ratios is sufficient.
  expect_true(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, FALSE, TRUE, TRUE))
  expect_true(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, TRUE, TRUE))

  # Missing ratios or protein leaves it incomplete; feature alone is not enough.
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, TRUE, FALSE))
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, FALSE, TRUE))
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, FALSE, FALSE))
})

test_that("qc_required_mapping_columns is template-specific", {
  expect_equal(MSstatsShiny:::qc_required_mapping_columns(TEMPLATES$protein_turnover),
               c("GROUP", "TimeVal"))
  expect_equal(MSstatsShiny:::qc_required_mapping_columns(TEMPLATES$chemoproteomics),
               c("GROUP", "DoseVal"))
  expect_equal(MSstatsShiny:::qc_required_mapping_columns(TEMPLATES$default), character(0))
  expect_equal(MSstatsShiny:::qc_required_mapping_columns(NULL), character(0))
})

test_that("qc_required_ratios_columns matches the turnover-ratios fit inputs", {
  expect_equal(MSstatsShiny:::qc_required_ratios_columns(),
               c("Protein", "TimeVal", "H_frac", "L_frac"))
})

test_that("qc_mapping_to_condition_metadata renames GROUP to Condition (turnover)", {
  parsed = data.frame(GROUP = c("0hr", "1hr"), TimeVal = c(0, 1),
                      stringsAsFactors = FALSE)
  out = MSstatsShiny:::qc_mapping_to_condition_metadata(parsed, TEMPLATES$protein_turnover)
  expect_equal(colnames(out), c("Condition", "TimeVal"))
  expect_equal(out$Condition, c("0hr", "1hr"))
  expect_type(out$Condition, "character")
  expect_type(out$TimeVal, "character")
})

test_that("qc_mapping_to_condition_metadata keeps optional chemo columns when present", {
  # DoseUnit / DrugName are passed through only when present in the CSV.
  bare = data.frame(GROUP = c("DMSO", "Drug_10nM"), DoseVal = c(0, 10),
                    stringsAsFactors = FALSE)
  out_bare = MSstatsShiny:::qc_mapping_to_condition_metadata(bare, TEMPLATES$chemoproteomics)
  expect_equal(colnames(out_bare), c("Condition", "DoseVal"))
  expect_type(out_bare$DoseVal, "character")

  full = data.frame(GROUP = c("DMSO", "Drug_10nM"), DoseVal = c(0, 10),
                    DoseUnit = c("nM", "nM"), DrugName = c("DMSO", "Drug"),
                    stringsAsFactors = FALSE)
  out_full = MSstatsShiny:::qc_mapping_to_condition_metadata(full, TEMPLATES$chemoproteomics)
  expect_equal(colnames(out_full), c("Condition", "DoseVal", "DoseUnit", "DrugName"))
  expect_type(out_full$DrugName, "character")
})
