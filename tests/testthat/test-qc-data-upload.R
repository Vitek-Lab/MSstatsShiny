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
  # Turnover adds the heavy/light channel column.
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

test_that("qc_uploads_complete needs both feature and protein tables (default)", {
  # Default template ignores the mapping-validity flag.
  expect_true(MSstatsShiny:::qc_uploads_complete(TEMPLATES$default, TRUE, TRUE, FALSE, FALSE))

  # Any single missing file leaves the upload incomplete.
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$default, TRUE, FALSE, FALSE, FALSE))
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$default, FALSE, TRUE, FALSE, FALSE))
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$default, FALSE, FALSE, FALSE, FALSE))
})

test_that("qc_uploads_complete chemo needs feature + protein + valid mapping", {
  expect_true(MSstatsShiny:::qc_uploads_complete(TEMPLATES$chemoproteomics, TRUE, TRUE, FALSE, TRUE))

  # Without a valid mapping the chemo upload is not ready, even with both files.
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$chemoproteomics, TRUE, TRUE, FALSE, FALSE))
  # A missing file is still incomplete regardless of mapping.
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$chemoproteomics, FALSE, TRUE, FALSE, TRUE))
})

test_that("qc_uploads_complete turnover needs protein + ratios + mapping (feature optional)", {
  # Turnover consumes the uploaded ratios table directly; FeatureLevelData is not
  # used on the response-curve path, so protein + ratios + a valid mapping suffice.
  expect_true(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, FALSE, TRUE, TRUE, TRUE))
  expect_true(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, TRUE, TRUE, TRUE))

  # Missing ratios, protein, or mapping leaves it incomplete.
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, TRUE, TRUE, FALSE))
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, TRUE, FALSE, TRUE))
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, FALSE, TRUE, TRUE))
})

test_that("qc_required_mapping_columns is template-specific", {
  expect_equal(MSstatsShiny:::qc_required_mapping_columns(TEMPLATES$protein_turnover),
               c("GROUP", "TimeVal"))
  expect_equal(MSstatsShiny:::qc_required_mapping_columns(TEMPLATES$chemoproteomics),
               c("GROUP", "DoseVal", "DoseUnit", "DrugName"))
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

test_that("qc_dose_units_valid accepts known units case-insensitively", {
  expect_true(MSstatsShiny:::qc_dose_units_valid(c("nM", "uM", "mM", "M")))
  expect_true(MSstatsShiny:::qc_dose_units_valid(c("nm", "UM", " mm ", "m")))

  # Unknown, blank, or NA units are rejected.
  expect_false(MSstatsShiny:::qc_dose_units_valid(c("nM", "ng")))
  expect_false(MSstatsShiny:::qc_dose_units_valid(c("nM", "")))
  expect_false(MSstatsShiny:::qc_dose_units_valid(c("nM", NA)))
  expect_false(MSstatsShiny:::qc_dose_units_valid(character(0)))
  expect_false(MSstatsShiny:::qc_dose_units_valid(NULL))
})

test_that("qc_mapping_group_errors flags unknown, missing, and duplicate groups", {
  protein_groups = c("DMSO", "D1", "D2")

  # Exact one-to-one mapping -> no errors.
  expect_equal(
    MSstatsShiny:::qc_mapping_group_errors(c("DMSO", "D1", "D2"), protein_groups),
    character(0)
  )
  # Unknown mapping group.
  unknown = MSstatsShiny:::qc_mapping_group_errors(c("DMSO", "D1", "D2", "D9"), protein_groups)
  expect_length(unknown, 1)
  expect_match(unknown, "not found in ProteinLevelData")
  # Missing ProteinLevelData group.
  missing = MSstatsShiny:::qc_mapping_group_errors(c("DMSO", "D1"), protein_groups)
  expect_length(missing, 1)
  expect_match(missing, "missing")
  # Duplicate mapping rows.
  dup = MSstatsShiny:::qc_mapping_group_errors(c("DMSO", "D1", "D1", "D2"), protein_groups)
  expect_length(dup, 1)
  expect_match(dup, "duplicate")
})

test_that("qc_values_numeric_finite requires numeric, finite values in all columns", {
  ok = data.frame(TimeVal = c(0, 1, 2), H_frac = c(0.1, 0.2, 0.3),
                  L_frac = c(0.9, 0.8, 0.7), stringsAsFactors = FALSE)
  expect_true(MSstatsShiny:::qc_values_numeric_finite(ok, c("TimeVal", "H_frac", "L_frac")))

  # Non-numeric value.
  bad_char = ok
  bad_char$H_frac = c("a", "0.2", "0.3")
  expect_false(MSstatsShiny:::qc_values_numeric_finite(bad_char, c("TimeVal", "H_frac", "L_frac")))
  # Non-finite value.
  bad_inf = ok
  bad_inf$L_frac = c(0.9, Inf, 0.7)
  expect_false(MSstatsShiny:::qc_values_numeric_finite(bad_inf, c("TimeVal", "H_frac", "L_frac")))
  # NA value.
  bad_na = ok
  bad_na$TimeVal = c(0, NA, 2)
  expect_false(MSstatsShiny:::qc_values_numeric_finite(bad_na, c("TimeVal", "H_frac", "L_frac")))
  # Absent column.
  expect_false(MSstatsShiny:::qc_values_numeric_finite(ok, c("TimeVal", "Missing")))
})
