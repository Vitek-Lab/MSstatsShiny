# Truth-table tests for the QC Data Upload column-validation and
# upload-completeness helpers (pure functions, no Shiny session required).

test_that("get_missing_upload_columns returns the required columns that are absent", {
  # All required columns present -> nothing missing.
  expect_equal(
    MSstatsShiny:::get_missing_upload_columns(c("A", "B", "C"), c("A", "B")),
    character(0)
  )
  # Missing columns are reported in required order.
  expect_equal(
    MSstatsShiny:::get_missing_upload_columns(c("A", "C"), c("A", "B", "D")),
    c("B", "D")
  )
  # Column order and extra present columns do not matter (name-based, not position).
  expect_equal(
    MSstatsShiny:::get_missing_upload_columns(c("C", "B", "A", "Z"), c("A", "B", "C")),
    character(0)
  )
})

test_that("get_qc_required_protein_columns matches the summarization output keys", {
  expect_equal(
    MSstatsShiny:::get_qc_required_protein_columns(TEMPLATES$default),
    c("Protein", "GROUP", "RUN", "LogIntensities")
  )
  expect_equal(
    MSstatsShiny:::get_qc_required_protein_columns(TEMPLATES$chemoproteomics),
    c("Protein", "GROUP", "RUN", "LogIntensities")
  )
  # Turnover adds the heavy/light channel column.
  expect_equal(
    MSstatsShiny:::get_qc_required_protein_columns(TEMPLATES$protein_turnover),
    c("Protein", "GROUP", "RUN", "LogIntensities", "LABEL")
  )
  # A NULL template (module called without app_template) falls back to the base set.
  expect_equal(
    MSstatsShiny:::get_qc_required_protein_columns(NULL),
    c("Protein", "GROUP", "RUN", "LogIntensities")
  )
})

test_that("get_qc_required_feature_columns is the MSstats feature-level key set", {
  expected = c("PROTEIN", "PEPTIDE", "FEATURE", "RUN", "GROUP", "LABEL", "INTENSITY")
  expect_equal(MSstatsShiny:::get_qc_required_feature_columns(), expected)
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

test_that("qc_uploads_complete turnover needs feature + protein + ratios + mapping", {
  expect_true(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, TRUE, TRUE, TRUE))

  # Missing feature, protein, ratios, or mapping leaves it incomplete.
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, FALSE, TRUE, TRUE, TRUE))
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, FALSE, TRUE, TRUE))
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, TRUE, FALSE, TRUE))
  expect_false(MSstatsShiny:::qc_uploads_complete(TEMPLATES$protein_turnover, TRUE, TRUE, TRUE, FALSE))
})

test_that("get_qc_required_mapping_columns is template-specific", {
  expect_equal(MSstatsShiny:::get_qc_required_mapping_columns(TEMPLATES$protein_turnover),
               c("GROUP", "TimeVal"))
  expect_equal(MSstatsShiny:::get_qc_required_mapping_columns(TEMPLATES$chemoproteomics),
               c("GROUP", "DoseVal", "DoseUnit", "DrugName"))
  expect_equal(MSstatsShiny:::get_qc_required_mapping_columns(TEMPLATES$default), character(0))
  expect_equal(MSstatsShiny:::get_qc_required_mapping_columns(NULL), character(0))
})

test_that("get_qc_required_ratios_columns matches the turnover-ratios fit inputs", {
  expect_equal(MSstatsShiny:::get_qc_required_ratios_columns(),
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

test_that("qc_mapping_to_condition_metadata stores the same trimmed Condition that was validated", {
  # qc_mapping_group_errors compares TRIMMED values, so a quoted "DMSO " cell
  # (fread's strip.white leaves quoted fields alone) passes validation. If the
  # raw value were stored, it would then miss the ProteinLevelData join and
  # silently drop that condition's rows.
  parsed = data.frame(GROUP = c("DMSO ", " 6h"), TimeVal = c(0, 6),
                      stringsAsFactors = FALSE)
  protein_groups = c("DMSO", "6h")

  expect_length(MSstatsShiny:::qc_mapping_group_errors(parsed$GROUP, protein_groups), 0)

  out = MSstatsShiny:::qc_mapping_to_condition_metadata(parsed, TEMPLATES$protein_turnover)
  expect_equal(out$Condition, protein_groups)
  expect_true(all(out$Condition %in% protein_groups))
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

test_that("qc_values_numeric_finite allows NA / blank when allow_na = TRUE", {
  # Real missing values (NA and blank) pass for H_frac / L_frac.
  with_na = data.frame(H_frac = c(0.1, NA, 0.3), L_frac = c("0.9", "", "0.7"),
                       stringsAsFactors = FALSE)
  expect_true(MSstatsShiny:::qc_values_numeric_finite(with_na, c("H_frac", "L_frac"),
                                                      allow_na = TRUE))
  # A typo (non-numeric, non-blank) is still rejected.
  typo = data.frame(H_frac = c(0.1, 0.2, 0.3), L_frac = c("0.9", "abc", "0.7"),
                    stringsAsFactors = FALSE)
  expect_false(MSstatsShiny:::qc_values_numeric_finite(typo, c("H_frac", "L_frac"),
                                                       allow_na = TRUE))
  # Non-finite (Inf) is still rejected even with allow_na.
  inf = data.frame(H_frac = c(0.1, Inf, 0.3), stringsAsFactors = FALSE)
  expect_false(MSstatsShiny:::qc_values_numeric_finite(inf, "H_frac", allow_na = TRUE))
  # allow_na does not relax the default: NA still fails without the flag.
  na_default = data.frame(TimeVal = c(0, NA, 2), stringsAsFactors = FALSE)
  expect_false(MSstatsShiny:::qc_values_numeric_finite(na_default, "TimeVal"))
})

# ============================================================================
# Tracer-constant upload helpers (protein turnover).
# ============================================================================

test_that("get_qc_required_tracer_columns names the upload schema", {
  expect_equal(MSstatsShiny:::get_qc_required_tracer_columns(),
               c("GROUP", "TracerConstant"))
})

test_that("qc_tracer_values_in_range accepts the inclusive [min, max] band", {
  tracer_min = MSstatsShiny:::CONSTANTS_QC$tracer_min
  tracer_max = MSstatsShiny:::CONSTANTS_QC$tracer_max
  in_range = function(x) MSstatsShiny:::qc_tracer_values_in_range(
    data.frame(TracerConstant = x, stringsAsFactors = FALSE), "TracerConstant")

  # Both bounds are inclusive, and the band is populated in between.
  expect_true(in_range(tracer_min))
  expect_true(in_range(tracer_max))
  expect_true(in_range(c(tracer_min, 0.5, tracer_max)))

  # Just under the floor fails, so the boundary is >= and not >.
  expect_false(in_range(tracer_min - 0.001))
  # 0 divides to Inf; 1e-10 is worse because it passes a naive (0, 1] check and
  # yields a huge H_frac whose L_frac is then silently clamped to 0.
  expect_false(in_range(0))
  expect_false(in_range(1e-10))
  # Above the ceiling, negative, and non-finite.
  expect_false(in_range(tracer_max + 0.5))
  expect_false(in_range(-0.2))
  expect_false(in_range(NA_real_))
  expect_false(in_range(Inf))
  # One bad value among good ones fails the whole file.
  expect_false(in_range(c(0.5, NA_real_)))

  # A character column must be coerced before comparison: uncoerced,
  # all(c("0.5", "0.5abc") > 0 & <= 1) is TRUE and the bad cell only becomes NA
  # later, inside the analysis.
  expect_true(in_range(c("0.5", "1")))
  expect_false(in_range(c("0.5", "0.5abc")))

  # all(logical(0)) is TRUE, so empty input must be rejected explicitly.
  expect_false(in_range(character(0)))
  expect_false(in_range(numeric(0)))
  expect_false(MSstatsShiny:::qc_tracer_values_in_range(NULL, "TracerConstant"))
  # Absent column.
  expect_false(MSstatsShiny:::qc_tracer_values_in_range(
    data.frame(GROUP = "0h", stringsAsFactors = FALSE), "TracerConstant"))
})

test_that("qc_default_tracer_constants is all ones, named by condition", {
  defaults = MSstatsShiny:::qc_default_tracer_constants(c("0h", "6h", "24h"))
  # identical, not equal: the contract is a double vector, and expect_equal
  # would accept rep(1L, n).
  expect_identical(defaults, c("0h" = 1, "6h" = 1, "24h" = 1))

  # A named numeric(0) is not NULL, so it would pass calculateTurnoverRatios'
  # own guard and make every H_frac NA. Error instead.
  expect_error(MSstatsShiny:::qc_default_tracer_constants(character(0)),
               "no experimental conditions")
})

test_that("qc_resolve_tracer_constants defaults to ones when nothing is uploaded", {
  conditions = c("0h", "6h", "24h")
  expect_equal(MSstatsShiny:::qc_resolve_tracer_constants(conditions, NULL),
               c("0h" = 1, "6h" = 1, "24h" = 1))
  expect_equal(MSstatsShiny:::qc_resolve_tracer_constants(conditions, numeric(0)),
               c("0h" = 1, "6h" = 1, "24h" = 1))
})

test_that("qc_resolve_tracer_constants reindexes to condition order", {
  # File order must not leak through: calculateTurnoverRatios re-keys by name,
  # so the returned vector has to carry the raw condition strings in the
  # experiment's own order.
  uploaded = c("24h" = 0.93, "0h" = 0.98, "6h" = 0.95)
  expect_equal(
    MSstatsShiny:::qc_resolve_tracer_constants(c("0h", "6h", "24h"), uploaded),
    c("0h" = 0.98, "6h" = 0.95, "24h" = 0.93)
  )
})

test_that("qc_resolve_tracer_constants errors instead of reindexing to NA", {
  # uploaded[conditions] silently returns NA under an NA name for an uncovered
  # condition, and H_frac / NA is NA -- an empty result set with no warning.
  expect_error(
    MSstatsShiny:::qc_resolve_tracer_constants(c("0h", "6h"), c("0h" = 0.98)),
    "missing for condition\\(s\\): 6h"
  )
  # Unreadable value rather than absent key.
  expect_error(
    MSstatsShiny:::qc_resolve_tracer_constants(c("0h"), c("0h" = NA_real_)),
    "could not be read for condition\\(s\\): 0h"
  )
  # Zero conditions is a defect, not a default.
  expect_error(
    MSstatsShiny:::qc_resolve_tracer_constants(character(0), c("0h" = 0.98)),
    "no experimental conditions"
  )
})

test_that("qc_tracer_timepoint_hours mirrors the MSstatsResponse parser", {
  expect_equal(MSstatsShiny:::qc_tracer_timepoint_hours(c("0h", "6h", "24h")),
               c(0, 6, 24))
  # The number is anchored at the start of the string.
  expect_true(is.na(MSstatsShiny:::qc_tracer_timepoint_hours("Time_0h")))
  # Bare "d"/"w" matched anywhere, not just as a unit suffix.
  expect_equal(MSstatsShiny:::qc_tracer_timepoint_hours("1d"), 24)
  expect_equal(MSstatsShiny:::qc_tracer_timepoint_hours("6h_drug"), 144)
  expect_equal(MSstatsShiny:::qc_tracer_timepoint_hours("2w"), 336)
  # NA in, NA out. The upstream parser errors here ("NAs are not allowed in
  # subscripted assignments") once the vector is long enough for the day/week
  # subscript assignment to fire, so this needs a length > 1 case to bite.
  expect_true(is.na(MSstatsShiny:::qc_tracer_timepoint_hours(NA_character_)))
  expect_equal(MSstatsShiny:::qc_tracer_timepoint_hours(c("1d", NA, "2w")),
               c(24, NA, 336))
  expect_equal(MSstatsShiny:::qc_tracer_timepoint_hours(c("6h_drug", NA)),
               c(144, NA))
})

test_that("qc_tracer_misleading_units flags a stray d or w, not any suffix", {
  flags = function(x) MSstatsShiny:::qc_tracer_misleading_units(x)
  # The "d" of "drug" and the "w" of "washout" fire the day/week multipliers.
  expect_true(flags("6h_drug"))
  expect_true(flags("24h_washout"))
  # A genuine day/week unit is not misleading.
  expect_equal(flags(c("1d", "2w", "3 days", "4 weeks", "5wk")), rep(FALSE, 5))
  # A suffix carrying no "d" or "w" never fires the multiplier, so leaving it
  # alone avoids rejecting a run that would have been correct.
  expect_equal(flags(c("0h", "24h_rep1", "168hrs", "12hrs")), rep(FALSE, 4))
  expect_false(flags(NA_character_))
})

test_that("qc_tracer_timepoint_errors rejects unparseable and colliding names", {
  # The working dataset's conditions all parse, and none collide.
  expect_equal(
    MSstatsShiny:::qc_tracer_timepoint_errors(
      c("0hr", "1hr", "4hr", "12hrs", "24hrs", "48hrs", "96hrs", "168hrs")),
    character(0)
  )
  expect_equal(MSstatsShiny:::qc_tracer_timepoint_errors(c("0h", "6h", "24h")),
               character(0))
  expect_equal(MSstatsShiny:::qc_tracer_timepoint_errors(character(0)),
               character(0))

  # Resolves to NA -> would drop every row of that condition.
  unparseable = MSstatsShiny:::qc_tracer_timepoint_errors(c("Time_0h", "6h"))
  expect_length(unparseable, 1)
  expect_match(unparseable, "Time_0h", fixed = TRUE)
  expect_match(unparseable, "annotation file")

  # "24h" and "1d" both resolve to 24, so the second silently takes the first's
  # constant.
  collision = MSstatsShiny:::qc_tracer_timepoint_errors(c("0h", "24h", "1d"))
  expect_length(collision, 1)
  expect_match(collision, "same timepoint (24 hours)", fixed = TRUE)
  expect_match(collision, "24h", fixed = TRUE)
  expect_match(collision, "1d", fixed = TRUE)

  # A stray unit letter resolves to a wrong-but-plausible timepoint, which
  # neither the NA check nor the collision check would catch.
  misleading = MSstatsShiny:::qc_tracer_timepoint_errors(c("0h", "6h_drug"))
  expect_length(misleading, 1)
  expect_match(misleading, "6h_drug", fixed = TRUE)
  expect_match(misleading, "144 hours", fixed = TRUE)
  # A suffix with no "d" or "w" is left alone.
  expect_equal(MSstatsShiny:::qc_tracer_timepoint_errors(c("0h", "24h_rep1")),
               character(0))

  # Conditions are de-duplicated first, so a repeated name is not a collision
  # with itself.
  expect_equal(MSstatsShiny:::qc_tracer_timepoint_errors(c("6h", "6h")),
               character(0))

  # All three failure modes at once are reported separately.
  expect_length(
    MSstatsShiny:::qc_tracer_timepoint_errors(c("Time_0h", "6h_drug", "24h", "1d")), 3)
})

test_that("qc_tracer_values_in_range does not read factor level codes", {
  # as.numeric(factor("2.0")) is 1, so an unconverted factor would validate an
  # out-of-range value and reject valid ones.
  expect_false(MSstatsShiny:::qc_tracer_values_in_range(
    data.frame(TracerConstant = factor("2.0"))))
  expect_true(MSstatsShiny:::qc_tracer_values_in_range(
    data.frame(TracerConstant = factor(c("0.5", "0.7")))))
  # Degenerate column arguments return FALSE rather than throwing from `if`.
  expect_false(MSstatsShiny:::qc_tracer_values_in_range(
    data.frame(TracerConstant = 0.5), character(0)))
  expect_false(MSstatsShiny:::qc_tracer_values_in_range(
    data.frame(TracerConstant = 0.5), NA_character_))
})

test_that("qc_resolve_tracer_constants matches on trimmed names", {
  # condition_metadata is never normalized but fread strips whitespace, so an
  # Excel-sourced trailing space would otherwise be unmatchable -- and
  # unfixable, since the metadata table disables editing on Condition.
  resolved = MSstatsShiny:::qc_resolve_tracer_constants(
    c("0h ", "6h"), c("0h" = 0.98, " 6h" = 0.95))
  # Matching is trimmed; the returned NAMES stay raw, because
  # calculateTurnoverRatios re-keys them through parse_timepoint.
  expect_equal(resolved, c("0h " = 0.98, "6h" = 0.95))
  # Case stays strict.
  expect_error(
    MSstatsShiny:::qc_resolve_tracer_constants(c("0H"), c("0h" = 0.98)),
    "missing for condition"
  )
})

test_that("qc_resolve_tracer_constants rejects a condition listed twice", {
  # Silently taking the first of two rows is the same class of failure as a
  # duplicated column: a corrected value loses to the stale one above it.
  expect_error(
    MSstatsShiny:::qc_resolve_tracer_constants("0h", c("0h" = 0.8, "0h" = 0.2)),
    "more than once: 0h"
  )
})

test_that("qc_mapping_group_errors matches on trimmed names", {
  expect_equal(MSstatsShiny:::qc_mapping_group_errors(c("0h "), c("0h")),
               character(0))
  # Case is deliberately not folded.
  expect_length(MSstatsShiny:::qc_mapping_group_errors(c("0H"), c("0h")), 2)
})

test_that("qc_mapping_group_errors labels are parameterized, defaults unchanged", {
  # Defaults still name the GROUP mapping and ProteinLevelData, so the existing
  # caller's messages are untouched.
  default_msg = MSstatsShiny:::qc_mapping_group_errors(c("A"), c("A", "B"))
  expect_match(default_msg, "GROUP mapping is missing row(s) for ProteinLevelData GROUP(s): B.",
               fixed = TRUE)

  # A different upload names itself and its own reference, so the message does
  # not send the user off to edit an unrelated file.
  tracer_msg = MSstatsShiny:::qc_mapping_group_errors(
    c("0h", "99h"), c("0h", "6h"),
    subject = "Tracer constants file", reference = "the annotation")
  expect_length(tracer_msg, 2)
  expect_match(tracer_msg[1],
               "Tracer constants file has GROUP value(s) not found in the annotation: 99h.",
               fixed = TRUE)
  expect_match(tracer_msg[2],
               "Tracer constants file is missing row(s) for the annotation GROUP(s): 6h.",
               fixed = TRUE)
  expect_false(any(grepl("ProteinLevelData", tracer_msg, fixed = TRUE)))
})
