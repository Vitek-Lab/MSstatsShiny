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

test_that("loadpage_show_mzmine_upload is TRUE only for the mzmine converter", {
  expect_true(MSstatsShiny:::loadpage_show_mzmine_upload("mzmine"))

  expect_false(MSstatsShiny:::loadpage_show_mzmine_upload("diann"))
  expect_false(MSstatsShiny:::loadpage_show_mzmine_upload("sky"))
  expect_false(MSstatsShiny:::loadpage_show_mzmine_upload("sample"))
  expect_false(MSstatsShiny:::loadpage_show_mzmine_upload(NULL))
})

test_that("LOADPAGE_METABOLOMICS_FILETYPE_CHOICES offers MZmine and MSstats Format", {
  expect_equal(LOADPAGE_METABOLOMICS_FILETYPE_CHOICES,
               c("MZmine" = "mzmine", "MSstats Format" = "msstats"))
  expect_equal(names(LOADPAGE_METABOLOMICS_FILETYPE_CHOICES),
               c("MZmine", "MSstats Format"))
  expect_true(all(c("mzmine", "msstats") %in%
                    LOADPAGE_METABOLOMICS_FILETYPE_CHOICES))
})

test_that("loadpage_show_diann_mbr requires both q_val and diann", {
  expect_true(MSstatsShiny:::loadpage_show_diann_mbr(TRUE, "diann"))

  expect_false(MSstatsShiny:::loadpage_show_diann_mbr(FALSE, "diann"))
  expect_false(MSstatsShiny:::loadpage_show_diann_mbr(NULL, "diann"))
  expect_false(MSstatsShiny:::loadpage_show_diann_mbr(TRUE, "sky"))
  expect_false(MSstatsShiny:::loadpage_show_diann_mbr(TRUE, "spec"))
  expect_false(MSstatsShiny:::loadpage_show_diann_mbr(TRUE, NULL))
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


# ============================================================================
# Phase 2 predicate truth tables + namespace assertions
# ============================================================================

test_that("loadpage_show_sample_dataset_description matches one mode at a time", {
  # All cases below hold the picker VISIBLE (bio = "Protein", dda_dia = "LType")
  # so they isolate the filetype/LabelFreeType logic; the picker-hidden orphan
  # case is covered by the regression test below.
  for (mode in c("DDA", "DIA", "SRM_PRM")) {
    # Active mode positive
    expect_true(MSstatsShiny:::loadpage_show_sample_dataset_description("sample", mode, mode, "Protein", "LType"),
                info = paste("active mode", mode))
    # Wrong filetype always FALSE
    for (ft in c("diann", "sky", "spec", "maxq", "msstats", NULL)) {
      expect_false(MSstatsShiny:::loadpage_show_sample_dataset_description(ft, mode, mode, "Protein", "LType"),
                   info = paste("ft", ft %||% "NULL", "mode", mode))
    }
    # Wrong LabelFreeType FALSE
    other_modes <- setdiff(c("DDA", "DIA", "SRM_PRM"), mode)
    for (other in other_modes) {
      expect_false(MSstatsShiny:::loadpage_show_sample_dataset_description("sample", other, mode, "Protein", "LType"),
                   info = paste("target", mode, "actual", other))
    }
    # NULL LabelFreeType FALSE
    expect_false(MSstatsShiny:::loadpage_show_sample_dataset_description("sample", NULL, mode, "Protein", "LType"))
  }
})

test_that("loadpage_show_sample_dataset_description hides with the picker (orphaned-description regression)", {
  # CodeRabbit orphan: filetype stays 'sample' and a LabelFreeType value is
  # still selected, but the picker hides because BIO == 'PTM' or DDA_DIA leaves
  # 'LType'. The description MUST hide with the picker — it now composes on
  # loadpage_show_sample_dataset_label_free_type_selector, so a visible
  # description implies a visible picker by construction.

  # Positive control: picker visible -> description shows.
  expect_true(MSstatsShiny:::loadpage_show_sample_dataset_description(
    "sample", "DDA", "DDA", "Protein", "LType"))

  # BIO flipped to PTM -> picker hidden -> description hidden (was the orphan).
  expect_false(MSstatsShiny:::loadpage_show_sample_dataset_description(
    "sample", "DDA", "DDA", "PTM", "LType"))
  # DDA_DIA left LType (e.g. TMT) -> picker hidden -> description hidden.
  expect_false(MSstatsShiny:::loadpage_show_sample_dataset_description(
    "sample", "DDA", "DDA", "Protein", "TMT"))
})

test_that("loadpage_show_sample_dataset_label_free_type_selector requires non-PTM + sample + LType", {
  expect_true(MSstatsShiny:::loadpage_show_sample_dataset_label_free_type_selector("Protein", "sample", "LType"))
  expect_true(MSstatsShiny:::loadpage_show_sample_dataset_label_free_type_selector("Peptide", "sample", "LType"))

  # NULL bio behaves like "not PTM" by design — the original JS condition
  # `input['loadpage-BIO'] != 'PTM'` is TRUE for unset BIO (in JS,
  # `null != 'PTM'` is true), so the original `conditionalPanel` did show
  # the LabelFreeType selector at startup once `filetype=='sample'` and
  # `DDA_DIA=='LType'` were selected even if BIO was still untouched. The
  # predicate mirrors that behavior; do not regress it.
  expect_true(MSstatsShiny:::loadpage_show_sample_dataset_label_free_type_selector(NULL, "sample", "LType"))

  expect_false(MSstatsShiny:::loadpage_show_sample_dataset_label_free_type_selector("PTM", "sample", "LType"))
  expect_false(MSstatsShiny:::loadpage_show_sample_dataset_label_free_type_selector("Protein", "diann", "LType"))
  expect_false(MSstatsShiny:::loadpage_show_sample_dataset_label_free_type_selector("Protein", "sample", "TMT"))
})

test_that("loadpage_show_standard_quant_upload covers non-PTM converters only", {
  for (ft in c("10col", "prog", "PD", "open", "openms", "spmin", "phil", "meta")) {
    expect_true(MSstatsShiny:::loadpage_show_standard_quant_upload(ft, "Protein"),
                info = paste("ft", ft))
    expect_true(MSstatsShiny:::loadpage_show_standard_quant_upload(ft, "Peptide"),
                info = paste("ft Peptide", ft))
    expect_false(MSstatsShiny:::loadpage_show_standard_quant_upload(ft, "PTM"),
                 info = paste("ft", ft, "PTM"))
  }
  for (ft in c("diann", "sky", "spec", "maxq", "ump", "msstats", "sample", NULL)) {
    expect_false(MSstatsShiny:::loadpage_show_standard_quant_upload(ft, "Protein"),
                 info = paste("excluded ft", ft %||% "NULL"))
  }
})

test_that("loadpage_show_standard_annot_upload — Spectronaut/DIANN gated by big-file", {
  expect_false(MSstatsShiny:::loadpage_show_standard_annot_upload("spec", "Protein", FALSE, FALSE) == FALSE)
  expect_true(MSstatsShiny:::loadpage_show_standard_annot_upload("spec",  "Protein", FALSE, FALSE))
  expect_false(MSstatsShiny:::loadpage_show_standard_annot_upload("spec", "Protein", TRUE,  FALSE))
  expect_true(MSstatsShiny:::loadpage_show_standard_annot_upload("diann", "Protein", FALSE, FALSE))
  expect_false(MSstatsShiny:::loadpage_show_standard_annot_upload("diann","Protein", FALSE, TRUE))

  for (ft in c("sky", "prog", "PD", "open", "spmin", "phil", "meta")) {
    expect_true(MSstatsShiny:::loadpage_show_standard_annot_upload(ft, "Protein", FALSE, FALSE),
                info = paste("ft", ft))
    expect_false(MSstatsShiny:::loadpage_show_standard_annot_upload(ft, "PTM",     FALSE, FALSE),
                 info = paste("ft", ft, "PTM"))
  }
  expect_false(MSstatsShiny:::loadpage_show_standard_annot_upload("maxq",   "Protein", FALSE, FALSE))
  expect_false(MSstatsShiny:::loadpage_show_standard_annot_upload(NULL,     "Protein", FALSE, FALSE))
})

test_that("loadpage_show_msstats_label_free_upload is non-PTM label-free only", {
  expect_true(MSstatsShiny:::loadpage_show_msstats_label_free_upload("msstats", "Protein", "LType"))
  expect_true(MSstatsShiny:::loadpage_show_msstats_label_free_upload("msstats", "Peptide", "LType"))
  expect_false(MSstatsShiny:::loadpage_show_msstats_label_free_upload("msstats", "Protein", "TMT"))
  expect_false(MSstatsShiny:::loadpage_show_msstats_label_free_upload("msstats", "PTM",     "LType"))
  expect_false(MSstatsShiny:::loadpage_show_msstats_label_free_upload("diann",   "Protein", "LType"))
})

test_that("loadpage_show_msstats_ptm_upload is PTM only (collapsed TMT clause)", {
  expect_true(MSstatsShiny:::loadpage_show_msstats_ptm_upload("msstats", "PTM"))
  expect_false(MSstatsShiny:::loadpage_show_msstats_ptm_upload("msstats", "Protein"))
  expect_false(MSstatsShiny:::loadpage_show_msstats_ptm_upload("diann", "PTM"))
})

test_that("loadpage_show_skyline_upload is non-PTM Skyline", {
  expect_true(MSstatsShiny:::loadpage_show_skyline_upload("sky", "Protein"))
  expect_true(MSstatsShiny:::loadpage_show_skyline_upload("sky", "Peptide"))
  expect_false(MSstatsShiny:::loadpage_show_skyline_upload("sky", "PTM"))
  expect_false(MSstatsShiny:::loadpage_show_skyline_upload("diann", "Protein"))
})

test_that("loadpage_show_ptm_fragpipe_upload is PTM FragPipe only", {
  expect_true(MSstatsShiny:::loadpage_show_ptm_fragpipe_upload("phil", "PTM"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_fragpipe_upload("phil", "Protein"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_fragpipe_upload("maxq", "PTM"))
})

test_that("loadpage_show_maxquant_upload is non-PTM MaxQuant under TMT or LType", {
  for (dd in c("TMT", "LType")) {
    expect_true(MSstatsShiny:::loadpage_show_maxquant_upload("maxq", "Protein", dd),
                info = paste("dd", dd))
    expect_false(MSstatsShiny:::loadpage_show_maxquant_upload("maxq", "PTM", dd),
                 info = paste("dd", dd, "PTM"))
  }
  expect_false(MSstatsShiny:::loadpage_show_maxquant_upload("sky", "Protein", "TMT"))
  expect_false(MSstatsShiny:::loadpage_show_maxquant_upload("maxq", "Protein", NULL))
})

test_that("loadpage_show_ptm_uploads collapses the redundant TMT clause", {
  for (ft in c("maxq", "PD", "spec", "sky", "meta")) {
    expect_true(MSstatsShiny:::loadpage_show_ptm_uploads(ft, "PTM"),
                info = paste("ft", ft))
    expect_false(MSstatsShiny:::loadpage_show_ptm_uploads(ft, "Protein"),
                 info = paste("ft", ft, "Protein"))
  }
  expect_false(MSstatsShiny:::loadpage_show_ptm_uploads("diann", "PTM"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_uploads("phil",  "PTM"))  # phil has its own uploader
  expect_false(MSstatsShiny:::loadpage_show_ptm_uploads(NULL,    "PTM"))
})

test_that("loadpage_show_ptm_maxquant_pgroup is MaxQuant PTM only", {
  expect_true(MSstatsShiny:::loadpage_show_ptm_maxquant_pgroup("maxq", "PTM"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_maxquant_pgroup("maxq", "Protein"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_maxquant_pgroup("PD",   "PTM"))
})

test_that("loadpage_show_ptm_metamorpheus_extras is Metamorpheus PTM only", {
  expect_true(MSstatsShiny:::loadpage_show_ptm_metamorpheus_extras("meta", "PTM"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_metamorpheus_extras("meta", "Protein"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_metamorpheus_extras("maxq", "PTM"))
})

test_that("loadpage_show_ptm_fasta_id_column matches ptm_uploads gate exactly", {
  for (ft in c("maxq", "PD", "spec", "sky", "meta", "diann", "phil", NULL)) {
    for (bio in c("PTM", "Protein", "Peptide")) {
      expect_equal(
        MSstatsShiny:::loadpage_show_ptm_fasta_id_column(ft, bio),
        MSstatsShiny:::loadpage_show_ptm_uploads(ft, bio),
        info = paste("ft", ft %||% "NULL", "bio", bio)
      )
    }
  }
})

test_that("loadpage_show_ptm_mod_id_maxq / pd / spec gate on PTM + the matching filetype", {
  expect_true(MSstatsShiny:::loadpage_show_ptm_mod_id_maxq("maxq", "PTM"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_mod_id_maxq("maxq", "Protein"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_mod_id_maxq("PD",   "PTM"))

  expect_true(MSstatsShiny:::loadpage_show_ptm_mod_id_pd("PD", "PTM"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_mod_id_pd("PD", "Protein"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_mod_id_pd("maxq", "PTM"))

  expect_true(MSstatsShiny:::loadpage_show_ptm_mod_id_spec("spec", "PTM"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_mod_id_spec("spec", "Protein"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_mod_id_spec("PD",   "PTM"))
})

test_that("loadpage_show_dia_umpire_upload is just filetype == 'ump'", {
  expect_true(MSstatsShiny:::loadpage_show_dia_umpire_upload("ump"))
  expect_false(MSstatsShiny:::loadpage_show_dia_umpire_upload("diann"))
  expect_false(MSstatsShiny:::loadpage_show_dia_umpire_upload(NULL))
})

test_that("loadpage_show_label_free_options excludes sample + big-file paths", {
  # baseline label-free converter visible
  expect_true(MSstatsShiny:::loadpage_show_label_free_options("sky", "LType", FALSE, FALSE))
  expect_true(MSstatsShiny:::loadpage_show_label_free_options("maxq", "LType", FALSE, FALSE))
  # sample excluded
  expect_false(MSstatsShiny:::loadpage_show_label_free_options("sample", "LType", FALSE, FALSE))
  # TMT excluded
  expect_false(MSstatsShiny:::loadpage_show_label_free_options("maxq", "TMT", FALSE, FALSE))
  # NULL filetype excluded
  expect_false(MSstatsShiny:::loadpage_show_label_free_options(NULL, "LType", FALSE, FALSE))
  # big-file Spectronaut excluded; small-file allowed
  expect_false(MSstatsShiny:::loadpage_show_label_free_options("spec",  "LType", TRUE,  FALSE))
  expect_true(MSstatsShiny:::loadpage_show_label_free_options("spec",  "LType", FALSE, FALSE))
  # big-file DIANN excluded; small-file allowed
  expect_false(MSstatsShiny:::loadpage_show_label_free_options("diann", "LType", FALSE, TRUE))
  expect_true(MSstatsShiny:::loadpage_show_label_free_options("diann", "LType", FALSE, FALSE))
})

test_that("loadpage_show_openswath_mscore is filetype == 'open'", {
  expect_true(MSstatsShiny:::loadpage_show_openswath_mscore("open"))
  expect_false(MSstatsShiny:::loadpage_show_openswath_mscore("sky"))
  expect_false(MSstatsShiny:::loadpage_show_openswath_mscore(NULL))
})

test_that("loadpage_show_openswath_mscore_cutoff gates on full ancestor chain (regression check)", {
  # Both clauses TRUE → visible
  expect_true(MSstatsShiny:::loadpage_show_openswath_mscore_cutoff("open", TRUE))

  # m_score TRUE but filetype wrong → HIDDEN (the regression a naive
  # immediate-driver-only predicate would introduce)
  expect_false(MSstatsShiny:::loadpage_show_openswath_mscore_cutoff("sky",  TRUE))
  expect_false(MSstatsShiny:::loadpage_show_openswath_mscore_cutoff("diann", TRUE))
  expect_false(MSstatsShiny:::loadpage_show_openswath_mscore_cutoff(NULL,  TRUE))

  # filetype right but m_score off → hidden
  expect_false(MSstatsShiny:::loadpage_show_openswath_mscore_cutoff("open", FALSE))
  expect_false(MSstatsShiny:::loadpage_show_openswath_mscore_cutoff("open", NULL))
})

test_that("loadpage_show_tmt_options gates on DDA_DIA == 'TMT' AND filetype in {PD, maxq}", {
  expect_true(MSstatsShiny:::loadpage_show_tmt_options("PD",   "TMT"))
  expect_true(MSstatsShiny:::loadpage_show_tmt_options("maxq", "TMT"))

  expect_false(MSstatsShiny:::loadpage_show_tmt_options("PD",   "LType"))
  expect_false(MSstatsShiny:::loadpage_show_tmt_options("maxq", "LType"))
  expect_false(MSstatsShiny:::loadpage_show_tmt_options("sky",  "TMT"))
  expect_false(MSstatsShiny:::loadpage_show_tmt_options(NULL,   "TMT"))
})

test_that("loadpage_default_proteinid_for_filetype picks the right default per converter", {
  expect_equal(MSstatsShiny:::loadpage_default_proteinid_for_filetype("PD"),   "Protein.Accessions")
  expect_equal(MSstatsShiny:::loadpage_default_proteinid_for_filetype("maxq"), "Proteins")
  expect_null(MSstatsShiny:::loadpage_default_proteinid_for_filetype("sky"))
  expect_null(MSstatsShiny:::loadpage_default_proteinid_for_filetype(NULL))
})


# ----------------------------------------------------------------------------
# `loadpage_seed_proteinid` — the typed-vs-default seeding rule.
# Pins the four cases that drive the TMT renderUI's seed value across
# converter switches. Failing any of these would re-introduce the
# "default-vs-typed" regression where switching PD <-> MaxQuant either
# clobbered a custom value or failed to update the per-converter default.
# ----------------------------------------------------------------------------

test_that("loadpage_seed_proteinid: first build applies incoming default", {
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("PD",   NULL, NULL),
               "Protein.Accessions")
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("maxq", NULL, NULL),
               "Proteins")
})

test_that("loadpage_seed_proteinid: same-converter rebuild carries the current value", {
  # No converter switch — keep whatever is currently in the textInput, whether
  # default or typed. Covers renderUI re-evaluations triggered by deps that
  # don't actually change the filetype.
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("PD",   "PD",   "Protein.Accessions"),
               "Protein.Accessions")
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("PD",   "PD",   "MyProtCol"),
               "MyProtCol")
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("maxq", "maxq", "Proteins"),
               "Proteins")
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("maxq", "maxq", "Anything"),
               "Anything")
})

test_that("loadpage_seed_proteinid: switch from outgoing-default applies incoming default", {
  # The default-vs-typed distinction: preserved equals outgoing's default →
  # user never typed → apply incoming default. This was the broken case where
  # PD -> MaxQuant stayed on "Protein.Accessions".
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("maxq", "PD",   "Protein.Accessions"),
               "Proteins")
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("PD",   "maxq", "Proteins"),
               "Protein.Accessions")
})

test_that("loadpage_seed_proteinid: switch with custom typed value carries it across", {
  # preserved differs from outgoing's default → user typed → carry verbatim,
  # do NOT clobber with the incoming default.
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("maxq", "PD",   "MyProtCol"),
               "MyProtCol")
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("PD",   "maxq", "MyProtCol"),
               "MyProtCol")
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("PD",   "maxq", "AnotherCol"),
               "AnotherCol")
})

test_that("loadpage_seed_proteinid: NULL preserved (post-unmount) re-applies incoming default", {
  # When the user leaves TMT entirely the renderUI returns NULL, the textInput
  # unmounts, and `input$which.proteinid` becomes NULL. On re-entry to TMT,
  # `preserved` is NULL but the tracker `last_tmt_filetype` may still hold
  # the previous filetype. Rule 1 (first-build) takes precedence and applies
  # the incoming converter's default. Typed values do NOT survive a full TMT
  # exit-and-return — this is the renderUI rebuild cost we accepted for the
  # duplicate-ns() carveout.
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("PD",   "PD",   NULL),
               "Protein.Accessions")
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("maxq", "PD",   NULL),
               "Proteins")
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("PD",   "maxq", NULL),
               "Protein.Accessions")
})

test_that("loadpage_seed_proteinid: NULL outgoing with non-NULL preserved carries the value", {
  # Conservative edge case: if outgoing is somehow NULL but preserved is set
  # (the tracker never ran but the input has a value — unusual race / pre-fill
  # / restoration), we cannot compare against an outgoing default. Carry the
  # preserved value rather than clobber it.
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("PD",   NULL, "Protein.Accessions"),
               "Protein.Accessions")
  expect_equal(MSstatsShiny:::loadpage_seed_proteinid("maxq", NULL, "MyProtCol"),
               "MyProtCol")
})

test_that("loadpage_seed_proteinid: full acceptance-test sequence (PD <-> MaxQuant)", {
  # Threads the reactiveVal tracker by hand through the 6-step acceptance test
  # from the PR brief. Each step asserts the seed value AND the tracker update
  # the renderUI performs after computing the seed.
  last_filetype <- NULL

  # 1. TMT + PD → "Protein.Accessions" (first build)
  seed1 <- MSstatsShiny:::loadpage_seed_proteinid("PD", last_filetype, NULL)
  expect_equal(seed1, "Protein.Accessions")
  last_filetype <- "PD"

  # 2. switch to MaxQuant without typing → "Proteins"
  seed2 <- MSstatsShiny:::loadpage_seed_proteinid("maxq", last_filetype, seed1)
  expect_equal(seed2, "Proteins")
  last_filetype <- "maxq"

  # 3. back to PD without typing → "Protein.Accessions"
  seed3 <- MSstatsShiny:::loadpage_seed_proteinid("PD", last_filetype, seed2)
  expect_equal(seed3, "Protein.Accessions")
  last_filetype <- "PD"

  # 4. type "MyProtCol" under PD, switch to MaxQuant → "MyProtCol" (carried)
  #    (the user typing is simulated by passing "MyProtCol" as `preserved`.)
  seed4 <- MSstatsShiny:::loadpage_seed_proteinid("maxq", last_filetype, "MyProtCol")
  expect_equal(seed4, "MyProtCol")
  last_filetype <- "maxq"

  # 5. back to PD → "MyProtCol"
  seed5 <- MSstatsShiny:::loadpage_seed_proteinid("PD", last_filetype, seed4)
  expect_equal(seed5, "MyProtCol")
  last_filetype <- "PD"

  # 6. restart app, TMT + PD → "Protein.Accessions"
  # Fresh session: tracker and preserved both reset to NULL.
  fresh_seed <- MSstatsShiny:::loadpage_seed_proteinid("PD", NULL, NULL)
  expect_equal(fresh_seed, "Protein.Accessions")
})


# ----------------------------------------------------------------------------
# Post-proceed1 summary-table predicates (BIO-driven). NULL bio behaves like
# "not PTM" — mirroring the original `input['loadpage-BIO'] !== 'PTM'` JS — so
# the non-PTM summary shows and the PTM summary hides when BIO is unset.
# ----------------------------------------------------------------------------

test_that("loadpage_show_nonptm_summary is TRUE for non-PTM (incl. NULL), FALSE for PTM", {
  expect_true(MSstatsShiny:::loadpage_show_nonptm_summary("Protein"))
  expect_true(MSstatsShiny:::loadpage_show_nonptm_summary("Peptide"))
  expect_true(MSstatsShiny:::loadpage_show_nonptm_summary(NULL))
  expect_false(MSstatsShiny:::loadpage_show_nonptm_summary("PTM"))
})

test_that("loadpage_show_ptm_summary is TRUE only for PTM (NULL -> FALSE)", {
  expect_true(MSstatsShiny:::loadpage_show_ptm_summary("PTM"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_summary("Protein"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_summary("Peptide"))
  expect_false(MSstatsShiny:::loadpage_show_ptm_summary(NULL))
})


# ----------------------------------------------------------------------------
# Phase 2 namespace assertions — every new container/driver id matches the
# literal string `R/utils.R` (or the UI ns(...) wrappers) read.
# ----------------------------------------------------------------------------

test_that("NAMESPACE_LOADPAGE — Phase 2 driver IDs preserve literal values", {
  expect_equal(NAMESPACE_LOADPAGE$label_free_type,           "LabelFreeType")
  expect_equal(NAMESPACE_LOADPAGE$big_file_spec,             "big_file_spec")
  expect_equal(NAMESPACE_LOADPAGE$calculate_anomaly_scores,  "calculate_anomaly_scores")
  expect_equal(NAMESPACE_LOADPAGE$m_score,                   "m_score")
  expect_equal(NAMESPACE_LOADPAGE$which_proteinid,           "which.proteinid")
})

test_that("NAMESPACE_LOADPAGE — Phase 2 container IDs match their UI div IDs", {
  # Sample / LabelFreeType
  expect_equal(NAMESPACE_LOADPAGE$sample_dda_description_panel,     "sample_dda_description_panel")
  expect_equal(NAMESPACE_LOADPAGE$sample_dia_description_panel,     "sample_dia_description_panel")
  expect_equal(NAMESPACE_LOADPAGE$sample_srm_prm_description_panel, "sample_srm_prm_description_panel")
  expect_equal(NAMESPACE_LOADPAGE$label_free_type_selection_panel,  "label_free_type_selection_panel")
  # Converter uploads
  expect_equal(NAMESPACE_LOADPAGE$standard_quant_upload_panel,  "standard_quant_upload_panel")
  expect_equal(NAMESPACE_LOADPAGE$standard_annot_upload_panel,  "standard_annot_upload_panel")
  expect_equal(NAMESPACE_LOADPAGE$msstats_regular_upload_panel, "msstats_regular_upload_panel")
  expect_equal(NAMESPACE_LOADPAGE$msstats_ptm_upload_panel,     "msstats_ptm_upload_panel")
  expect_equal(NAMESPACE_LOADPAGE$skyline_upload_panel,         "skyline_upload_panel")
  expect_equal(NAMESPACE_LOADPAGE$ptm_fragpipe_upload_panel,    "ptm_fragpipe_upload_panel")
  expect_equal(NAMESPACE_LOADPAGE$maxquant_upload_panel,        "maxquant_upload_panel")
  expect_equal(NAMESPACE_LOADPAGE$dia_umpire_upload_panel,      "dia_umpire_upload_panel")
  # PTM cluster
  expect_equal(NAMESPACE_LOADPAGE$ptm_uploads_panel,             "ptm_uploads_panel")
  expect_equal(NAMESPACE_LOADPAGE$ptm_maxquant_pgroup_panel,     "ptm_maxquant_pgroup_panel")
  expect_equal(NAMESPACE_LOADPAGE$ptm_metamorpheus_extras_panel, "ptm_metamorpheus_extras_panel")
  expect_equal(NAMESPACE_LOADPAGE$ptm_fasta_id_column_panel,     "ptm_fasta_id_column_panel")
  expect_equal(NAMESPACE_LOADPAGE$ptm_mod_id_maxq_panel,         "ptm_mod_id_maxq_panel")
  expect_equal(NAMESPACE_LOADPAGE$ptm_mod_id_pd_panel,           "ptm_mod_id_pd_panel")
  expect_equal(NAMESPACE_LOADPAGE$ptm_mod_id_spec_panel,         "ptm_mod_id_spec_panel")
  # Options + OpenSWATH
  expect_equal(NAMESPACE_LOADPAGE$label_free_options_panel,        "label_free_options_panel")
  expect_equal(NAMESPACE_LOADPAGE$openswath_mscore_panel,          "openswath_mscore_panel")
  expect_equal(NAMESPACE_LOADPAGE$openswath_mscore_cutoff_panel,   "openswath_mscore_cutoff_panel")
  # Post-proceed1 summary tables
  expect_equal(NAMESPACE_LOADPAGE$summary_nonptm_panel,            "summary_nonptm_panel")
  expect_equal(NAMESPACE_LOADPAGE$summary_ptm_panel,               "summary_ptm_panel")
  # TMT renderUI slot
  expect_equal(NAMESPACE_LOADPAGE$tmt_options_ui,                  "tmt_options_ui")
})
