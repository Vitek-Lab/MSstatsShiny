# Truth-table tests for the QC sidebar visibility predicates and a guard that the
# NAMESPACE_QC input ids keep their original literal string values.

test_that("qc_show_tmt is TRUE only for the TMT label type", {
  expect_true(MSstatsShiny:::qc_show_tmt("TMT", "Protein"))
  expect_true(MSstatsShiny:::qc_show_tmt("TMT", "PTM"))
  expect_true(MSstatsShiny:::qc_show_tmt("TMT", "Peptide"))
  expect_true(MSstatsShiny:::qc_show_tmt("TMT", NULL))

  expect_false(MSstatsShiny:::qc_show_tmt("LType", "Protein"))
  expect_false(MSstatsShiny:::qc_show_tmt("LType", "PTM"))
  expect_false(MSstatsShiny:::qc_show_tmt(NULL, "Protein"))
  expect_false(MSstatsShiny:::qc_show_tmt(NULL, NULL))
})

test_that("qc_show_lf is TRUE for the label-free branch", {
  expect_true(MSstatsShiny:::qc_show_lf("LType", "Protein"))
  expect_true(MSstatsShiny:::qc_show_lf("LType", "PTM"))
  expect_true(MSstatsShiny:::qc_show_lf("LType", NULL))
  # Mirrors the original `DDA_DIA != 'TMT'`: an unset label type with PTM still shows.
  expect_true(MSstatsShiny:::qc_show_lf(NULL, "PTM"))

  expect_false(MSstatsShiny:::qc_show_lf("TMT", "Protein"))
  expect_false(MSstatsShiny:::qc_show_lf("TMT", "PTM"))
  expect_false(MSstatsShiny:::qc_show_lf(NULL, "Protein"))
})

test_that("qc_show_maxqc_msstats needs the TMT branch AND msstats summarization", {
  expect_true(MSstatsShiny:::qc_show_maxqc_msstats("TMT", "Protein", "msstats"))

  # Ancestor-chain regression: summarization is right but the branch is wrong.
  expect_false(MSstatsShiny:::qc_show_maxqc_msstats("LType", "Protein", "msstats"))
  # Branch is right but summarization is wrong.
  expect_false(MSstatsShiny:::qc_show_maxqc_msstats("TMT", "Protein", "MedianPolish"))
  expect_false(MSstatsShiny:::qc_show_maxqc_msstats("TMT", "Protein", NULL))
  expect_false(MSstatsShiny:::qc_show_maxqc_msstats(NULL, "Protein", "msstats"))
})

test_that("qc_show_standards needs globalStandards, non-PTM, label-free", {
  expect_true(MSstatsShiny:::qc_show_standards("globalStandards", "Protein", "LType"))

  expect_false(MSstatsShiny:::qc_show_standards("equalizeMedians", "Protein", "LType"))
  expect_false(MSstatsShiny:::qc_show_standards("globalStandards", "PTM", "LType"))
  expect_false(MSstatsShiny:::qc_show_standards("globalStandards", "Protein", "TMT"))
  expect_false(MSstatsShiny:::qc_show_standards(NULL, "Protein", "LType"))
})

test_that("qc_show_features_topn needs the label-free branch AND the topN subset", {
  expect_true(MSstatsShiny:::qc_show_features_topn("LType", "Protein", "topN"))

  # Ancestor-chain regression: subset is right but the branch is wrong.
  expect_false(MSstatsShiny:::qc_show_features_topn("TMT", "Protein", "topN"))
  expect_false(MSstatsShiny:::qc_show_features_topn("LType", "Protein", "all"))
  expect_false(MSstatsShiny:::qc_show_features_topn("LType", "Protein", NULL))
})

test_that("qc_show_mbi needs the label-free branch AND a censoring assumption", {
  expect_true(MSstatsShiny:::qc_show_mbi("LType", "Protein", "NA"))
  expect_true(MSstatsShiny:::qc_show_mbi("LType", "Protein", "0"))

  # Ancestor-chain regression: censoring is set but the branch is wrong.
  expect_false(MSstatsShiny:::qc_show_mbi("TMT", "Protein", "NA"))
  expect_false(MSstatsShiny:::qc_show_mbi("LType", "Protein", NULL))
  expect_false(MSstatsShiny:::qc_show_mbi("LType", "Protein", "other"))
})

test_that("qc_show_log_section needs the label-free branch AND a non-turnover template", {
  expect_true(MSstatsShiny:::qc_show_log_section("LType", "Protein", TEMPLATES$default))
  expect_true(MSstatsShiny:::qc_show_log_section("LType", "Protein", TEMPLATES$chemoproteomics))

  # Template regression: label-free but protein turnover hides the panel.
  expect_false(MSstatsShiny:::qc_show_log_section("LType", "Protein", TEMPLATES$protein_turnover))
  # Branch regression: non-turnover but TMT hides the panel.
  expect_false(MSstatsShiny:::qc_show_log_section("TMT", "Protein", TEMPLATES$default))
})

test_that("qc_show_profileplot_options and qc_show_qualitymetrics_options key off type1", {
  expect_true(MSstatsShiny:::qc_show_profileplot_options("ProfilePlot"))
  expect_false(MSstatsShiny:::qc_show_profileplot_options("QCPlot"))
  expect_false(MSstatsShiny:::qc_show_profileplot_options("QualityMetricsPlot"))
  expect_false(MSstatsShiny:::qc_show_profileplot_options(NULL))

  expect_true(MSstatsShiny:::qc_show_qualitymetrics_options("QualityMetricsPlot"))
  expect_false(MSstatsShiny:::qc_show_qualitymetrics_options("ProfilePlot"))
  expect_false(MSstatsShiny:::qc_show_qualitymetrics_options("QCPlot"))
  expect_false(MSstatsShiny:::qc_show_qualitymetrics_options(NULL))
})

test_that("download panels are complementary on the PTM biological question", {
  expect_true(MSstatsShiny:::qc_show_nonptm_downloads("Protein"))
  expect_true(MSstatsShiny:::qc_show_nonptm_downloads("Peptide"))
  expect_true(MSstatsShiny:::qc_show_nonptm_downloads(NULL))
  expect_false(MSstatsShiny:::qc_show_nonptm_downloads("PTM"))

  expect_true(MSstatsShiny:::qc_show_ptm_downloads("PTM"))
  expect_false(MSstatsShiny:::qc_show_ptm_downloads("Protein"))
  expect_false(MSstatsShiny:::qc_show_ptm_downloads("Peptide"))
  expect_false(MSstatsShiny:::qc_show_ptm_downloads(NULL))
})

test_that("NAMESPACE_QC retains literal string values (no renames)", {
  expect_equal(NAMESPACE_QC$global_norm, "global_norm")
  expect_equal(NAMESPACE_QC$log, "log")
  expect_equal(NAMESPACE_QC$summarization, "summarization")
  expect_equal(NAMESPACE_QC$null, "null")
  expect_equal(NAMESPACE_QC$max_qc, "maxQC")
  expect_equal(NAMESPACE_QC$norm, "norm")
  expect_equal(NAMESPACE_QC$standards, "standards")
  expect_equal(NAMESPACE_QC$names, "names")
  expect_equal(NAMESPACE_QC$reference_norm, "reference_norm")
  expect_equal(NAMESPACE_QC$remove_norm_channel, "remove_norm_channel")
  expect_equal(NAMESPACE_QC$features_used, "features_used")
  expect_equal(NAMESPACE_QC$n_feat, "n_feat")
  expect_equal(NAMESPACE_QC$cens_int, "censInt")
  expect_equal(NAMESPACE_QC$null1, "null1")
  expect_equal(NAMESPACE_QC$max_qc_censored, "maxQC1")
  expect_equal(NAMESPACE_QC$mbi, "MBi")
  expect_equal(NAMESPACE_QC$remove50, "remove50")
  expect_equal(NAMESPACE_QC$summary_method, "summaryMethod")
  expect_equal(NAMESPACE_QC$typequant, "typequant")
  expect_equal(NAMESPACE_QC$format, "format")
  expect_equal(NAMESPACE_QC$summ, "summ")
  expect_equal(NAMESPACE_QC$fname, "fname")
  expect_equal(NAMESPACE_QC$qc_page_plot_type, "qc_page_plot_type")
  expect_equal(NAMESPACE_QC$which_protein_for_data_process_plots, "which_protein_for_data_process_plots")
  expect_equal(NAMESPACE_QC$quality_metric, "quality_metric")
  expect_equal(NAMESPACE_QC$qm_protein, "qm_protein")
  expect_equal(NAMESPACE_QC$run, "run")
  expect_equal(NAMESPACE_QC$update_results, "update_results")
  expect_equal(NAMESPACE_QC$proceed6, "proceed6")
})
