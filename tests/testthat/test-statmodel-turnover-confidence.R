context("Turnover confidence scoring and classification")

make_prepared <- function(with_weight = TRUE) {
  prepared <- data.frame(
    protein      = rep(c("ProtA", "ProtB"), each = 6),
    drug         = "time",
    dose         = rep(c(0, 2, 6), times = 4),
    response     = c(0.0, 0.3, 0.6, 0.0, 0.35, 0.65,
                     0.0, 0.1, 0.2, 0.0, 0.15, 0.25),
    BaseSequence = rep(c("PEP1", "PEP2", "PEP1", "PEP2"), each = 3),
    stringsAsFactors = FALSE
  )
  if (with_weight) {
    prepared$weight <- rep(c(0.8, 0.4), each = 6)
  }
  prepared
}

make_features <- function() {
  features <- expand.grid(
    PROTEIN = c("ProtA", "ProtB", "ProtC"),
    PEPTIDE = c("PEP1", "PEP2"),
    GROUP   = c(0, 2, 6),
    LABEL   = c("H", "L"),
    stringsAsFactors = FALSE
  )
  features <- features[!(features$PROTEIN == "ProtC" & features$LABEL == "H"), ]
  features$RUN <- paste0("run_", features$GROUP)
  features$INTENSITY <- seq_len(nrow(features)) * 1000
  features$PROTEIN <- factor(features$PROTEIN)
  features$PEPTIDE <- factor(features$PEPTIDE)
  features
}

make_fit <- function() {
  data.frame(
    Protein     = c("ProtA", "ProtB"),
    drug        = "time",
    direction   = "increasing",
    SSE_Full    = c(0.01, 0.05),
    SSE_Null    = c(0.5, 0.4),
    F_statistic = c(20, 8),
    P_value     = c(0.001, 0.02),
    log2FC      = c(1.5, 0.8),
    adj.pvalue  = c(0.002, 0.02),
    stringsAsFactors = FALSE
  )
}

test_that("turnover_weights_present detects the weight column", {
  expect_true(MSstatsShiny:::turnover_weights_present(make_prepared()))
  expect_false(MSstatsShiny:::turnover_weights_present(
    make_prepared(with_weight = FALSE)))
})

test_that("turnover_weights_present is FALSE for NULL or empty input", {
  expect_false(MSstatsShiny:::turnover_weights_present(NULL))
  expect_false(MSstatsShiny:::turnover_weights_present(
    make_prepared()[0, , drop = FALSE]))
})

test_that("turnover_confidence_applies requires weights and the synthesis direction", {
  prepared <- make_prepared()

  expect_true(MSstatsShiny:::turnover_confidence_applies(prepared, TRUE))
  expect_false(MSstatsShiny:::turnover_confidence_applies(prepared, FALSE),
               info = "classification is only defined for H_frac / increasing fits")
  expect_false(MSstatsShiny:::turnover_confidence_applies(
    make_prepared(with_weight = FALSE), TRUE),
    info = "confidence averages the weight column, so weights are required")
})

test_that("prepare_turnover_for_classification adds the columns MSstatsResponse reads", {
  result <- MSstatsShiny:::prepare_turnover_for_classification(make_prepared())

  expect_true(all(c("Protein", "H_frac") %in% colnames(result)),
              info = "calculateConfidence / classifyTurnoverProteins read Protein and H_frac")
  expect_true(all(c("protein", "drug", "dose", "response") %in% colnames(result)),
              info = "predictIC50 still needs the lower-case fit columns")
  expect_type(result$Protein, "character")
  expect_equal(result$Protein, result$protein)
  expect_equal(result$H_frac, result$response,
               info = "H_frac must be the exact column that was fitted")
})

test_that("prepare_turnover_for_classification coerces a factor protein column", {
  prepared <- make_prepared()
  prepared$protein <- factor(prepared$protein)

  result <- MSstatsShiny:::prepare_turnover_for_classification(prepared)

  expect_type(result$Protein, "character")
  expect_equal(unique(result$Protein), c("ProtA", "ProtB"))
})

test_that("prepare_feature_data_for_qc_score makes identifiers character", {
  result <- MSstatsShiny:::prepare_feature_data_for_qc_score(make_features())

  expect_type(result$PROTEIN, "character")
  expect_type(result$PEPTIDE, "character")
  expect_type(result$LABEL, "character")
})

test_that("prepare_feature_data_for_qc_score names the missing columns", {
  features <- make_features()
  features$LABEL <- NULL
  features$INTENSITY <- NULL

  expect_error(
    MSstatsShiny:::prepare_feature_data_for_qc_score(features),
    "LABEL, INTENSITY")
})

test_that("classify_turnover_fit returns a confidence and tier per protein", {
  result <- MSstatsShiny:::classify_turnover_fit(
    make_prepared(), make_fit(), make_features())

  expect_true(all(c("Protein", "qc_score", "mean_weight", "n_heavy_peptides",
                    "confidence", "max_h_frac", "category", "tier") %in%
                    colnames(result)))
  expect_setequal(result$Protein, c("ProtA", "ProtB", "ProtC"))
  expect_true(all(result$confidence >= 0 & result$confidence <= 1,
                  na.rm = TRUE),
              info = "confidence is a score in [0, 1]")
  expect_true(all(result$tier %in% c("HIGH", "MEDIUM", "LOW")))
})

test_that("classify_turnover_fit keeps proteins that produced no fit", {
  result <- MSstatsShiny:::classify_turnover_fit(
    make_prepared(), make_fit(), make_features())

  prot_c <- result[result$Protein == "ProtC", ]
  expect_equal(nrow(prot_c), 1L)
  expect_true(is.na(prot_c$confidence),
              info = "a protein with no fit cannot be scored")
  expect_equal(prot_c$category, "no_heavy")
})

test_that("classify_turnover_fit weights higher-quality peptides into confidence", {
  prepared <- make_prepared()
  low <- prepared
  low$weight <- low$weight / 4

  high_conf <- MSstatsShiny:::classify_turnover_fit(
    prepared, make_fit(), make_features())
  low_conf <- MSstatsShiny:::classify_turnover_fit(
    low, make_fit(), make_features())

  fitted <- c("ProtA", "ProtB")
  expect_true(all(
    low_conf$confidence[match(fitted, low_conf$Protein)] <
      high_conf$confidence[match(fitted, high_conf$Protein)]),
    info = "down-weighting every peptide must lower every protein's confidence")
})

test_that("classify_turnover_fit surfaces a missing feature-level column", {
  features <- make_features()
  features$PEPTIDE <- NULL

  expect_error(
    MSstatsShiny:::classify_turnover_fit(make_prepared(), make_fit(), features),
    "PEPTIDE")
})

test_that("merge_turnover_confidence appends scores without changing the fit rows", {
  fit <- make_fit()
  classification <- MSstatsShiny:::classify_turnover_fit(
    make_prepared(), fit, make_features())

  result <- MSstatsShiny:::merge_turnover_confidence(fit, classification)

  expect_equal(nrow(result), nrow(fit),
               info = "the unfitted proteins stay out of the fit result")
  expect_equal(result$Protein, fit$Protein,
               info = "row order must survive so downstream filtering is unaffected")
  expect_equal(result$adj.pvalue, fit$adj.pvalue)
  expect_true(all(c("confidence", "category", "tier") %in% colnames(result)))
  expect_equal(result$confidence,
               classification$confidence[match(fit$Protein,
                                               classification$Protein)])
})

test_that("merge_turnover_confidence matches by protein, not by row position", {
  fit <- make_fit()
  classification <- data.frame(
    Protein    = c("ProtB", "ProtA"),
    confidence = c(0.2, 0.9),
    category   = c("medium_lived", "fit"),
    tier       = c("LOW", "HIGH"),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::merge_turnover_confidence(fit, classification)

  expect_equal(result$confidence[result$Protein == "ProtA"], 0.9)
  expect_equal(result$tier[result$Protein == "ProtB"], "LOW")
})

test_that("merge_turnover_confidence returns the fit unchanged when there is no classification", {
  fit <- make_fit()

  expect_equal(MSstatsShiny:::merge_turnover_confidence(fit, NULL), fit)
  expect_equal(
    MSstatsShiny:::merge_turnover_confidence(fit, data.frame()), fit)
})

test_that("merge_turnover_confidence does not overwrite existing fit columns", {
  fit <- make_fit()
  fit$confidence <- c(99, 99)
  classification <- data.frame(
    Protein    = c("ProtA", "ProtB"),
    confidence = c(0.1, 0.2),
    tier       = c("HIGH", "LOW"),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::merge_turnover_confidence(fit, classification)

  expect_equal(result$confidence, c(99, 99))
  expect_equal(result$tier, c("HIGH", "LOW"))
})

test_that("weighted synthesis script emits the confidence and classification chain", {
  code <- MSstatsShiny:::build_turnover_analysis_code(
    qc_input = stats::setNames(list(TRUE), NAMESPACE_QC$assign_feature_weights),
    comp_mat = data.frame(GROUP = c("0hr", "2hr"), TimeVal = c(0, 2),
                          stringsAsFactors = FALSE),
    increasing = TRUE,
    tracer_constants = list(values = c("0hr" = 1, "2hr" = 1),
                            source = CONSTANTS_QC$tracer_source_none,
                            file = NULL)
  )

  expect_true(grepl("calculateQCScore(summarized$FeatureLevelData)", code,
                    fixed = TRUE))
  expect_true(grepl("confidence_scores = calculateConfidence(", code,
                    fixed = TRUE))
  expect_true(grepl("turnover_classification = classifyTurnoverProteins(", code,
                    fixed = TRUE))
  expect_true(grepl("classification_input$H_frac", code, fixed = TRUE),
              info = "the classifier needs the H_frac alias on the fitted frame")
  expect_lt(regexpr("calculateConfidence", code, fixed = TRUE),
            regexpr("classifyTurnoverProteins", code, fixed = TRUE))
  expect_lt(regexpr("response_results = doseResponseFit", code, fixed = TRUE),
            regexpr("calculateConfidence", code, fixed = TRUE))
})

test_that("unweighted script omits the confidence and classification chain", {
  code <- MSstatsShiny:::build_turnover_analysis_code(
    qc_input = stats::setNames(list(FALSE), NAMESPACE_QC$assign_feature_weights),
    comp_mat = data.frame(GROUP = c("0hr", "2hr"), TimeVal = c(0, 2),
                          stringsAsFactors = FALSE),
    increasing = TRUE,
    tracer_constants = list(values = c("0hr" = 1, "2hr" = 1),
                            source = CONSTANTS_QC$tracer_source_none,
                            file = NULL)
  )

  expect_false(grepl("calculateConfidence", code, fixed = TRUE))
  expect_false(grepl("classifyTurnoverProteins", code, fixed = TRUE))
  expect_false(grepl("calculateQCScore", code, fixed = TRUE))
})

test_that("degradation script omits the classification chain even when weighted", {
  code <- MSstatsShiny:::build_turnover_analysis_code(
    qc_input = stats::setNames(list(TRUE), NAMESPACE_QC$assign_feature_weights),
    comp_mat = data.frame(GROUP = c("0hr", "2hr"), TimeVal = c(0, 2),
                          stringsAsFactors = FALSE),
    increasing = FALSE,
    tracer_constants = list(values = c("0hr" = 1, "2hr" = 1),
                            source = CONSTANTS_QC$tracer_source_none,
                            file = NULL)
  )

  expect_true(grepl("calculatePeptideWeights", code, fixed = TRUE),
              info = "weights still apply to a degradation fit")
  expect_false(grepl("classifyTurnoverProteins", code, fixed = TRUE),
               info = "classification is H_frac / increasing only")
})

test_that("turnover_confidence_direction_message names the checkbox to re-check", {
  message_text <- MSstatsShiny:::turnover_confidence_direction_message()

  expect_true(grepl("Synthesis", message_text, fixed = TRUE))
  expect_true(grepl("were not calculated", message_text, fixed = TRUE))
})
