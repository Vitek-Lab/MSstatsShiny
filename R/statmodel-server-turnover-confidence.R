# Per-protein confidence scoring and turnover classification
# (MSstatsResponse::calculateQCScore -> calculateConfidence ->
# classifyTurnoverProteins).
#
# This step only runs when the user asked for per-peptide weights on the
# data-processing page: calculateConfidence averages the `weight` column, so
# without it there is nothing to score. Kept as plain functions so the whole
# chain can be unit-tested without a running session.

#' Per-protein score columns lifted from the classification table onto the
#' turnover fit result, in display order.
#' @noRd
TURNOVER_CONFIDENCE_COLUMNS <- c("mean_weight", "n_obs", "qc_score",
                                 "n_heavy_peptides", "confidence",
                                 "max_h_frac", "category", "tier")

#' Feature-level columns calculateQCScore / calculateConfidence read.
#' @noRd
TURNOVER_CONFIDENCE_FEATURE_COLUMNS <- c("PROTEIN", "PEPTIDE", "LABEL",
                                         "INTENSITY", "GROUP")

#' Are per-peptide weights available on a prepared dose-response frame?
#'
#' @param prepared Output of `prepare_turnover_for_dose_response()`.
#' @return TRUE when the frame has rows and carries a `weight` column.
#' @noRd
turnover_weights_present <- function(prepared) {
  !is.null(prepared) && NROW(prepared) > 0 &&
    "weight" %in% colnames(prepared)
}

#' Does the confidence / classification step apply to this fit?
#'
#' Two conditions. Weights must be present (they are the input
#' calculateConfidence averages), and the fit must be in the synthesis
#' direction: classifyTurnoverProteins scores `H_frac` against increasing
#' IC50 targets, so a degradation (`L_frac`) fit cannot be classified.
#'
#' @param prepared Output of `prepare_turnover_for_dose_response()`.
#' @param increasing Logical. The fit's trend direction.
#' @return TRUE when `classify_turnover_fit()` can be called.
#' @noRd
turnover_confidence_applies <- function(prepared, increasing) {
  turnover_weights_present(prepared) && isTRUE(increasing)
}

#' Reshape a prepared dose-response frame into classifyTurnoverProteins input.
#'
#' MSstatsResponse wants both naming conventions in one frame: predictIC50()
#' reads the lower-case fit columns (`protein`, `drug`, `dose`, `response`),
#' while calculateConfidence() and classifyTurnoverProteins() read `Protein`
#' and `H_frac`. Deriving the latter from the former (rather than re-deriving
#' them from the raw ratios) guarantees the scores describe exactly the rows
#' that were fitted.
#'
#' @param prepared Output of `prepare_turnover_for_dose_response()`, which must
#'   carry a `weight` column.
#' @return `prepared` with `Protein` and `H_frac` columns added.
#' @noRd
prepare_turnover_for_classification <- function(prepared) {
  prepared <- as.data.frame(prepared, stringsAsFactors = FALSE)
  prepared$Protein <- as.character(prepared$protein)
  prepared$H_frac <- prepared$response
  prepared
}

#' Normalize feature-level data for the QC-score / heavy-peptide counts.
#'
#' dataProcess() returns PROTEIN and PEPTIDE as factors; coercing them keeps
#' the joined `Protein` column a character vector so the score columns match
#' the fit result by value rather than by factor level.
#'
#' @param feature_data `preprocess_data()$FeatureLevelData`.
#' @return A data frame with character protein / peptide identifiers.
#' @noRd
prepare_feature_data_for_qc_score <- function(feature_data) {
  feature_data <- as.data.frame(feature_data, stringsAsFactors = FALSE)
  missing <- setdiff(TURNOVER_CONFIDENCE_FEATURE_COLUMNS, colnames(feature_data))
  if (length(missing) > 0) {
    stop("the feature-level data is missing required column(s): ",
         paste(missing, collapse = ", "),
         ". Re-run protein summarization on the data-processing page.",
         call. = FALSE)
  }
  for (col in c("PROTEIN", "PEPTIDE", "LABEL")) {
    feature_data[[col]] <- as.character(feature_data[[col]])
  }
  feature_data
}

#' Score and classify a turnover fit.
#'
#' Runs the MSstatsResponse chain: light-channel QC score per protein, then
#' the combined confidence score (peptide weights x fit residuals x QC score x
#' heavy-peptide shrinkage), then the biological category / confidence tier.
#'
#' The result has one row per protein in the feature-level data, which is a
#' superset of the fitted proteins: proteins that produced no fit come back
#' with NA scores and category `no_heavy`.
#'
#' @param prepared Output of `prepare_turnover_for_dose_response()` with weights.
#' @param fit Output of `doseResponseFit()`.
#' @param feature_data `preprocess_data()$FeatureLevelData`.
#' @param k_shrinkage Numeric. Bayesian shrinkage constant on the heavy-peptide
#'   count, passed to `calculateConfidence()`.
#' @return A data frame of per-protein QC, confidence, category and tier.
#' @noRd
classify_turnover_fit <- function(prepared, fit, feature_data,
                                  k_shrinkage = 2) {
  weights_df <- prepare_turnover_for_classification(prepared)
  features <- prepare_feature_data_for_qc_score(feature_data)

  qc_scores <- calculateQCScore(features)
  confidence_scores <- calculateConfidence(
    weights_df   = weights_df,
    fit_df       = fit,
    qc_df        = qc_scores,
    feature_data = features,
    k_shrinkage  = k_shrinkage
  )
  classification <- classifyTurnoverProteins(
    weights_df = weights_df,
    fit_df     = fit,
    qc_df      = qc_scores,
    conf_df    = confidence_scores
  )

  as.data.frame(classification, stringsAsFactors = FALSE)
}

#' Attach the per-protein score columns to the turnover fit result.
#'
#' Row order and row count of `fit` are preserved, so the significance
#' filtering and downloads that already read `ComparisonResult` are unaffected;
#' the classification's extra (unfitted) proteins stay in the classification
#' table only.
#'
#' @param fit Output of `doseResponseFit()`.
#' @param classification Output of `classify_turnover_fit()`.
#' @return `fit` with the confidence / category / tier columns appended.
#' @noRd
merge_turnover_confidence <- function(fit, classification) {
  if (is.null(classification) || NROW(classification) == 0 ||
      is.null(fit) || NROW(fit) == 0) {
    return(fit)
  }
  score_cols <- setdiff(
    intersect(TURNOVER_CONFIDENCE_COLUMNS, colnames(classification)),
    colnames(fit))
  if (length(score_cols) == 0) {
    return(fit)
  }

  matched <- match(as.character(fit$Protein),
                   as.character(classification$Protein))
  for (col in score_cols) {
    fit[[col]] <- classification[[col]][matched]
  }
  fit
}

#' The notification shown when weights were calculated but the fit is a
#' degradation fit, so no classification is possible.
#' @noRd
turnover_confidence_direction_message <- function() {
  paste0("Per-protein confidence scores and turnover categories were not ",
         "calculated: they are defined for the synthesis direction (heavy ",
         "fraction, increasing over time) only. Check \"Synthesis ",
         "(heavy-isotope incorporation, increasing)\" and calculate again to ",
         "score this fit.")
}
