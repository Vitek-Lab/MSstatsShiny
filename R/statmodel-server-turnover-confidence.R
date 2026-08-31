#' Per-protein score columns lifted from the classification table onto the
#' turnover fit result, in display order.
#' @noRd
TURNOVER_CONFIDENCE_COLUMNS <- c("mean_weight", "n_obs", "qc_score",
                                 "n_heavy_peptides", "confidence",
                                 "max_h_frac", "category", "tier")

#' Feature-level columns calculateQCScore and calculateConfidence read.
#' @noRd
TURNOVER_CONFIDENCE_FEATURE_COLUMNS <- c("PROTEIN", "PEPTIDE", "LABEL",
                                         "INTENSITY", "GROUP")

#' Checks if per-peptide weights are available in a data frame
#'
#' @param prepared Output of `prepare_turnover_for_dose_response()`.
#' @return TRUE when the frame has rows and carries a `weight` column.
#' @noRd
turnover_weights_present <- function(prepared) {
  !is.null(prepared) && NROW(prepared) > 0 &&
    "weight" %in% colnames(prepared)
}

#' Checks if confidence scoring and classification can be applied
#'
#' It can be applied if weights are present and the fit is w.r.t. the synthesis
#' direction.  At the moment, classifyTurnoverProteins only handles `H_frac` 
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

#' Prepare feature-level data for the QC-score / heavy-peptide counts.
#'
#' Specifically, turn PROTEIN, PEPTIDE, and LABEL columns into character columns
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

#' Score and classify a turnover fit as long-lived vs short-lived and
#' high-quality or low-quality w.r.t. quality scores.
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

#' Attach per-protein confidence score columns to the turnover fit statistical 
#' result.
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
