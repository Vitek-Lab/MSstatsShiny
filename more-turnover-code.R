if (number_of_samples > 1) {
  dataset = preprocess_data()$ProteinLevelData
} else {
  dataset = preprocess_data()$FeatureLevelData
}
tracer_consts <- c("0" = 1.0, "1" = 0.286, "4" = 0.294, 
                   "12" = 0.261, "24" = 0.304, "48" = 0.266, 
                   "96" = 0.263, "168" = 0.313) # should be defined by the user
ratios = calculateTurnoverRatios(
  feature_level_data,
  channel_col = "LABEL",
  heavy_label = "H",
  light_label = "L",
  time_col = "GROUP",
  peptide_col = "PEPTIDE",
  protein_col = "PROTEIN",
  intensity_col = "INTENSITY",
  run_col = "RUN",
  peptide_selector = NULL,
  agg_function = max,
  normalize_tracer = TRUE,
  tracer_constants = tracer_consts
)