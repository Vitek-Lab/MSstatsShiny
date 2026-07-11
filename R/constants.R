TEMPLATES = list(
  default = "default",
  chemoproteomics = "chemoproteomics",
  protein_turnover = "protein_turnover",
  metabolomics = "metabolomics"
  
)

TEMPLATE_LABELS = list(
  default = "Protein Differential Abundance Analysis",
  chemoproteomics = "Chemoproteomics",
  protein_turnover = "Protein Turnover",
  metabolomics = "Metabolite Differential Abundance Analysis"
  
)

NAMESPACE_STATMODEL = list(
  comparisons_conditional_panel = "comparisons_conditional_panel",
  comparison_mode = "comparisons_mode",
  comparisons_submit = "comparisons_submit",
  comparisons_clear = "comparisons_clear",
  comparisons_custom_pairwise_choice1 = "comparisons_custom_pairwise_choice1", 
  comparisons_custom_pairwise_choice2 = "comparisons_custom_pairwise_choice2", 
  comparisons_all_vs_one_choice = "comparisons_all_vs_one_choice", 
  comparisons_custom_nonpairwise_name = "comparisons_custom_nonpairwise_name",
  comparisons_custom_nonpairwise_weights = "comparisons_custom_nonpairwise_weights",
  modeling_start = "modeling_start", # "calculate"
  modeling_significance_level = "modeling_significance_level", 
  modeling_tmt_moderation = "modeling_tmt_moderation", 
  modeling_response_curve_fitting_options = "modeling_response_curve_fitting_options",
  modeling_response_curve_increasing_trend = "modeling_response_curve_increasing_trend",
  modeling_response_curve_log_xaxis = "modeling_response_curve_log_xaxis", 
  visualization_plot_options_conditional_panel = "plot_options_conditional_panel",
  modeling_section_header = "modeling_section_header",
  visualization_plot_type = "visualization_plot_type", 
  visualization_logp_base = "visualization_logp_base", 
  visualization_which_protein = "visualization_which_protein", 
  visualization_fold_change_checkbox = "visualization_fold_change_checkbox",
  visualization_fold_change_input = "visualization_fold_change_input", 
  visualization_which_comparison = "visualization_which_comparison",
  visualization_volcano_significance_cutoff = "visualization_volcano_significance_cutoff", 
  visualization_heatmap_number_proteins = "visualization_heatmap_number_proteins",
  visualization_heatmap_cluster_option = "visualization_heatmap_cluster_option", 
  visualization_response_curve_which_drug = "visualization_response_curve_which_drug",
  visualization_response_curve_ratio_scale = "visualization_response_curve_ratio_scale",
  visualization_view_results = "visualization_view_results", 
  visualization_download_plot_results = "visualization_download_plot_results",
  visualization_plot_output = "visualization_plot_output",
  visualization_plot_height_slider = "visualization_plot_height_slider",
  comparisons_exclude_conditions = "comparisons_exclude_conditions"
)

CONSTANTS_STATMODEL = list(
  comparison_mode_all_pairwise = "comparison_mode_all_pairwise", 
  comparison_mode_all_vs_one = "comparison_mode_all_vs_one",
  comparison_mode_custom_pairwise = "comparison_mode_custom_pairwise",
  comparison_mode_custom_nonpairwise = "comparison_mode_custom_nonpairwise",
  comparison_mode_response_curve = "comparison_mode_response_curve",
  plot_type_volcano_plot = "VolcanoPlot", # VolcanoPlot
  plot_type_heatmap = "Heatmap", # Heatmap
  plot_type_comparison_plot = "ComparisonPlot", # ComparisonPlot
  plot_type_response_curve = "ResponseCurve", # ResponseCurve
  plot_type_qq_plot = "QQPlots" # QQPlots — matches MSstats::groupComparisonQCPlots(type = "QQPlots")
)

NAMESPACE_LOADPAGE = list(
  bio = "BIO",
  dda_dia = "DDA_DIA",
  filetype = "filetype",
  proceed1 = "proceed1",
  big_file_diann = "big_file_diann",
  big_diann_calculate_anomaly_scores = "big_diann_calculate_anomaly_scores",
  big_diann_run_order_file = "big_diann_run_order_file",
  diann_2plus = "diann_2plus",
  intensity_column = "intensity_column",
  q_val = "q_val",
  q_cutoff = "q_cutoff",
  mbr = "MBR",
  diann_calculate_anomaly_scores = "diann_calculate_anomaly_scores",
  diann_run_order_file = "diann_run_order_file",
  big_file_spec = "big_file_spec",
  label_free_type = "LabelFreeType",
  calculate_anomaly_scores = "calculate_anomaly_scores",
  m_score = "m_score",
  which_proteinid = "which.proteinid",
  diann_lf_options_panel = "diann_lf_options_panel",
  diann_intensity_column_panel = "diann_intensity_column_panel",
  qval_filter_panel = "qval_filter_panel",
  qval_cutoff_panel = "qval_cutoff_panel",
  qval_mbr_panel = "qval_mbr_panel",
  sample_dda_description_panel = "sample_dda_description_panel",
  sample_dia_description_panel = "sample_dia_description_panel",
  sample_srm_prm_description_panel = "sample_srm_prm_description_panel",
  label_free_type_selection_panel = "label_free_type_selection_panel",
  standard_quant_upload_panel = "standard_quant_upload_panel",
  standard_annot_upload_panel = "standard_annot_upload_panel",
  msstats_regular_upload_panel = "msstats_regular_upload_panel",
  msstats_ptm_upload_panel = "msstats_ptm_upload_panel",
  skyline_upload_panel = "skyline_upload_panel",
  ptm_fragpipe_upload_panel = "ptm_fragpipe_upload_panel",
  maxquant_upload_panel = "maxquant_upload_panel",
  ptm_uploads_panel = "ptm_uploads_panel",
  ptm_maxquant_pgroup_panel = "ptm_maxquant_pgroup_panel",
  ptm_metamorpheus_extras_panel = "ptm_metamorpheus_extras_panel",
  ptm_fasta_id_column_panel = "ptm_fasta_id_column_panel",
  ptm_mod_id_maxq_panel = "ptm_mod_id_maxq_panel",
  ptm_mod_id_pd_panel = "ptm_mod_id_pd_panel",
  ptm_mod_id_spec_panel = "ptm_mod_id_spec_panel",
  dia_umpire_upload_panel = "dia_umpire_upload_panel",
  label_free_options_panel = "label_free_options_panel",
  openswath_mscore_panel = "openswath_mscore_panel",
  openswath_mscore_cutoff_panel = "openswath_mscore_cutoff_panel",
  summary_nonptm_panel = "summary_nonptm_panel",
  summary_ptm_panel = "summary_ptm_panel",
  tmt_options_ui = "tmt_options_ui",
  mzmine_upload_panel = "mzmine_upload_panel"
)

# File-type choices for the loadpage converter radio.
LOADPAGE_FILETYPE_CHOICES = c("Example dataset" = "sample",
                              "MSstats Format" = "msstats",
                              "Skyline" = "sky", "MaxQuant" = "maxq",
                              "Progenesis" = "prog", "Proteome Discoverer" = "PD",
                              "OpenMS" = "openms", "Spectronaut" = "spec",
                              "OpenSWATH" = "open", "DIA-Umpire" = "ump",
                              "SpectroMine" = "spmin", "FragPipe" = "phil", "DIANN"="diann",
                              "Metamorpheus" = "meta")

NAMESPACE_QC = list(
  # Sidebar processing-option input ids
  global_norm = "global_norm",
  log = "log",
  summarization = "summarization",
  null = "null",
  max_qc = "maxQC",
  norm = "norm",
  standards = "standards",
  names = "names",
  reference_norm = "reference_norm",
  remove_norm_channel = "remove_norm_channel",
  features_used = "features_used",
  n_feat = "n_feat",
  cens_int = "censInt",
  null1 = "null1",
  max_qc_censored = "maxQC1",
  mbi = "MBi",
  remove50 = "remove50",
  summary_method = "summaryMethod",
  # Summarized-results, plot and download-tab input ids
  typequant = "typequant",
  format = "format",
  summ = "summ",
  fname = "fname",
  qc_page_plot_type = "qc_page_plot_type",
  which_protein_for_data_process_plots = "which_protein_for_data_process_plots",
  quality_metric = "quality_metric",
  qm_protein = "qm_protein",
  run = "run",
  update_results = "update_results",
  proceed6 = "proceed6",
  # Server-toggled sidebar visibility containers
  global_norm_panel = "global_norm_panel",
  log_section = "log_section",
  summarization_panel = "summarization_panel",
  maxqc_msstats_panel = "maxqc_msstats_panel",
  norm_panel = "norm_panel",
  standards_panel = "standards_panel",
  standards_type_section = "standards_type_section",
  reference_norm_panel = "reference_norm_panel",
  lf_options_panel = "lf_options_panel",
  features_topn_panel = "features_topn_panel",
  censoring_section = "censoring_section",
  mbi_panel = "mbi_panel",
  # Server-toggled plot-tab and download-tab visibility containers
  profileplot_options_panel = "profileplot_options_panel",
  qualitymetrics_options_panel = "qualitymetrics_options_panel",
  nonptm_downloads_panel = "nonptm_downloads_panel",
  ptm_downloads_panel = "ptm_downloads_panel"
)

NAMESPACE_EXPDES = list(
  sidebar_controls = "sidebar_controls",
  protein_select = "protein_select",
  rep_range = "rep_range",
  run_simulation = "run_simulation",
  result_plot = "result_plot",
  download_future = "download_future"
)