#' Main LF calculation summarization function for MSstatsShiny application
#' 
#' Main LF function to calculate MSstatsShiny results. 
#' 
#' @export
#' @import MSstats
#' @importFrom shinybusy show_modal_progress_line update_modal_progress remove_modal_progress
#' 
#' @param data Data converted into MSstats format.
#' @param input options for data processing input by the user
#' @param busy_indicator Boolean indicator indicating whether or not to display 
#' shiny waiting indicator.
#' @return list of LF Summarization results
#' @examples
#' data("example_dia_skyline")
#' data("example_skyline_annotation")
#' testdata = MSstats::SkylinetoMSstatsFormat(example_dia_skyline,
#'                                             annotation = example_skyline_annotation,
#'                                             filter_with_Qvalue = TRUE, 
#'                                             qvalue_cutoff = 0.01, 
#'                                             fewMeasurements="remove", 
#'                                             removeProtein_with1Feature = TRUE,
#'                                             use_log_file = FALSE)
#' 
#' ## Source app functionality
#' input = list()
#' input$norm = "equalizeMedians"
#' input$log = 2
#' input$names = NULL
#' input$features_used	= "all"
#' code_n_feat=3
#' input$censInt = "NA"
#' input$features_used	= "all"
#' input$MBi = TRUE
#' input$remove50 = FALSE
#' input$maxQC = 0.999
#' input$null = FALSE
#' input$null1 = FALSE
#' input$DDA_DIA = "LF"
#' 
#' lf_summarization_loop(testdata, input, busy_indicator=FALSE)
#' 
lf_summarization_loop = function(data, input, busy_indicator = TRUE){
  
  proteins = as.character(unique(data[, 'ProteinName']))
  
  if (busy_indicator){
    show_modal_progress_line() # show the modal window
    
    ## Setup progress bar
    update_val = 1/length(proteins)
    counter = 0
  }
  
  if (input$features_used == "highQuality"){
    rm_feat = TRUE
  } else {
    rm_feat = FALSE
  }
  
  ## Prepare MSstats for summarization
  peptides_dict = makePeptidesDictionary(as.data.table(unclass(data)), 
                                         toupper(input$norm))
  prep_input = MSstatsPrepareForDataProcess(data, as.numeric(input$log), NULL)
  prep_input = MSstatsNormalize(prep_input, input$norm, peptides_dict, input$names)
  prep_input = MSstatsMergeFractions(prep_input)
  prep_input = MSstatsHandleMissing(prep_input, "TMP", input$MBi,
                                    "NA", QC_check(input))
  prep_input = MSstatsSelectFeatures(prep_input, input$features_used, input$n_feat, 2)
  processed = getProcessed(prep_input)
  prep_input = MSstatsPrepareForSummarization(prep_input, "TMP", input$MBi, 
                                              input$censInt, rm_feat)
  
  input_split = split(prep_input, prep_input$PROTEIN)
  summarized_results = vector("list", length(proteins))
  
  ## Loop over proteins
  for (i in seq_along(proteins)){
    
    temp_data = input_split[[i]]
    summarized_results[[i]] = MSstatsSummarizeSingleTMP(temp_data,
                                                        input$MBi, input$censInt, 
                                                        input$remove50)
    
    ## Update progress bar
    if (busy_indicator){
      counter = counter + update_val
      update_modal_progress(counter)
    }
  }
  
  ## Summarization output
  preprocessed = MSstatsSummarizationOutput(prep_input, summarized_results, 
                                             processed, "TMP", input$MBi, 
                                             input$censInt)
  
  if (busy_indicator){
    remove_modal_progress() # remove it when done
  }
  return(preprocessed)
  
}

#' Main TMT summarization calculation function for MSstatsShiny application
#' 
#' Main TMT function to calculate MSstatsShiny results. 
#' 
#' @export
#' @import MSstats
#' @import MSstatsTMT
#' @import MSstatsConvert
#' @import data.table
#' @importFrom shinybusy show_modal_progress_line update_modal_progress remove_modal_progress
#' @importFrom methods new
#' @importFrom stats median na.omit
#' 
#' @param data Data converted into MSstats format.
#' @param input options for data processing input by the user
#' @param busy_indicator Boolean indicator indicating whether or not to display 
#' shiny waiting indicator.
#' 
#' @return list of TMT summarization results
#' @examples
#' data("tmt_pd_summarized")
#' names(tmt_pd_summarized)
#' head(tmt_pd_summarized$ProteinLevelData)
#' 
tmt_summarization_loop = function(data, input, busy_indicator = TRUE){
  MBimpute = FALSE ## Add option for MBimpute to server..
  
  MSstatsConvert::MSstatsLogsSettings(FALSE,
                                      pkg_name = "MSstatsTMT")
  
  ## Prep functions
  prep_input = MSstatsTMT:::MSstatsPrepareForSummarizationTMT(
    data, input$summarization, input$global_norm, input$reference_norm,
    input$remove_norm_channel, TRUE, MBimpute, QC_check(input) 
  )
  prep_input = MSstatsTMT:::MSstatsNormalizeTMT(prep_input, "peptides", 
                                                input$global_norm)
  
  ## Go inside summarization loop to track progress
  log2Intensity = NULL
  annotation = unique(prep_input[!is.na(log2Intensity),
                                 c("Run", "Channel", "BioReplicate", "Condition",
                                   "Mixture", "TechRepMixture", "RunChannel"),
                                 with = FALSE])
  
  ## Current implementatin only keeps track of msstats progress
  ## Other functions are vectorized and should be faster (?)
  if (input$summarization == "msstats") {
    MSRun = FragmentIon = ProductCharge = IsotopeLabelType = ProteinName = 
      PeptideSequence = PrecursorCharge = Run = Condition = BioReplicate =
      Intensity = PSM = RunChannel = NULL
    
    runs = na.omit(unique(annotation$Run))
    num_runs = length(runs)
    
    data.table::setnames(prep_input, c("Run", "RunChannel", "Charge"),
                         c("MSRun", "Run", "PrecursorCharge"))
    prep_input[, FragmentIon := NA]
    prep_input[, ProductCharge := NA]
    prep_input[, IsotopeLabelType := "L"]
    
    processed_data = vector("list", num_runs)
    summarized_results = vector("list", num_runs)
    
    ## Setup progress bar
    if (busy_indicator){
      show_modal_progress_line() # show the modal window
      update_val = 1/num_runs
      counter = 0
    }
    
    for (i in seq_len(num_runs)) {
      
      single_run = prep_input[MSRun == runs[i],
                              list(ProteinName, PeptideSequence, PrecursorCharge,
                                   FragmentIon, ProductCharge, Run, Condition,
                                   BioReplicate, Intensity, IsotopeLabelType,
                                   Fraction = 1)]
      single_run = new("MSstatsValidated", single_run)
      
      ## Make LF flow into a function and replace it here
      msstats_summary = lf_summarization_loop(single_run, input, FALSE)
      
      feature_level_data = msstats_summary$FeatureLevelData
      msstats_cols = c("PROTEIN", "PEPTIDE", "originalRUN", "censored",
                       "predicted", "newABUNDANCE")
      msstats_cols = intersect(msstats_cols, colnames(feature_level_data))
      feature_level_data = feature_level_data[, msstats_cols]
      processed_data[[i]] = feature_level_data
      
      protein_level_data = msstats_summary$ProteinLevelData
      protein_level_data = protein_level_data[, c("Protein", "LogIntensities",
                                                  "originalRUN")]
      summarized_results[[i]] = protein_level_data
      
      ## Update progress bar
      if (busy_indicator){
        counter = counter + update_val
        update_modal_progress(counter)
      }
    }
    
    processed = data.table::rbindlist(processed_data)
    summarized_results = data.table::rbindlist(summarized_results)
    
    data.table::setnames(summarized_results,
                         c("LogIntensities", "originalRUN"),
                         c("Abundance", "RunChannel"))
    summarized_results = merge(summarized_results, annotation,
                               by = "RunChannel", all.x = TRUE)
    summarized_results = summarized_results[, colnames(summarized_results) != "RunChannel",
                                            with = FALSE]
    data.table::setnames(processed, 
                         c("PROTEIN", "PEPTIDE",
                           "originalRUN", "newABUNDANCE"),
                         c("ProteinName", "PSM", 
                           "RunChannel", "log2Intensity"))
    processed = merge(processed, annotation,
                      by = "RunChannel", all.x = TRUE)
    processed[, c("PeptideSequence", "Charge") := tstrsplit(PSM, "_", fixed=TRUE)]
    processed[, RunChannel := NULL]
    summarized = list(summarized_results, processed)
    
  } else if (input$summarization == "MedianPolish") {
    summarized = MSstatsTMT:::.summarizeTMP(prep_input, annotation)
  } else if (input$summarization == "LogSum") {
    summarized = MSstatsTMT:::.summarizeSimpleStat(prep_input, annotation, 
                                                   .logSum)
  } else if (input$summarization == "Median") {
    summarized = MSstatsTMT:::.summarizeSimpleStat(prep_input, annotation, median)
  }
  
  ## Output functions
  processed = MSstatsTMT:::getProcessedTMT(summarized, prep_input)
  summarized = MSstatsTMT:::getSummarizedTMT(summarized)
  summarized = MSstatsTMT:::MSstatsNormalizeTMT(summarized, "proteins", 
                                                input$reference_norm)
  preprocessed = MSstatsTMT:::MSstatsSummarizationOutputTMT(summarized,
                                                            processed, TRUE,
                                                            input$remove_norm_channel)
  if (busy_indicator){
    remove_modal_progress() # remove it when done
  }
  
  return(preprocessed)
}

#' Main LF modeling function for MSstatsShiny application
#' 
#' Main LF function to model MSstatsShiny data. 
#' 
#' @export
#' @import MSstats
#' @import data.table
#' @importFrom shinybusy show_modal_progress_line update_modal_progress remove_modal_progress
#' @importFrom utils txtProgressBar
#' 
#' @param data summarized data from output of MSstats summarization function.
#' @param contrast.matrix contrast matrix specifying which conditions should be compared
#' @param busy_indicator Boolean indicator indicating whether or not to display 
#' shiny waiting indicator.
#' 
#' @return list of LF modeling results
#' @examples
#' data("dia_skyline_model")
#' names(dia_skyline_model)
#' head(dia_skyline_model$ComparisonResult)
#' 
lf_model = function(data, contrast.matrix, busy_indicator = TRUE){
  
  proteins = as.character(unique(data$ProteinLevelData[, 'Protein']))
  
  if (busy_indicator){
    show_modal_progress_line() # show the modal window
    
    ## Setup progress bar
    update_val = 1/length(proteins)
    counter = 0
  }
  
  ## Prepare data for modeling
  labeled = data.table::uniqueN(data$FeatureLevelData$Label) > 1
  split_summarized = MSstatsPrepareForGroupComparison(data)
  repeated = checkRepeatedDesign(data)
  samples_info = getSamplesInfo(data)
  groups = unique(data$ProteinLevelData$GROUP)
  contrast_matrix = MSstatsContrastMatrix(contrast.matrix, groups)
  
  ## Inside MSstatsGroupComparison function
  groups = sort(colnames(contrast_matrix))
  has_imputed = attr(split_summarized, "has_imputed")
  all_proteins_id = seq_along(split_summarized)
  test_results = vector("list", length(all_proteins_id))
  pb = txtProgressBar(max = length(all_proteins_id), style = 3)
  
  for (i in all_proteins_id) {
    comparison_outputs = MSstatsGroupComparisonSingleProtein(
      split_summarized[[i]], contrast_matrix, repeated, 
      groups, samples_info, TRUE, has_imputed
    )
    test_results[[i]] = comparison_outputs
    
    ## Update progress bar
    if (busy_indicator){
      counter = counter + update_val
      update_modal_progress(counter)
    }
  }
  
  results = MSstatsGroupComparisonOutput(test_results, data, 2) ## 2 is log_base param
  
  if (busy_indicator){
    remove_modal_progress() # remove it when done
  }
  
  return(results)
  
}

#' Main TMT modeling function for MSstatsShiny application
#' 
#' Main TMT function to model MSstatsShiny data. 
#' 
#' @export
#' @import MSstatsTMT
#' @import data.table
#' @importFrom shinybusy show_modal_progress_line update_modal_progress remove_modal_progress
#' 
#' @param data summarized data from output of MSstats summarization function.
#' @param input options for data processing input by the user
#' @param contrast.matrix contrast matrix specifying which conditions should be compared
#' @param busy_indicator Boolean indicator indicating whether or not to display 
#' shiny waiting indicator.
#' 
#' @return list of TMT modeling results
#' @examples
#' data("tmt_pd_model")
#' names(tmt_pd_model)
#' head(tmt_pd_model$ComparisonResult)
#' 
tmt_model = function(data, input, contrast.matrix, busy_indicator = TRUE){
  
  proteins = as.character(unique(data$ProteinLevelData[, 'Protein']))
  
  if (busy_indicator){
    show_modal_progress_line() # show the modal window
    
    ## Setup progress bar
    update_val = 1/length(proteins)
    counter = 0
  }
  
  ## Prep data for modeling
  summarized = MSstatsTMT:::MSstatsPrepareForGroupComparisonTMT(data$ProteinLevelData, 
                                                                TRUE,#remove_norm_channel
                                                                TRUE)#remove_empty_channel
  contrast_matrix = MSstats::MSstatsContrastMatrix(contrast.matrix,
                                                   unique(summarized$Group))
  fitted_models = MSstatsTMT:::MSstatsFitComparisonModelsTMT(summarized)
  FittedModel = fitted_models$fitted_model
  names(FittedModel) = fitted_models$protein
  
  fitted_models = MSstatsTMT:::MSstatsModerateTTest(summarized, fitted_models, 
                                                    input$moderated)#moderated
  
  testing_results = vector("list", length(fitted_models))
  
  for (i in seq_along(fitted_models)) {
    testing_result = MSstatsTMT:::MSstatsTestSingleProteinTMT(fitted_models[[i]], 
                                                              contrast_matrix)
    testing_results[[i]] = testing_result
    
    ## Update progress bar
    if (busy_indicator){
      counter = counter + update_val
      update_modal_progress(counter)
    }
  }
  
  testing_results = MSstatsTMT:::MSstatsGroupComparisonOutputTMT(
    testing_results, "BH") #adj.method
  
  results = list(ComparisonResult = testing_results, 
                 ModelQC = NULL,
                 FittedModel = FittedModel)   
  
  if (busy_indicator){
    remove_modal_progress() # remove it when done
  }
  
  return(results)
  
}

#' Main PTM adjustment function
#' 
#' Main PTM function to model MSstatsShiny data. 
#' 
#' @export
#' @import MSstats
#' @import data.table
#' @importFrom shinybusy show_modal_progress_line update_modal_progress remove_modal_progress
#' 
#' @param ptm_model output of MSstats modeling function modeling PTMs
#' @param protein_model output of MSstats modeling function modeling unmodified proteins
#' 
#' @return list of PTM modeling results
#' @examples
#' #apply_adj(NULL)
#' 
apply_adj = function(ptm_model, protein_model){
  
  ptm_model_site_sep = copy(ptm_model)
  ## extract global protein name
  ptm_model_site_sep = MSstatsPTM:::.extractProtein(ptm_model_site_sep, 
                                                    protein_model)
  
  ## adjustProteinLevel function can only compare one label at a time
  comparisons = unique(ptm_model_site_sep[, Label])
  
  adjusted_model_list = list()
  for (i in seq_len(length(comparisons))) {
    
    temp_adjusted_model = MSstatsPTM:::.applyPtmAdjustment(comparisons[[i]],
                                                           ptm_model_site_sep,
                                                           protein_model)
    adjusted_model_list[[i]] = temp_adjusted_model
  }
  
  adjusted_models = rbindlist(adjusted_model_list)
  
  adjusted_models$GlobalProtein = adjusted_models$Protein
  adjusted_models$Protein = adjusted_models$Site
  adjusted_models[, Site := NULL]
  
  return(adjusted_models)
}

#' Quick QC value check
#' 
#' Quick QC value check for LF vs TMT
#' 
#' @export
#' @param input options for data processing input by the user
#' @return string
#' @examples
#' input = list(null=TRUE)
#' QC_check(input)
QC_check = function(input) {
  if (input$null == TRUE || input$null1 == TRUE) {
    maxQC = NULL
  }
  else {
    if(input$DDA_DIA=="TMT"){
      maxQC = input$maxQC1
    }
    else{
      maxQC = input$maxQC
    }
    
  }
  return(maxQC)
}