#' Read preview columns from a data file (handles CSV, TSV, and Parquet)
#'
#' @param filepath Path to the file.
#' @param filename Original filename (used to detect parquet extension).
#' @param nrows Number of rows to read. Default 100. Parquet returns columns only.
#' @return A data frame with up to `nrows` rows, or NULL on error.
#' @noRd
.read_preview <- function(filepath, filename = NULL, nrows = 100) {
  ext <- if (!is.null(filename)) tolower(tools::file_ext(basename(filename))) else ""
  tryCatch({
    if (ext %in% c("parquet", "pq")) {
      # For parquet, read only the schema (column names) to avoid OOM on large files.
      # Return an empty data frame with the correct column structure for detection.
      schema <- arrow::open_dataset(filepath, format = "parquet")$schema
      col_names <- schema$names
      empty_df <- as.data.frame(
        setNames(lapply(col_names, function(x) logical(0)), col_names)
      )
      empty_df
    } else {
      data.table::fread(filepath, nrows = nrows, header = TRUE)
    }
  }, error = function(e) NULL)
}

#' Detect whether a DIANN preview is in 2.0+ format
#'
#' DIANN 2.0+ files have per-fragment columns (Fr.0.Quantity, Fr.1.Quantity, etc.)
#' and no FragmentQuantCorrected column. Older versions use a single
#' Fragment.Quant.Corrected / FragmentQuantCorrected column.
#'
#' @param preview_df Data frame preview of the DIANN file.
#' @return Logical. TRUE if the file appears to be DIANN 2.0+.
#' @noRd
.is_diann_2plus <- function(preview_df) {
  if (is.null(preview_df) || ncol(preview_df) == 0) return(FALSE)
  cols <- names(preview_df)
  # DIANN 2.0+ signature: numbered fragment columns like "Fr.0.Quantity"
  has_numbered_fragments <- any(grepl("^Fr\\.[0-9]+\\.Quantity$", cols))
  # DIANN 1.x signature: the legacy fragment column
  has_legacy_fragments <- any(cols %in% c("Fragment.Quant.Corrected", "FragmentQuantCorrected"))
  has_numbered_fragments && !has_legacy_fragments
}

#' Extract unique modification IDs from preview data
#'
#' Parses the Full Sequence column to find bracket-enclosed modification IDs.
#' @param preview_df Data frame with first 100 rows of uploaded file.
#' @return Character vector of unique modification IDs (clean, no escaping).
#' @noRd
.extract_mod_ids_from_preview <- function(preview_df) {
  if (is.null(preview_df) || nrow(preview_df) == 0) return(character(0))

  col_name <- grep("Full.Sequence|FullSequence|Full Sequence",
                   names(preview_df), value = TRUE, ignore.case = TRUE)
  if (length(col_name) == 0) return(character(0))

  sequences <- as.character(preview_df[[col_name[1]]])
  all_mods <- regmatches(sequences, gregexpr("\\[[^\\]]+\\]", sequences, perl = TRUE))
  unique_mods <- sort(unique(unlist(all_mods)))
  return(unique_mods)
}

#' Resolve the modification ID from dropdown or manual entry
#'
#' Returns an escaped modification ID string ready for regex matching.
#' @param selected The value from the dropdown (mod_id_meta_select).
#' @param custom The value from the manual text input (mod_id_meta_custom).
#' @return Character string with escaped brackets for regex.
#' @noRd
.resolve_mod_id <- function(selected = NULL, custom = NULL) {
  if (!is.null(selected) && selected != "__other__") {
    return(gsub("(\\[|\\])", "\\\\\\1", selected))
  }
  if (!is.null(custom) && nchar(custom) > 0) {
    val <- custom
    # Normalize: strip any existing escapes, then re-escape both brackets
    val <- gsub("\\\\\\[", "[", val)
    val <- gsub("\\\\\\]", "]", val)
    val <- gsub("(\\[|\\])", "\\\\\\1", val)
    return(val)
  }
  stop("No modification ID selected. Please select a modification from the dropdown or enter one manually.")
}

# loadpage server functions
getEvidence <- function(input) {
  evidence = input$evidence
  if(is.null(input$evidence)) {
    return(NULL)
  }
  evidence = try(data.table::fread(evidence$datapath), silent=TRUE)
  if (is(evidence,"try-error")) {
    evidence = "File load error. Please ensure file is in csv format."
  }

  cat(file=stderr(), "Reached in evidence\n")
  return(evidence)
}

getEvidence2 <- function(input) {
  evidence2 = input$evidence2
  if(is.null(input$evidence2)) {
    return(NULL)
  }
  evidence2 = try(data.table::fread(evidence2$datapath), silent=TRUE)
  if (is(evidence2,"try-error")) {
    evidence2 = "File load error. Please ensure file is in csv format."
  }
  cat(file=stderr(), "Reached in evidence\n")
  return(evidence2)

}

getGlobal <- function(input) {
  unmod = input$unmod
  if(is.null(input$unmod)) {
    return(NULL)
  }
  unmod = try(data.table::fread(unmod$datapath), silent=TRUE)
  if (is(unmod,"try-error")) {
    unmod = "File load error. Please ensure file is in csv format."
  }

  cat(file=stderr(), "Reached in unmod\n")
  return(unmod)

}

getProteinGroups <- function(input) {
  pGroup = input$pGroup
  if(is.null(input$pGroup)) {
    return(NULL)
  }
  pGroup = try(data.table::fread(pGroup$datapath), silent=TRUE)
  if (is(pGroup,"try-error")) {
    pGroup = "File load error. Please ensure file is in csv format."
  }

  cat(file=stderr(), "Reached in proteins_group\n")
  return(pGroup)
}

getProteinGroups2 <- function(input) {
  pGroup2 = input$pGroup2
  if(is.null(input$pGroup2)) {
    return(NULL)
  }
  pGroup2 = try(data.table::fread(pGroup2$datapath), silent=TRUE)
  if (is(pGroup2,"try-error")) {
    pGroup2 = "File load error. Please ensure file is in csv format."
  }
  cat(file=stderr(), "Reached in proteins_group\n")
  return(pGroup2)
}

getFragSummary <- function(input) {
  fragSummary = input$fragSummary
  if(is.null(input$fragSummary)) {
    return(NULL)
  }
  fragSummary = try(data.table::fread(fragSummary$datapath), silent=TRUE)
  if (is(fragSummary,"try-error")) {
    fragSummary = "File load error. Please ensure file is in excel format."
  }
  return(fragSummary)

}

getPeptideSummary <- function(input) {
  peptideSummary = input$peptideSummary
  if(is.null(input$peptideSummary)) {
    return(NULL)
  }
  peptideSummary = try(data.table::fread(peptideSummary$datapath), silent=TRUE)
  if (is(peptideSummary,"try-error")) {
    peptideSummary = "File load error. Please ensure file is in csv format."
  }
  return(peptideSummary)

}

getProtSummary <- function(input) {
  protSummary = input$protSummary
  if(is.null(input$protSummary)) {
    return(NULL)
  }
  protSummary = try(data.table::fread(protSummary$datapath), silent=TRUE)
  if (is(protSummary,"try-error")) {
    protSummary = "File load error. Please ensure file is in csv format."
  }
  return(protSummary)

}

getMaxqPtmSites <- function(input) {
  maxq_ptm_sites = input$maxq_ptm_sites
  if(is.null(input$maxq_ptm_sites)) {
    return(NULL)
  }
  maxq_ptm_sites = try(data.table::fread(maxq_ptm_sites$datapath), silent=TRUE)
  if (is(maxq_ptm_sites,"try-error")) {
    maxq_ptm_sites = "File load error. Please ensure file is in csv format."
  }
  cat(file=stderr(), "Reached in maxq_ptm_sites\n")
  return(maxq_ptm_sites)

}

getAnnot3 <- function(input) {
  annot3 = input$annot3
  if(is.null(input$annot3)) {
    return(NULL)
  }
  annot3 = try(data.table::fread(annot3$datapath), silent=TRUE)
  if (is(annot3,"try-error")) {
    annot3 = "File load error. Please ensure file is in csv format."
  }
  cat(file=stderr(), "Reached in ump annot\n")
  return(annot3)

}

getAnnot2 <- function(input) {
  annot1 = input$annot2
  if(is.null(input$annot2)) {
    return(NULL)
  }
  annot2=try(data.table::fread(annot1$datapath), silent=TRUE)

  if (is(annot2,"try-error")) {
    annot2 = "File load error. Please ensure file is in csv format."
  }
  cat(file=stderr(), "Reached in ump annot\n")
  return(annot2)

}

getAnnot <- function(input) {
  print("Inside Annot");
  annot = input$annot
  if(is.null(annot)) {
    return(NULL)
  }
  else if (input$DDA_DIA == "TMT" && input$filetype == "sample")
  {
    return(annotation.pd)
  }
  print("before reading annot file")
  annot_file = try(data.table::fread(annot$datapath), silent=TRUE)
  print("after reading annot file")
  if(is(annot_file,"try-error")) {
    annot_file = "File load error. Please ensure file is in csv format."
  }
  return(annot_file)
}

getAnnot1 <- function(input) {
  annot1 = input$annot1
  if(is.null(input$annot1)) {
    return(NULL)
  }
  annot1=try(data.table::fread(annot1$datapath), silent=TRUE)

  if (is(annot1,"try-error")) {
    annot1 = "File load error. Please ensure file is in csv format."
  }
  cat(file=stderr(), "Reached in maxq annot\n")
  return(annot1)

}

#' @importFrom tools file_ext
getFileExtension <- function(filename) {
  if (is.null(filename) || is.na(filename) || identical(filename, "")) return("")
  tolower(file_ext(basename(filename)))
}

#' @importFrom arrow read_parquet
getData <- function(input) {
  show_modal_spinner()
  ev_maxq = getEvidence(input)
  pg_maxq = getProteinGroups(input)
  # ev_maxq2 = getEvidence2(input)
  # pg_maxq2 = getProteinGroups2(input)
  an_maxq = getAnnot1(input)
  raw.frag = getFragSummary(input)
  raw.pep = getPeptideSummary(input)
  raw.pro = getProtSummary(input)
  annot2 = getAnnot2(input)
  annot3 = getAnnot3(input)
  unmod = getGlobal(input)
  # ptm_sites_data = getMaxqPtmSites(input)

  cat(file=stderr(), "Reached in get_data\n")

  cat(file=stderr(), paste("File type is",input$filetype,"\n"))
  if(is.null(input$filetype)) {
    return(NULL)
  }
  if(input$filetype == 'sample') {
    if(input$BIO != "PTM" && input$DDA_DIA =='LType' && input$LabelFreeType == "SRM_PRM") {
      mydata = MSstats::DDARawData
    }
    else if(input$BIO != "PTM" &&  input$DDA_DIA == 'LType' && input$LabelFreeType == "DDA") {
      mydata = MSstats::DDARawData
    }
    else if(input$BIO != "PTM" && input$DDA_DIA =='LType' && input$LabelFreeType == "DIA"){
      mydata = data.table::fread(system.file("extdata/dataset.csv",
                                             package = "MSstatsShiny"))
    }
    else if(input$BIO != "PTM" && input$DDA_DIA == "TMT"){
      mydata = PDtoMSstatsTMTFormat(input = MSstatsTMT::raw.pd,
                                    annotation = MSstatsTMT::annotation.pd,
                                    which.proteinid = input$which.proteinid,
                                    use_log_file = FALSE
      )
    }
    else if (input$BIO == "PTM"){ 
      if (input$DDA_DIA != "TMT"){
        mydata = MSstatsPTM::raw.input
      } else {
        mydata = MSstatsPTM::raw.input.tmt
      }
    }
  }
  else if (input$BIO == 'PTM' || (input$BIO == 'PTM' && input$DDA_DIA == 'TMT')){
    if (input$filetype == 'maxq') {
      mydata = data.table::fread(input$ptm_input$datapath)
      print(input$globaldata$datapath)
      mydata_protein = try(data.table::fread(input$ptm_protein_input$datapath),silent=TRUE)
      if (typeof(mydata_protein)=="character"){
        mydata_protein=NULL
        use_unmod_peptides=TRUE
      } else {
        use_unmod_peptides=FALSE
      }

      pg_maxq_ptm = try(data.table::fread(input$ptm_pgroup$datapath),silent=TRUE)
      annotation = try(data.table::fread(input$ptm_annot$datapath),silent=TRUE)
      if (input$BIO == "PTM" && input$DDA_DIA == "TMT"){
        label = "TMT"
      } else {
        label = "LF"
      }
      mydata = MaxQtoMSstatsPTMFormat(evidence=mydata,
                                      annotation=annotation,
                                      input$fasta$datapath,
                                      mod_id=input$mod_id_maxq,
                                      evidence_prot=mydata_protein,
                                      proteinGroups=pg_maxq_ptm,
                                      annotation_protein=annotation,
                                      use_unmod_peptides=use_unmod_peptides,
                                      labeling_type=label)
      mydata$PROTEIN = as.data.frame(mydata$PROTEIN)
      print(mydata)

    } else if(input$filetype=='phil'){

      mydata = data.table::fread(input$ptmdata$datapath)
      mydata_protein = try(data.table::fread(input$globaldata$datapath),silent=TRUE)
      annotation = data.table::fread(input$annotation$datapath)
      annotation_protein = try(data.table::fread(input$globalannotation$datapath),silent=TRUE)

      mydata = FragPipetoMSstatsPTMFormat(mydata,
                                          annotation,
                                          mydata_protein,
                                          annotation_protein,
                                          mod_id_col = input$mod_id_col,
                                          localization_cutoff = as.numeric(input$localization_cutoff),
                                          remove_unlocalized_peptides=input$remove_unlocalized_peptides)

      mydata$PTM$Condition = as.character(mydata$PTM$Condition)
      mydata$PTM[mydata$PTM$Condition == "NORM", "Condition"] = "Norm"

      mydata$PROTEIN$Condition = as.character(mydata$PROTEIN$Condition)
      mydata$PROTEIN[mydata$PROTEIN$Condition == "NORM", "Condition"] = "Norm"

    } else if (input$filetype=='PD'){
      mydata = data.table::fread(input$ptm_input$datapath)
      mydata_protein = try(
        data.table::fread(input$ptm_protein_input$datapath),
        silent=TRUE)
      annotation = data.table::fread(input$ptm_annot$datapath)

      if (typeof(mydata_protein)=="character"){
        mydata_protein=NULL
        use_unmod_peptides=TRUE
      } else {
        use_unmod_peptides=FALSE
      }

      if (input$BIO == "PTM" && input$DDA_DIA == "TMT"){
        label = "TMT"
      } else {
        label = "LF"
      }

      mydata = PDtoMSstatsPTMFormat(mydata,
                                    annotation,
                                    input$fasta$datapath,
                                    protein_input=mydata_protein,
                                    annotation_protein=annotation,
                                    labeling_type = label,
                                    mod_id=input$mod_id_pd,
                                    use_unmod_peptides=use_unmod_peptides,
                                    which_proteinid = "Master.Protein.Accessions")
    } else if (input$filetype=='spec'){
      mydata = data.table::fread(input$ptm_input$datapath)
      mydata_protein = try(data.table::fread(input$ptm_protein_input$datapath),silent=TRUE)
      annotation = data.table::fread(input$ptm_annot$datapath)

      if (typeof(mydata_protein)=="character"){
        mydata_protein=NULL
        use_unmod_peptides=TRUE
      } else {
        use_unmod_peptides=FALSE
      }

      mydata = SpectronauttoMSstatsPTMFormat(mydata,
                                             annotation = annotation,
                                             fasta_path = input$fasta$datapath,
                                             protein_input = mydata_protein,
                                             annotation_protein = annotation,
                                             use_unmod_peptides=use_unmod_peptides,
                                             intensity = "PeakArea",
                                             mod_id=input$mod_id_spec)

    } else if (input$filetype=='sky') {
      mydata = read_excel(input$ptm_input$datapath)
      mydata_protein = try(read_excel(input$ptm_protein_input$datapath),silent=TRUE)
      annotation = try(data.table::fread(input$ptm_annot$datapath),silent=TRUE)

      if (typeof(mydata_protein)=="character"){
        mydata_protein=NULL
        use_unmod_peptides=TRUE
      } else {
        use_unmod_peptides=FALSE
      }

      if (typeof(annotation)=="character"){
        annotation = NULL
      }

      mydata = SkylinetoMSstatsPTMFormat(mydata,
                                         input$fasta$datapath,
                                         annotation=annotation,
                                         input_protein=mydata_protein,
                                         annotation_protein=annotation,
                                         use_unmod_peptides=use_unmod_peptides)

    } else if (input$filetype == "meta") {
      cat(file=stderr(), "Reached in metamorpheus PTM\n")

      # Read PTM input data and annotation
      if (is.null(input$ptm_input) || is.null(input$ptm_annot)) {
        remove_modal_spinner()
        showNotification("Please upload both PTM peaks and annotation files.", type = "error")
        return(NULL)
      }
      ptm_data = data.table::fread(input$ptm_input$datapath)
      ptm_annotation = as.data.frame(data.table::fread(input$ptm_annot$datapath))

      # Read recommended unmodified protein data
      protein_data_raw = try(data.table::fread(input$ptm_protein_input$datapath), silent=TRUE)
      if (is(protein_data_raw, "try-error")) {
        protein_data = NULL
      } else {
        protein_data = protein_data_raw
      }
      use_unmod_peptides = FALSE

      # Default protein annotation to NULL; override if uploaded
      protein_annotation = NULL
      if (!is.null(input$ptm_protein_annot)) {
        protein_annot_raw = try(as.data.frame(data.table::fread(input$ptm_protein_annot$datapath)), silent=TRUE)
        if (!is(protein_annot_raw, "try-error")) {
          protein_annotation = protein_annot_raw
        }
      }
      
      # Resolve mod ID from dropdown or manual entry, escaping brackets for regex
      mod_id_value <- tryCatch(
        .resolve_mod_id(input$mod_id_meta_select, input$mod_id_meta_custom),
        error = function(e) {
          remove_modal_spinner()
          showNotification(conditionMessage(e), type = "error", duration = 8)
          return(NULL)
        }
      )
      if (is.null(mod_id_value)) return(NULL)

      mydata = tryCatch(
        MetamorpheusToMSstatsPTMFormat(
          data.table::copy(ptm_data),
          ptm_annotation,
          fasta_path = input$fasta$datapath,
          input_protein = if (!is.null(protein_data)) data.table::copy(protein_data) else NULL,
          annotation_protein = protein_annotation,
          use_unmod_peptides = use_unmod_peptides,
          mod_ids = c(mod_id_value)
        ),
        error = function(e) {
          remove_modal_spinner()
          showNotification(
            paste("Failed to process Metamorpheus PTM data. Please check your modification ID and input files:", conditionMessage(e)),
            type = "error", duration = 10)
          return(NULL)
        }
      )
      if (is.null(mydata)) return(NULL)
    } else {
      data = data.table::fread(input$msstatsptmdata$datapath)
      mydata = list("PTM" = data, "PROTEIN" = unmod)
    }
  }
  else if (input$filetype == "msstats"){
    mydata = data.table::fread(input$msstatsdata$datapath)
  }
  else {
    if(input$filetype=='spec' || input$filetype=='spmin'){
      infile = input$data1
    }
    else if(input$filetype=='phil' & input$BIO != "PTM"){
      mydata = data.table::fread(input$data$datapath)

    }
    else{
      infile = input$data
    }

    # TODO: This code stops processing if a file is not uploaded correctly.
    #         ATM no error messages show and the load circle spins forever
    # if(input$filetype=='maxq'){
    #   if(is.null(ev_maxq) || is.null(pg_maxq) || is.null(an_maxq) ) {
    #     return(NULL)
    #   }
    # }
    # else if(input$filetype=='ump'){
    #   if(is.null(raw.frag) || is.null(raw.pep) || is.null(raw.pro) || is.null(annot2)) {
    #     return(NULL)
    #   }
    #
    #
    # } else {
    #   if(is.null(infile)) {
    #     return(NULL)
    #   }
    # }

    if(input$filetype == '10col') {
      mydata = data.table::fread(infile$datapath)
    }
    else if(input$filetype == 'sky') {
      cat(file=stderr(), "Reached here in skyline\n")
      data = data.table::fread(input$skylinedata$datapath)
        mydata = SkylinetoMSstatsFormat(data,
                                        annotation = getAnnot(input),
                                        filter_with_Qvalue = input$q_val, 
                                        qvalue_cutoff = input$q_cutoff,
                                        fewMeasurements="remove",
                                        removeProtein_with1Feature = TRUE,
                                        use_log_file = FALSE)
    }
    else if(input$filetype == 'maxq') {
      cat(file=stderr(), "Reached in maxq\n")
      if(input$DDA_DIA=="TMT"){
        mydata = MaxQtoMSstatsTMTFormat(evidence=ev_maxq,
                                        annotation=an_maxq,
                                        proteinGroups=pg_maxq,
                                        use_log_file = FALSE)

      }
      else{
        mydata = MaxQtoMSstatsFormat(evidence= ev_maxq, annotation= an_maxq,
                                     proteinGroups= pg_maxq,
                                     useUniquePeptide = TRUE,
                                     summaryforMultipleRows = max,
                                     removeProtein_with1Peptide=input$remove,
                                     use_log_file = FALSE)
      }

    }
    else if(input$filetype == 'prog') {
      cat(file=stderr(), "Reached in prog\n")

      data = data.table::fread(infile$datapath)

      mydata = ProgenesistoMSstatsFormat(data, annotation = getAnnot(input),
                                         removeProtein_with1Peptide = TRUE,
                                         use_log_file = FALSE)
      colnames(mydata)[colnames(mydata) == 'PeptideModifiedSequence'] = 'PeptideSequence'
    }
    else if(input$filetype == 'PD') {

      if(input$DDA_DIA=="TMT"){

        data = data.table::fread(infile$datapath)
        mydata = PDtoMSstatsTMTFormat(input = data,
                                      annotation = getAnnot(input),
                                      which.proteinid = input$which.proteinid, ## same as default
                                      use_log_file = FALSE
        )
      }
      else{
        data = data.table::fread(infile$datapath)
        print(data)
        mydata = PDtoMSstatsFormat(data, annotation = getAnnot(input),
                                   removeProtein_with1Peptide = input$remove,
                                   use_log_file = FALSE)
        colnames(mydata)[colnames(mydata) == 'PeptideModifiedSequence'] = 'PeptideSequence'
      }

    }
    else if(input$filetype == 'spec') {
      
      if (isTRUE(input$big_file_spec)) {
        # Logic for big Spectronaut files
        # Parse the file path from shinyFiles input
        volumes <- shinyFiles::getVolumes()()
        path_info <- shinyFiles::parseFilePaths(volumes, input$big_file_browse)
        local_big_file_path <- if (nrow(path_info) > 0) path_info$datapath else NULL
        
        # Validate inputs
        if (!is.numeric(input$qvalue_cutoff) || is.na(input$qvalue_cutoff) || input$qvalue_cutoff < 0 || input$qvalue_cutoff > 1) {
          showNotification("Error: qvalue_cutoff must be between 0 and 1.", type = "error")
          shinybusy::remove_modal_spinner()
          return(NULL)
        }

        if (!is.numeric(input$max_feature_count) || is.na(input$max_feature_count) || input$max_feature_count <= 0) {
          showNotification("Error: max_feature_count must be a positive number.", type = "error")
          shinybusy::remove_modal_spinner()
          return(NULL)
        }

        if (is.null(local_big_file_path) || !file.exists(local_big_file_path)) {
          showNotification("Error: The selected file does not exist or is not readable.", type = "error")
          shinybusy::remove_modal_spinner()
          return(NULL)
        }

        if (isTRUE(input$calculate_anomaly_scores) && is.null(input$run_order_file)) {
          showNotification(
            "Error: Run Order CSV is required when Calculate Anomaly Scores is enabled. Please upload a CSV with Run and Order columns.",
            type = "error",
            duration = NULL)
          shinybusy::remove_modal_spinner()
          return(NULL)
        }

        shinybusy::update_modal_spinner(text = "Processing large Spectronaut file...")

        # Base arguments shared by every large-file Spectronaut run.
        # Optional args (annotation override, anomaly-feature
        # carry-through) are spliced in below so callers that don't
        # supply them aren't forced to pass NULL / FALSE explicitly.
        big_spec_args <- list(
          input_file = local_big_file_path,
          output_file_name = "output_file.csv",
          backend = "arrow",
          filter_by_excluded = input$filter_by_excluded,
          filter_by_identified = input$filter_by_identified,
          filter_by_qvalue = input$filter_by_qvalue,
          qvalue_cutoff = input$qvalue_cutoff,
          max_feature_count = input$max_feature_count,
          filter_unique_peptides = input$filter_unique_peptides,
          aggregate_psms = input$aggregate_psms,
          filter_few_obs = input$filter_few_obs
        )

        if (!is.null(input$spec_intensity_col) &&
            nchar(trimws(input$spec_intensity_col)) > 0) {
          big_spec_args$intensity <- trimws(input$spec_intensity_col)
        }

        if (!is.null(input$big_spec_annotation)) {
          big_spec_args$annotation <- data.table::fread(
            input$big_spec_annotation$datapath)
        }

        if (isTRUE(input$calculate_anomaly_scores)) {
          big_spec_args$calculateAnomalyScores <- TRUE
          big_spec_args$anomalyModelFeatures <- c(
            "FG.ShapeQualityScore (MS2)",
            "FG.ShapeQualityScore (MS1)",
            "EG.DeltaRT")
        }

        converted_data <- do.call(
          MSstatsBig::bigSpectronauttoMSstatsFormat, big_spec_args)
        
        # Attempt to load the data into memory. 
        mydata <- tryCatch({
          dplyr::collect(converted_data)
        }, error = function(e) {
          showNotification(
            paste("Memory Error: The dataset is too large to process in-memory.", e$message),
            type = "error",
            duration = NULL
          )
          return(NULL)
        })
        
        if (is.null(mydata)) {
          shinybusy::remove_modal_spinner()
          return(NULL)
        }

        if (isTRUE(input$calculate_anomaly_scores) &&
            !is.null(input$run_order_file)) {
          run_order <- data.table::fread(input$run_order_file$datapath)
          mydata <- MSstatsConvert::MSstatsAnomalyScores(
            input = mydata,
            quality_metrics = c("FGShapeQualityScore(MS2)",
                                "FGShapeQualityScore(MS1)",
                                "EGDeltaRT"),
            temporal_direction = c("mean_decrease",
                                   "mean_decrease",
                                   "dispersion_increase"),
            missing_run_count = 0.5,
            n_feat = 100,
            run_order = run_order,
            n_trees = 100,
            max_depth = "auto",
            cores = 1)
        }

      } else {

        if (isTRUE(input$calculate_anomaly_scores) && is.null(input$run_order_file)) {
          showNotification(
            "Error: Run Order CSV is required when Calculate Anomaly Scores is enabled. Please upload a CSV with Run and Order columns.",
            type = "error",
            duration = NULL)
          remove_modal_spinner()
          return(NULL)
        }

        data = data.table::fread(input$specdata$datapath)
        # Base arguments for the Spectronaut converter
        converter_args = list(
          input = data,
          annotation = getAnnot(input),
          filter_with_Qvalue = input$q_val,
          qvalue_cutoff = input$q_cutoff,
          removeProtein_with1Feature = input$remove,
          use_log_file = FALSE
        )

        # Add protein turnover specific parameters if provided
        if (!is.null(input$spec_intensity_col) && nchar(trimws(input$spec_intensity_col)) > 0) {
          converter_args$intensity <- trimws(input$spec_intensity_col)
        }
        if (!is.null(input$spec_peptide_seq_col) && nchar(trimws(input$spec_peptide_seq_col)) > 0) {
          converter_args$peptideSequenceColumn <- trimws(input$spec_peptide_seq_col)
        }
        if (!is.null(input$spec_heavy_labels) && nchar(trimws(input$spec_heavy_labels)) > 0) {
          converter_args$heavyLabels <- trimws(strsplit(trimws(input$spec_heavy_labels), ",\\s*")[[1]])
        }

        if (isTRUE(input$calculate_anomaly_scores) && !is.null(input$run_order_file)) {
          # Add anomaly score parameters only if the checkbox is checked
          converter_args$calculateAnomalyScores = TRUE
          converter_args$runOrder = data.table::fread(input$run_order_file$datapath)
          converter_args$anomalyModelFeatures = c("FG.ShapeQualityScore (MS2)", "FG.ShapeQualityScore (MS1)", "EGDeltaRT")
          converter_args$anomalyModelFeatureTemporal = c("mean_decrease", "mean_decrease", "dispersion_increase")
          converter_args$n_trees = 100
          converter_args$max_depth = "auto"
          converter_args$numberOfCores = 1
        }
        mydata = do.call(SpectronauttoMSstatsFormat, converter_args)
      }
    }
    else if(input$filetype == 'diann') {
      if (getFileExtension(input$dianndata$name) %in% c("parquet", "pq")) {
        data = read_parquet(input$dianndata$datapath)
      } else {
        data = data.table::fread(input$dianndata$datapath)
      }
      
      qvalue_cutoff = 0.01
      MBR = FALSE
      if (isTRUE(input$q_val)) {
        if (is.numeric(input$q_cutoff) && length(input$q_cutoff) == 1L &&
            !is.na(input$q_cutoff) && input$q_cutoff >= 0 && input$q_cutoff <= 1) {
          qvalue_cutoff = input$q_cutoff
        }
        MBR = isTRUE(input$MBR)
      }
      quantificationColumn = if (isTRUE(input$diann_2plus)) "auto" else {
        if (!is.null(input$intensity_column) && nzchar(input$intensity_column)) input$intensity_column else "auto"
      }
      labeled_aa <- if (!is.null(input$diann_labeled_aa) && nzchar(input$diann_labeled_aa)) {
        trimws(strsplit(input$diann_labeled_aa, ",")[[1]])
      } else {
        NULL
      }

      mydata = DIANNtoMSstatsFormat(data,
                                    annotation = getAnnot(input),
                                    qvalue_cutoff = qvalue_cutoff,
                                    MBR = MBR,
                                    removeProtein_with1Feature = TRUE,
                                    removeFewMeasurements = FALSE,
                                    use_log_file = FALSE,
                                    quantificationColumn = quantificationColumn,
                                    labeledAminoAcids = labeled_aa
      )
      print("Mydata from mstats")
      print(mydata)
    }
    else if(input$filetype == 'meta') {
      cat(file=stderr(), "Reached in metamorpheus\n")
      data = data.table::fread(infile$datapath)
      mydata = MetamorpheusToMSstatsFormat(data,
                                           annotation = getAnnot(input),
                                           useUniquePeptide = input$unique_peptides,
                                           removeFewMeasurements = FALSE,
                                           removeProtein_with1Feature = input$remove,
                                           use_log_file = FALSE)
    }
    else if(input$filetype == 'open') {

      data = data.table::fread(infile$datapath)
      mydata =OpenSWATHtoMSstatsFormat(data,
                                       annotation = getAnnot(input),
                                       filter_with_mscore = TRUE, ## same as default
                                       mscore_cutoff = 0.01, ## same as default
                                       removeProtein_with1Feature = TRUE,
                                       use_log_file = FALSE)
      cat(file=stderr(), "Reached in openSwath\n")
    }
    else if(input$filetype == 'openms') {
      if(input$DDA_DIA=="TMT"){

        data = data.table::fread(infile$datapath)
        mydata = OpenMStoMSstatsTMTFormat(data, use_log_file = FALSE)

      }
      else{
        data = data.table::fread(infile$datapath)
        unique(data[, c('Run', 'BioReplicate', 'Condition')])
        mydata =OpenMStoMSstatsFormat(data,
                                      removeProtein_with1Feature=TRUE,
                                      use_log_file = FALSE)

      }

    }
    else if(input$filetype == 'ump') {
      mydata = DIAUmpiretoMSstatsFormat(raw.frag, raw.pep, raw.pro,
                                        annot2,
                                        useSelectedFrag = TRUE,
                                        useSelectedPep = FALSE,
                                        removeProtein_with1Feature = TRUE,
                                        use_log_file = FALSE)
    }
    else if(input$filetype == 'spmin') {
      data = data.table::fread(infile$datapath)
      mydata = SpectroMinetoMSstatsTMTFormat(data, getAnnot(input),
                                             use_log_file = FALSE)
    }
    else if(input$filetype == 'phil') {
      
      if (input$DDA_DIA=="TMT"){
      
        mydata = PhilosophertoMSstatsTMTFormat(input = mydata, 
                                               annotation = getAnnot(input),
                                               use_log_file = FALSE)
        mydata$Condition = as.character(mydata$Condition)
        mydata[mydata$Condition == "NORM", "Condition"] = "Norm"
        mydata$Condition = as.factor(mydata$Condition)
      } else {
        mydata = FragPipetoMSstatsFormat(input = mydata, 
                                               annotation = NULL,
                                               use_log_file = FALSE)
      }

    }
  }


  if (input$BIO == "Peptide"){
    mydata$ProteinName = mydata$PeptideSequence
  }

  remove_modal_spinner()
  
  return(mydata)
}

getDataCode <- function(input) {
  codes = ""
  codes = paste(codes, "\n# Load Packages
library(MSstats)
library(MSstatsTMT)
library(MSstatsPTM)\n", sep = "")
  codes = paste(codes, "\n# Package versions\n# MSstats version ", packageVersion("MSstats"),
                "\n# MSstatsTMT version ", packageVersion("MSstatsTMT"),
                "\n# MSstatsPTM version ", packageVersion("MSstatsPTM"), sep = "")
  codes = paste(codes, "\n\n# Read data\n", sep = "")
  if(input$filetype == 'sample') {
    if(input$BIO != "PTM" &&  input$DDA_DIA =='LType' && input$LabelFreeType == "SRM_PRM") {
      codes = paste(codes, "data = SRM_yeast\n", sep = "")
    }
    else if(input$BIO != "PTM" &&  input$DDA_DIA =='LType' && input$LabelFreeType == "DDA") {
      codes = paste(codes, "data = DDARawData\n", sep = "")
    }
    else if(input$BIO != "PTM" &&  input$DDA_DIA =='LType' && input$LabelFreeType == "DIA"){
      codes = paste(codes, "data = data.table::fread(\"dataset.csv\")\n", sep = "")
    }
    else if(input$BIO != "PTM" &&  input$DDA_DIA == "TMT"){
      codes = paste(codes, "data = PDtoMSstatsTMTFormat(input = MSstatsTMT::raw.pd,
                                       annotation = MSstatsTMT::annotation.pd,
                                       which.proteinid =\'", input$which.proteinid,"\',\n\t\t\t\t       ",
                    "use_log_file = FALSE)\n", sep = "")
    } else if (input$BIO == "PTM") {
      if (input$BIO == "PTM" && input$DDA_DIA == "TMT"){
        codes = paste(codes, "data = MSstatsPTM::raw.input.tmt\n", sep = "")
      } else if (input$BIO == "PTM" && input$DDA_DIA != "TMT"){
        codes = paste(codes, "data = MSstatsPTM::raw.input\n", sep = "")
      }
    }

  } else if (input$filetype == "msstats") {
    if (input$BIO == "PTM") {
      codes = paste(codes, "\nptm_data = data.table::fread(\"Enter PTM data file path here\")\nglobal_data = data.table::fread(\"Enter unmod data file path here\")\ndata = list(PTM = ptm_data, PROTEIN = global_data)\n")
    } else {
      codes = paste(codes, "data = data.table::fread(\"Enter MSstats formatted data file path here\")\n")
    }
  } else {

    if(input$filetype == '10col') {
      codes = paste(codes, "data = data.table::fread(\"insert your quantification dataset filepath\")\n", sep = "")
    }
    else if(input$filetype == 'sky') {
      cat(file=stderr(), "Reached here in skyline\n")
      codes = paste(codes, "data = data.table::fread(\"insert your MSstats report from Skyline filepath\")\n", sep = "")

      codes = paste(codes, "annot_file = data.table::fread(\"insert your annotation filepath\")\n"
                    , sep = "")


      codes = paste(codes, "data = SkylinetoMSstatsFormat(data,
                                     annotation = annot_file,
                                     filter_with_Qvalue = TRUE,
                                     qvalue_cutoff = 0.01,
                                     fewMeasurements=\"remove\",
                                     removeProtein_with1Feature = TRUE,
                                     use_log_file = FALSE)\n", sep = "")

    }
    else if(input$filetype == 'maxq') {
      cat(file=stderr(), "Reached in maxq\n")
      codes = paste(codes, "an_maxq = data.table::fread(\"insert your annotation filepath\")\n ev_maxq = data.table::fread(\"insert your evidence.txt filepath\")\n pg_maxq = data.table::fread(\"insert your proteinGroups.txt filepath\")\n"
                    , sep = "")
      if(input$DDA_DIA=="TMT"){

        codes = paste(codes, "data = MaxQtoMSstatsTMTFormat(evidence=ev_maxq,
                                         annotation=an_maxq,
                                         proteinGroups=\'", input$which.proteinid,"\',\n\t\t\t\t       ",
                      "use_log_file = FALSE)\n", sep = "")
      } else if (input$BIO=="PTM"){
        codes = paste(codes, "sites.data = data.table::fread(\"insert your PTM site data filepath\")\n data = MaxQtoMSstatsPTMFormat(sites.data, an_maxq, ev_maxq, pg_maxq, an_maxq)\n",
                      sep="")
      } else {
        codes = paste(codes, "data = MaxQtoMSstatsFormat(evidence=ev_maxq,
                                         annotation=an_maxq,
                                         proteinGroups= pg_maxq,
                                         useUniquePeptide = TRUE,
                                         removeProtein_with1Peptide=", input$remove,",\n\t\t\t\t       ",
                      "use_log_file = FALSE)\n", sep = "")
      }

    }
    else if(input$filetype == 'prog') {
      cat(file=stderr(), "Reached in prog\n")

      codes = paste(codes, "data = data.table::fread(\"insert your quantification dataset filepath\")
                       annot_file = data.table::fread(\"insert your annotation filepath\")\n"
                    , sep = "")

      codes = paste(codes, "data = ProgenesistoMSstatsFormat(input = data,
                                       annotation = annot_file,
                                       removeProtein_with1Peptide = TRUE,
                                       use_log_file = FALSE)\n", sep = "")

      codes = paste(codes, "colnames(data)[colnames(data) == \'PeptideModifiedSequence\'] = \'PeptideSequence\'\n", sep = "")

    }
    else if(input$filetype == 'PD') {

      if(input$DDA_DIA=="TMT"){

        codes = paste(codes, "data = data.table::fread(\"insert your quantification dataset filepath\")
                       annot_file = data.table::fread(\"insert your annotation filepath\")\n"
                      , sep = "")


        codes = paste(codes, "data = PDtoMSstatsTMTFormat(input = data,
                                       annotation = annot_file,
                                       which.proteinid =\'", input$which.proteinid,"\',\n\t\t\t\t       ",
                      "use_log_file = FALSE)\n", sep = "")

      }
      else{
        codes = paste(codes, "data = data.table::fread(\"insert your quantification dataset filepath\")\nannot_file = data.table::fread(\"insert your annotation filepath\")\n"
                      , sep = "")

        codes = paste(codes, "data = PDtoMSstatsFormat(data,
                                       annotation = annot_file,
                                       removeProtein_with1Peptide = ", input$remove,",\n\t\t\t\t       ",
                      "use_log_file = FALSE)\n", sep = "")

        codes = paste(codes, "colnames(data)[colnames(data) == \'PeptideModifiedSequence\'] = \'PeptideSequence\'\n", sep = "")

      }

    }
    else if(input$filetype == 'spec') {

      if (isTRUE(input$big_file_spec)) {
        codes = paste(codes,
                      "# Large-file (out-of-memory) Spectronaut path.\n",
                      "input_file = \"insert your raw Spectronaut export filepath\"\n",
                      sep = "")

        big_spec_extra <- ""
        if (!is.null(input$spec_intensity_col) &&
            nchar(trimws(input$spec_intensity_col)) > 0) {
          big_spec_extra <- paste0(big_spec_extra,
                                   ",\n                                          intensity = \"",
                                   trimws(input$spec_intensity_col), "\"")
        }
        if (!is.null(input$big_spec_annotation)) {
          codes = paste(codes,
                        "annot_file = data.table::fread(\"insert your annotation filepath (Run, BioReplicate, Condition)\")\n",
                        sep = "")
          big_spec_extra <- paste0(big_spec_extra,
                                   ",\n                                          annotation = annot_file")
        }
        if (isTRUE(input$calculate_anomaly_scores)) {
          big_spec_extra <- paste0(big_spec_extra,
                                   ",\n                                          calculateAnomalyScores = TRUE",
                                   ",\n                                          anomalyModelFeatures = c(\"FG.ShapeQualityScore (MS2)\", \"FG.ShapeQualityScore (MS1)\", \"EG.DeltaRT\")")
        }

        codes = paste(codes,
                      "converted = MSstatsBig::bigSpectronauttoMSstatsFormat(input_file,
                                          output_file_name = \"output_file.csv\",
                                          backend = \"arrow\",
                                          filter_by_excluded = ", input$filter_by_excluded, ",
                                          filter_by_identified = ", input$filter_by_identified, ",
                                          filter_by_qvalue = ", input$filter_by_qvalue, ",
                                          qvalue_cutoff = ", input$qvalue_cutoff, ",
                                          max_feature_count = ", input$max_feature_count, ",
                                          filter_unique_peptides = ", input$filter_unique_peptides, ",
                                          aggregate_psms = ", input$aggregate_psms, ",
                                          filter_few_obs = ", input$filter_few_obs,
                      big_spec_extra,
                      ")\ndata = dplyr::collect(converted)\n",
                      sep = "")

        if (isTRUE(input$calculate_anomaly_scores)) {
          codes = paste(codes,
                        "# Step 2 of the anomaly scoring pipeline: fit the\n",
                        "# isolation-forest model on the collected data and\n",
                        "# add an AnomalyScores column.\n",
                        "run_order = data.table::fread(\"insert your run order CSV filepath (Run, Order columns)\")\n",
                        "data = MSstatsConvert::MSstatsAnomalyScores(\n",
                        "  input = data,\n",
                        "  # Standardized column names (raw Spectronaut names\n",
                        "  # had `.` and ` ` stripped during the converter step).\n",
                        "  quality_metrics = c(\"FGShapeQualityScore(MS2)\", \"FGShapeQualityScore(MS1)\", \"EGDeltaRT\"),\n",
                        "  temporal_direction = c(\"mean_decrease\", \"mean_decrease\", \"dispersion_increase\"),\n",
                        "  missing_run_count = 0.5,\n",
                        "  n_feat = 100,\n",
                        "  run_order = run_order,\n",
                        "  n_trees = 100,\n",
                        "  max_depth = \"auto\",\n",
                        "  cores = 1)\n",
                        sep = "")
        }

      } else {

      codes = paste(codes, "data = data.table::fread(\"insert your MSstats scheme output from Spectronaut filepath\")\nannot_file = data.table::fread(\"insert your annotation filepath\")#Optional\n"
                    , sep = "")

      reg_spec_intensity_arg <- if (!is.null(input$spec_intensity_col) &&
                                    nchar(trimws(input$spec_intensity_col)) > 0) {
        paste0("                                       intensity = \"",
               trimws(input$spec_intensity_col), "\",\n")
      } else {
        ""
      }

      if (isTRUE(input$calculate_anomaly_scores)) {
        codes = paste(codes, "run_order = data.table::fread(\"insert your run order CSV filepath (Run, Order columns)\")\n", sep = "")
        codes = paste(codes, "data = SpectronauttoMSstatsFormat(data,
                                       annotation = annot_file, #Optional
", reg_spec_intensity_arg, "                                       filter_with_Qvalue = ", input$q_val, ",
                                       qvalue_cutoff = ", input$q_cutoff, ",
                                       removeProtein_with1Feature = ", input$remove, ",
                                       use_log_file = FALSE,
                                       calculateAnomalyScores = TRUE,
                                       runOrder = run_order,
                                       anomalyModelFeatures = c(\"FG.ShapeQualityScore (MS2)\", \"FG.ShapeQualityScore (MS1)\", \"EGDeltaRT\"),
                                       anomalyModelFeatureTemporal = c(\"mean_decrease\", \"mean_decrease\", \"dispersion_increase\"),
                                       n_trees = 100,
                                       max_depth = \"auto\",
                                       numberOfCores = 1)\n", sep = "")
      } else {
        codes = paste(codes, "data = SpectronauttoMSstatsFormat(data,
                                       annotation = annot_file, #Optional
", reg_spec_intensity_arg, "                                       filter_with_Qvalue = ", input$q_val, ",
                                       qvalue_cutoff = ", input$q_cutoff, ",
                                       removeProtein_with1Feature = ", input$remove, ",
                                       use_log_file = FALSE)\n", sep = "")
      }

      }
    }
    else if(input$filetype == 'diann') {
      
      codes = paste(codes, "data = data.table::fread(\"insert your MSstats scheme output from DIANN filepath\")\nannot_file = data.table::fread(\"insert your annotation filepath\")#Optional\n"
                    , sep = "")
      
      codes = paste(codes, "data = DIANNtoMSstatsFormat(data,
                                       annotation = annot_file, #Optional
                                       qvalue_cutoff = 0.01, ## same as default
                                       removeProtein_with1Feature = TRUE,
                                       use_log_file = FALSE)\n", sep = "")
    }
    else if(input$filetype == 'meta') {
      if (input$BIO == "PTM") {
        codes = paste(codes, "ptm_data = data.table::fread(\"insert your AllQuantifiedPeaks.tsv filepath\")\n", sep = "")
        codes = paste(codes, "annot = data.table::fread(\"insert your ExperimentalDesign annotation filepath\")\n", sep = "")
        codes = paste(codes, "fasta_path = \"insert your FASTA filepath\"\n", sep = "")
        codes = paste(codes, "# Optional: set protein_data = NULL if no GlobalProteome data\nprotein_data = tryCatch(data.table::fread(\"insert your GlobalProteome AllQuantifiedPeaks.tsv filepath\"), error = function(e) NULL)\n", sep = "")
        codes = paste(codes, "annot_protein = if (!is.null(protein_data)) data.table::fread(\"insert your GlobalProteome annotation filepath\") else NULL\n", sep = "")
        # Resolve mod ID for generated code
        code_mod_id <- tryCatch(
          .resolve_mod_id(input$mod_id_meta_select, input$mod_id_meta_custom),
          error = function(e) {
            showNotification(conditionMessage(e), type = "error", duration = 8)
            return("\\[UNSET_MODIFICATION_ID\\]")
          }
        )
        codes = paste(codes, "use_unmod_peptides = FALSE\ndata = MetamorpheusToMSstatsPTMFormat(data.table::copy(ptm_data),
                                       annot,
                                       fasta_path = fasta_path,
                                       input_protein = protein_data,
                                       annotation_protein = annot_protein,
                                       use_unmod_peptides = use_unmod_peptides,
                                       mod_ids = c(\"", gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", code_mod_id)), "\"))\n", sep = "")
      } else {
        codes = paste(codes, "data = data.table::fread(\"insert your QuantifiedPeaks.tsv filepath\")\nannot_file = data.table::fread(\"insert your annotation filepath\")\n"
                      , sep = "")

        codes = paste(codes, "data = MetamorpheusToMSstatsFormat(data,
                                       annotation = annot_file,
                                       useUniquePeptide = ", input$unique_peptides, ",
                                       removeFewMeasurements = FALSE,
                                       removeProtein_with1Feature = ", input$remove, ",\n\t\t\t\t       ",
                      "use_log_file = FALSE)\n", sep = "")
      }
    }
    else if(input$filetype == 'open') {

      codes = paste(codes, "data = data.table::fread(\"insert your quantification dataset filepath\")\nannot_file = data.table::fread(\"insert your annotation filepath\")\n"
                    , sep = "")

      codes = paste(codes, "data = OpenSWATHtoMSstatsFormat(data,
                                       annotation = annot_file,
                                       filter_with_Qvalue = TRUE, ## same as default
                                       mscore_cutoff = 0.01, ## same as default
                                       fewMeasurements=\"remove\",
                                       removeProtein_with1Feature = TRUE,
                                       use_log_file = FALSE)\n", sep = "")

    }
    else if(input$filetype == 'openms') {
      if(input$DDA_DIA=="TMT"){

        codes = paste(codes, "data = data.table::fread(\"insert your quantification dataset filepath\")\ndata = OpenMStoMSstatsTMTFormat(data, use_log_file = FALSE)\n"
                      , sep = "")

      }
      else{

        codes = paste(codes, "data = data.table::fread(\"insert your quantification dataset filepath\")\nunique(data[, c('Run', 'BioReplicate', 'Condition')])\ndata = OpenMStoMSstatsFormat(data, removeProtein_with1Feature=TRUE, use_log_file = FALSE)\n"
                      , sep = "")

      }
    }
    else if(input$filetype == 'spmin') {

      codes = paste(codes, "data = data.table::fread(\"insert your quantification dataset filepath\")\nannot_file = data.table::fread(\"insert your annotation filepath\")\ndata = SpectroMinetoMSstatsTMTFormat(data, annot_file,
                                              use_log_file = FALSE)"
                    , sep = "")
    }
    else if(input$filetype == 'phil' & input$DDA_DIA == "TMT") {

      codes = paste(codes,"data = data.table::fread(\"insert your msstats filepath\")\n"
                    , sep = "")
      codes = paste(codes,"annot_file = data.table::fread(\"insert your annotation filepath\")\n"
                    , sep = "")

      codes = paste(codes, "data = PhilosophertoMSstatsTMTFormat(data,
                                       annotation = annot_file)\n", sep = "")
    }else if (input$filetype == 'phil' & input$BIO == "PTM"){
      codes = paste(codes,"data = data.table::fread(\"insert your msstats filepath\")\n"
                    , sep = "")
      codes = paste(codes,"annot_file = data.table::fread(\"insert your annotation filepath\")\n"
                    , sep = "")

      codes = paste(codes,"data_protein = data.table::fread(\"insert your global profiling msstats filepath\")\n"
                    , sep = "")
      codes = paste(codes,"annot_protein_file = data.table::fread(\"insert your global profiling annotation filepath\")\n"
                    , sep = "")

      codes = paste(codes, paste0("data = PhilosophertoMSstatsPTMFormat(data,
                                       annot_file,
                                       data_protein,
                                       annot_protein_file,
                                       mod_id_col = ", input$mod_id_col, ",
                                       localization_cutoff = ",input$localization_cutoff, ",
                                       remove_unlocalized_peptides = ", as.character(input$remove_unlocalized_peptides),
                                  ")\n"), sep = "")
    } else if (input$filetype == 'phil'){
      codes = paste(codes,
                    "data = data.table::fread(\"insert your msstats.csv filepath\")\n"
                    , sep = "")
      codes = paste(codes, paste0("data = FragPipetoMSstatsFormat(data)\n"), 
                    sep = "")
    }
  }

  return(codes)

}

getSummary1 <- function(input, df,annot_df) {
  # df = getData(input)
  print("+++++++++ In getSummary1 +++++++++")
  # annot_df = getAnnot(input)
  if (input$BIO != "PTM"){
    df = as.data.frame(df)
    df = df %>% filter(!Condition %in% c("Norm", "Empty"))
    nf = ifelse("Fraction" %in% colnames(df),n_distinct(df$Fraction),1)
  }

  if(input$BIO != "PTM" && input$DDA_DIA=="TMT"){
    if(is.null(annot_df)){
      df1 = df %>% summarise("Number of Conditions" = n_distinct(Condition),
                             "Number of Biological Replicates" = n_distinct(BioReplicate),
                             "Number of Mixtures" = n_distinct(Mixture),
                             "Number of Fractions" = nf,
                             "Number of MS runs" = n_distinct(Run),
                             "Number of Technical Replicates" = n_distinct(TechRepMixture))
    } else {
      annot_df = annot_df %>% filter(!Condition %in% c("Norm", "Empty"))
      df1 = annot_df %>% summarise("Number of Conditions" = n_distinct(Condition),
                                   "Number of Biological Replicates" = n_distinct(BioReplicate),
                                   "Number of Mixtures" = n_distinct(Mixture),
                                   "Number of Fractions" = n_distinct(Fraction),
                                   "Number of MS runs" = n_distinct(Run),
                                   "Number of Technical Replicates" = n_distinct(TechRepMixture))
    }

  } else if (input$BIO == "PTM"){
    ptm_df = as.data.frame(df$PTM)
    unmod_df = as.data.frame(df$PROTEIN)
    if ((input$BIO == "PTM" & input$DDA_DIA == "TMT") | input$filetype=='phil'){

      ptm_df1 = ptm_df %>% summarise("Number of Conditions" = n_distinct(Condition),
                                     "Number of PTM Mixtures" = n_distinct(Mixture),
                                     "Number of PTM Biological Replicates" = n_distinct(BioReplicate),
                                     "Number of PTM MS runs" = n_distinct(Run),
                                     "Number of PTM Technical Replicates" = n_distinct(TechRepMixture))
      unmod_df1 = unmod_df %>% summarise(
        "Number of Unmod Mixtures" = n_distinct(Mixture),
        "Number of Unmod Biological Replicates" = n_distinct(BioReplicate),
        "Number of Unmod MS runs" = n_distinct(Run),
        "Number of Unmod Technical Replicates" = n_distinct(TechRepMixture))
      df = cbind(ptm_df1, unmod_df1)
    } else {
      ptm_df = df$PTM
      unmod_df = df$PROTEIN
      ptm_df1 = ptm_df %>% summarise("Number of Conditions" = n_distinct(Condition),
                                     "Number of PTM Biological Replicates" = n_distinct(BioReplicate),
                                     "Number of PTM MS runs" = n_distinct(Run))
      unmod_df1 = unmod_df %>% summarise("Number of Unmod Conditions" = n_distinct(Condition),
                                         "Number of Unmod Biological Replicates" = n_distinct(BioReplicate),
                                         "Number of Unmod MS runs" = n_distinct(Run))
      df = cbind(ptm_df1, unmod_df1)
    }
  } else {
    df1 = df %>% summarise("Number of Conditions" = n_distinct(Condition),
                           "Number of Biological Replicates" = n_distinct(BioReplicate),
                           "Number of Fractions" = nf,
                           "Number of MS runs" = n_distinct(Run)
    )
  }

  if (input$BIO != "PTM"){
    df2 = df %>% group_by(Condition, Run) %>% summarise("Condition_Run" = n()) %>% ungroup() %>%
      select("Condition_Run")
    df3 = df %>% group_by(Run, BioReplicate) %>% summarise("BioReplicate_Run" = n()) %>% ungroup() %>%
      select("BioReplicate_Run")

    df1 = head(df1,1)
    df2 = head(df2,1)
    df3 = head(df3,1)

    if(input$DDA_DIA !="TMT"){
      df1 = cbind(df1,df2,df3) %>%
        mutate("Number of Technical Replicates" = Condition_Run/(BioReplicate_Run*`Number of Fractions`) ) %>%
        select(-Condition_Run,-BioReplicate_Run)
      df = df1[,c(1,2,5,3,4)]
    }
    else{
      df = df1[,c(1,2,3,6,4,5)]
    }

  }

  t_df = as.data.frame(t(df))
  rownames(t_df) = colnames(df)
  t_df = cbind(rownames(t_df), t_df)
  colnames(t_df) = c("", "value")
  t_df$value = sub("\\.\\d+$", "", t_df$value)
  colnames(t_df) = c("", "")
  return(t_df)

}

getSummary2 <- function(input,df) {
  # df = getData(input)
  print("+++++++++ In getSummary2 +++++++++")
  #print(input$PTMTMT)
  if(input$BIO != "PTM" && input$DDA_DIA=="TMT"){
    df = as.data.frame(df)
    df = df %>% mutate("FEATURES" = paste(ProteinName, PeptideSequence, Charge,
                                          sep = '_'))
  } else if (input$BIO == "PTM" & ((input$BIO == "PTM" & input$DDA_DIA == "TMT" )| input$filetype=='phil')){
    df_ptm = as.data.frame(df$PTM) %>% mutate("FEATURES" = paste(ProteinName, PeptideSequence,
                                                                 Charge, sep = '_'))
    df_prot = as.data.frame(df$PROTEIN) %>% mutate("FEATURES" = paste(ProteinName,
                                                                      PeptideSequence,
                                                                      Charge, sep = '_'))
  } else if (input$BIO == "PTM" & (input$BIO == "PTM" & input$DDA_DIA != "TMT" )){
    df_ptm = as.data.frame(df$PTM) %>% mutate("FEATURES" = paste(PeptideSequence,
                                                                 PrecursorCharge,
                                                                 FragmentIon,
                                                                 ProductCharge, sep = '_'))
    df_prot = as.data.frame(df$PROTEIN) %>% mutate("FEATURES" = paste(PeptideSequence,
                                                                      PrecursorCharge,
                                                                      FragmentIon,
                                                                      ProductCharge,
                                                                      sep = '_'))
  } else {
    df = as.data.frame(df)
    df = df %>% mutate("FEATURES" = paste(PeptideSequence, PrecursorCharge,
                                          FragmentIon, ProductCharge,
                                          sep = '_'))
  }

  if (input$BIO != "PTM"){

    df1 = df %>% summarise("Number of Proteins" = n_distinct(ProteinName),
                           "Number of Peptides" = n_distinct(PeptideSequence),
                           "Number of Features" = n_distinct(FEATURES),
                           "Min_Intensity" = ifelse(!is.finite(min(Intensity, na.rm=TRUE)),0,round(min(Intensity, na.rm=TRUE),0)),
                           "Max_Intensity" = ifelse(!is.finite(max(Intensity, na.rm=TRUE)),0,
                                                    round(max(Intensity, na.rm=TRUE),0))) %>%
      unite("Intensity Range", Min_Intensity:Max_Intensity, sep = " - ")

    Peptides_Proteins = df %>% group_by(ProteinName)  %>%
      summarise(npep = n_distinct(PeptideSequence)) %>% summarise(Peptides_Proteins_min=min(npep),
                                                                  Peptides_Proteins_max=max(npep))

    Features_Peptides = df %>% group_by(PeptideSequence)  %>%
      summarise(nfea = n_distinct(FEATURES)) %>% summarise(Features_Peptides_min=min(nfea),
                                                           Features_Peptides_max=max(nfea))

    df1 = cbind(df1,Features_Peptides,Peptides_Proteins) %>%
      unite("Number of Features/Peptide",Features_Peptides_min:Features_Peptides_max,sep = " - ") %>%
      unite("Number of Peptides/Protein",Peptides_Proteins_min:Peptides_Proteins_max, sep = " - ")

    df1 = df1[,c(1,2,3,6,5,4)]
  } else {

    df_ptm1 = as.data.frame(df_ptm) %>% summarise("Number of PTMs" = n_distinct(ProteinName),
                                                  "Number of PTM Features" = n_distinct(FEATURES),
                                                  "Number of Features/PTM" = as.numeric(n_distinct(FEATURES) / n_distinct(PeptideSequence)),
                                                  "Min_Intensity" = ifelse(!is.finite(
                                                    min(Intensity, na.rm=TRUE)), 0,
                                                    round(min(Intensity, na.rm=TRUE),0)),
                                                  "Max_Intensity" = ifelse(!is.finite(
                                                    max(Intensity, na.rm=TRUE)), 0,
                                                    round(max(Intensity, na.rm=TRUE),0))) %>%
      unite("PTM Intensity Range", Min_Intensity:Max_Intensity, sep = " - ")
    # df_ptm1 = df_ptm1 %>% select(!Min_Intensity, !Max_Intensity)

    df_prot1 = as.data.frame(df_prot) %>% summarise("Number of Unmod Proteins" = n_distinct(ProteinName),
                                                    "Number of Protein Peptides" = n_distinct(PeptideSequence),
                                                    "Number of Protein Features" = n_distinct(FEATURES),
                                                    "Number of Features/Peptide" = as.numeric(n_distinct(FEATURES) / n_distinct(PeptideSequence)),
                                                    "Number of Peptides/Protein" = as.numeric(n_distinct(PeptideSequence) / n_distinct(ProteinName)),
                                                    "Min_Intensity" = ifelse(!is.finite(
                                                      min(Intensity, na.rm=TRUE)), 0,
                                                      round(min(Intensity, na.rm=TRUE),0)),
                                                    "Max_Intensity" = ifelse(!is.finite(
                                                      max(Intensity, na.rm=TRUE)), 0,
                                                      round(max(Intensity, na.rm=TRUE),0))) %>%
      unite("Protein Intensity Range", Min_Intensity:Max_Intensity, sep = " - ")
    df1 = cbind(df_ptm1, df_prot1)
  }


  t_df = as.data.frame(t(df1))
  rownames(t_df) = colnames(df1)
  t_df = cbind(rownames(t_df), t_df)
  colnames(t_df) = c("", "value")
  colnames(t_df) = c("", "")
  return(t_df)

}

# qc server functions
preprocessData <- function(qc_input,loadpage_input,input_data ) {
  print("+++++++++ In preprocessData +++++++++")
  # validate(need(getData(loadpage_input),
  #               message = "PLEASE UPLOAD DATASET OR SELECT SAMPLE"))
  #
  # ## Preprocess input for loop
  #
  # input_data = getData(loadpage_input)

  validate(need(input_data,
                message = "PLEASE UPLOAD DATASET OR SELECT SAMPLE"))

  ## Preprocess input for loop
  preprocess_list = list()
  MSstatsLogsSettings(FALSE)
  ## Here we run the underlying functions for MSstats and MSstatsTMT
  ## summarization. Done so we can loop over proteins and create a progress bar
  if(loadpage_input$BIO == "PTM" & ((loadpage_input$BIO == "PTM" & loadpage_input$DDA_DIA == "TMT" ) | loadpage_input$filetype=='phil')){

    preprocessed_ptm = MSstatsShiny::tmt_summarization_loop(input_data$PTM, qc_input,loadpage_input)
    preprocessed_unmod = MSstatsShiny::tmt_summarization_loop(input_data$PROTEIN, qc_input,loadpage_input)
    preprocessed = list(PTM = preprocessed_ptm, PROTEIN = preprocessed_unmod)

  } else if (loadpage_input$BIO == "PTM" & (loadpage_input$BIO == "PTM" & loadpage_input$DDA_DIA != "TMT" )){

    preprocessed_ptm = MSstatsShiny::lf_summarization_loop(input_data$PTM, qc_input, loadpage_input)
    preprocessed_unmod = MSstatsShiny::lf_summarization_loop(input_data$PROTEIN, qc_input, loadpage_input)
    preprocessed = list(PTM = preprocessed_ptm, PROTEIN = preprocessed_unmod)

  } else if(loadpage_input$DDA_DIA == "TMT"){

    ## Run MSstatsTMT summarization
    preprocessed = MSstatsShiny::tmt_summarization_loop(input_data, qc_input,loadpage_input)
  } else {

    ## Run LF MSstats summarization
    preprocessed = MSstatsShiny::lf_summarization_loop(input_data, qc_input, loadpage_input)

  }
  return(preprocessed)
}

preprocessDataCode <- function(qc_input,loadpage_input) {

  codes = getDataCode(loadpage_input)

  if(loadpage_input$DDA_DIA == "TMT"){

    codes = paste(codes, "\n# use MSstats for protein summarization\n", sep = "")
    codes = paste(codes, "summarized = MSstatsTMT::proteinSummarization(data,
                   method = '",qc_input$summarization,"\',\t\t\t\t
                   global_norm = ", qc_input$global_norm,",\t\t\t\t
                   reference_norm = ", qc_input$reference_norm,",\t\t\t\t
                   remove_norm_channel  = ", qc_input$remove_norm_channel,",\t\t\t\t
                   remove_empty_channel = TRUE, \t\t\t\t
                   MBimpute = FALSE, \t\t\t\t
                   maxQuantileforCensored = ", qc_input$maxQC1,")\n", sep = "")
    codes = paste(codes, "\n# use to create data summarization plots\n", sep = "")
    codes = paste(codes, "dataProcessPlotsTMT(summarized,
                            type= \"Enter ProfilePlot or QCPlot Here\",
                            ylimUp = FALSE,
                            ylimDown = FALSE,
                            which.Protein = \"Enter Protein to Plot Here\",
                            originalPlot = TRUE,
                            summaryPlot =", qc_input$summ,",\t\t\t\t
                            address = FALSE)\n", sep="")
  } else if (loadpage_input$BIO == "PTM"){
    if ((loadpage_input$BIO == "PTM" & loadpage_input$DDA_DIA == "TMT" ) | loadpage_input$filetype=='phil'){
      codes = paste(codes, "\n# use MSstats for protein summarization\n", sep = "")
      codes = paste(codes, "summarized = MSstatsPTM::dataSummarizationPTM_TMT(data,
                     method = '",qc_input$summarization,"\',\t\t\t\t
                     global_norm.PTM = ", qc_input$global_norm,",\t\t\t\t
                     reference_norm.PTM = ", qc_input$reference_norm,",\t\t\t\t
                     remove_norm_channel  = ", qc_input$remove_norm_channel,",\t\t\t\t
                     remove_empty_channel = TRUE, \t\t\t\t
                     MBimpute.PTM = FALSE, \t\t\t\t
                     maxQuantileforCensored = ", qc_input$maxQC1,")\n", sep = "")
    } else{
      codes = paste(codes, "\n# use MSstats for protein summarization\n", sep = "")
      codes = paste(codes, "summarized = MSstatsPTM::dataSummarizationPTM(data,
                               normalization.PTM = \'", qc_input$norm,"\',\t\t\t\t
                               logTrans = ", as.numeric(qc_input$log),",\t\t\t\t
                               nameStandards = ", paste0("c('", paste(qc_input$names, collapse = "', '"), "')"), ",\t\t\t\t
                               featureSubset = \'", qc_input$features_used, "\',\t\t\t\t
                               n_top_feature = ", code_n_feat, ",\t\t\t\t
                               summaryMethod=\"TMP\",
                               censoredInt=\'", qc_input$censInt, "\',\t\t\t\t
                               MBimpute.PTM=", qc_input$MBi, ",\t\t\t\t
                               remove50missing=", qc_input$remove50, ",\t\t\t\t
                               maxQuantileforCensored=", qc_input$maxQC, ")\n", sep = "")
    }
    codes = paste(codes, "\n# use to create data summarization plots\n", sep = "")
    codes = paste(codes, "dataProcessPlotsPTM(summarized,
                            type= \"Enter ProfilePlot or QCPlot Here\",
                            ylimUp = FALSE,
                            ylimDown = FALSE,
                            which.PTM = \"Enter PTM to Plot Here\",
                            originalPlot = TRUE,
                            summaryPlot =", qc_input$summ,",\t\t\t\t
                            address = FALSE)\n", sep="")
  }
  else{
    if (qc_input$features_used == "all"){
      code_n_feat = 'NULL'
    } else if (qc_input$features_used == "topN") {
      code_n_feat = qc_input$n_feat
    } else {
      code_n_feat = 'NULL'
    }

    sum_method = if (!is.null(qc_input$summaryMethod)) qc_input$summaryMethod else "TMP"

    codes = paste(codes, "\n# use MSstats for protein summarization\n", sep = "")
    codes = paste(codes, "summarized = MSstats::dataProcess(data,
                               normalization = \'", qc_input$norm,"\',\t\t\t\t
                               logTrans = ", as.numeric(qc_input$log),",\t\t\t\t
                               nameStandards = ", paste0("c('", paste(qc_input$names, collapse = "', '"), "')"), ",\t\t\t\t
                               featureSubset = \'", qc_input$features_used, "\',\t\t\t\t
                               n_top_feature = ", code_n_feat, ",\t\t\t\t
                               summaryMethod=\"", sum_method, "\",
                               censoredInt=\'", qc_input$censInt, "\',\t\t\t\t
                               MBimpute=", qc_input$MBi, ",\t\t\t\t
                               remove50missing=", qc_input$remove50, ",\t\t\t\t
                               maxQuantileforCensored=", qc_input$maxQC, ")\n", sep = "")

    codes = paste(codes, "dataProcessPlots(data=summarized,
                           type=\"Enter ProfilePlot or QCPlot Here\",
                           ylimUp = FALSE,
                           ylimDown = FALSE,
                           which.Protein = \"Enter Protein to Plot Here\",
                           summaryPlot = TRUE,
                           address = FALSE,isPlotly=TRUE)\n", sep="")

    if (isTRUE(loadpage_input$calculate_anomaly_scores)) {
      codes = paste(codes, "\n# Plot per-feature quality metrics (e.g. AnomalyScores) carried through from the converter\n", sep = "")
      codes = paste(codes, "MSstats::MSstatsQualityMetricsPlot(data,
                                       metric = \"AnomalyScores\",
                                       which.Protein = \"Enter Protein to Plot Here\",
                                       isPlotly = TRUE)\n", sep = "")
    }
  }

  return(codes)

}



# statmodel server functions
dataComparison <- function(statmodel_input,qc_input,loadpage_input,matrix,input_data) {
  print("+++++++++ In Data Comparison +++++++++")
  # input_data = preprocessData(qc_input,loadpage_input,get_data())
  contrast.matrix = matrix
  if (loadpage_input$BIO == "PTM" & ((loadpage_input$BIO == "PTM" & loadpage_input$DDA_DIA == "TMT" ) | loadpage_input$filetype=='phil')){
    model_ptm = MSstatsShiny::tmt_model(input_data$PTM, statmodel_input, contrast.matrix)
    model_protein = MSstatsShiny::tmt_model(input_data$PROTEIN, statmodel_input, contrast.matrix)
    model_adj = MSstatsShiny::apply_adj(model_ptm$ComparisonResult,
                                        model_protein$ComparisonResult)
    model = list('PTM.Model' = model_ptm$ComparisonResult,
                 'PROTEIN.Model' = model_protein$ComparisonResult,
                 'ADJUSTED.Model' = model_adj)

  } else if(loadpage_input$BIO == "PTM" & (loadpage_input$BIO == "PTM" & loadpage_input$DDA_DIA != "TMT" )){
    model_ptm = MSstatsShiny::lf_model(input_data$PTM, contrast.matrix)
    model_protein = MSstatsShiny::lf_model(input_data$PROTEIN, contrast.matrix)
    model_adj = MSstatsShiny::apply_adj(model_ptm$ComparisonResult,
                                        model_protein$ComparisonResult)
    model = list('PTM.Model' = model_ptm$ComparisonResult,
                 'PROTEIN.Model' = model_protein$ComparisonResult,
                 'ADJUSTED.Model' = model_adj)

  } else if(loadpage_input$DDA_DIA=="TMT"){
    model = MSstatsShiny::tmt_model(input_data, statmodel_input, contrast.matrix)
  }
  else{
    model = MSstatsShiny::lf_model(input_data, contrast.matrix)
  }
  return(model)
}

#' @importFrom MSstatsResponse doseResponseFit
fitResponseCurves <- function(statmodel_input, matrix, input_data, transform_dose = TRUE) {
  protein_level_data <- merge(input_data$ProteinLevelData, matrix, by = "GROUP")
  dia_prepared <- prepare_dose_response_fit(protein_level_data)
  response_results <- doseResponseFit(
    data = dia_prepared,
    increasing = statmodel_input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]],
    transform_dose = transform_dose,
    ratio_response = FALSE
  )
  return(list(ComparisonResult = response_results))
}
