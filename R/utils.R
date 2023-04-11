# loadpage server functions
getEvidence <- function(input) {
  evidence = input$evidence
  if(is.null(input$evidence)) {
    return(NULL)
  }
  # if (input$subset){
  #   evidence = try(read.csv.sql(evidence$datapath, sep="\t", 
  #                               sql = "select * from file order by random() limit 100000"), 
  #                  silent=TRUE)
  # } else {
  evidence = try(read.table(evidence$datapath, sep="\t", header=TRUE), silent=TRUE)
  # }
  print("***")
  print(class(evidence))
  print(typeof(evidence))
  print(isS4(evidence))
  print("***")
  # if (class(evidence) == "try-error") {
  #   evidence = "File load error. Please ensure file is in csv format."
  # }
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
  # if (input$subset){
  #   evidence2 = try(read.csv.sql(evidence2$datapath, sep = "\t",
  #                               sql = "select * from file order by random() limit 100000"), 
  #                  silent=TRUE)
  # } else {
  evidence2 = try(read.delim(evidence2$datapath), silent=TRUE)
  # }
  
  # if (class(evidence2) == "try-error"){
  #   evidence2 = "File load error. Please ensure file is in csv format." 
  # }
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
  # if (input$subset){
  #   unmod = try(read.csv.sql(unmod$datapath, sep=",",
  #                            sql = "select * from file order by random() limit 100000"), 
  #                   silent=TRUE)
  # } else {
  unmod = try(read.csv(unmod$datapath, sep=",", header=TRUE, 
                       stringsAsFactors=FALSE), silent=TRUE)
  # }
  
  # if (class(unmod) == "try-error"){
  #   unmod = "File load error. Please ensure file is in csv format." 
  # }
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
  # if (input$subset){
  #   pGroup = try(read.csv.sql(pGroup$datapath, sep="\t",
  #                            sql = "select * from file order by random() limit 100000"), 
  #               silent=TRUE)
  # } else {
  pGroup = try(read.table(pGroup$datapath, sep="\t", header=TRUE), silent=TRUE)
  # }
  
  # if (class(pGroup) == "try-error"){
  #   pGroup = "File load error. Please ensure file is in csv format." 
  # }
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
  # if (input$subset){
  #   pGroup2 = try(read.csv.sql(pGroup2$datapath, sep="\t",
  #                             sql = "select * from file order by random() limit 100000"), 
  #                silent=TRUE)
  # } else {
  pGroup2 = try(read.delim(pGroup2$datapath), silent=TRUE)
  # }
  
  # if (class(pGroup2) == "try-error"){
  #   pGroup2 = "File load error. Please ensure file is in csv format." 
  # }
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
  
  # if (input$subset){
  #   fragSummary = try(read.csv.sql(fragSummary$datapath, sep="\t",
  #                              sql = "select * from file order by random() limit 100000"), 
  #                 silent=TRUE)
  # } else {
  fragSummary = try(read.table(fragSummary$datapath, sep="\t", header=TRUE),
                    silent=TRUE)
  # }
  
  # if (class(fragSummary) == "try-error"){
  #   fragSummary = "File load error. Please ensure file is in excel format." 
  # }
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
  
  # if (input$subset){
  #   peptideSummary = try(read.csv.sql(peptideSummary$datapath, sep="\t",
  #                                  sql = "select * from file order by random() limit 100000"), 
  #                     silent=TRUE)
  # } else {
  peptideSummary = try(read.table(peptideSummary$datapath, sep="\t", 
                                  header=TRUE), silent=TRUE)
  # }
  
  # if (class(peptideSummary) == "try-error"){
  #   peptideSummary = "File load error. Please ensure file is in csv format." 
  # }
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
  # if (input$subset){
  #   protSummary = try(read.csv.sql(protSummary$datapath, sep="\t",
  #                                     sql = "select * from file order by random() limit 100000"), 
  #                        silent=TRUE)
  # } else {
  protSummary = try(read.table(protSummary$datapath, sep="\t", header=TRUE),
                    silent=TRUE)
  # }
  
  # if (class(protSummary) == "try-error"){
  #   protSummary = "File load error. Please ensure file is in csv format." 
  # }
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
  # if (input$subset){
  #   maxq_ptm_sites = try(read.csv.sql(maxq_ptm_sites$datapath, sep="\t",
  #                                  sql = "select * from file order by random() limit 100000"), 
  #                     silent=TRUE)
  # } else {
  maxq_ptm_sites = try(read.delim(maxq_ptm_sites$datapath), silent=TRUE)
  # }
  
  # if (class(maxq_ptm_sites) == "try-error"){
  #   maxq_ptm_sites = "File load error. Please ensure file is in csv format." 
  # }
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
  annot3 = try(read.delim(annot3$datapath), silent=TRUE)
  
  # if (class(annot3) == "try-error") {
  #   annot3 = "File load error. Please ensure file is in csv format."
  # }
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
  annot2=try(read.csv(annot1$datapath, header = TRUE), silent=TRUE)
  
  # if (class(annot2) == "try-error") {
  #   annot2 = "File load error. Please ensure file is in csv format."
  # }
  if (is(annot2,"try-error")) {
    annot2 = "File load error. Please ensure file is in csv format."
  }
  cat(file=stderr(), "Reached in ump annot\n")
  return(annot2)
  
}

getAnnot <- function(input) {
  annot = input$annot
  if(is.null(annot)) {
    return(NULL)
  }
  else if (input$DDA_DIA == "TMT" && input$filetype == "sample") 
  {
    return(annotation.pd)
  }
  annot_file = try(read.csv(annot$datapath), silent=TRUE)
  # if (class(annot_file) == "try-error") {
  #   annot_file = "File load error. Please ensure file is in csv format."
  # }
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
  annot1=try(read.csv(annot1$datapath, header = TRUE), silent=TRUE)
  # if (class(annot1) == "try-error") {
  #   annot1 = "File load error. Please ensure file is in csv format."
  # }
  if (is(annot1,"try-error")) {
    annot1 = "File load error. Please ensure file is in csv format."
  }
  cat(file=stderr(), "Reached in maxq annot\n")
  return(annot1)
  
}

getData <- function(input) {
  show_modal_spinner()
  ev_maxq = getEvidence(input)
  pg_maxq = getProteinGroups(input)
  ev_maxq2 = getEvidence2(input)
  pg_maxq2 = getProteinGroups2(input)
  an_maxq = getAnnot1(input)
  raw.frag = getFragSummary(input)
  raw.pep = getPeptideSummary(input)
  raw.pro = getProtSummary(input)
  annot2 = getAnnot2(input)
  annot3 = getAnnot3(input)
  unmod = getGlobal(input)
  ptm_sites_data = getMaxqPtmSites(input)

  cat(file=stderr(), "Reached in get_data\n")

  cat(file=stderr(), paste("File type is",input$filetype,"\n"))
  if(is.null(input$filetype)) {
    return(NULL)
  }
  if(input$filetype == 'sample') {
    if(input$DDA_DIA == "SRM_PRM") {
      mydata = MSstats::DDARawData
    }
    else if(input$DDA_DIA == "DDA") {
      mydata = MSstats::DDARawData
    }
    else if(input$DDA_DIA == "DIA"){
      mydata = read.csv("data/dataset.csv", header = TRUE, sep = ";")
    }
    else if(input$DDA_DIA == "TMT"){
      mydata = PDtoMSstatsTMTFormat(input = raw.pd,
                                    annotation = annotation.pd,
                                    which.proteinid = input$which.proteinid,
                                    use_log_file = FALSE
      )
    }
    else if (input$DDA_DIA == "PTM"){
      if (input$PTMTMT == "No"){
        mydata = MSstatsPTM::raw.input
      } else {
        mydata = MSstatsPTM::raw.input.tmt
      }
    }
  }
  else if (input$DDA_DIA %in% c("PTM", "PTM_TMT")){
    if (input$filetype == 'maxq') {
      mydata = MaxQtoMSstatsPTMFormat(ptm_sites_data,
                                      annot3,
                                      evidence=ev_maxq2,
                                      proteinGroups=pg_maxq2,
                                      annotation.prot=annot3,
                                      mod.num=input$mod.num,
                                      TMT.keyword=input$TMT.keyword,
                                      ptm.keyword=input$PTM.keyword)
      mydata$PROTEIN = as.data.frame(mydata$PROTEIN)

    } else if(input$filetype=='phil'){
      mydata = read.csv(input$ptmdata$datapath)
      mydata_protein = read.csv(input$globaldata$datapath)
      annotation = read.csv(input$annotation$datapath)
      annotation_protein = read.csv(input$globalannotation$datapath)
      
      mydata = PhilosophertoMSstatsPTMFormat(mydata,
                                             annotation,
                                             mydata_protein,
                                             annotation_protein,
                                             mod_id_col = input$mod_id_col,
                                             localization_cutoff = as.numeric(input$localization_cutoff),
                                             remove_unlocalized_peptides=input$remove_unlocalized_peptides)
      
    }
    
    else {
      data = read.csv(input$data$datapath, header = TRUE, sep = input$sep,
                      stringsAsFactors=FALSE)
      mydata = list("PTM" = data, "PROTEIN" = unmod)
    }
  }
  else if (input$filetype == "msstats"){
    mydata = read.csv(input$data$datapath, header = TRUE, sep = input$sep,
                      stringsAsFactors=FALSE)
  }
  else {
    if(input$filetype=='spec' || input$filetype=='spmin'){
      infile = input$data1
    }
    else if(input$filetype=='phil' & input$DDA_DIA != "PTM"){
      mydata = read.csv(input$data$datapath)
      
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
      mydata = read.csv(infile$datapath, header = TRUE, sep = input$sep)
    }
    else if(input$filetype == 'sky') {
      cat(file=stderr(), "Reached here in skyline\n")
      # if (input$subset){
      #   data = read.csv.sql(infile$datapath, sep=input$sep,
      #                           sql = "select * from file order by random() limit 100000")
      # } else {
      data = read.csv(infile$datapath, header = TRUE, sep = input$sep,
                      stringsAsFactors=FALSE)
      # }
      if(input$DDA_DIA=="DDA" ){
        data = data[which(data$Fragment.Ion %in% c("precursor", "precursor [M+1]","precursor [M+2]")),]

        mydata = SkylinetoMSstatsFormat(data,
                                        annotation = getAnnot(input),
                                        fewMeasurements="remove",
                                        removeProtein_with1Feature = input$remove,
                                        use_log_file = FALSE)
      }
      else if(input$DDA_DIA=="DIA"){
        mydata = SkylinetoMSstatsFormat(data,
                                        annotation = getAnnot(input),
                                        filter_with_Qvalue = TRUE,
                                        qvalue_cutoff = 0.01,
                                        fewMeasurements="remove",
                                        removeProtein_with1Feature = TRUE,
                                        use_log_file = FALSE)

      }
      else if(input$DDA_DIA=="SRM_PRM") {
        mydata = SkylinetoMSstatsFormat(data,
                                        annotation = getAnnot(input),
                                        filter_with_Qvalue = TRUE,
                                        qvalue_cutoff = 0.01,
                                        fewMeasurements="remove",
                                        removeProtein_with1Feature = TRUE,
                                        use_log_file = FALSE)
      }

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

      # if (input$subset){
      #   data = read.csv.sql(infile$datapath, sep=input$sep,
      #                       sql = "select * from file order by random() limit 100000")
      # } else {
      data = read.csv(infile$datapath, header = TRUE, sep = input$sep,
                      stringsAsFactors=FALSE)
      # }

      mydata = ProgenesistoMSstatsFormat(data, annotation = getAnnot(input),
                                         removeProtein_with1Peptide = TRUE,
                                         use_log_file = FALSE)
      colnames(mydata)[colnames(mydata) == 'PeptideModifiedSequence'] = 'PeptideSequence'
    }
    else if(input$filetype == 'PD') {

      if(input$DDA_DIA=="TMT"){

        # if (input$subset){
        #   data = read.csv.sql(infile$datapath, sep=input$sep,
        #                       sql = "select * from file order by random() limit 100000")
        # } else {
        data = read.csv(infile$datapath, header = TRUE, sep = input$sep,
                        stringsAsFactors=FALSE)
        # }
        mydata = PDtoMSstatsTMTFormat(input = data,
                                      annotation = getAnnot(input),
                                      which.proteinid = input$which.proteinid, ## same as default
                                      use_log_file = FALSE
        )
      }
      else{
        # if (input$subset){
        #   data = read.csv.sql(infile$datapath, sep=input$sep,
        #                       sql = "select * from file order by random() limit 100000")
        # } else {
        data = read.csv(infile$datapath, header = TRUE, sep = input$sep,
                        stringsAsFactors=FALSE)
        # }
        mydata = PDtoMSstatsFormat(data, annotation = getAnnot(input),
                                   removeProtein_with1Peptide = input$remove,
                                   use_log_file = FALSE)
        colnames(mydata)[colnames(mydata) == 'PeptideModifiedSequence'] = 'PeptideSequence'
      }

    }
    else if(input$filetype == 'spec') {

      # if (input$subset){
      #   data = read.csv.sql(infile$datapath, sep="\t",
      #                       sql = "select * from file order by random() limit 100000")
      # } else {
      data = read.csv(infile$datapath, sep="\t")
      # }
      mydata = SpectronauttoMSstatsFormat(data,
                                          annotation = getAnnot(input),
                                          filter_with_Qvalue = TRUE, ## same as default
                                          qvalue_cutoff = 0.01, ## same as default
                                          removeProtein_with1Feature = TRUE,
                                          use_log_file = FALSE)
    }
    else if(input$filetype == 'open') {

      # if (input$subset){
      #   data = read.csv.sql(infile$datapath, sep = input$sep,
      #                       sql = "select * from file order by random() limit 100000")
      # } else {
      data = read.csv(infile$datapath, header = TRUE, sep = input$sep)
      # }
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

        # if (input$subset){
        #   data = read.csv.sql(infile$datapath, sep = input$sep,
        #                       sql = "select * from file order by random() limit 100000")
        # } else {
        data = read.csv(infile$datapath, header = TRUE, sep = input$sep)
        # }
        mydata = OpenMStoMSstatsTMTFormat(data, use_log_file = FALSE)

      }
      else{
        # if (input$subset){
        #   data = read.csv.sql(infile$datapath, sep = input$sep,
        #                       sql = "select * from file order by random() limit 100000")
        # } else {
        data = read.csv(infile$datapath, header = TRUE, sep = input$sep)
        # }
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
      # if (input$subset){
      #   data = read.csv.sql(infile$datapath, sep = "\t",
      #                       sql = "select * from file order by random() limit 100000")
      # } else {
      data = read.csv(infile$datapath, sep="\t")
      # }
      mydata = SpectroMinetoMSstatsTMTFormat(data, getAnnot(input),
                                             use_log_file = FALSE)
    }
    else if(input$filetype == 'phil' & input$DDA_DIA=="TMT") {
      mydata = PhilosophertoMSstatsTMTFormat(input = mydata, 
                                             annotation = get_annot(),
                                             use_log_file = FALSE)
    }
  }


  if (input$level == "Peptide"){
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
    if(input$DDA_DIA == "SRM_PRM") {
      codes = paste(codes, "data = SRM_yeast\n", sep = "")
    }
    else if(input$DDA_DIA == "DDA") {
      codes = paste(codes, "data = DDARawData\n", sep = "")
    }
    else if(input$DDA_DIA == "DIA"){
      codes = paste(codes, "data = read.csv(\"dataset.csv\", header = TRUE, sep = \";\")\n", sep = "")
    }
    else if(input$DDA_DIA == "TMT"){
      codes = paste(codes, "data = PDtoMSstatsTMTFormat(input = MSstatsTMT::raw.pd,
                                       annotation = MSstatsTMT::annotation.pd,
                                       which.proteinid =\'", input$which.proteinid,"\',\n\t\t\t\t       ",
                    "use_log_file = FALSE)\n", sep = "")
    } else if (input$DDA_DIA == "PTM") {
      if (input$PTMTMT == "Yes"){
        codes = paste(codes, "data = MSstatsPTM::raw.input.tmt\n", sep = "")
      } else if (input$PTMTMT == "No"){
        codes = paste(codes, "data = MSstatsPTM::raw.input\n", sep = "")
      }
    }
    
  } else if (input$filetype == "msstats") {
    if (input$DDA_DIA == "PTM") {
      codes = paste(codes, "\nptm_data = read.csv(\'Enter PTM data file path here\')\nglobal_data = read.csv(\'Enter unmod data file path here\')\ndata = list(PTM = ptm_data, PROTEIN = unmod)\n")
    } else {
      codes = paste(codes, "data = read.csv(\'Enter MSstats formatted data file path here\')\n")
    }
  } else {
    
    if(input$filetype == '10col') {
      codes = paste(codes, "data = read.csv(\"insert your quantification dataset filepath\", header = TRUE, sep = ",input$sep,")\n", sep = "")
    }
    else if(input$filetype == 'sky') {
      cat(file=stderr(), "Reached here in skyline\n")
      codes = paste(codes, "data = read.csv(\"insert your MSstats report from Skyline filepath\", header = TRUE, sep = \",\", stringsAsFactors=F)\n", sep = "")
      
      if(input$DDA_DIA=="DDA" ){
        codes = paste(codes, "data = data[which(data$Fragment.Ion %in% c( \"precursor\", \"precursor [M+1]\",\"precursor [M+2]\")), ]\nannot_file = read.csv(\"insert your annotation filepath\")\n", sep = "")
        codes = paste(codes, "data = SkylinetoMSstatsFormat(data,\n\t\t\t\t        annotation = annot_file,\n\t\t\t\t        fewMeasurements=\"remove\",\n\t\t\t\t        removeProtein_with1Feature = ", 
                      input$remove,",\n\t\t\t\t       ", "use_log_file = FALSE)\n", sep = "")
        
      }
      else if(input$DDA_DIA=="DIA"){
        
        codes = paste(codes, "annot_file = read.csv(\"insert your annotation filepath\")\n"
                      , sep = "")
        
        
        codes = paste(codes, "data = SkylinetoMSstatsFormat(data,
                                       annotation = annot_file,
                                       filter_with_Qvalue = TRUE, 
                                       qvalue_cutoff = 0.01,
                                       fewMeasurements=\"remove\",
                                       removeProtein_with1Feature = TRUE,
                                       use_log_file = FALSE)\n", sep = "")
        
      }
    }
    else if(input$filetype == 'maxq') {
      cat(file=stderr(), "Reached in maxq\n")
      codes = paste(codes, "an_maxq = read.csv(\"insert your annotation filepath\")\n ev_maxq = read.table(\"insert your evidence.txt filepath\",sep=\"\t\", header=TRUE)\n pg_maxq = read.table(\"insert your proteinGroups.txt filepath\",sep=\"\t\", header=TRUE)\n"
                    , sep = "")
      if(input$DDA_DIA=="TMT"){
        
        codes = paste(codes, "data = MaxQtoMSstatsTMTFormat(evidence=ev_maxq, 
                                         annotation=an_maxq,
                                         proteinGroups=\'", input$which.proteinid,"\',\n\t\t\t\t       ",
                      "use_log_file = FALSE)\n", sep = "")
      } else if (input$DDA_DIA=="PTM"){
        codes = paste(codes, "sites.data = read.csv(\"insert your PTM site data filepath\")\n data = MaxQtoMSstatsPTMFormat(sites.data, an_maxq, ev_maxq, pg_maxq, an_maxq)\n",
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
      
      codes = paste(codes, "data = read.csv(\"insert your quantification dataset filepath\", header = TRUE, sep = ",input$sep,")
                       annot_file = read.csv(\"insert your annotation filepath\")\n"
                    , sep = "")
      
      codes = paste(codes, "data = ProgenesistoMSstatsFormat(input = data,
                                       annotation = annot_file,
                                       removeProtein_with1Peptide = TRUE,
                                       use_log_file = FALSE)\n", sep = "")
      
      codes = paste(codes, "colnames(data)[colnames(data) == \'PeptideModifiedSequence\'] = \'PeptideSequence\'\n", sep = "")
      
    }
    else if(input$filetype == 'PD') {
      
      if(input$DDA_DIA=="TMT"){
        
        codes = paste(codes, "data = read.delim(\"insert your quantification dataset filepath\")
                       annot_file = read.csv(\"insert your annotation filepath\")\n"
                      , sep = "")
        
        
        codes = paste(codes, "data = PDtoMSstatsTMTFormat(input = data,
                                       annotation = annot_file,
                                       which.proteinid =\'", input$which.proteinid,"\',\n\t\t\t\t       ",
                      "use_log_file = FALSE)\n", sep = "")
        
      }
      else{
        codes = paste(codes, "data = read.delim(\"insert your quantification dataset filepath\")\nannot_file = read.csv(\"insert your annotation filepath\")\n"
                      , sep = "")
        
        codes = paste(codes, "data = PDtoMSstatsFormat(data,
                                       annotation = annot_file,
                                       removeProtein_with1Peptide = ", input$remove,",\n\t\t\t\t       ",
                      "use_log_file = FALSE)\n", sep = "")
        
        codes = paste(codes, "colnames(data)[colnames(data) == \'PeptideModifiedSequence\'] = \'PeptideSequence\'\n", sep = "")
        
      }
      
    }
    else if(input$filetype == 'spec') {
      
      codes = paste(codes, "data = read.csv(\"insert your MSstats scheme output from Spectronaut filepath\", header = TRUE)\nannot_file = read.csv(\"insert your annotation filepath\", sep='\t')#Optional\n"
                    , sep = "")
      
      codes = paste(codes, "data = SpectronauttoMSstatsFormat(data,
                                       annotation = annot_file #Optional,
                                       filter_with_Qvalue = TRUE, ## same as default
                                       qvalue_cutoff = 0.01, ## same as default
                                       fewMeasurements=\"remove\",
                                       removeProtein_with1Feature = TRUE,
                                       use_log_file = FALSE)\n", sep = "")
    }
    else if(input$filetype == 'open') {
      
      codes = paste(codes, "data = read.csv(\"insert your quantification dataset filepath\", header = TRUE, sep = ",input$sep,")\nannot_file = read.csv(\"insert your annotation filepath\")\n"
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
        
        codes = paste(codes, "data = read.csv(\"insert your quantification dataset filepath\", header = TRUE, sep = ",input$sep,")\ndata = OpenMStoMSstatsTMTFormat(data, use_log_file = FALSE)\n"
                      , sep = "")
        
      }
      else{
        
        codes = paste(codes, "data = read.csv(\"insert your quantification dataset filepath\", header = TRUE, sep = ",input$sep,")\nunique(data[, c('Run', 'BioReplicate', 'Condition')])\ndata = OpenMStoMSstatsFormat(data, removeProtein_with1Feature=TRUE, use_log_file = FALSE)\n"
                      , sep = "")
        
      }
    }
    else if(input$filetype == 'spmin') {
      
      codes = paste(codes, "data = read.csv(\"insert your quantification dataset filepath\", header = TRUE, sep = \"\t\")\nannot_file = read.csv(\"insert your annotation filepath\")\ndata = SpectroMinetoMSstatsTMTFormat(data, annot_file,
                                              use_log_file = FALSE)"
                    , sep = "")
    }
    else if(input$filetype == 'phil' & input$DDA_DIA == "TMT") {
      
      codes = paste(codes,"data = read.csv(\"insert your msstats filepath\")\n"
                    , sep = "")
      codes = paste(codes,"annot_file = read.csv(\"insert your annotation filepath\")\n"
                    , sep = "")
      
      codes = paste(codes, "data = PhilosophertoMSstatsTMTFormat(data,
                                       annotation = annot_file)\n", sep = "")
    }else if (input$filetype == 'phil' & input$DDA_DIA == "PTM"){
      codes = paste(codes,"data = read.csv(\"insert your msstats filepath\")\n"
                    , sep = "")
      codes = paste(codes,"annot_file = read.csv(\"insert your annotation filepath\")\n"
                    , sep = "")
      
      codes = paste(codes,"data_protein = read.csv(\"insert your global profiling msstats filepath\")\n"
                    , sep = "")
      codes = paste(codes,"annot_protein_file = read.csv(\"insert your global profiling annotation filepath\")\n"
                    , sep = "")
      
      codes = paste(codes, paste0("data = PhilosophertoMSstatsPTMFormat(data,
                                       annot_file,
                                       data_protein,
                                       annot_protein_file,
                                       mod_id_col = ", input$mod_id_col, ",
                                       localization_cutoff = ",input$localization_cutoff, ",
                                       remove_unlocalized_peptides = ", as.character(input$remove_unlocalized_peptides),
                                  ")\n"), sep = "")
    }
  }
  
  if (input$DDA_DIA != "PTM"){
    codes = paste(codes,"data = unique(as.data.frame(data))\n", sep = "")
  }
  return(codes)
  
}

getSummary1 <- function(input) {
  df = getData(input)
  annot_df = getAnnot(input)
  if (input$DDA_DIA != "PTM"){
    df = as.data.frame(df)
    df = df %>% filter(!Condition %in% c("Norm", "Empty"))
    nf = ifelse("Fraction" %in% colnames(df),n_distinct(df$Fraction),1)
  }
  
  if(input$DDA_DIA=="TMT"){
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
    
  } else if (input$DDA_DIA == "PTM"){
    if (input$PTMTMT == "Yes"){
      ptm_df = df$PTM
      unmod_df = df$PROTEIN
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
  
  if (input$DDA_DIA != "PTM"){
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

getSummary2 <- function(input) {
  df = getData(input)
  if(input$DDA_DIA=="TMT"){
    df = as.data.frame(df)
    df = df %>% mutate("FEATURES" = paste(ProteinName, PeptideSequence, Charge,
                                          sep = '_'))
  } else if (input$DDA_DIA == "PTM" & input$PTMTMT == "Yes"){
    df_ptm = as.data.frame(df$PTM) %>% mutate("FEATURES" = paste(ProteinName, PeptideSequence,
                                                                 Charge, sep = '_'))
    df_prot = as.data.frame(df$PROTEIN) %>% mutate("FEATURES" = paste(ProteinName, 
                                                                      PeptideSequence,
                                                                      Charge, sep = '_'))
  } else if (input$DDA_DIA == "PTM" & input$PTMTMT == "No"){
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
  
  if (input$DDA_DIA != "PTM"){
    
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
preprocessData <- function(qc_input,loadpage_input) {
  validate(need(getData(loadpage_input), 
                message = "PLEASE UPLOAD DATASET OR SELECT SAMPLE"))
  
  ## Preprocess input for loop

  input_data = getData(loadpage_input)
  preprocess_list = list()
  MSstatsLogsSettings(FALSE)
  ## Here we run the underlying functions for MSstats and MSstatsTMT 
  ## summarization. Done so we can loop over proteins and create a progress bar
  if (loadpage_input$DDA_DIA == "PTM" & loadpage_input$PTMTMT == "No"){
    
    preprocessed_ptm = MSstatsShiny::lf_summarization_loop(input_data$PTM, qc_input, loadpage_input)
    preprocessed_unmod = MSstatsShiny::lf_summarization_loop(input_data$PROTEIN, qc_input, loadpage_input)
    preprocessed = list(PTM = preprocessed_ptm, PROTEIN = preprocessed_unmod)
    
  } else if(loadpage_input$DDA_DIA == "PTM" & loadpage_input$PTMTMT == "Yes"){
    
    preprocessed_ptm = MSstatsShiny::tmt_summarization_loop(input_data$PTM, qc_input,loadpage_input)
    preprocessed_unmod = MSstatsShiny::tmt_summarization_loop(input_data$PROTEIN, qc_input,loadpage_input)
    preprocessed = list(PTM = preprocessed_ptm, PROTEIN = preprocessed_unmod)
    
  } else if(loadpage_input$DDA_DIA == "TMT"){
    
    ## Run MSstatsTMT summarization
    print("hereee")
    preprocessed = MSstatsShiny::tmt_summarization_loop(input_data, qc_input,loadpage_input)
    print("thereee")
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
  } else if (loadpage_input$DDA_DIA == "PTM"){
    if (loadpage_input$PTMTMT == "Yes"){
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
    print(qc_input$features_used)
    print("xxx")
    if (qc_input$features_used == "all"){
      code_n_feat = 'NULL'
    } else if (qc_input$features_used == "topN") {
      code_n_feat = qc_input$n_feat
    } else {
      code_n_feat = 'NULL'
    }
    
    codes = paste(codes, "\n# use MSstats for protein summarization\n", sep = "")
    codes = paste(codes, "summarized = MSstats::dataProcess(data,
                               normalization = \'", qc_input$norm,"\',\t\t\t\t   
                               logTrans = ", as.numeric(qc_input$log),",\t\t\t\t   
                               nameStandards = ", paste0("c('", paste(qc_input$names, collapse = "', '"), "')"), ",\t\t\t\t  
                               featureSubset = \'", qc_input$features_used, "\',\t\t\t\t  
                               n_top_feature = ", code_n_feat, ",\t\t\t\t  
                               summaryMethod=\"TMP\",
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
                           address = FALSE)\n", sep="")
  }
  
  return(codes)
  
}



# statmodel server functions
dataComparison <- function(statmodel_input,qc_input,loadpage_input,matrix) {
  input_data = preprocessData(qc_input,loadpage_input)
  contrast.matrix = matrix
  if (loadpage_input$DDA_DIA == "PTM" & loadpage_input$PTMTMT == "Yes"){
    model_ptm = MSstatsShiny::tmt_model(input_data$PTM, statmodel_input, contrast.matrix)
    model_protein = MSstatsShiny::tmt_model(input_data$PROTEIN, statmodel_input, contrast.matrix)
    model_adj = MSstatsShiny::apply_adj(model_ptm$ComparisonResult,
                                        model_protein$ComparisonResult)
    model = list('PTM.Model' = model_ptm$ComparisonResult,
                 'PROTEIN.Model' = model_protein$ComparisonResult,
                 'ADJUSTED.Model' = model_adj)
    
  } else if(loadpage_input$DDA_DIA == "PTM" & loadpage_input$PTMTMT == "No"){
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