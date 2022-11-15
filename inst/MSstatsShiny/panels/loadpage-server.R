# toggle ui (DDA DIA SRM)
observe({
  if (input$DDA_DIA == "DDA") {
    runjs("$('[type=radio][name=filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
    enable("filetype")
    disable(selector = "[type=radio][value=spec]")
    disable(selector = "[type=radio][value=open]")
    disable(selector = "[type=radio][value=ump]")
    disable(selector = "[type=radio][value=spmin]")
    disable(selector = "[type=radio][value=phil]")
    runjs("$.each($('[type=radio][name=filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
  }
  else if (input$DDA_DIA == "DIA") {
    runjs("$('[type=radio][name=filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
    enable("filetype")
    disable(selector = "[type=radio][value=maxq]")
    disable(selector = "[type=radio][value=prog]")
    disable(selector = "[type=radio][value=PD]")
    disable(selector = "[type=radio][value=openms]")
    disable(selector = "[type=radio][value=spmin]")
    disable(selector = "[type=radio][value=phil]")
    runjs("$.each($('[type=radio][name=filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
  }
  else if (input$DDA_DIA == "SRM_PRM") {
    runjs("$('[type=radio][name=filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
    enable("filetype")
    disable(selector = "[type=radio][value=maxq]")
    disable(selector = "[type=radio][value=prog]")
    disable(selector = "[type=radio][value=PD]")
    disable(selector = "[type=radio][value=openms]")
    disable(selector = "[type=radio][value=spec]")
    disable(selector = "[type=radio][value=open]")
    disable(selector = "[type=radio][value=ump]")
    disable(selector = "[type=radio][value=spmin]")
    disable(selector = "[type=radio][value=phil]")
    runjs("$.each($('[type=radio][name=filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
  }
  
  else if (input$DDA_DIA == "TMT") {
    runjs("$('[type=radio][name=filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
    enable("filetype")
    disable(selector = "[type=radio][value=sky]")
    disable(selector = "[type=radio][value=prog]")
    disable(selector = "[type=radio][value=spec]")
    disable(selector = "[type=radio][value=open]")
    disable(selector = "[type=radio][value=ump]")
    runjs("$.each($('[type=radio][name=filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
    
  }
  else if (input$DDA_DIA %in% c("PTM", "PTM_TMT")) {
    runjs("$('[type=radio][name=filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
    enable("filetype")
    disable(selector = "[type=radio][value=sky]")
    disable(selector = "[type=radio][value=prog]")
    disable(selector = "[type=radio][value=PD]")
    disable(selector = "[type=radio][value=openms]")
    disable(selector = "[type=radio][value=spec]")
    disable(selector = "[type=radio][value=open]")
    disable(selector = "[type=radio][value=ump]")
    disable(selector = "[type=radio][value=spmin]")
    disable(selector = "[type=radio][value=phil]")
    runjs("$.each($('[type=radio][name=filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
  }
})

observeEvent(input$filetype,{
  enable("proceed1")
})


### functions ###
get_annot = eventReactive(input$proceed1, {
  annot = input$annot
  if(is.null(annot)) {
    return(NULL)
  }
  else if (input$DDA_DIA == "TMT" && input$filetype == "sample") 
  {
    return(annotation.pd)
  }
  annot_file = try(read.csv(annot$datapath), silent=TRUE)
  if (class(annot_file) == "try-error") {
    annot_file = "File load error. Please ensure file is in csv format."
  }
  return(annot_file)
})

get_annot1 = reactive({
  annot1 = input$annot1
  if(is.null(input$annot1)) {
    return(NULL)
  }
  annot1=try(read.csv(annot1$datapath, header = TRUE), silent=TRUE)
  if (class(annot1) == "try-error") {
    annot1 = "File load error. Please ensure file is in csv format."
  }
  
  cat(file=stderr(), "Reached in maxq annot\n")
  return(annot1)
  
})

get_annot2 = reactive({
  annot1 = input$annot2
  if(is.null(input$annot2)) {
    return(NULL)
  }
  annot2=try(read.csv(annot1$datapath, header = TRUE), silent=TRUE)
  
  if (class(annot2) == "try-error") {
    annot2 = "File load error. Please ensure file is in csv format."
  }
  
  cat(file=stderr(), "Reached in ump annot\n")
  return(annot2)
  
})

get_annot3 = reactive({
  annot3 = input$annot3
  if(is.null(input$annot3)) {
    return(NULL)
  }
  annot3 = try(read.delim(annot3$datapath), silent=TRUE)
  
  if (class(annot3) == "try-error") {
    annot3 = "File load error. Please ensure file is in csv format."
  }
  
  cat(file=stderr(), "Reached in ump annot\n")
  return(annot3)
  
})

get_evidence = reactive({
  evidence = input$evidence
  if(is.null(input$evidence)) {
    return(NULL)
  }
  evidence = try(read.table(evidence$datapath, sep="\t", header=TRUE), silent=TRUE)
  
  if (class(evidence) == "try-error") {
    evidence = "File load error. Please ensure file is in csv format."
  }
  
  cat(file=stderr(), "Reached in evidence\n")
  return(evidence)
  
})

get_evidence2 = reactive({
  evidence2 = input$evidence2
  if(is.null(input$evidence2)) {
    return(NULL)
  }
  evidence2 = try(read.delim(evidence2$datapath), silent=TRUE)
  if (class(evidence2) == "try-error"){
    evidence2 = "File load error. Please ensure file is in csv format." 
  }
  cat(file=stderr(), "Reached in evidence\n")
  return(evidence2)
  
})

get_global = reactive({
  unmod = input$unmod
  if(is.null(input$unmod)) {
    return(NULL)
  }
  unmod = try(read.csv(unmod$datapath, sep=",", header=TRUE, 
                       stringsAsFactors=FALSE), silent=TRUE)
  
  if (class(unmod) == "try-error"){
    unmod = "File load error. Please ensure file is in csv format." 
  }
  
  cat(file=stderr(), "Reached in unmod\n")
  return(unmod)
  
})
get_proteinGroups = reactive({
  pGroup = input$pGroup
  if(is.null(input$pGroup)) {
    return(NULL)
  }
  pGroup = try(read.table(pGroup$datapath, sep="\t", header=TRUE), silent=TRUE)
  
  if (class(pGroup) == "try-error"){
    pGroup = "File load error. Please ensure file is in csv format." 
  }
  
  cat(file=stderr(), "Reached in proteins_group\n")
  return(pGroup)
})

get_proteinGroups2 = reactive({
  pGroup2 = input$pGroup2
  if(is.null(input$pGroup2)) {
    return(NULL)
  }
  pGroup2 = try(read.delim(pGroup2$datapath), silent=TRUE)
  
  if (class(pGroup2) == "try-error"){
    pGroup2 = "File load error. Please ensure file is in csv format." 
  }
  
  cat(file=stderr(), "Reached in proteins_group\n")
  return(pGroup2)
})

get_FragSummary = reactive({
  fragSummary = input$fragSummary
  if(is.null(input$fragSummary)) {
    return(NULL)
  }
  fragSummary = try(read.table(fragSummary$datapath, sep="\t", header=TRUE),
                    silent=TRUE)
  
  if (class(fragSummary) == "try-error"){
    fragSummary = "File load error. Please ensure file is in excel format." 
  }
  
  return(fragSummary)
  
})

get_peptideSummary = reactive({
  peptideSummary = input$peptideSummary
  if(is.null(input$peptideSummary)) {
    return(NULL)
  }
  peptideSummary = try(read.table(peptideSummary$datapath, sep="\t", 
                                  header=TRUE), silent=TRUE)
  
  if (class(peptideSummary) == "try-error"){
    peptideSummary = "File load error. Please ensure file is in csv format." 
  }
  
  return(peptideSummary)
  
})

get_protSummary = reactive({
  protSummary = input$protSummary
  if(is.null(input$protSummary)) {
    return(NULL)
  }
  protSummary = try(read.table(protSummary$datapath, sep="\t", header=TRUE),
                    silent=TRUE)
  
  if (class(protSummary) == "try-error"){
    protSummary = "File load error. Please ensure file is in csv format." 
  }
  
  return(protSummary)
  
})

get_maxq_ptm_sites = reactive({
  maxq_ptm_sites = input$maxq_ptm_sites
  if(is.null(input$maxq_ptm_sites)) {
    return(NULL)
  }
  maxq_ptm_sites = try(read.delim(maxq_ptm_sites$datapath), silent=TRUE)
  
  if (class(maxq_ptm_sites) == "try-error"){
    maxq_ptm_sites = "File load error. Please ensure file is in csv format." 
  }
  
  cat(file=stderr(), "Reached in maxq_ptm_sites\n")
  return(maxq_ptm_sites)
  
})

get_data = eventReactive(input$proceed1, {
  show_modal_spinner()
  ev_maxq = get_evidence()
  pg_maxq = get_proteinGroups()
  ev_maxq2 = get_evidence2()
  pg_maxq2 = get_proteinGroups2()
  an_maxq = get_annot1()
  raw.frag = get_FragSummary()
  raw.pep = get_peptideSummary()
  raw.pro = get_protSummary()
  annot2 = get_annot2()
  annot3 = get_annot3()
  unmod = get_global()
  ptm_sites_data = get_maxq_ptm_sites()

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
      mydata = read.csv("dataset.csv", header = TRUE, sep = ";")
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

    } else {
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
    else if(input$filetype=='phil'){
      extracted.files = unzip(input$folder$datapath, list = TRUE)
      unzip(input$folder$datapath, list = FALSE)
      infile = paste0("./", str_split(extracted.files$Name[1], "/")[[1]][[1]])
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
      data = read.csv(infile$datapath, header = TRUE, sep = input$sep, 
                      stringsAsFactors=FALSE)
      
      if(input$DDA_DIA=="DDA" ){
        data = data[which(data$Fragment.Ion %in% c("precursor", "precursor [M+1]","precursor [M+2]")),]
        
        mydata = SkylinetoMSstatsFormat(data,
                                         annotation = get_annot(),
                                         fewMeasurements="remove",
                                         removeProtein_with1Feature = input$remove,
                                         use_log_file = FALSE)
      }
      else if(input$DDA_DIA=="DIA"){
        mydata = SkylinetoMSstatsFormat(data,
                                         annotation = get_annot(),
                                         filter_with_Qvalue = TRUE, 
                                         qvalue_cutoff = 0.01, 
                                         fewMeasurements="remove", 
                                         removeProtein_with1Feature = TRUE,
                                         use_log_file = FALSE)
        
      }
      else if(input$DDA_DIA=="SRM_PRM") {
        mydata = SkylinetoMSstatsFormat(data,
                                         annotation = get_annot(),
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
      data = read.csv(infile$datapath, header = TRUE, sep = input$sep, 
                      stringsAsFactors=FALSE)
      
      mydata = ProgenesistoMSstatsFormat(data, annotation = get_annot(), 
                                          removeProtein_with1Peptide = TRUE,
                                          use_log_file = FALSE)
      colnames(mydata)[colnames(mydata) == 'PeptideModifiedSequence'] = 'PeptideSequence'
    }
    else if(input$filetype == 'PD') {
      
      if(input$DDA_DIA=="TMT"){

        data = read.csv(infile$datapath, header = TRUE, sep = input$sep, 
                        stringsAsFactors=FALSE)
        mydata = PDtoMSstatsTMTFormat(input = data, 
                                       annotation = get_annot(),
                                       which.proteinid = input$which.proteinid, ## same as default
                                       use_log_file = FALSE
        )
      }
      else{
        data = read.csv(infile$datapath, header = TRUE, sep = input$sep, 
                        stringsAsFactors=FALSE)
        mydata = PDtoMSstatsFormat(data, annotation = get_annot(), 
                                    removeProtein_with1Peptide = input$remove,
                                    use_log_file = FALSE)
        colnames(mydata)[colnames(mydata) == 'PeptideModifiedSequence'] = 'PeptideSequence'
      }
      
    }
    else if(input$filetype == 'spec') {

      data = read.csv(infile$datapath, sep = "\t")
      mydata = SpectronauttoMSstatsFormat(data,
                                           annotation = get_annot(),
                                           filter_with_Qvalue = TRUE, ## same as default
                                           qvalue_cutoff = 0.01, ## same as default
                                           removeProtein_with1Feature = TRUE,
                                           use_log_file = FALSE)
    }
    else if(input$filetype == 'open') {
      data = read.csv(infile$datapath, header = TRUE, sep = input$sep)
      mydata =OpenSWATHtoMSstatsFormat(data,
                                        annotation = get_annot(),
                                        filter_with_mscore = TRUE, ## same as default
                                        mscore_cutoff = 0.01, ## same as default
                                        removeProtein_with1Feature = TRUE,
                                        use_log_file = FALSE)
      cat(file=stderr(), "Reached in openSwath\n")
    }
    else if(input$filetype == 'openms') {
      if(input$DDA_DIA=="TMT"){
        data = read.csv(infile$datapath, header = TRUE, sep = input$sep)
        mydata = OpenMStoMSstatsTMTFormat(data, use_log_file = FALSE)
        
      }
      else{
        data = read.csv(infile$datapath, header = TRUE, sep = input$sep)
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
      data = read.csv(infile$datapath, sep="\t")
      mydata = SpectroMinetoMSstatsTMTFormat(data, get_annot(),
                                              use_log_file = FALSE)
    }
    else if(input$filetype == 'phil') {
      mydata = PhilosophertoMSstatsTMTFormat(path = infile, folder = TRUE, annotation = get_annot(),
                                              protein_id_col = input$which.proteinid,
                                              use_log_file = FALSE)
    }
  }
  
  
  if (input$level == "Peptide"){
    mydata$ProteinName = mydata$PeptideSequence
  }
  
  remove_modal_spinner()
  return(mydata)
})

get_data_code = eventReactive(input$calculate, { 
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
      codes = paste(codes, "ptm_data = read.csv(\'Enter PTM data file path here\')\nglobal_data = read.csv(\'Enter unmod data file path here\')\ndata = list(PTM = input$data, PROTEIN = unmod)\n")
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
      
      codes = paste(codes, "data = read.csv(\"insert your MSstats scheme output from Spectronaut filepath\", header = TRUE)\nannot_file = read.csv(\"insert your annotation filepath\")\n"
                     , sep = "")
      
      codes = paste(codes, "data = SpectronauttoMSstatsFormat(data,
                                       annotation = annot_file,
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
    else if(input$filetype == 'phil') {
      
      codes = paste(codes,"annot_file = read.csv(\"insert your annotation filepath\")\n"
                     , sep = "")
      
      codes = paste(codes, "data = PhilosophertoMSstatsTMTFormat((path = \"insert your folder path\",
                                       folder = TRUE,
                                       annotation = annot_file,
                                       protein_id_col =\'", input$which.proteinid,"\',\n\t\t\t\t       ",
                     "use_log_file = FALSE)\n", sep = "")
    }
  }
  
  codes = paste(codes,"data = unique(as.data.frame(data))\n"
                 , sep = "")  
  return(codes)
  
})
get_summary1 = eventReactive(input$proceed1, {
  df = get_data()
  annot_df = get_annot()
  
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
})

get_summary2 = eventReactive(input$proceed1, {
  
  df = get_data()
  if(input$DDA_DIA=="TMT"){
    df = as.data.frame(df)
    df = df %>% mutate("FEATURES" = paste(ProteinName, PeptideSequence, Charge,
                                          sep = '_'))
  } else if (input$DDA_DIA == "PTM" & input$PTMTMT == "Yes"){
    df_ptm = df$PTM %>% mutate("FEATURES" = paste(ProteinName, PeptideSequence,
                                                  Charge, sep = '_'))
    df_prot = df$PROTEIN %>% mutate("FEATURES" = paste(ProteinName, 
                                                       PeptideSequence,
                                                       Charge, sep = '_'))
  } else if (input$DDA_DIA == "PTM" & input$PTMTMT == "No"){
    df_ptm = df$PTM %>% mutate("FEATURES" = paste(PeptideSequence, 
                                                  PrecursorCharge, 
                                                  FragmentIon, 
                                                  ProductCharge, sep = '_'))
    df_prot = df$PROTEIN %>% mutate("FEATURES" = paste(PeptideSequence, 
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
                           "Min_Intensity" = ifelse(!is.finite(min(Intensity, na.rm=T)),0,round(min(Intensity, na.rm=T),0)),
                           "Max_Intensity" = ifelse(!is.finite(max(Intensity, na.rm=T)),0,
                                                    round(max(Intensity, na.rm=T),0))) %>%
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
    
    df_ptm1 = df_ptm %>% summarise("Number of PTMs" = n_distinct(ProteinName), 
                                   "Number of PTM Features" = n_distinct(FEATURES),
                                   "Number of Features/PTM" = as.numeric(n_distinct(FEATURES) / n_distinct(PeptideSequence)),
                                   "Min_Intensity" = ifelse(!is.finite(
                                     min(Intensity, na.rm=T)), 0, 
                                     round(min(Intensity, na.rm=T),0)),
                                   "Max_Intensity" = ifelse(!is.finite(
                                     max(Intensity, na.rm=T)), 0, 
                                     round(max(Intensity, na.rm=T),0))) %>%
      unite("PTM Intensity Range", Min_Intensity:Max_Intensity, sep = " - ")
    # df_ptm1 = df_ptm1 %>% select(!Min_Intensity, !Max_Intensity)
    
    df_prot1 = df_prot %>% summarise("Number of Unmod Proteins" = n_distinct(ProteinName), 
                                     "Number of Protein Peptides" = n_distinct(PeptideSequence),
                                     "Number of Protein Features" = n_distinct(FEATURES),
                                     "Number of Features/Peptide" = as.numeric(n_distinct(FEATURES) / n_distinct(PeptideSequence)),
                                     "Number of Peptides/Protein" = as.numeric(n_distinct(PeptideSequence) / n_distinct(ProteinName)),
                                     "Min_Intensity" = ifelse(!is.finite(
                                       min(Intensity, na.rm=T)), 0, 
                                       round(min(Intensity, na.rm=T),0)),
                                     "Max_Intensity" = ifelse(!is.finite(
                                       max(Intensity, na.rm=T)), 0, 
                                       round(max(Intensity, na.rm=T),0))) %>%
      unite("Protein Intensity Range", Min_Intensity:Max_Intensity, sep = " - ")
    df1 = cbind(df_ptm1, df_prot1)
  }
  

  t_df = as.data.frame(t(df1))
  rownames(t_df) = colnames(df1)
  t_df = cbind(rownames(t_df), t_df)
  
  colnames(t_df) = c("", "value")
  colnames(t_df) = c("", "")
  
  return(t_df)
})



onclick("proceed1", {
  get_data()
  get_annot()
  shinyjs::show("summary_tables")
  
  ### outputs ###
  get_summary = reactive({
    if(is.null(get_data())) {
      return(NULL)
    }
    data1 = get_data()
    data_summary = describe(data1)
  })

  output$template = downloadHandler(
    filename = function() {
      paste("templateannotation", "csv", sep=".")
    },
    
    content = function(file) {
      file.copy("templateannotation.csv", file)
    },
    contentType = "csv"
  )

  output$template1 = downloadHandler(
    filename = function() {
      paste("templateevidence", "txt", sep = ".")
    },
    
    content = function(file) {
      file.copy("templateevidence.txt", file)
    },
    contentType = "txt"
  )

  output$summary = renderTable(
    {
      head(get_data())
    }, bordered = TRUE
  )
  output$summary_ptm = renderTable(
    {
      head(get_data()$PTM)
    }, bordered = TRUE
  )
  output$summary_prot = renderTable(
    {
      head(get_data()$PROTEIN)
    }, bordered = TRUE
  )

  
  output$summary1 =  renderTable(
    {
      req(get_data())
      get_summary1()

    }, colnames = FALSE, bordered = TRUE
  )

  output$summary2 =  renderTable(
    {
      req(get_data())
      get_summary2()

    }, colnames = FALSE, bordered = TRUE, align='lr'
  )

  onclick("proceed2", {
    updateTabsetPanel(session = session, inputId = "tablist", selected = "DataProcessing")
  })
  
  output$summary_tables = renderUI({
    
    tagList(
      tags$head(
        tags$style(HTML('#proceed2{background-color:orange}'))
      ),
      actionButton(inputId = "proceed2", label = "Next step"),
      h4("Summary of experimental design"),
      tableOutput('summary1'),
      tags$br(),
      h4("Summary of dataset"), 
      tableOutput("summary2"),
      tags$br(),
      conditionalPanel(condition = "input.DDA_DIA !== 'PTM'",
                       h4("Top 6 rows of the dataset"),
                       tableOutput("summary")
      ),
      conditionalPanel(condition = "input.DDA_DIA == 'PTM'",
                       h4("Top 6 rows of the PTM dataset"),
                       tableOutput("summary_ptm"),
                       tags$br(),
                       h4("Top 6 rows of the unmodified protein dataset"),
                       tableOutput("summary_prot")
      )
    )
  })

})
