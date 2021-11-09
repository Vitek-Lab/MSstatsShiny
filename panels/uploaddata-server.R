# toggle ui (DDA DIA SRM)


observe({
  if (input$DDA_DIA == "DDA") {
    shinyjs::runjs("$('[type=radio][name=filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
    shinyjs::enable("filetype")
    shinyjs::disable(selector = "[type=radio][value=spec]")
    shinyjs::disable(selector = "[type=radio][value=open]")
    shinyjs::disable(selector = "[type=radio][value=ump]")
    shinyjs::disable(selector = "[type=radio][value=spmin]")
    shinyjs::runjs("$.each($('[type=radio][name=filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
  }
  else if (input$DDA_DIA == "DIA") {
    shinyjs::runjs("$('[type=radio][name=filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
    shinyjs::enable("filetype")
    shinyjs::disable(selector = "[type=radio][value=maxq]")
    shinyjs::disable(selector = "[type=radio][value=prog]")
    shinyjs::disable(selector = "[type=radio][value=PD]")
    shinyjs::disable(selector = "[type=radio][value=openms]")
    shinyjs::disable(selector = "[type=radio][value=spmin]")
    shinyjs::runjs("$.each($('[type=radio][name=filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
  }
  else if (input$DDA_DIA == "SRM_PRM") {
    shinyjs::runjs("$('[type=radio][name=filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
    shinyjs::enable("filetype")
    shinyjs::disable(selector = "[type=radio][value=maxq]")
    shinyjs::disable(selector = "[type=radio][value=prog]")
    shinyjs::disable(selector = "[type=radio][value=PD]")
    shinyjs::disable(selector = "[type=radio][value=openms]")
    shinyjs::disable(selector = "[type=radio][value=spec]")
    shinyjs::disable(selector = "[type=radio][value=open]")
    shinyjs::disable(selector = "[type=radio][value=ump]")
    shinyjs::disable(selector = "[type=radio][value=spmin]")
    shinyjs::runjs("$.each($('[type=radio][name=filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
    
  }
  
  else if (input$DDA_DIA == "TMT") {
    shinyjs::runjs("$('[type=radio][name=filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
    shinyjs::enable("filetype")
    shinyjs::disable(selector = "[type=radio][value=sky]")
    shinyjs::disable(selector = "[type=radio][value=prog]")
    shinyjs::disable(selector = "[type=radio][value=spec]")
    shinyjs::disable(selector = "[type=radio][value=open]")
    shinyjs::disable(selector = "[type=radio][value=ump]")
    shinyjs::runjs("$.each($('[type=radio][name=filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
    
  }
  
})

### functions ###

get_annot = reactive({
  annot <- input$annot
  if(is.null(annot)) {
    return(NULL)
  }
  annot_file <- read.csv(annot$datapath)
  return(annot_file)
})

get_annot1 = reactive({
  annot1 <- input$annot1
  if(is.null(input$annot1)) {
    return(NULL)
  }
  annot1<-read.csv(annot1$datapath, header = T)
  cat(file=stderr(), "Reached in maxq annot\n")
  return(annot1)
  
})

get_annot2 = reactive({
  annot1 <- input$annot2
  if(is.null(input$annot2)) {
    return(NULL)
  }
  annot2<-read.csv(annot1$datapath, header = T)
  cat(file=stderr(), "Reached in ump annot\n")
  return(annot2)
  
})

get_evidence = reactive({
  evidence <- input$evidence
  if(is.null(input$evidence)) {
    return(NULL)
    }
  evidence <- read.table(evidence$datapath, sep="\t", header=TRUE)
  cat(file=stderr(), "Reached in evidence\n")
  return(evidence)
  
})

get_proteinGroups = reactive({
  pGroup <- input$pGroup
  if(is.null(input$pGroup)) {
    return(NULL)
  }
  pGroup<-read.table(pGroup$datapath, sep="\t", header=TRUE)
  cat(file=stderr(), "Reached in proteins_group\n")
  return(pGroup)
})

get_FragSummary = reactive({
  fragSummary <- input$fragSummary
  if(is.null(input$fragSummary)) {
    return(NULL)
  }
  fragSummary <- read.table(fragSummary$datapath, sep="\t", header=TRUE)
  return(fragSummary)
  
})

get_peptideSummary = reactive({
  peptideSummary <- input$peptideSummary
  if(is.null(input$peptideSummary)) {
    return(NULL)
  }
  peptideSummary <- read.table(peptideSummary$datapath, sep="\t", header=TRUE)
  return(peptideSummary)
  
})

get_protSummary = reactive({
  protSummary <- input$protSummary
  if(is.null(input$protSummary)) {
    return(NULL)
  }
  protSummary <- read.table(protSummary$datapath, sep="\t", header=TRUE)
  return(protSummary)
  
})


get_data = reactive({
  
  ev_maxq <- get_evidence()
  pg_maxq <- get_proteinGroups()
  an_maxq <- get_annot1()
  
  raw.frag <- get_FragSummary()
  raw.pep <- get_peptideSummary()
  raw.pro <- get_protSummary()
  annot2 <- get_annot2()
  
  cat(file=stderr(), "Reached in get_data\n")
  
  cat(file=stderr(), paste("File type is",input$filetype,"\n"))
  if(is.null(input$filetype)) {
    return(NULL)
    }
  if(input$filetype == 'sample') {
     if(input$DDA_DIA == "SRM_PRM") {
       mydata <- SRM_yeast
     }
    else if(input$DDA_DIA == "DDA") {
     mydata <- DDARawData
    }
     else if(input$DDA_DIA == "DIA"){
       mydata <- read.csv("dataset.csv", header = T, sep = ";")
     }
    else if(input$DDA_DIA == "TMT"){
      mydata <- PDtoMSstatsTMTFormat(input = MSstatsTMT::raw.pd, 
                                     annotation = MSstatsTMT::annotation.pd,
                                     which.proteinid = "Protein.Accessions" ## same as default
      )
    }
    
    }
  else {
    if(input$filetype=='spec' || input$filetype=='spmin'){
      infile <- input$data1
    }
    else{
      infile <- input$data
    }
    
    
    if(input$filetype=='maxq'){
      if(is.null(ev_maxq) || is.null(pg_maxq) || is.null(an_maxq) ) {
        return(NULL)
      }
    }
    else if(input$filetype=='ump'){
      
      if(is.null(raw.frag) || is.null(raw.pep) || is.null(raw.pro) || is.null(annot2)) {
        return(NULL)
      }
      

    }
    
    else {
      if(is.null(infile)) {
        return(NULL)
      }
    }
    
    
    if(input$filetype == '10col') {
      mydata <- read.csv(infile$datapath, header = T, sep = input$sep)
    }
    else if(input$filetype == 'sky') {
      cat(file=stderr(), "Reached here in skyline\n")
      data <- read.csv(infile$datapath, header = T, sep = input$sep, stringsAsFactors=F)
      
      if(input$DDA_DIA=="DDA" ){
        data <- data[which(data$Fragment.Ion %in% c( "precursor", "precursor [M+1]","precursor [M+2]")), ]
        
        mydata <- SkylinetoMSstatsFormat(data,
                                         annotation = get_annot(),
                                         fewMeasurements="remove",
                                         removeProtein_with1Feature = input$remove)
      }
      else if(input$DDA_DIA=="DIA"){
        mydata <- SkylinetoMSstatsFormat(data,
                                        annotation = get_annot(),
                                        filter_with_Qvalue = TRUE, 
                                        qvalue_cutoff = 0.01, 
                                        fewMeasurements="remove", 
                                        removeProtein_with1Feature = TRUE)
        
      }
      else if(input$DDA_DIA=="SRM_PRM") {
        mydata <- data
      }
      
    }
    else if(input$filetype == 'maxq') {
      cat(file=stderr(), "Reached in maxq\n")
      if(input$DDA_DIA=="TMT"){
        mydata <- MaxQtoMSstatsTMTFormat(evidence=ev_maxq, 
                                         annotation=an_maxq,
                                         proteinGroups=pg_maxq)
        
      }
      else{
        mydata <- MaxQtoMSstatsFormat(evidence= ev_maxq, annotation= an_maxq, proteinGroups= pg_maxq,
                                      useUniquePeptide = TRUE,
                                      summaryforMultipleRows = max,
                                      removeProtein_with1Peptide=input$remove)
      }
      
    }
    else if(input$filetype == 'prog') {
      cat(file=stderr(), "Reached in prog\n")
      data <- read.csv(infile$datapath, header = T, sep = input$sep, stringsAsFactors=F)
      
      mydata <- ProgenesistoMSstatsFormat(data, annotation = get_annot(), removeProtein_with1Peptide = TRUE)
      colnames(mydata)[colnames(mydata) == 'PeptideModifiedSequence'] <- 'PeptideSequence'
    }
    else if(input$filetype == 'PD') {
      
      if(input$DDA_DIA=="TMT"){
        data <- read.delim(infile$datapath)
        mydata <- PDtoMSstatsTMTFormat(input = data, 
                                       annotation = get_annot(),
                                       which.proteinid = "Protein.Accessions" ## same as default
        )
      }
      else{
        data <- read.csv(infile$datapath, header = T, sep = input$sep, stringsAsFactors=F)
        mydata <- PDtoMSstatsFormat(data, annotation = get_annot(), removeProtein_with1Peptide = input$remove)
        colnames(mydata)[colnames(mydata) == 'PeptideModifiedSequence'] <- 'PeptideSequence'
      }
      
    }
    else if(input$filetype == 'spec') {
      data <- read_xls(infile$datapath, header = T)
      mydata <- SpectronauttoMSstatsFormat(data,
                                           annotation = get_annot(),
                                           filter_with_Qvalue = TRUE, ## same as default
                                           qvalue_cutoff = 0.01, ## same as default
                                           fewMeasurements="remove",
                                           removeProtein_with1Feature = TRUE)
      
    }
    else if(input$filetype == 'open') {
      data <- read.csv(infile$datapath, header = T, sep = input$sep)
      mydata <-OpenSWATHtoMSstatsFormat(data,
                               annotation = get_annot(),
                               filter_with_mscore = TRUE, ## same as default
                               mscore_cutoff = 0.01, ## same as default
                               fewMeasurements="remove",
                               removeProtein_with1Feature = TRUE)
      cat(file=stderr(), "Reached in openSwath\n")
    }
    else if(input$filetype == 'openms') {
      if(input$DDA_DIA=="TMT"){
        data <- read.csv(infile$datapath, header = T, sep = input$sep)
        mydata <- OpenMStoMSstatsTMTFormat(data)
        
      }
      else{
        data <- read.csv(infile$datapath, header = T, sep = input$sep)
        unique(data[, c('Run', 'BioReplicate', 'Condition')])
        mydata <-OpenMStoMSstatsFormat(data,
                                       removeProtein_with1Feature=TRUE)
        
      }
      
    }
    else if(input$filetype == 'ump') {
      #data <- read.csv(infile$datapath, header = T, sep = input$sep)
      #unique(data[, c('Run', 'BioReplicate', 'Condition')])
      mydata <- DIAUmpiretoMSstatsFormat(raw.frag, raw.pep, raw.pro,
                                                 annot2,
                                                 useSelectedFrag = TRUE,
                                                 useSelectedPep = FALSE,
                                                 fewMeasurements="remove",
                                                 removeProtein_with1Feature = TRUE)
    }
    else if(input$filetype == 'spmin') {
      data <- read.csv(infile$datapath, sep="\t")
      mydata <- SpectroMinetoMSstatsTMTFormat(data, get_annot())
    }
    }
  mydata <- unique(data.frame(mydata))
  return(mydata)
})

### outputs ###

get_summary <- reactive({
  if(is.null(get_data())) {
    return(NULL)
  }
  data1 <- get_data()
  data_summary <- Hmisc::describe(data1)
})

output$template <- downloadHandler(
  filename <- function() {
    paste("templateannotation", "csv", sep=".")
  },
  
  content <- function(file) {
    file.copy("templateannotation.csv", file)
  },
  contentType = "csv"
)

output$template1 <- downloadHandler(
  filename <- function() {
    paste("templateevidence", "txt", sep = ".")
  },
  
  content <- function(file) {
    file.copy("templateevidence.txt", file)
  },
  contentType = "txt"
)

output$summary <- renderTable(
  {
    req(get_data())
    head(get_data())
  }, bordered = T
)

output$summary1 <-  renderTable(
  {
    req(get_data())
    df <- get_data()
    nf <- ifelse("Fraction" %in% colnames(df),n_distinct(df$Fraction),1)
    df1 <- df %>% summarise("Number of Conditions" = n_distinct(Condition),
                            "Number of Biological Replicates" = n_distinct(BioReplicate),
                            "Number_of_Fraction" = nf,
                            "Number of MS runs" = n_distinct(Run)
    )
    df2 <- df %>% group_by(Condition, Run) %>% summarise("Condition_Run" = n()) %>% ungroup() %>%
      select("Condition_Run")
    df3 <- df %>% group_by(Run, BioReplicate) %>% summarise("BioReplicate_Run" = n()) %>% ungroup() %>%
      select("BioReplicate_Run")
    
    df1 <- head(df1,1)
    df2 <- head(df2,1)
    df3 <- head(df3,1)
    
    df <- cbind(df1,df2,df3) %>%
      mutate("Number of Technical Replicates" = Condition_Run/(BioReplicate_Run*Number_of_Fraction) ) %>%
      select(-Condition_Run,-BioReplicate_Run)
    df <- df[,c(1,2,5,3,4)]
 
    
    t_df <- as.data.frame(t(df))
    rownames(t_df) <- colnames(df)
    t_df <- cbind(rownames(t_df), t_df)
    colnames(t_df) <- c("", "value")
    t_df$value <- sub("\\.\\d+$", "", t_df$value)
    colnames(t_df) <- c("", "")
    t_df
  }, colnames = FALSE, bordered = T
)

output$summary2 <-  renderTable(
  {
    req(get_data())
    df <- get_data()
    if(input$DDA_DIA=="TMT"){
      df <- df %>% mutate("FEATURES" = paste(ProteinName, PeptideSequence, Charge, sep = '_'))
    }
    else{
      df <- df %>% mutate("FEATURES" = paste(PeptideSequence, PrecursorCharge, FragmentIon, ProductCharge, sep = '_'))
      
    }
    
    
    
    df1 <- df %>% summarise("Number of Protiens" = n_distinct(ProteinName), 
                            "Number of Peptides" = n_distinct(PeptideSequence),
                            "Number of Features" = n_distinct(FEATURES),
                            "Min_Intensity" = ifelse(!is.finite(min(Intensity, na.rm=T)),0,round(min(Intensity, na.rm=T),0)),
                            "Max_Intensity" = ifelse(!is.finite(max(Intensity, na.rm=T)),0,
                                                     round(max(Intensity, na.rm=T),0))) %>%
      unite("Intensity", Min_Intensity:Max_Intensity, sep = " - ")
    
    Peptides_Proteins <- df %>% group_by(ProteinName)  %>%
      summarise(npep = n_distinct(PeptideSequence)) %>% summarize(Peptides_Proteins_min=min(npep),
                                                                  Peptides_Proteins_max=max(npep))
    
    Features_Peptides <- df %>% group_by(PeptideSequence)  %>%
      summarise(nfea = n_distinct(FEATURES)) %>% summarize(Features_Peptides_min=min(nfea),
                                                           Features_Peptides_max=max(nfea))
    
    df1 <- cbind(df1,Features_Peptides,Peptides_Proteins) %>%
      unite("Number of Features/Peptide",Features_Peptides_min:Features_Peptides_max,sep = " - ") %>%
      unite("Number of Peptides/Protein",Peptides_Proteins_min:Peptides_Proteins_max, sep = " - ")
    
    df1 <- df1[,c(1,2,3,6,5,4)]
    
    t_df <- as.data.frame(t(df1))
    rownames(t_df) <- colnames(df1)
    t_df <- cbind(rownames(t_df), t_df)
    
    colnames(t_df) <- c("", "value")
    t_df$value <- sub("\\.\\d+$", "", t_df$value)
    
    colnames(t_df) <- c("", "")
    
    #t_df <- get_summary2()
    t_df
    
  }, colnames = FALSE, bordered = T, align='lr'
)

shinyjs::enable("proceed1")
shinyjs::enable("reset1")
# observeEvent(get_data(),{
#   shinyjs::enable("proceed1")
# })
# 
# observeEvent(get_data(),{
#   shinyjs::enable("reset1")
# })

onclick("proceed2", {
  updateTabsetPanel(session = session, inputId = "tablist", selected = "DataProcessing")
})


output$summary_tables <- renderUI({
  
  tagList(conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                           tags$br(),
                           tags$h4("Calculation in progress...")),
          tags$head(
            tags$style(HTML('#proceed2{background-color:orange}'))
          ),
          actionButton(inputId = "proceed2", label = "Next step"),
          h4("Summary of experimental design"),
          column(width=12, tableOutput('summary1'), style = "height:200px; overflow-y: scroll;overflow-x: scroll;"),
          tags$br(),
          h4("Summary of dataset"),
          column(width=12, tableOutput("summary2"), style = "height:250px; overflow-y: scroll;overflow-x: scroll;"),
          h4("Top 6 rows of the dataset"),
          column(width=12, tableOutput("summary"), style = "height:250px; overflow-y: scroll;overflow-x: scroll;"))

})

onclick("proceed1", {
  shinyjs::show("summary_tables")
})




