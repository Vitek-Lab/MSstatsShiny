loadpageServer <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    # toggle ui (DDA DIA SRM)
    observe({
      if (input$DDA_DIA == "DDA") {
        runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
        enable("filetype")
        disable(selector = "[type=radio][value=spec]")
        disable(selector = "[type=radio][value=open]")
        disable(selector = "[type=radio][value=ump]")
        disable(selector = "[type=radio][value=spmin]")
        disable(selector = "[type=radio][value=phil]")
        runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
      }
      else if (input$DDA_DIA == "DIA") {
        runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
        enable("filetype")
        disable(selector = "[type=radio][value=maxq]")
        disable(selector = "[type=radio][value=prog]")
        disable(selector = "[type=radio][value=PD]")
        disable(selector = "[type=radio][value=openms]")
        disable(selector = "[type=radio][value=spmin]")
        disable(selector = "[type=radio][value=phil]")
        runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
      }
      else if (input$DDA_DIA == "SRM_PRM") {
        runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
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
        runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
      }
      
      else if (input$DDA_DIA == "TMT") {
        runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
        enable("filetype")
        disable(selector = "[type=radio][value=sky]")
        disable(selector = "[type=radio][value=prog]")
        disable(selector = "[type=radio][value=spec]")
        disable(selector = "[type=radio][value=open]")
        disable(selector = "[type=radio][value=ump]")
        runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
        
      }
      else if (input$DDA_DIA %in% c("PTM", "PTM_TMT")) {
        runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
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
        runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
      }
    })
    
    observeEvent(input$filetype,{
      enable("proceed1")
    })

    
    get_annot = eventReactive(input$proceed1, {
      getAnnot(input)
    })

    
    get_annot1 = reactive({
      getAnnot1(input)
    })
    
    get_annot2 = reactive({
      getAnnot2(input)
    })
    
    get_annot3 = reactive({
      getAnnot3(input)
    })
    
    get_evidence = reactive({
      getEvidence(input)
    })
    
    get_evidence2 = reactive({
      getEvidence2(input)
    })
    
    get_global = reactive({
      getGlobal(input)
    })
    
    get_proteinGroups = reactive({
      getProteinGroups(input)
    })
    
    get_proteinGroups2 = reactive({
      getProteinGroups2(input)
    })
    
    get_FragSummary = reactive({
      getFragSummary(input)
    })
    
    get_peptideSummary = reactive({
      getPeptideSummary(input)
    })
    
    get_protSummary = reactive({
      getProtSummary(input)
    })
    
    get_maxq_ptm_sites = reactive({
      getMaxqPtmSites(input)
    })
    
    
    get_data = eventReactive(input$proceed1, {
      getData(input)
    })

    
    get_data_code = eventReactive(input$calculate, {
      getDataCode(input)
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
        filename = "templateannotation.csv",
        
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
      # browser()
      onclick("proceed2", {
        updateTabsetPanel(session = parent_session, inputId = "tablist", selected = "DataProcessing")
      })
      output$summary_tables = renderUI({
        ns <- session$ns
        tagList(
          tags$head(
            tags$style(HTML('#loadpage-proceed2{background-color:orange}'))
          ),
          actionButton(inputId = ns("proceed2"), label = "Next step"),
          h4("Summary of experimental design"),
          tableOutput(ns('summary1')),
          tags$br(),
          h4("Summary of dataset"), 
          tableOutput(ns("summary2")),
          tags$br(),
          conditionalPanel(condition = "input.DDA_DIA !== 'PTM'",
                           h4("Top 6 rows of the dataset"),
                           tableOutput(ns("summary"))
          ),
          conditionalPanel(condition = "input.DDA_DIA == 'PTM'",
                           h4("Top 6 rows of the PTM dataset"),
                           tableOutput(ns("summary_ptm")),
                           tags$br(),
                           h4("Top 6 rows of the unmodified protein dataset"),
                           tableOutput(ns("summary_prot"))
          )
        )
      })
      
    })
    return(input)
  })
  
}