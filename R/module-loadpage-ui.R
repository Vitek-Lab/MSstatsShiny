loadpageUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      useShinyjs(),
      headerPanel(list("Upload data")),
      p("To run the MSstats Pipeline, please upload your dataset. The required files\
    depend on the spectral processing tool used. Generally the raw data and an \
    annotation file are needed. The output of this step is your experimental \
    data processed in MSstats format. For examples on how to prepare your input \
    please review the MSstats ", a("User Guide", href="https://msstats.org/wp-content/uploads/2020/02/MSstats_v3.18.1_manual_2020Feb26-v2.pdf",
                                   target="_blank")),
      p("PTM data must be processed using MaxQuant or preformatted into MSstats format. For information on how \
  to format your data please see the MSstatsPTM ", 
        a("documentation", href="https://www.bioconductor.org/packages/release/bioc/vignettes/MSstatsPTM/inst/doc/MSstatsPTM_LabelFree_Workflow.html",
          target="_blank")),
      p("Note all files must in csv format (unless otherwise indicated) and under 250 MB if using msstatsshiny.com (there is no limit when running the application locally)."),
      p("Some users may have trouble uploading files while using the application via Google Chrome. If the 'Browse...' button does not work please try a different web browser."),
      tags$br(),
      conditionalPanel(
        condition = "input['loadpage-filetype'] == 'sample' && input['loadpage-DDA_DIA'] == 'DDA'",
        p("The sample dataset for DDA acquisition is on its way ")
      ),
      conditionalPanel(
        condition = "input['loadpage-filetype'] && input['loadpage-DDA_DIA'] == 'DIA'",
        p("The sample dataset for DIA acquisition is taken from the publication ",
          a("Selevsek, N. et al. Reproducible and Consistent Quantification of the Saccharomyces Cerevisiae Proteome by SWATH-Mass Spectrometry. Molecular & Cellular Proteomics : MCP 14.3 (2015): 739–749. ", href = "http://www.mcponline.org/content/14/3/739.long", target="_blank"))
      ),
      conditionalPanel(
        condition = "input['loadpage-filetype'] == 'sample' && input['loadpage-DDA_DIA'] == 'SRM_PRM'",
        p("The sample dataset for SRM/PRM acquisition is taken from the publication ",
          a("Picotti, P. et al. Full dynamic range proteome analysis of S. cerevisiae by targeted proteomics. Cell (2009), 138, 795–806.", href = "http://www.cell.com/cell/fulltext/S0092-8674(09)00715-6", target="_blank"))
      ),
      tags$br(),
      sidebarPanel(
        tags$head(
          tags$style(HTML('#loadpage-proceed1{background-color:orange}')),
          tags$style(HTML('#loadpage-reset1{background-color:orange}')),
        ),
        # selection for DIA DDA or SRM/PRM
        
        radioButtons(ns("DDA_DIA"),
                     label = h4("1. Experimental Design", tipify(icon("question-circle"), 
                                                                 title = "DDA/DIA/SRM/PRM are used for label-free preparation. PTM can either by label-free or TMT.")),
                     c("DDA" = "DDA", "DIA" = "DIA", "SRM/PRM" = "SRM_PRM", 
                       "DDA/TMT"="TMT", "PTM"="PTM")),
        
        # upload  
        
        radioButtons(ns("filetype"),
                     label = h4("2. Type of File", tipify(icon("question-circle"), 
                                                          title = "Choose the spectral processing tool used to process your data")),
                     choices = c("Example dataset" = "sample", 
                                 "MSstats Format" = "msstats",
                                 "Skyline" = "sky", "MaxQuant" = "maxq", 
                                 "Progenesis" = "prog", "Proteome Discoverer" = "PD", 
                                 "OpenMS" = "openms", "Spectronaut" = "spec", 
                                 "OpenSWATH" = "open", "DIA-Umpire" = "ump", 
                                 "SpectroMine" = "spmin", "Philosopher" = "phil"), 
                     selected = character(0)),
        conditionalPanel(
          condition = "input['loadpage-DDA_DIA'] != 'PTM'",
          radioButtons(ns("level"),
                       label = h4("3. Analysis Level", tipify(icon("question-circle"), 
                                                              title = "Choose either peptide or protein level of analysis. MSstats is classically designed for protein level analysis and peptide analysis may produce inconsistent results.")),                 
                       choices = c("Protein" = "Protein", "Peptide" = "Peptide"),
                       selected = "Protein")
        ),
        # conditionalPanel(
        #   condition = "input['loadpage-DDA_DIA'] != 'PTM'",
        #   radioButtons(ns("subset"),
        #                label = h4("4. Subset data", 
        #                           tipify(icon("question-circle"), 
        #                                  title = "Whether to use the full dataset or randomly sample a portion. Useful for generating analysis code.")),
        #                choices = c("True" = TRUE, "False" = FALSE),
        #                selected = FALSE)
        # ),
        tags$hr(),
        conditionalPanel(
          condition = "input['loadpage-filetype'] == 'sample' && input['loadpage-DDA_DIA'] == 'PTM' && input['loadpage-filetype'] != 'phil'",
          h4("3. TMT Experiment"),
          radioButtons(ns("PTMTMT"), tipify(icon("question-circle"),
                                        title = "Indicate whether to use a label free or TMT sample experiment"),
                       c(No='No', Yes='Yes'),
                       inline=TRUE)
        ),
        conditionalPanel(
          condition = "input['loadpage-filetype'] =='10col' || input['loadpage-filetype'] =='prog' || input['loadpage-filetype'] =='PD' || input['loadpage-filetype'] =='open'||
                   input['loadpage-filetype'] =='openms' || input['loadpage-filetype'] =='spmin' || input['loadpage-filetype'] == 'phil' && input['loadpage-DDA_DIA'] != 'PTM'",
          h4("4. Upload quantification dataset")),
        conditionalPanel(condition = "input['loadpage-filetype'] == 'msstats' && input['loadpage-DDA_DIA'] != 'PTM' && input['loadpage-DDA_DIA'] != 'PTM_TMT'",
                         h4("4. Upload data in MSstats Format")),
        conditionalPanel(condition = "input['loadpage-filetype'] == 'msstats' && (input['loadpage-DDA_DIA'] == 'PTM' || input['loadpage-DDA_DIA'] == 'PTM_TMT')",
                         h4("4. Upload PTM data in MSstats Format")),
        conditionalPanel(condition = "input['loadpage-filetype'] == 'sky'",
                         h4("4. Upload MSstats report from Skyline")),
        conditionalPanel(condition = "input['loadpage-filetype'] == 'spec'",
                         h4("4. Upload MSstats scheme output from Spectronaut")),
        conditionalPanel(
          condition = "input['loadpage-filetype'] && input['loadpage-filetype'] != 'maxq' && input['loadpage-filetype'] != 'sample' && input['loadpage-filetype'] != 'ump' && input['loadpage-filetype'] != 'MRF' && input['loadpage-filetype'] != 'spec' && input['loadpage-filetype'] != 'spmin' && input['loadpage-DDA_DIA'] != 'PTM'",# && input['loadpage-filetype'] != 'phil'
          fileInput(ns('data'), "", multiple = F, 
                    accept = c("text/csv", 
                               "text/comma-separated-values,text/plain", 
                               ".csv")),
          radioButtons(ns("sep"),
                       label = h5("Column separator in uploaded file", 
                                  tipify(icon("question-circle"), 
                                         title = "Choose how columns are separated in the uploaded file")),
                       c(Comma=",",Semicolon=";", Tab="\t",Pipe="|"), 
                       inline = T)),
        # conditionalPanel(
        #   condition = "input['loadpage-filetype'] && input['loadpage-filetype'] == 'phil'",
        #   fileInput(ns("folder"), "Upload a zip file", accept = ".zip")
        # ),
        
        conditionalPanel(
          condition = "input['loadpage-filetype'] == 'phil' && input['loadpage-DDA_DIA'] == 'PTM'",
          h4("3. Upload PTM msstats dataset"),
          fileInput(ns('ptmdata'), "", multiple = F, 
                    accept = c("text/csv", 
                               "text/comma-separated-values,text/plain", 
                               ".csv")),
          h4("4. Upload PTM annotation file"),
          fileInput(ns('annotation'), "", multiple = F, 
                    accept = c("text/csv", 
                               "text/comma-separated-values,text/plain", 
                               ".csv")),
          h4("5. Upload global profling msstats dataset (optional)"),
          fileInput(ns('globaldata'), "", multiple = F, 
                    accept = c("text/csv", 
                               "text/comma-separated-values,text/plain", 
                               ".csv")),
          h4("6. Upload global profling annotation file (optional)"),
          fileInput(ns('globalannotation'), "", multiple = F, 
                    accept = c("text/csv", 
                               "text/comma-separated-values,text/plain", 
                               ".csv")),
          h4("Select the options for pre-processing"),
          textInput(ns("mod_id_col"), h5("Please enter the name of the modification id column", 
                                     tipify(icon("question-circle"), 
                                            title = "Only part of the string is required. For example if your mod id column is named 'STY:1221.12' you only need to enter 'STY' here.")), 
                    value = "STY"),
          textInput(ns("localization_cutoff"), h5("Please enter the localization_cutoff", 
                                              tipify(icon("question-circle"), 
                                                     title = "The probability cutoff used to determine if a modification should be marked or not. If a site cannot be localized it may be dropped depending on the option below.")), 
                    value = ".75"),
          radioButtons(ns("remove_unlocalized_peptides"), h5("Remove unlocalized peptides", 
                                                         tipify(icon("question-circle"), 
                                                                title = "Should peptides without all sites localized be kept or removed.")), 
                       c(Yes=TRUE, No=FALSE),
                       inline=TRUE)
        ),
        
        
        conditionalPanel(condition = "input['loadpage-filetype'] && (input['loadpage-filetype'] == 'spec' || input['loadpage-filetype'] =='spmin')",
                         fileInput(ns('data1'), "", multiple = FALSE, accept = c(".xls")),
        ),
        tags$br(),
        conditionalPanel(
          condition = "input['loadpage-filetype'] == 'sky' || input['loadpage-filetype'] == 'prog' || input['loadpage-filetype'] == 'PD' || input['loadpage-filetype'] == 'spec' || input['loadpage-filetype'] == 'open'|| input['loadpage-filetype'] =='spmin' || input['loadpage-filetype'] == 'phil' && input['loadpage-DDA_DIA'] != 'PTM'",
          h4("5. Upload annotation File", tipify(icon("question-circle"), 
                                                 title = "Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.")),
           #downloadLink("template", "Annotation file template"),
          fileInput(ns('annot'), "", multiple = F,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain", ".csv"))
        ),
        tags$br(),
        conditionalPanel(condition = "input['loadpage-filetype'] == 'msstats' && (input['loadpage-DDA_DIA'] == 'PTM' || input['loadpage-DDA_DIA'] == 'PTM_TMT')",
                         h4("4. (Optional) Upload unmodified data in MSstats Format"), 
                         fileInput(ns('unmod'), "", multiple = FALSE, 
                                   accept = c("text/csv", 
                                              "text/comma-separated-values,text/plain", 
                                              ".csv")),
                         tags$br(),
                         h4("5. TMT Experiment"),
                         radioButtons(ns("PTMTMT"), tipify(icon("question-circle"),
                                                       title = "Indicate if experiment was processed using TMT labeling"),
                                      c(No='No', Yes='Yes'),
                                      inline=TRUE)),
        conditionalPanel(
          condition = "input['loadpage-filetype'] == 'maxq' && (input['loadpage-DDA_DIA'] != 'PTM' && input['loadpage-DDA_DIA'] != 'PTM_TMT')",
          h4("5. Upload evidence.txt File"),
          fileInput(ns('evidence'), "", multiple = FALSE, 
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          h4("6. Upload proteinGroups.txt File"),
          fileInput(ns('pGroup'), "", multiple = FALSE, 
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          h4("7. Upload annotation File", tipify(icon("question-circle"), 
                                                 title = "Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.")),
          fileInput(ns('annot1'), "", multiple = FALSE, 
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
        ),
        conditionalPanel(
          condition = "input['loadpage-filetype'] == 'maxq' && (input['loadpage-DDA_DIA'] == 'PTM' || input['loadpage-DDA_DIA'] == 'PTM_TMT')",
          h4("4. Upload PTM sites Sites.txt File"),
          fileInput(ns('maxq_ptm_sites'), "", multiple = FALSE,
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          h4("5. Upload annotation File", tipify(icon("question-circle"), 
                                                 title = "Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.")),
          fileInput(ns('annot3'), "", multiple = FALSE, 
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          h4("6. (Optional) Upload Unmodified Protein evidence.txt File"),
          fileInput(ns('evidence2'), "", multiple = FALSE, 
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          h4("7. (Optional) Upload Unmodified Protein proteinGroups.txt File"),
          fileInput(ns('pGroup2'), "", multiple = FALSE, 
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          h4("8. TMT Experiment"),
          radioButtons(ns("PTMTMT"), tipify(icon("question-circle"),
                                        title = "Indicate if experiment was processed using TMT labeling"),
                       c(No='No', Yes='Yes'),
                       selected='Yes', inline=TRUE)
        ),
        conditionalPanel(
          condition = "input['loadpage-filetype'] == 'ump'",
          h4("5. Upload FragSummary.xls File"),
          fileInput(ns('fragSummary'), "", multiple = FALSE,
                    accept = c("xls", "text/comma-separated-values,text/plain", ".xls")),
          h4("6. Upload PeptideSummary.xls File"),
          fileInput(ns('peptideSummary'), "", multiple = FALSE, 
                    accept = c("xls", "text/comma-separated-values,text/plain", ".xls")),
          h4("7. Upload ProtSummary.xls File"),
          fileInput(ns('protSummary'), "", multiple = FALSE, 
                    accept = c("xls", "text/comma-separated-values,text/plain", ".xls")),
          h4("8. Upload Annotation File", tipify(icon("question-circle"), 
                                                 title = "Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.")),
          fileInput(ns('annot2'), "", multiple = FALSE, 
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
        ),
        tags$hr(),
        conditionalPanel(condition = "input['loadpage-filetype'] && input['loadpage-DDA_DIA'] == 'TMT' && input['loadpage-filetype'] == 'PD'",
                         h4("Select the options for pre-processing"),
                         textInput(ns("which.proteinid"), h5("Protein Name Column", 
                                                         tipify(icon("question-circle"), 
                                                                title = "Enter the column in your data containing protein names")), 
                                   value = "Protein.Accessions")),
        conditionalPanel(condition = "input['loadpage-filetype'] && input['loadpage-DDA_DIA'] == 'TMT' && input['loadpage-filetype'] == 'maxq'",
                         h4("Select the options for pre-processing"),
                         textInput(ns("which.proteinid"), h5("Protein Name Column", 
                                                         tipify(icon("question-circle"), 
                                                                title = "Enter the column in your data containing protein names")), 
                                   value = "Proteins")),
        # conditionalPanel(condition = "input['loadpage-filetype'] && input['loadpage-DDA_DIA'] == 'TMT' && input['loadpage-filetype'] == 'phil'",
        #                  h4("Select the options for pre-processing"),
        #                  textInput(ns("which.proteinid"), h5("Protein Name Column", 
        #                                                  tipify(icon("question-circle"), 
        #                                                         title = "Enter the column in your data containing protein names")), 
        #                            value = "ProteinAccessions")),
        
        conditionalPanel(condition = "input['loadpage-filetype'] == 'maxq' && (input['loadpage-DDA_DIA'] == 'PTM' || input['loadpage-DDA_DIA'] == 'PTM_TMT')",
                         h4("Select the options for pre-processing"),
                         textInput(ns("which.proteinid"), h5("Protein Name Column", 
                                                         tipify(icon("question-circle"), 
                                                                title = "Enter the column in your data containing protein names")), 
                                   value = "Proteins"),
                         radioButtons(ns("mod.num"), h5("Modification Number", tipify(icon("question-circle"), 
                                                                                  title = "Use single or multiple modifications per peptide")),
                                      c(Single="Single", Total="Total"),
                                      inline=TRUE),
                         textInput(ns("TMT.keyword"), h5("TMT.keyword", 
                                                     tipify(icon("question-circle"), 
                                                            title = "The sub-name of columns in sites.data file")), 
                                   value = "TMT"),
                         textInput(ns("PTM.keyword"), h5("PTM.keyword", 
                                                     tipify(icon("question-circle"), 
                                                            title = "he sub-name of columns in sites.data file")), 
                                   value = "phos"),
        ),
        
        conditionalPanel(condition = "input['loadpage-filetype'] && input['loadpage-DDA_DIA'] == 'DDA' && input['loadpage-filetype'] !== 'sample' && input['loadpage-filetype'] !== 'MRF'",
                         h4("Select the options for pre-processing"),
                         checkboxInput(ns("uniqe_peptides"), "Use unique peptides", value = TRUE),
                         checkboxInput(ns("remove"), "Remove proteins with 1 peptide and charge", value = FALSE)),
        
        conditionalPanel(condition = "input['loadpage-filetype'] && input['loadpage-DDA_DIA'] == 'DIA' && input['loadpage-filetype'] !== 'sample'",
                         checkboxInput(ns("remove"), "Remove proteins with 1 feature", value = FALSE),
                         conditionalPanel(condition = "input['loadpage-filetype'] == 'sky' || input['loadpage-filetype'] == 'spec'",
                                          checkboxInput(ns("q_val"), "Filter with Q-value"),
                                          conditionalPanel(condition = "input['loadpage-q_val']",
                                                           numericInput(ns("q_cutoff"), "Q-value cutoff", 0.01, 0, 1, 0.01))),
                         conditionalPanel(condition = "input['loadpage-filetype'] == 'open'",
                                          checkboxInput(ns("m_score"), "Filter with M-score"),
                                          conditionalPanel(condition = "input['loadpage-m_score']",
                                                           numericInput(ns("m_cutoff"), "M-score cutoff", 0.01, 0, 1, 0.01)))
        ),
        disabled(actionButton(inputId = ns("proceed1"), label = "Upload Data"))
      )
      ,
      column(width = 8,
             shinyjs::hidden(uiOutput(ns("summary_tables")))
         
      )
    )
  )
  
}

