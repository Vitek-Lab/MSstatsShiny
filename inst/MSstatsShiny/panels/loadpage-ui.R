
##### sidebar #####

sbp_load = sidebarPanel(
  tags$head(
    tags$style(HTML('#proceed1{background-color:orange}')),
    tags$style(HTML('#reset1{background-color:orange}')),
  ),
  # selection for DIA DDA or SRM/PRM
  
  radioButtons("DDA_DIA",
               label = h4("1. Experimental Design", tipify(icon("question-circle"), 
                          title = "DDA/DIA/SRM/PRM are used for label-free preparation. PTM can either by label-free or TMT.")),
               c("DDA" = "DDA", "DIA" = "DIA", "SRM/PRM" = "SRM_PRM", 
                 "DDA/TMT"="TMT", "PTM"="PTM")),
  
  # upload  
  
  radioButtons("filetype",
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
    condition = "input.DDA_DIA != 'PTM'",
    radioButtons("level",
                 label = h4("3. Analysis Level", tipify(icon("question-circle"), 
                            title = "Choose either peptide or protein level of analysis. MSstats is classically designed for protein level analysis and peptide analysis may produce inconsistent results.")),                 
                 choices = c("Protein" = "Protein", "Peptide" = "Peptide"),
                 selected = "Protein")
  ),
  # conditionalPanel(
  #   condition = "input.DDA_DIA != 'PTM'",
  #   radioButtons("subset",
  #                label = h4("4. Subset data", 
  #                           tipify(icon("question-circle"), 
  #                                  title = "Whether to use the full dataset or randomly sample a portion. Useful for generating analysis code.")),
  #                choices = c("True" = TRUE, "False" = FALSE),
  #                selected = FALSE)
  # ),
  tags$hr(),
  conditionalPanel(
    condition = "input.filetype == 'sample' && input.DDA_DIA == 'PTM'",
    h4("3. TMT Experiment"),
    radioButtons("PTMTMT", tipify(icon("question-circle"),
                                  title = "Indicate whether to use a label free or TMT sample experiment"),
                 c(No='No', Yes='Yes'),
                 inline=TRUE)
  ),
  conditionalPanel(
  condition = "input.filetype =='10col' || input.filetype =='prog' || input.filetype =='PD' || input.filetype =='open'||
                   input.filetype =='openms' || input.filetype =='spmin'",
                   h4("4. Upload quantification dataset")),
  conditionalPanel(condition = "input.filetype == 'msstats' && input.DDA_DIA != 'PTM' && input.DDA_DIA != 'PTM_TMT'",
                   h4("4. Upload data in MSstats Format")),
  conditionalPanel(condition = "input.filetype == 'msstats' && (input.DDA_DIA == 'PTM' || input.DDA_DIA == 'PTM_TMT')",
                   h4("4. Upload PTM data in MSstats Format")),
  conditionalPanel(condition = "input.filetype == 'sky'",
                   h4("4. Upload MSstats report from Skyline")),
  conditionalPanel(condition = "input.filetype == 'spec'",
                   h4("4. Upload MSstats scheme output from Spectronaut")),
  conditionalPanel(
    condition = "input.filetype && input.filetype != 'maxq' && input.filetype != 'sample' && input.filetype != 'ump' && input.filetype != 'MRF' && input.filetype != 'spec' && input.filetype != 'spmin' && input.filetype != 'phil'",
    fileInput('data', "", multiple = F, 
              accept = c("text/csv", 
                         "text/comma-separated-values,text/plain", 
                         ".csv")),
    radioButtons("sep",
                 label = h5("Column separator in uploaded file", 
                            tipify(icon("question-circle"), 
                                   title = "Choose how columns are separated in the uploaded file")),
                 c(Comma=",",Semicolon=";", Tab="\t",Pipe="|"), 
                 inline = T)),
  conditionalPanel(
    condition = "input.filetype && input.filetype == 'phil'",
    fileInput("folder", "Upload a zip file", accept = ".zip")
  ),
  
  conditionalPanel(condition = "input.filetype && (input.filetype == 'spec' || input.filetype =='spmin')",
                   fileInput('data1', "", multiple = FALSE, accept = c(".xls")),
  ),
  tags$br(),
  conditionalPanel(
    condition = "input.filetype == 'sky' || input.filetype == 'prog' || input.filetype == 'PD' || input.filetype == 'spec' || input.filetype == 'open'|| input.filetype =='spmin' || input.filetype == 'phil'",
    h4("5. Upload annotation File", tipify(icon("question-circle"), 
        title = "Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.")),
    # downloadLink("template", "Annotation file template"),
    fileInput('annot', "", multiple = F,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain", ".csv"))
  ),
  tags$br(),
  conditionalPanel(condition = "input.filetype == 'msstats' && (input.DDA_DIA == 'PTM' || input.DDA_DIA == 'PTM_TMT')",
                   h4("4. (Optional) Upload unmodified data in MSstats Format"), 
                   fileInput('unmod', "", multiple = FALSE, 
                             accept = c("text/csv", 
                                        "text/comma-separated-values,text/plain", 
                                        ".csv")),
                   tags$br(),
                   h4("5. TMT Experiment"),
                   radioButtons("PTMTMT", tipify(icon("question-circle"),
                                                 title = "Indicate if experiment was processed using TMT labeling"),
                                c(No='No', Yes='Yes'),
                                inline=TRUE)),
  conditionalPanel(
    condition = "input.filetype == 'maxq' && (input.DDA_DIA != 'PTM' && input.DDA_DIA != 'PTM_TMT')",
    h4("5. Upload evidence.txt File"),
    fileInput('evidence', "", multiple = FALSE, 
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    h4("6. Upload proteinGroups.txt File"),
    fileInput('pGroup', "", multiple = FALSE, 
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    h4("7. Upload annotation File", tipify(icon("question-circle"), 
                                           title = "Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.")),
    fileInput('annot1', "", multiple = FALSE, 
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
  ),
  conditionalPanel(
    condition = "input.filetype == 'maxq' && (input.DDA_DIA == 'PTM' || input.DDA_DIA == 'PTM_TMT')",
    h4("4. Upload PTM sites Sites.txt File"),
    fileInput('maxq_ptm_sites', "", multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    h4("5. Upload annotation File", tipify(icon("question-circle"), 
                                           title = "Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.")),
    fileInput('annot3', "", multiple = FALSE, 
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    h4("6. (Optional) Upload Unmodified Protein evidence.txt File"),
    fileInput('evidence2', "", multiple = FALSE, 
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    h4("7. (Optional) Upload Unmodified Protein proteinGroups.txt File"),
    fileInput('pGroup2', "", multiple = FALSE, 
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    h4("8. TMT Experiment"),
    radioButtons("PTMTMT", tipify(icon("question-circle"),
                                  title = "Indicate if experiment was processed using TMT labeling"),
                 c(No='No', Yes='Yes'),
                 selected='Yes', inline=TRUE)
  ),
  conditionalPanel(
    condition = "input.filetype == 'ump'",
    h4("5. Upload FragSummary.xls File"),
    fileInput('fragSummary', "", multiple = FALSE,
              accept = c("xls", "text/comma-separated-values,text/plain", ".xls")),
    h4("6. Upload PeptideSummary.xls File"),
    fileInput('peptideSummary', "", multiple = FALSE, 
              accept = c("xls", "text/comma-separated-values,text/plain", ".xls")),
    h4("7. Upload ProtSummary.xls File"),
    fileInput('protSummary', "", multiple = FALSE, 
              accept = c("xls", "text/comma-separated-values,text/plain", ".xls")),
    h4("8. Upload Annotation File", tipify(icon("question-circle"), 
                                           title = "Upload manually created annotation file. This file maps MS runs to experiment metadata (i.e. conditions, bioreplicates). Please see Help tab for information on creating this file.")),
    fileInput('annot2', "", multiple = FALSE, 
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
  ),
  tags$hr(),
  conditionalPanel(condition = "input.filetype && input.DDA_DIA == 'TMT' && input.filetype == 'PD'",
                   h4("Select the options for pre-processing"),
                   textInput("which.proteinid", h5("Protein Name Column", 
                                                   tipify(icon("question-circle"), 
                            title = "Enter the column in your data containing protein names")), 
                            value = "Protein.Accessions")),
  conditionalPanel(condition = "input.filetype && input.DDA_DIA == 'TMT' && input.filetype == 'maxq'",
                   h4("Select the options for pre-processing"),
                   textInput("which.proteinid", h5("Protein Name Column", 
                                                   tipify(icon("question-circle"), 
                            title = "Enter the column in your data containing protein names")), 
                            value = "Proteins")),
    conditionalPanel(condition = "input.filetype && input.DDA_DIA == 'TMT' && input.filetype == 'phil'",
                   h4("Select the options for pre-processing"),
                   textInput("which.proteinid", h5("Protein Name Column", 
                                                   tipify(icon("question-circle"), 
                            title = "Enter the column in your data containing protein names")), 
                            value = "ProteinAccessions")),
  
  conditionalPanel(condition = "input.filetype == 'maxq' && (input.DDA_DIA == 'PTM' || input.DDA_DIA == 'PTM_TMT')",
                   h4("Select the options for pre-processing"),
                   textInput("which.proteinid", h5("Protein Name Column", 
                                                   tipify(icon("question-circle"), 
                                                          title = "Enter the column in your data containing protein names")), 
                             value = "Proteins"),
                   radioButtons("mod.num", h5("Modification Number", tipify(icon("question-circle"), 
                                                          title = "Use single or multiple modifications per peptide")),
                                              c(Single="Single", Total="Total"),
                                              inline=TRUE),
                   textInput("TMT.keyword", h5("TMT.keyword", 
                                                   tipify(icon("question-circle"), 
                                                          title = "The sub-name of columns in sites.data file")), 
                             value = "TMT"),
                   textInput("PTM.keyword", h5("PTM.keyword", 
                                               tipify(icon("question-circle"), 
                                                      title = "he sub-name of columns in sites.data file")), 
                             value = "phos"),
                   ),
  
  conditionalPanel(condition = "input.filetype && input.DDA_DIA == 'DDA' && input.filetype !== 'sample' && input.filetype !== 'MRF'",
                   h4("Select the options for pre-processing"),
                   checkboxInput("uniqe_peptides", "Use unique peptides", value = TRUE),
                   checkboxInput("remove", "Remove proteins with 1 peptide and charge", value = FALSE)),
  
  conditionalPanel(condition = "input.filetype && input.DDA_DIA == 'DIA' && input.filetype !== 'sample'",
                   checkboxInput("remove", "Remove proteins with 1 feature", value = FALSE),
                   conditionalPanel(condition = "input.filetype == 'sky' || input.filetype == 'spec'",
                                    checkboxInput("q_val", "Filter with Q-value"),
                                    conditionalPanel(condition = "input.q_val",
                                                     numericInput("q_cutoff", "Q-value cutoff", 0.01, 0, 1, 0.01))),
                   conditionalPanel(condition = "input.filetype == 'open'",
                                    checkboxInput("m_score", "Filter with M-score"),
                                    conditionalPanel(condition = "input.m_score",
                                                     numericInput("m_cutoff", "M-score cutoff", 0.01, 0, 1, 0.01)))
  ),
  disabled(actionButton(inputId = "proceed1", label = "Upload Data"))
)

##########################################
loadpage = fluidPage(
  
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
  p("**Note all files must in csv format (unless otherwise indicated) and under 250 MB if using msstatsshiny.com (there is no limit when running the application locally)**"),
  tags$br(),
  conditionalPanel(
    condition = "input.filetype == 'sample' && input.DDA_DIA == 'DDA'",
    p("The sample dataset for DDA acquisition is on its way ")
  ),
  conditionalPanel(
    condition = "input.filetype == 'sample' && input.DDA_DIA == 'DIA'",
    p("The sample dataset for DIA acquisition is taken from the publication ",
    a("Selevsek, N. et al. Reproducible and Consistent Quantification of the Saccharomyces Cerevisiae Proteome by SWATH-Mass Spectrometry. Molecular & Cellular Proteomics : MCP 14.3 (2015): 739–749. ", href = "http://www.mcponline.org/content/14/3/739.long", target="_blank"))
  ),
  conditionalPanel(
    condition = "input.filetype == 'sample' && input.DDA_DIA == 'SRM_PRM'",
    p("The sample dataset for SRM/PRM acquisition is taken from the publication ",
    a("Picotti, P. et al. Full dynamic range proteome analysis of S. cerevisiae by targeted proteomics. Cell (2009), 138, 795–806.", href = "http://www.cell.com/cell/fulltext/S0092-8674(09)00715-6", target="_blank"))
  ),
  tags$br(),
  sbp_load,
  column(width = 8,
         shinyjs::hidden(uiOutput("summary_tables"))
         
  )
)
