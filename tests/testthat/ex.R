library(testthat)
library(mockery)

test_file_tsv <- tempfile(fileext = ".tsv")
writeLines("a\tb\tcd", test_file_tsv)

mock_input <- list(
  level = "Protein",
  which.proteinid = "Protein.Accessions",
  PTMTMT = "No",
  TMT.keyword = "TMT",
  PTM.keyword = "phos",
  mod.num = "Single",
  evidence = list(
    datapath = NULL
  ),
  evidence2 = list(
    datapath = NULL
  ),
  unmod = list(
    datapath = NULL
  ),
  pGroup = list(
    datapath = NULL
  ),
  pGroup2 = list(
    datapath = NULL
  ),
  maxq_ptm_sites = list(
    datapath = NULL
  ),
  annot3 = list(
    datapath = NULL
  ),
  annot2 = list(
    datapath = NULL
  ),
  annot1 = list(
    datapath = NULL
  ),
  annot = list(
    datapath = NULL
  ),
  data = list(
    datapath = NULL
  ),
  mydata = list(
    datapath = NULL
  ),
  data1 = list(
    datapath = NULL
  ),
  fragSummary = list(
    datapath = NULL
  ),
  peptideSummary = list(
    datapath = NULL
  ),
  protSummary = list(
    datapath = NULL
  ),
  remove = FALSE,
  filetype = NULL,
  DDA_DIA = NULL,
  sep = NULL,
  norm = NULL,
  features_used = "all",
  log = NULL,
  MBi = TRUE,
  null = TRUE,
  null1 = FALSE,
  n_feat = 1,
  censInt = "NA",
  remove50 = FALSE,
  summarization = NULL,
  global_norm = TRUE,
  reference_norm = TRUE,
  remove_norm_channel = TRUE,
  maxQC1 = NULL,
  summ = TRUE,
  sep_skylinedata = NULL,
  moderated = TRUE
  
)


test_that("dda openms", {
  suppressWarnings({
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
    mock_input$filetype = "openms"
    mock_input$sep = ","
    
    mock_input$data$datapath <- system.file("tinytest/raw_data/OpenMS/openms_input.csv",
                                            package = "MSstatsConvert")
    
    output <- getData(mock_input)
    expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})