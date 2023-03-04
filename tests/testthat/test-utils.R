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
  pGroup = list(
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
  remove = FALSE,
  filetype = NULL,
  DDA_DIA = NULL,
  sep = NULL
)

# Define a test for the getEvidence function
test_that("getEvidence returns a data frame when given valid input", {
  mock_input$evidence$datapath = test_file_tsv
  evidence <- getEvidence(mock_input)
  print(evidence)
  expect_s3_class(evidence, "data.frame")
})

test_that("getEvidence returns NULL when given a null input", {
  mock_input$evidence <- NULL
  evidence <- getEvidence(mock_input)
  expect_equal(evidence, NULL)
})

test_that("getEvidence returns an error message when given invalid input", {
  mock_input$evidence$datapath <- '/path/to'
  evidence <- getEvidence(mock_input)
  expect_equal(evidence, "File load error. Please ensure file is in csv format.")
})
################################################################################
stub(getData,"show_modal_spinner",{})
stub(getData,"remove_modal_spinner",{})

test_that("Empty file type returns NULL", {
  mock_input$filetype = NULL
  output <- getData(mock_input)
  expect_equal(output, NULL)
})

test_that("sample file type returns expected value", {
  mock_input$filetype = "sample"
  ips_vec <- c("SRM_PRM", "DIA", "DDA","TMT")
  for (ddadia in ips_vec) {
    mock_input$DDA_DIA <- ddadia
    output <- getData(mock_input)
    expect_is(output, "data.frame")
  }
  
  mock_input$DDA_DIA <- "PTM"
  output <- getData(mock_input)
  print(typeof(output))
  expect_type(output,"list")
  
  mock_input$PTMTMT <- "Yes"
  output <- getData(mock_input)
  print(typeof(output))
  expect_type(output,"list")
})

# data testing 
test_that("dda maxquant", {
  suppressWarnings({
    mock_input$DDA_DIA <- "DDA"
    mock_input$filetype = "maxq"
    
    mock_input$evidence$datapath <- '../data/DDA-Controlledmix-MaxQuant/ControlMixture_DDA_MaxQuant_evidence.txt'
    mock_input$pGroup$datapath <- '../data/DDA-Controlledmix-MaxQuant/ControlMixture_DDA_MaxQuant_proteinGroups.txt'
    mock_input$annot1$datapath <- '../data/DDA-Controlledmix-MaxQuant/ControlMixture_DDA_MaxQuant_annotation.csv'
    
    output <- getData(mock_input)
    expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

test_that("dda pd", {
  suppressWarnings({
    mock_input$DDA_DIA <- "DDA"
    mock_input$filetype = "PD"
    mock_input$sep = ","
    
    mock_input$data$datapath <- '../data/DDA-Controlledmix-PD/ControlMixture_DDA_ProteomeDiscoverer_input.csv'
    mock_input$annot$datapath <- '../data/DDA-Controlledmix-PD/ControlMixture_DDA_ProteomeDiscoverer_annotation.csv'
    
    output <- getData(mock_input)
    print(names(output))
    expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

test_that("dda prog", {
  suppressWarnings({
    mock_input$DDA_DIA <- "DDA"
    mock_input$filetype = "prog"
    mock_input$sep = ","
    
    mock_input$data$datapath <- '../data/DDA-Controlledmix-Progenesis/ControlMixture_DDA_Progenesis_input.csv'
    mock_input$annot$datapath <- '../data/DDA-Controlledmix-Progenesis/ControlMixture_DDA_Progenesis_annotation.csv'
    
    output <- getData(mock_input)
    print(names(output))
    expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})
