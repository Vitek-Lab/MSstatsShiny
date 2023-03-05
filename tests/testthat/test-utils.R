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

# test_that("Empty file type returns NULL", {
#   mock_input$filetype = NULL
#   output <- getData(mock_input)
#   expect_equal(output, NULL)
# })
# 
# test_that("sample file type returns expected value", {
#   mock_input$filetype = "sample"
#   ips_vec <- c("SRM_PRM", "DIA", "DDA","TMT")
#   for (ddadia in ips_vec) {
#     mock_input$DDA_DIA <- ddadia
#     output <- getData(mock_input)
#     expect_is(output, "data.frame")
#   }
#   
#   mock_input$DDA_DIA <- "PTM"
#   output <- getData(mock_input)
#   print(typeof(output))
#   expect_type(output,"list")
#   
#   mock_input$PTMTMT <- "Yes"
#   output <- getData(mock_input)
#   print(typeof(output))
#   expect_type(output,"list")
# })
# 
# # data testing 
# test_that("dda maxquant", {
#   suppressWarnings({
#     mock_input$DDA_DIA <- "DDA"
#     mock_input$filetype = "maxq"
#     
#     mock_input$evidence$datapath <- '../data/DDA-Controlledmix-MaxQuant/ControlMixture_DDA_MaxQuant_evidence.txt'
#     mock_input$pGroup$datapath <- '../data/DDA-Controlledmix-MaxQuant/ControlMixture_DDA_MaxQuant_proteinGroups.txt'
#     mock_input$annot1$datapath <- '../data/DDA-Controlledmix-MaxQuant/ControlMixture_DDA_MaxQuant_annotation.csv'
#     
#     output <- getData(mock_input)
#     expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
#     expect_type(output,"list")
#     expect_identical(names(output), expected_names)
#   })
# })
# 
# test_that("dda pd", {
#   suppressWarnings({
#     mock_input$DDA_DIA <- "DDA"
#     mock_input$filetype = "PD"
#     mock_input$sep = ","
#     
#     mock_input$data$datapath <- '../data/DDA-Controlledmix-PD/ControlMixture_DDA_ProteomeDiscoverer_input.csv'
#     mock_input$annot$datapath <- '../data/DDA-Controlledmix-PD/ControlMixture_DDA_ProteomeDiscoverer_annotation.csv'
#     
#     output <- getData(mock_input)
#     expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
#     expect_type(output,"list")
#     expect_identical(names(output), expected_names)
#   })
# })
# 
# test_that("dda prog", {
#   suppressWarnings({
#     mock_input$DDA_DIA <- "DDA"
#     mock_input$filetype = "prog"
#     mock_input$sep = ","
#     
#     mock_input$data$datapath <- '../data/DDA-Controlledmix-Progenesis/ControlMixture_DDA_Progenesis_input.csv'
#     mock_input$annot$datapath <- '../data/DDA-Controlledmix-Progenesis/ControlMixture_DDA_Progenesis_annotation.csv'
#     
#     output <- getData(mock_input)
#     expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
#     expect_type(output,"list")
#     expect_identical(names(output), expected_names)
#   })
# })
# 
# test_that("dda skyline", {
#   suppressWarnings({
#     mock_input$DDA_DIA <- "DDA"
#     mock_input$filetype = "sky"
#     mock_input$sep = ","
#     
#     mock_input$data$datapath <- '../data/DDA-Controlledmix-Skyline/ControlMixture_DDA_Skyline_input.csv'
#     mock_input$annot$datapath <- '../data/DDA-Controlledmix-Skyline/ControlMixture_DDA_Skyline_annotation.csv'
#     
#     output <- getData(mock_input)
#     expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
#     expect_type(output,"list")
#     expect_identical(names(output), expected_names)
#   })
# })
# 
# test_that("dda openms", {
#   suppressWarnings({
#     mock_input$DDA_DIA <- "DDA"
#     mock_input$filetype = "openms"
#     mock_input$sep = ","
#     
#     mock_input$data$datapath <- '../data/DDA-iPRG-OpenMS/ABRF2015_OpenMS_raw.csv'
# 
#     output <- getData(mock_input)
#     expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
#     expect_type(output,"list")
#     expect_identical(names(output), expected_names)
#   })
# })
# 
# test_that("dia diaumpire", {
#   suppressWarnings({
#     mock_input$DDA_DIA <- "DIA"
#     mock_input$filetype = "ump"
#     
#     mock_input$fragSummary$datapath <- '../data/DIA-Navarro2016-DIAUmpire/Navarro2016_DIA_DIAumpire_input_FragSummary.xls'
#     mock_input$peptideSummary$datapath <- '../data/DIA-Navarro2016-DIAUmpire/Navarro2016_DIA_DIAumpire_input_PeptideSummary.xls'
#     mock_input$protSummary$datapath <- '../data/DIA-Navarro2016-DIAUmpire/Navarro2016_DIA_DIAumpire_input_ProtSummary.xls'
#     
#     mock_input$annot2$datapath <- '../data/DIA-Navarro2016-DIAUmpire/Navarro2016_DIA_DIAumpire_input_annotation.csv'
#     
#     output <- getData(mock_input)
#     expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
#     expect_type(output,"list")
#     expect_identical(names(output), expected_names)
#   })
# })
# 
# test_that("dia skyline", {
#   suppressWarnings({
#     mock_input$DDA_DIA <- "DIA"
#     mock_input$filetype = "sky"
#     mock_input$sep = ","
#     
#     mock_input$data$datapath <- '../data/DIA-Navarro2016-Skyline/Navarro2016_DIA_Skyline_input.csv'
#     mock_input$annot$datapath <- '../data/DIA-Navarro2016-Skyline/Navarro2016_DIA_Skyline_annotation.csv'
#     
#     output <- getData(mock_input)
#     expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
#     expect_type(output,"list")
#     expect_identical(names(output), expected_names)
#   })
# })
# 
# test_that("dia spectronaut", {
#   suppressWarnings({
#     mock_input$DDA_DIA <- "DIA"
#     mock_input$filetype = "spec"
# 
#     mock_input$data1$datapath <- '../data/DIA-Navarro2016-Spectronaut/Navarro2016_DIA_Spectronaut_input.xls'
#     mock_input$annot$datapath <- '../data/DIA-Navarro2016-Spectronaut/Navarro2016_DIA_Spectronaut_annotation.csv'
#     
#     output <- getData(mock_input)
#     expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
#     expect_type(output,"list")
#     expect_identical(names(output), expected_names)
#   })
# })

# test_that("dia openswath", {
#   suppressWarnings({
#     mock_input$DDA_DIA <- "DIA"
#     mock_input$filetype = "open"
#     mock_input$sep = "\t"
#     
#     mock_input$data$datapath <- '../data/DIA-Rost2014-OpenSWATH/Rost2014_DIA_OpenSWATH_input.txt'
#     mock_input$annot$datapath <- '../data/DIA-Rost2014-OpenSWATH/Rost2014_DIA_OpenSWATH_annotation.csv'
#     
#     output <- getData(mock_input)
#     expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
#     expect_type(output,"list")
#     expect_identical(names(output), expected_names)
#   })
# })
# 
# test_that("tmt maxquant", {
#   suppressWarnings({
#     mock_input$DDA_DIA <- "TMT"
#     mock_input$filetype = "maxq"
#     
#     mock_input$evidence$datapath <- '../data/TMT-Controlledmix-MS3-MaxQuant/evidence.txt'
#     mock_input$pGroup$datapath <- '../data/TMT-Controlledmix-MS3-MaxQuant/proteinGroups.txt'
#     mock_input$annot1$datapath <- '../data/TMT-Controlledmix-MS3-MaxQuant/MaxQuant_annotation.csv'
#     
#     output <- getData(mock_input)
#     print(names(output))
#     expected_names <- c("ProteinName","PeptideSequence","Charge","PSM","Mixture","TechRepMixture","Run","Channel","BioReplicate","Condition","Intensity")
#     expect_type(output,"list")
#     expect_identical(names(output), expected_names)
#   })
# })
# 
# test_that("tmt openms", {
#   suppressWarnings({
#     mock_input$DDA_DIA <- "TMT"
#     mock_input$filetype = "openms"
#     mock_input$sep = ","
#     
#     mock_input$data$datapath <- '../data/TMT-Plubell2016-OpenMS/20200225_MSstatsTMT_OpenMS_Export.csv'
#     
#     output <- getData(mock_input)
#     print(names(output))
#     expected_names <- c("ProteinName","PeptideSequence","Charge","PSM","Mixture","TechRepMixture","Run","Channel","BioReplicate","Condition","Intensity")
#     expect_identical(names(output), expected_names)
#   })
# })

# test_that("tmt spectromine", {
#   suppressWarnings({
#     mock_input$DDA_DIA <- "TMT"
#     mock_input$filetype = "spmin"
# 
#     mock_input$data1$datapath <- '../data/TMT-exampledata-SpectroMine/20180831_095547_CID-OT-MS3-Short_PSM Report_20180831_103118.xls'
#     mock_input$annot$datapath <- '../data/TMT-exampledata-SpectroMine/SpectroMine_annotation.csv'
# 
#     output <- getData(mock_input)
#     expected_names <- c("ProteinName","PeptideSequence","Charge","PSM","Mixture","TechRepMixture","Run","Channel","BioReplicate","Condition","Intensity")
#     expect_type(output,"list")
#     expect_identical(names(output), expected_names)
#   })
# })
