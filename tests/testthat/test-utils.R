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

################################################################################
# GETX FUNCTION TESTING
################################################################################

# # Define a test for the getEvidence function
# test_that("getEvidence returns a data frame when given valid input", {
#   mock_input$evidence$datapath = test_file_tsv
#   evidence <- getEvidence(mock_input)
#   print(evidence)
#   expect_s3_class(evidence, "data.frame")
# })
# 
# test_that("getEvidence returns NULL when given a null input", {
#   mock_input$evidence <- NULL
#   evidence <- getEvidence(mock_input)
#   expect_equal(evidence, NULL)
# })
# 
# test_that("getEvidence returns an error message when given invalid input", {
#   mock_input$evidence$datapath <- '/path/to'
#   evidence <- getEvidence(mock_input)
#   expect_equal(evidence, "File load error. Please ensure file is in csv format.")
# })

# # Define a test for the getEvidence2 function
# test_that("getEvidence2 returns a data frame when given valid input", {
#   mock_input$evidence2$datapath = test_file_tsv
#   evidence2 <- getEvidence2(mock_input)
#   expect_s3_class(evidence2, "data.frame")
# })
# 
# test_that("getEvidence2 returns NULL when given a null input", {
#   mock_input$evidence2 <- NULL
#   evidence2 <- getEvidence2(mock_input)
#   expect_equal(evidence2, NULL)
# })
# 
# test_that("getEvidence2 returns an error message when given invalid input", {
#   mock_input$evidence2$datapath <- '/path/to'
#   evidence2 <- getEvidence2(mock_input)
#   expect_equal(evidence2, "File load error. Please ensure file is in csv format.")
# })

# # Define a test for the getGlobal function
# test_that("getGlobal returns a data frame when given valid input", {
#   mock_input$unmod$datapath = test_file_tsv
#   unmod <- getGlobal(mock_input)
#   expect_s3_class(unmod, "data.frame")
# })
# 
# test_that("getGlobal returns NULL when given a null input", {
#   mock_input$unmod <- NULL
#   unmod <- getGlobal(mock_input)
#   expect_equal(unmod, NULL)
# })
# 
# test_that("getGlobal returns an error message when given invalid input", {
#   mock_input$unmod$datapath <- '/path/to'
#   unmod <- getGlobal(mock_input)
#   expect_equal(unmod, "File load error. Please ensure file is in csv format.")
# })

# # Define a test for the getProteinGroups function
# test_that("getProteinGroups returns a data frame when given valid input", {
#   mock_input$pGroup$datapath = test_file_tsv
#   pGroup <- getProteinGroups(mock_input)
#   expect_s3_class(pGroup, "data.frame")
# })
# 
# test_that("getProteinGroups returns NULL when given a null input", {
#   mock_input$pGroup <- NULL
#   pGroup <- getProteinGroups(mock_input)
#   expect_equal(pGroup, NULL)
# })
# 
# test_that("getProteinGroups returns an error message when given invalid input", {
#   mock_input$pGroup$datapath <- '/path/to'
#   pGroup <- getProteinGroups(mock_input)
#   expect_equal(pGroup, "File load error. Please ensure file is in csv format.")
# })

# # Define a test for the getProteinGroups2 function
# test_that("getProteinGroups2 returns a data frame when given valid input", {
#   mock_input$pGroup2$datapath = test_file_tsv
#   pGroup2 <- getProteinGroups2(mock_input)
#   expect_s3_class(pGroup2, "data.frame")
# })
# 
# test_that("getProteinGroups2 returns NULL when given a null input", {
#   mock_input$pGroup2 <- NULL
#   pGroup2 <- getProteinGroups2(mock_input)
#   expect_equal(pGroup2, NULL)
# })
# 
# test_that("getProteinGroups2 returns an error message when given invalid input", {
#   mock_input$pGroup2$datapath <- '/path/to'
#   pGroup2 <- getProteinGroups2(mock_input)
#   expect_equal(pGroup2, "File load error. Please ensure file is in csv format.")
# })

# # Define a test for the getFragSummary function
# test_that("getFragSummary returns a data frame when given valid input", {
#   mock_input$fragSummary$datapath = test_file_tsv
#   fragSummary <- getFragSummary(mock_input)
#   expect_s3_class(fragSummary, "data.frame")
# })
# 
# test_that("getFragSummary returns NULL when given a null input", {
#   mock_input$fragSummary <- NULL
#   fragSummary <- getFragSummary(mock_input)
#   expect_equal(fragSummary, NULL)
# })
# 
# test_that("getFragSummary returns an error message when given invalid input", {
#   mock_input$fragSummary$datapath <- '/path/to'
#   fragSummary <- getFragSummary(mock_input)
#   expect_equal(fragSummary, "File load error. Please ensure file is in excel format.")
# })

# # Define a test for the getPeptideSummary function
# test_that("getPeptideSummary returns a data frame when given valid input", {
#   mock_input$peptideSummary$datapath = test_file_tsv
#   peptideSummary <- getPeptideSummary(mock_input)
#   expect_s3_class(peptideSummary, "data.frame")
# })
# 
# test_that("getPeptideSummary returns NULL when given a null input", {
#   mock_input$peptideSummary <- NULL
#   peptideSummary <- getPeptideSummary(mock_input)
#   expect_equal(peptideSummary, NULL)
# })
# 
# test_that("getPeptideSummary returns an error message when given invalid input", {
#   mock_input$peptideSummary$datapath <- '/path/to'
#   peptideSummary <- getPeptideSummary(mock_input)
#   expect_equal(peptideSummary, "File load error. Please ensure file is in csv format.")
# })

# # Define a test for the getProtSummary function
# test_that("getProtSummary returns a data frame when given valid input", {
#   mock_input$protSummary$datapath = test_file_tsv
#   protSummary <- getProtSummary(mock_input)
#   expect_s3_class(protSummary, "data.frame")
# })
# 
# test_that("getProtSummary returns NULL when given a null input", {
#   mock_input$protSummary <- NULL
#   protSummary <- getProtSummary(mock_input)
#   expect_equal(protSummary, NULL)
# })
# 
# test_that("getProtSummary returns an error message when given invalid input", {
#   mock_input$protSummary$datapath <- '/path/to'
#   protSummary <- getProtSummary(mock_input)
#   expect_equal(protSummary, "File load error. Please ensure file is in csv format.")
# })

# # Define a test for the getMaxqPtmSites function
# test_that("getMaxqPtmSites returns a data frame when given valid input", {
#   mock_input$maxq_ptm_sites$datapath = test_file_tsv
#   maxq_ptm_sites <- getMaxqPtmSites(mock_input)
#   expect_s3_class(maxq_ptm_sites, "data.frame")
# })
# 
# test_that("getMaxqPtmSites returns NULL when given a null input", {
#   mock_input$maxq_ptm_sites <- NULL
#   maxq_ptm_sites <- getMaxqPtmSites(mock_input)
#   expect_equal(maxq_ptm_sites, NULL)
# })
# 
# test_that("getMaxqPtmSites returns an error message when given invalid input", {
#   mock_input$maxq_ptm_sites$datapath <- '/path/to'
#   maxq_ptm_sites <- getMaxqPtmSites(mock_input)
#   expect_equal(maxq_ptm_sites, "File load error. Please ensure file is in csv format.")
# })

# # Define a test for the getAnnot3 function
# test_that("getAnnot3 returns a data frame when given valid input", {
#   mock_input$annot3$datapath = test_file_tsv
#   annot3 <- getAnnot3(mock_input)
#   expect_s3_class(annot3, "data.frame")
# })
# 
# test_that("getAnnot3 returns NULL when given a null input", {
#   mock_input$annot3 <- NULL
#   annot3 <- getAnnot3(mock_input)
#   expect_equal(annot3, NULL)
# })
# 
# test_that("getAnnot3 returns an error message when given invalid input", {
#   mock_input$annot3$datapath <- '/path/to'
#   annot3 <- getAnnot3(mock_input)
#   expect_equal(annot3, "File load error. Please ensure file is in csv format.")
# })

# # Define a test for the getAnnot2 function
# test_that("getAnnot2 returns a data frame when given valid input", {
#   mock_input$annot2$datapath = test_file_tsv
#   annot2 <- getAnnot2(mock_input)
#   expect_s3_class(annot2, "data.frame")
# })
# 
# test_that("getAnnot2 returns NULL when given a null input", {
#   mock_input$annot2 <- NULL
#   annot2 <- getAnnot2(mock_input)
#   expect_equal(annot2, NULL)
# })
# 
# test_that("getAnnot2 returns an error message when given invalid input", {
#   mock_input$annot2$datapath <- '/path/to'
#   annot2 <- getAnnot2(mock_input)
#   expect_equal(annot2, "File load error. Please ensure file is in csv format.")
# })

# # Define a test for the getAnnot function
# test_that("getAnnot returns a data frame when given valid input", {
#   mock_input$filetype = "sample"
#   mock_input$DDA_DIA = "DIA"
#   mock_input$annot$datapath = test_file_tsv
#   annot <- getAnnot(mock_input)
#   expect_s3_class(annot, "data.frame")
# })
# 
# test_that("getAnnot returns NULL when given a null input", {
#   mock_input$filetype = "sample"
#   mock_input$DDA_DIA = "DIA"
#   mock_input$annot <- NULL
#   annot <- getAnnot(mock_input)
#   expect_equal(annot, NULL)
# })
# 
# test_that("getAnnot returns an error message when given invalid input", {
#   mock_input$filetype = "sample"
#   mock_input$DDA_DIA = "DIA"
#   mock_input$annot$datapath <- '/path/to'
#   annot <- getAnnot(mock_input)
#   expect_equal(annot, "File load error. Please ensure file is in csv format.")
# })

# # Define a test for the getAnnot1 function
# test_that("getAnnot1 returns a data frame when given valid input", {
#   mock_input$annot1$datapath = test_file_tsv
#   annot1 <- getAnnot1(mock_input)
#   expect_s3_class(annot1, "data.frame")
# })
# 
# test_that("getAnnot1 returns NULL when given a null input", {
#   mock_input$annot1 <- NULL
#   annot1 <- getAnnot1(mock_input)
#   expect_equal(annot1, NULL)
# })
# 
# test_that("getAnnot1 returns an error message when given invalid input", {
#   mock_input$annot1$datapath <- '/path/to'
#   annot1 <- getAnnot1(mock_input)
#   expect_equal(annot1, "File load error. Please ensure file is in csv format.")
# })

################################################################################
# GET DATA FUNCTION TESTING
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

################################################################################
# GET DATA CODES FUNCTION TESTING
################################################################################

# test_that("get data code", {
#   suppressWarnings({
#     mock_input$DDA_DIA <- "SRM_PRM"
#     mock_input$filetype = "sample"
# 
#     output <- getDataCode(mock_input)
#     print(output)
#     expect_type(output,"character")
#   })
# })

################################################################################
# preprocessData QC FUNCTION TESTING
################################################################################

stub(preprocessData,"show_modal_spinner",{},depth = 2)
stub(preprocessData,"remove_modal_spinner",{},depth = 2)

mockGetData = function(mock_input) {
  mock_input$DDA_DIA <- "DIA"
  mock_input$filetype <- "sample"
  output <- getData(mock_input)
  return(output)
}

stub(preprocessData,"getData",mockGetData(mock_input))

test_that("preproc qc", {
  suppressWarnings({
    mock_input$DDA_DIA <- "PTM"
    mock_input$PTMTMT = "No"
    mock_input$filetype = "sample"
    stub(preprocessData,"loadpage_input",mock_input)
    stub(preprocessData,"MSstatsShiny::lf_summarization_loop::loadpage_input",mock_input)
    stub(lf_summarization_loop,"qc_input",mock_input,depth=2)
    
    # x <- lf_summarization_loop("Y",mock_input,mock_input)
    
    output <- preprocessData(mock_input,mock_input)
    print("++++")
    print(output)
    print("++++")
    expect_type(output,"character")
  })
})
