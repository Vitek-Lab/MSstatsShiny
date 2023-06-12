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
  moderated = TRUE
)

################################################################################
# GETX FUNCTION TESTING
################################################################################

test_that("getEvidence returns a data frame when given valid input", {
  suppressWarnings({
    mock_input$evidence$datapath = test_file_tsv
    evidence <- getEvidence(mock_input)
    expect_s3_class(evidence, "data.frame")
  })
})

# test_that("getEvidence returns NULL when given a null input", {
#   mock_input$evidence = NULL
#   evidence <- getEvidence(mock_input)
#   expect_equal(evidence, NULL)
# })

test_that("getEvidence returns an error message when given invalid input", {
  suppressWarnings({
    mock_input$evidence$datapath <- '/path/to'
    evidence <- getEvidence(mock_input)
    expect_equal(evidence, "File load error. Please ensure file is in csv format.")
  })
})

test_that("getEvidence2 returns a data frame when given valid input", {
  suppressWarnings({
    mock_input$evidence2$datapath = test_file_tsv
    evidence2 <- getEvidence2(mock_input)
    expect_s3_class(evidence2, "data.frame")
  })
})

test_that("getEvidence2 returns NULL when given a null input", {
  suppressWarnings({
    mock_input$evidence2 <- NULL
    evidence2 <- getEvidence2(mock_input)
    expect_equal(evidence2, NULL)
  })
})

test_that("getEvidence2 returns an error message when given invalid input", {
  suppressWarnings({
    mock_input$evidence2$datapath <- '/path/to'
    evidence2 <- getEvidence2(mock_input)
    expect_equal(evidence2, "File load error. Please ensure file is in csv format.")
  })
})

test_that("getGlobal returns a data frame when given valid input", {
  suppressWarnings({
    mock_input$unmod$datapath = test_file_tsv
    unmod <- getGlobal(mock_input)
    expect_s3_class(unmod, "data.frame")
  })
})

test_that("getGlobal returns NULL when given a null input", {
  suppressWarnings({
    mock_input$unmod <- NULL
    unmod <- getGlobal(mock_input)
    expect_equal(unmod, NULL)
  })
})

test_that("getGlobal returns an error message when given invalid input", {
  suppressWarnings({
    mock_input$unmod$datapath <- '/path/to'
    unmod <- getGlobal(mock_input)
    expect_equal(unmod, "File load error. Please ensure file is in csv format.")
  })
})

test_that("getProteinGroups returns a data frame when given valid input", {
  suppressWarnings({
    mock_input$pGroup$datapath = test_file_tsv
    pGroup <- getProteinGroups(mock_input)
    expect_s3_class(pGroup, "data.frame")
  })
})

# test_that("getProteinGroups returns NULL when given a null input", {
#   mock_input$pGroup <- NULL
#   pGroup <- getProteinGroups(mock_input)
#   expect_equal(pGroup, NULL)
# })

test_that("getProteinGroups returns an error message when given invalid input", {
  suppressWarnings({
    mock_input$pGroup$datapath <- '/path/to'
    pGroup <- getProteinGroups(mock_input)
    expect_equal(pGroup, "File load error. Please ensure file is in csv format.")
  })
})

test_that("getProteinGroups2 returns a data frame when given valid input", {
  suppressWarnings({
    mock_input$pGroup2$datapath = test_file_tsv
    pGroup2 <- getProteinGroups2(mock_input)
    expect_s3_class(pGroup2, "data.frame")
  })
})

test_that("getProteinGroups2 returns NULL when given a null input", {
  suppressWarnings({
    mock_input$pGroup2 <- NULL
    pGroup2 <- getProteinGroups2(mock_input)
    expect_equal(pGroup2, NULL)
  })
})

test_that("getProteinGroups2 returns an error message when given invalid input", {
  suppressWarnings({
    mock_input$pGroup2$datapath <- '/path/to'
    pGroup2 <- getProteinGroups2(mock_input)
    expect_equal(pGroup2, "File load error. Please ensure file is in csv format.")
  })
})

test_that("getFragSummary returns a data frame when given valid input", {
  suppressWarnings({
    mock_input$fragSummary$datapath = test_file_tsv
    fragSummary <- getFragSummary(mock_input)
    expect_s3_class(fragSummary, "data.frame")
  })
})

test_that("getFragSummary returns NULL when given a null input", {
  suppressWarnings({
    mock_input$fragSummary <- NULL
    fragSummary <- getFragSummary(mock_input)
    expect_equal(fragSummary, NULL)
  })
})

test_that("getFragSummary returns an error message when given invalid input", {
  suppressWarnings({
    mock_input$fragSummary$datapath <- '/path/to'
    fragSummary <- getFragSummary(mock_input)
    expect_equal(fragSummary, "File load error. Please ensure file is in excel format.")
  })
})

test_that("getPeptideSummary returns a data frame when given valid input", {
  suppressWarnings({
    mock_input$peptideSummary$datapath = test_file_tsv
    peptideSummary <- getPeptideSummary(mock_input)
    expect_s3_class(peptideSummary, "data.frame")
  })
})

test_that("getPeptideSummary returns NULL when given a null input", {
  suppressWarnings({
    mock_input$peptideSummary <- NULL
    peptideSummary <- getPeptideSummary(mock_input)
    expect_equal(peptideSummary, NULL)
  })
})

test_that("getPeptideSummary returns an error message when given invalid input", {
  suppressWarnings({
    mock_input$peptideSummary$datapath <- '/path/to'
    peptideSummary <- getPeptideSummary(mock_input)
    expect_equal(peptideSummary, "File load error. Please ensure file is in csv format.")
  })
})

test_that("getProtSummary returns a data frame when given valid input", {
  suppressWarnings({
    mock_input$protSummary$datapath = test_file_tsv
    protSummary <- getProtSummary(mock_input)
    expect_s3_class(protSummary, "data.frame")
  })
})

test_that("getProtSummary returns NULL when given a null input", {
  suppressWarnings({
    mock_input$protSummary <- NULL
    protSummary <- getProtSummary(mock_input)
    expect_equal(protSummary, NULL)
  })
})

test_that("getProtSummary returns an error message when given invalid input", {
  suppressWarnings({
    mock_input$protSummary$datapath <- '/path/to'
    protSummary <- getProtSummary(mock_input)
    expect_equal(protSummary, "File load error. Please ensure file is in csv format.")
  })
})

test_that("getMaxqPtmSites returns a data frame when given valid input", {
  suppressWarnings({
    mock_input$maxq_ptm_sites$datapath = test_file_tsv
    maxq_ptm_sites <- getMaxqPtmSites(mock_input)
    expect_s3_class(maxq_ptm_sites, "data.frame")
  })
})

test_that("getMaxqPtmSites returns NULL when given a null input", {
  suppressWarnings({
    mock_input$maxq_ptm_sites <- NULL
    maxq_ptm_sites <- getMaxqPtmSites(mock_input)
    expect_equal(maxq_ptm_sites, NULL)
  })
})

test_that("getMaxqPtmSites returns an error message when given invalid input", {
  suppressWarnings({
    mock_input$maxq_ptm_sites$datapath <- '/path/to'
    maxq_ptm_sites <- getMaxqPtmSites(mock_input)
    expect_equal(maxq_ptm_sites, "File load error. Please ensure file is in csv format.")
  })
})

test_that("getAnnot3 returns a data frame when given valid input", {
  suppressWarnings({
    mock_input$annot3$datapath = test_file_tsv
    annot3 <- getAnnot3(mock_input)
    expect_s3_class(annot3, "data.frame")
  })
})

test_that("getAnnot3 returns NULL when given a null input", {
  suppressWarnings({
    mock_input$annot3 <- NULL
    annot3 <- getAnnot3(mock_input)
    expect_equal(annot3, NULL)
  })
})

test_that("getAnnot3 returns an error message when given invalid input", {
  suppressWarnings({
    mock_input$annot3$datapath <- '/path/to'
    annot3 <- getAnnot3(mock_input)
    expect_equal(annot3, "File load error. Please ensure file is in csv format.")
  })
})

test_that("getAnnot2 returns a data frame when given valid input", {
  suppressWarnings({
    mock_input$annot2$datapath = test_file_tsv
    annot2 <- getAnnot2(mock_input)
    expect_s3_class(annot2, "data.frame")
  })
})

test_that("getAnnot2 returns NULL when given a null input", {
  suppressWarnings({
    mock_input$annot2 <- NULL
    annot2 <- getAnnot2(mock_input)
    expect_equal(annot2, NULL)
  })
})

test_that("getAnnot2 returns an error message when given invalid input", {
  suppressWarnings({
    mock_input$annot2$datapath <- '/path/to'
    annot2 <- getAnnot2(mock_input)
    expect_equal(annot2, "File load error. Please ensure file is in csv format.")
  })
})

test_that("getAnnot returns a data frame when given valid input", {
  suppressWarnings({
    mock_input$filetype = "sample"
    mock_input$DDA_DIA = "DIA"
    mock_input$annot$datapath = test_file_tsv
    annot <- getAnnot(mock_input)
    expect_s3_class(annot, "data.frame")
  })
})

test_that("getAnnot returns NULL when given a null input", {
  suppressWarnings({
    mock_input$filetype = "sample"
    mock_input$DDA_DIA = "DIA"
    mock_input$annot <- NULL
    annot <- getAnnot(mock_input)
    expect_equal(annot, NULL)
  })
})

test_that("getAnnot returns an error message when given invalid input", {
  suppressWarnings({
    mock_input$filetype = "sample"
    mock_input$DDA_DIA = "DIA"
    mock_input$annot$datapath <- '/path/to'
    annot <- getAnnot(mock_input)
    expect_equal(annot, "File load error. Please ensure file is in csv format.")
  })
})

test_that("getAnnot1 returns a data frame when given valid input", {
  suppressWarnings({
    mock_input$annot1$datapath = test_file_tsv
    annot1 <- getAnnot1(mock_input)
    expect_s3_class(annot1, "data.frame")
  })
})

test_that("getAnnot1 returns NULL when given a null input", {
  suppressWarnings({
    mock_input$annot1 <- NULL
    annot1 <- getAnnot1(mock_input)
    expect_equal(annot1, NULL)
  })
})

test_that("getAnnot1 returns an error message when given invalid input", {
  suppressWarnings({
    mock_input$annot1$datapath <- '/path/to'
    annot1 <- getAnnot1(mock_input)
    expect_equal(annot1, "File load error. Please ensure file is in csv format.")
  })
})

################################################################################
# GET DATA FUNCTION TESTING
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
  ips_vec <- c("SRM_PRM", "DIA", "DDA")
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

test_that("dda maxquant", {
  suppressWarnings({
    mock_input$DDA_DIA <- "DDA"
    mock_input$filetype = "maxq"
    
    stub(getData,"getEvidence",data.table::fread(system.file("tinytest/raw_data/MaxQuant/mq_ev.csv",
                                                             package = "MSstatsConvert")))
    stub(getData,"getProteinGroups",data.table::fread(system.file("tinytest/raw_data/MaxQuant/mq_pg.csv",
                                                                  package = "MSstatsConvert")))
    stub(getData,"getAnnot1",data.table::fread(system.file("tinytest/raw_data/MaxQuant/annotation.csv",
                                                           package = "MSstatsConvert")))
    
    
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
    
    mock_input$data$datapath <- system.file("tinytest/raw_data/PD/pd_input.csv",
                                            package = "MSstatsConvert")
    
    stub(getData,"getAnnot",data.table::fread(system.file("tinytest/annotations/annot_pd.csv", package = "MSstats")))
    
    output <- getData(mock_input)
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
    
    mock_input$data$datapath <- system.file("tinytest/raw_data/Progenesis/progenesis_input.csv",
                                            package = "MSstatsConvert")
    
    stub(getData,"getAnnot",data.table::fread(system.file("tinytest/raw_data/Progenesis/progenesis_annot.csv",
                                                          package = "MSstatsConvert")))
    
    output <- getData(mock_input)
    expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

test_that("dda dia skyline", {
  suppressWarnings({
    mock_input$DDA_DIA <- "DDA"
    mock_input$filetype = "sky"
    mock_input$sep = ","
    
    mock_input$skylinedata$datapath <- system.file("tinytest/raw_data/Skyline/skyline_input.csv",
                                            package = "MSstatsConvert")
    
    stub(getData,"getAnnot",NULL)
    
    output <- getData(mock_input)
    expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
    
    mock_input$DDA_DIA <- "DIA"
    output <- getData(mock_input)
    expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

test_that("dda openms", {
  suppressWarnings({
    mock_input$DDA_DIA <- "DDA"
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

test_that("dia diaumpire", {
  suppressWarnings({
    mock_input$DDA_DIA <- "DIA"
    mock_input$filetype = "ump"
    
    stub(getData,"getFragSummary",data.table::fread(system.file("tinytest/raw_data/DIAUmpire/dia_frag.csv",
                                                                package = "MSstatsConvert")))
    stub(getData,"getPeptideSummary",data.table::fread(system.file("tinytest/raw_data/DIAUmpire/dia_pept.csv",
                                                                   package = "MSstatsConvert")))
    stub(getData,"getProtSummary",data.table::fread(system.file("tinytest/raw_data/DIAUmpire/dia_prot.csv",
                                                                package = "MSstatsConvert")))
    stub(getData,"getAnnot2",data.table::fread(system.file("tinytest/annotations/annot_diau.csv",
                                                           package = "MSstats")))
    
    output <- getData(mock_input)
    expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

test_that("dia spectronaut", {
  suppressWarnings({
    mock_input$DDA_DIA <- "DIA"
    mock_input$filetype = "spec"
    
    stub(getData,"getAnnot",NULL)
    
    stub(getData,"read.csv",data.table::fread(system.file("tinytest/raw_data/Spectronaut/spectronaut_input.csv",
                                                          package = "MSstatsConvert")))
    output <- getData(mock_input)
    expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

test_that("dia openswath", {
  suppressWarnings({
    mock_input$DDA_DIA <- "DIA"
    mock_input$filetype = "open"
    mock_input$sep = "\t"
    
    stub(getData,"getAnnot",data.table::fread(system.file("tinytest/annotations/annot_os.csv",
                                                          package = "MSstats")))
    stub(getData,"read.csv",data.table::fread(system.file("tinytest/raw_data/OpenSWATH/openswath_input.csv",
                                                          package = "MSstatsConvert")))
    output <- getData(mock_input)
    expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

test_that("tmt maxquant", {
  suppressWarnings({
    mock_input$DDA_DIA <- "TMT"
    mock_input$filetype = "maxq"
    
    load(system.file("data/evidence.rda",
                     package = "MSstatsShiny"))
    load(system.file("data/proteinGroups.rda",
                     package = "MSstatsShiny"))
    load(system.file("data/annotation.mq.rda",
                     package = "MSstatsShiny"))
    stub(getData,"getEvidence",evidence)
    stub(getData,"getProteinGroups",proteinGroups)
    stub(getData,"getAnnot1",annotation.mq)
    
    output <- getData(mock_input)
    print(names(output))
    expected_names <- c("ProteinName","PeptideSequence","Charge","PSM","Mixture","TechRepMixture","Run","Channel","BioReplicate","Condition","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

test_that("tmt openms", {
  suppressWarnings({
    mock_input$DDA_DIA <- "TMT"
    mock_input$filetype = "openms"
    mock_input$sep = ","
    
    load(system.file("data/raw.om.rda", package = "MSstatsShiny"))
    stub(getData,"read.csv",raw.om)
    
    output <- getData(mock_input)
    print(names(output))
    expected_names <- c("ProteinName","PeptideSequence","Charge","PSM","Mixture","TechRepMixture","Run","Channel","BioReplicate","Condition","Intensity")
    expect_identical(names(output), expected_names)
  })
})

test_that("tmt spectromine", {
  suppressWarnings({
    mock_input$DDA_DIA <- "TMT"
    mock_input$filetype = "spmin"
    
    load(system.file("data/raw.mine.rda",
                     package = "MSstatsShiny"))
    load(system.file("data/annotation.mine.rda",
                     package = "MSstatsShiny"))
    stub(getData,"read.csv",raw.mine)
    stub(getData,"getAnnot",annotation.mine)
    
    output <- getData(mock_input)
    expected_names <- c("ProteinName","PeptideSequence","Charge","PSM",
                        "Mixture","TechRepMixture","Run","Channel",
                        "BioReplicate","Condition","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

test_that("tmt phil", {
  suppressWarnings({
    mock_input$DDA_DIA <- "TMT"
    mock_input$filetype = "phil"
    
    expect_equal(1,1)
  })
})

################################################################################
# GET DATA CODES FUNCTION TESTING
################################################################################

test_that("get data code filetype sample", {
  suppressWarnings({
    mock_input$filetype = "sample"

    mock_input$DDA_DIA <- "SRM_PRM"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$DDA_DIA <- "DDA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$DDA_DIA <- "DIA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$DDA_DIA <- "PTM"
    mock_input$PTMTMT <- "Yes"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

test_that("get data code filetype msstats", {
  suppressWarnings({
    mock_input$filetype = "msstats"
    
    mock_input$DDA_DIA <- "PTM"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$DDA_DIA <- "DDA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

test_that("get data code filetype 10col", {
  suppressWarnings({
    mock_input$filetype = "10col"
    
    mock_input$DDA_DIA <- "DDA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

test_that("get data code filetype sky", {
  suppressWarnings({
    mock_input$filetype = "sky"
    
    mock_input$DDA_DIA <- "DDA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$DDA_DIA <- "DIA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

test_that("get data code filetype maxq", {
  suppressWarnings({
    mock_input$filetype = "maxq"
    
    mock_input$DDA_DIA <- "TMT"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$DDA_DIA <- "PTM"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$DDA_DIA <- "DIA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

test_that("get data code filetype prog", {
  suppressWarnings({
    mock_input$filetype = "prog"
    
    mock_input$DDA_DIA <- "DIA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

test_that("get data code filetype PD", {
  suppressWarnings({
    mock_input$filetype = "PD"
    
    mock_input$DDA_DIA <- "TMT"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$DDA_DIA <- "DIA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

test_that("get data code filetype spec & open & openms", {
  suppressWarnings({
    mock_input$filetype = "spec"
    
    mock_input$DDA_DIA <- "DIA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$filetype = "open"
    
    mock_input$DDA_DIA <- "DIA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$filetype = "openms"
    
    mock_input$DDA_DIA <- "DIA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$filetype = "spim"
    
    mock_input$DDA_DIA <- "DIA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$filetype = "phil"
    
    mock_input$DDA_DIA <- "DIA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

stub(getSummary1,"show_modal_spinner",{},depth=2)
stub(getSummary1,"remove_modal_spinner",{},depth=2)

mockGetData = function(mock_input) {
  output <- getData(mock_input)
  return(output)
}

# test_that("get summary 1 TMT", {
#   suppressWarnings({
#     mock_input$filetype = "sample"
#     mock_input$DDA_DIA <- "TMT"
#     stub(getSummary1,"getData",mockGetData(mock_input))
# 
#     output <- getSummary1(mock_input)
# 
#     expected_names <- c("Number of Conditions","Number of Biological Replicates","Number of Mixtures","Number of Technical Replicates","Number of Fractions","Number of MS runs")
#     expect_type(output,"list")
#     expect_identical(rownames(output), expected_names)
# 
#   })
# })

test_that("get summary 1 PTM PTMTMT:Yes", {
  suppressWarnings({
    mock_input$filetype = "sample"
    mock_input$DDA_DIA <- "PTM"
    mock_input$PTMTMT <- "Yes"
    stub(getSummary1,"getData",mockGetData(mock_input))
    
    output <- getSummary1(mock_input,getData(mock_input))
    
    expected_names <- c("Number of Conditions","Number of PTM Mixtures","Number of PTM Biological Replicates","Number of PTM MS runs","Number of PTM Technical Replicates","Number of Unmod Mixtures","Number of Unmod Biological Replicates","Number of Unmod MS runs","Number of Unmod Technical Replicates")
    expect_type(output,"list")
    expect_identical(rownames(output), expected_names)
  })
})

test_that("get summary 1 PTM PTMTMT:No", {
  suppressWarnings({
    mock_input$filetype = "sample"
    mock_input$DDA_DIA <- "PTM"
    mock_input$PTMTMT <- "No"
    stub(getSummary1,"getData",mockGetData(mock_input))
    
    output <- getSummary1(mock_input,getData(mock_input))
    
    expected_names <- c("Number of Conditions","Number of PTM Biological Replicates","Number of PTM MS runs","Number of Unmod Conditions","Number of Unmod Biological Replicates","Number of Unmod MS runs")
    expect_type(output,"list")
    expect_identical(rownames(output), expected_names)
  })
})

test_that("get summary 1 Other:DDA", {
  suppressWarnings({
    mock_input$filetype = "sample"
    mock_input$DDA_DIA <- "DDA"
    stub(getSummary1,"getData",mockGetData(mock_input))
    
    output <- getSummary1(mock_input,getData(mock_input))
    
    expected_names <- c("Number of Conditions","Number of Biological Replicates","Number of Technical Replicates","Number of Fractions","Number of MS runs")
    expect_type(output,"list")
    expect_identical(rownames(output), expected_names)
  })
})

stub(getSummary2,"show_modal_spinner",{},depth=2)
stub(getSummary2,"remove_modal_spinner",{},depth=2)

# test_that("get summary 2 TMT", {
#   suppressWarnings({
#     mock_input$filetype = "sample"
#     mock_input$DDA_DIA <- "TMT"
#     stub(getSummary2,"getData",mockGetData(mock_input))
# 
#     output <- getSummary2(mock_input)
# 
#     expected_names <- c("Number of Proteins","Number of Peptides","Number of Features","Number of Peptides/Protein","Number of Features/Peptide","Intensity Range")
#     expect_type(output,"list")
#     expect_identical(rownames(output), expected_names)
# 
#   })
# })

test_that("get summary 2 PTM PTMTMT:Yes", {
  suppressWarnings({
    mock_input$filetype = "sample"
    mock_input$DDA_DIA <- "PTM"
    mock_input$PTMTMT <- "Yes"
    stub(getSummary2,"getData",mockGetData(mock_input))
    
    output <- getSummary2(mock_input,getData(mock_input))
    print(rownames(output))
    
    expected_names <- c("Number of PTMs","Number of PTM Features",
                        "Number of Features/PTM","PTM Intensity Range",
                        "Number of Unmod Proteins","Number of Protein Peptides",
                        "Number of Protein Features",
                        "Number of Features/Peptide",
                        "Number of Peptides/Protein","Protein Intensity Range")
    expect_type(output,"list")
    expect_identical(rownames(output), expected_names)
  })
})

test_that("get summary 2 PTM PTMTMT:No", {
  suppressWarnings({
    mock_input$filetype = "sample"
    mock_input$DDA_DIA <- "PTM"
    mock_input$PTMTMT <- "No"
    stub(getSummary2,"getData",mockGetData(mock_input))
    
    output <- getSummary2(mock_input,getData(mock_input))
    print(rownames(output))
    
    expected_names <- c("Number of PTMs","Number of PTM Features","Number of Features/PTM","PTM Intensity Range","Number of Unmod Proteins","Number of Protein Peptides","Number of Protein Features","Number of Features/Peptide","Number of Peptides/Protein","Protein Intensity Range") 
    expect_type(output,"list")
    expect_identical(rownames(output), expected_names)
  })
})

test_that("get summary 2 Other:DDA", {
  suppressWarnings({
    mock_input$filetype = "sample"
    mock_input$DDA_DIA <- "DDA"
    stub(getSummary2,"getData",mockGetData(mock_input))
    
    output <- getSummary2(mock_input,getData(mock_input))
    expected_names <- c("Number of Proteins","Number of Peptides","Number of Features","Number of Peptides/Protein","Number of Features/Peptide","Intensity Range") 
    expect_type(output,"list")
    expect_identical(rownames(output), expected_names)
  })
})

################################################################################
# preprocessData QC FUNCTION TESTING
################################################################################

stub(preprocessData,"show_modal_spinner",{},depth = 2)
stub(preprocessData,"remove_modal_spinner",{},depth = 2)

mockGetData = function(mock_input) {
  output <- getData(mock_input)
  return(output)
}

mockPreprocessData = function(mock_input) {
  output <- preprocessData(mock_input,mock_input,getData(mock_input))
  return(output)
}

# err
test_that("preprocessData QC, PTM and PTMTMT: No", {
  suppressWarnings({
    mock_input$DDA_DIA <- "PTM"
    mock_input$PTMTMT = "No"
    mock_input$filetype = "sample"
    mock_input$norm = "equalizeMedians"
    mock_input$log = "2"

    stub(preprocessData,"getData",mockGetData(mock_input))
    stub(preprocessData,"loadpage_input",mock_input)
    stub(preprocessData,"qc_input",mock_input,2)

    mockery::stub(
      where = preprocessData,
      what = "MSstatsShiny::lf_summarization_loop",
      how = function(...){
        mockery::stub(MSstatsShiny::lf_summarization_loop, "qc_input", mock_input,depth=2);
        mockery::stub(MSstatsShiny::lf_summarization_loop, "show_modal_progress_line", NULL);
        mockery::stub(MSstatsShiny::lf_summarization_loop, "update_modal_progress", NULL);
        mockery::stub(MSstatsShiny::lf_summarization_loop, "remove_modal_progress", NULL);
        MSstatsShiny::lf_summarization_loop(...)})

    output <- preprocessData(mock_input,mock_input,getData(mock_input))
    expected_names <- c("PTM","PROTEIN")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})
 
# err
test_that("preprocessData QC, PTM and PTMTMT: Yes", {
  suppressWarnings({
    mock_input$DDA_DIA <- "PTM"
    mock_input$PTMTMT = "Yes"
    mock_input$filetype = "sample"
    mock_input$norm = "equalizeMedians"
    mock_input$log = "2"
    mock_input$summarization = "msstats"

    stub(preprocessData,"getData",mockGetData(mock_input))
    stub(preprocessData,"loadpage_input",mock_input)
    stub(preprocessData,"qc_input",mock_input,2)

    mockery::stub(
      where = preprocessData,
      what = "MSstatsShiny::tmt_summarization_loop",
      how = function(...){
        mockery::stub(MSstatsShiny::tmt_summarization_loop, "qc_input", mock_input,depth=2);
        mockery::stub(MSstatsShiny::tmt_summarization_loop, "show_modal_progress_line", NULL);
        mockery::stub(MSstatsShiny::tmt_summarization_loop, "update_modal_progress", NULL);
        mockery::stub(MSstatsShiny::tmt_summarization_loop, "remove_modal_progress", NULL);
        MSstatsShiny::tmt_summarization_loop(...)})

    output <- preprocessData(mock_input,mock_input,getData(mock_input))
    expected_names <- c("PTM","PROTEIN")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

# test_that("preprocessData QC TMT", {
#   suppressWarnings({
#     mock_input$DDA_DIA <- "TMT"
#     mock_input$filetype = "sample"
#     mock_input$summarization = "Median"
#     mock_input$norm = "equalizeMedians"
#     mock_input$standards = "proteins"
# 
#     stub(preprocessData,"getData",mockGetData(mock_input))
#     # stub(preprocessData,"loadpage_input",mock_input)
#     # stub(preprocessData,"qc_input",mock_input,2)
# 
#     mockery::stub(
#       where = preprocessData,
#       what = "MSstatsShiny::tmt_summarization_loop",
#       how = function(...){
#         # mockery::stub(MSstatsShiny::tmt_summarization_loop, "qc_input", mock_input,depth=2);
#         mockery::stub(MSstatsShiny::tmt_summarization_loop, "show_modal_progress_line", NULL);
#         mockery::stub(MSstatsShiny::tmt_summarization_loop, "update_modal_progress", NULL);
#         mockery::stub(MSstatsShiny::tmt_summarization_loop, "remove_modal_progress", NULL);
#         MSstatsShiny::tmt_summarization_loop(...)})
# 
#     output <- preprocessData(mock_input,mock_input)
#     expected_names <- c("FeatureLevelData","ProteinLevelData")
#     expect_type(output,"list")
#     expect_identical(names(output), expected_names)
#   })
# })

# err
test_that("preprocessData QC Other", {
  suppressWarnings({
    mock_input$DDA_DIA <- "DDA"
    mock_input$filetype = "sample"
    mock_input$norm = "equalizeMedians"
    mock_input$log = "2"

    stub(preprocessData,"getData",mockGetData(mock_input))
    stub(preprocessData,"loadpage_input",mock_input)
    stub(preprocessData,"qc_input",mock_input,2)

    mockery::stub(
      where = preprocessData,
      what = "MSstatsShiny::lf_summarization_loop",
      how = function(...){
        mockery::stub(MSstatsShiny::lf_summarization_loop, "MSstatsShiny::qc_input", mock_input);
        mockery::stub(MSstatsShiny::lf_summarization_loop, "show_modal_progress_line", NULL);
        mockery::stub(MSstatsShiny::lf_summarization_loop, "update_modal_progress", NULL);
        mockery::stub(MSstatsShiny::lf_summarization_loop, "remove_modal_progress", NULL);
        MSstatsShiny::lf_summarization_loop(...)})

    output <- preprocessData(mock_input,mock_input,getData(mock_input))
    expected_names <- c("FeatureLevelData","ProteinLevelData","SummaryMethod")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

################################################################################
#  preprocessData CODES FUNCTION TESTING
################################################################################

test_that("get preprocessData code TMT & PTM", {
  suppressWarnings({
    mock_input$DDA_DIA <- "TMT"
    mock_input$filetype = "sample"
    mock_input$summarization = "msstats"
    mock_input$global_norm = TRUE
    mock_input$reference_norm = TRUE
    mock_input$remove_norm_channel =TRUE
    mock_input$summ = TRUE

    stub(preprocessDataCode,"getDataCode","some test code")
    stub(preprocessDataCode,"loadpage_input",mock_input)
    stub(preprocessDataCode,"qc_input",mock_input)

    output <- preprocessDataCode(mock_input,mock_input)
    expect_type(output,"character")
    
    mock_input$DDA_DIA <- "PTM"
    mock_input$PTMTMT <- "Yes"
    stub(preprocessDataCode,"getDataCode","some test code")
    stub(preprocessDataCode,"loadpage_input",mock_input)
    stub(preprocessDataCode,"qc_input",mock_input)
    
    output <- preprocessDataCode(mock_input,mock_input)
    expect_type(output,"character")
    
    mock_input$DDA_DIA <- "DDA"
    mock_input$features_used <- "all"
    stub(preprocessDataCode,"getDataCode","some test code")
    stub(preprocessDataCode,"loadpage_input",mock_input)
    stub(preprocessDataCode,"qc_input",mock_input)
    
    output <- preprocessDataCode(mock_input,mock_input)
    expect_type(output,"character")
  })
})

################################################################################
#  dataComparison statmodel FUNCTION TESTING
################################################################################

stub(dataComparison,"remove_modal_spinner",{},depth = 3)

test_that("dataComparison statmodel PTM PTMTMT: Yes", {
  suppressWarnings({
    dummy_matrix <- matrix(0, nrow = 6, ncol = 6)
    conditions <- c("Condition_1", "Condition_2", "Condition_3", "Condition_4", "Condition_5", "Condition_6")
    for (i in 2:6) {
      dummy_matrix[i, 1] <- -1
      dummy_matrix[1, i] <- 1
      dummy_matrix[i, i] <- 1
    }
    colnames(dummy_matrix) <- conditions
    rownames(dummy_matrix) <- conditions

    mock_input$DDA_DIA <- "PTM"
    mock_input$PTMTMT = "Yes"
    mock_input$filetype = "sample"
    mock_input$summarization = "Median"

    mockery::stub(
        where = dataComparison,
        what = "MSstatsShiny::tmt_model",
        how = function(...){
          mockery::stub(MSstatsShiny::tmt_model, "show_modal_progress_line", NULL);
          mockery::stub(MSstatsShiny::tmt_model, "update_modal_progress", NULL);
          mockery::stub(MSstatsShiny::tmt_model, "remove_modal_progress", NULL);
          MSstatsShiny::tmt_model(...)})
    
    mockery::stub(
      where = preprocessData,
      what = "MSstatsShiny::tmt_summarization_loop",
      how = function(...){
        mockery::stub(MSstatsShiny::tmt_summarization_loop, "qc_input", mock_input,depth=2);
        mockery::stub(MSstatsShiny::tmt_summarization_loop, "show_modal_progress_line", NULL);
        mockery::stub(MSstatsShiny::tmt_summarization_loop, "update_modal_progress", NULL);
        mockery::stub(MSstatsShiny::tmt_summarization_loop, "remove_modal_progress", NULL);
        MSstatsShiny::tmt_summarization_loop(...)})

    output <- dataComparison(mock_input,mock_input,mock_input,dummy_matrix,preprocessData(mock_input,mock_input,getData(mock_input)))
    expected_names <- c("PTM.Model","PROTEIN.Model","ADJUSTED.Model")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

test_that("dataComparison statmodel PTM PTMTMT: No", {
  suppressWarnings({
    dummy_matrix <- matrix(0, nrow = 4, ncol = 4)
    conditions <- c("CCCP", "Combo", "Ctrl", "USP30_OE")
    for (i in 2:4) {
      dummy_matrix[i, 1] <- -1
      dummy_matrix[1, i] <- 1
      dummy_matrix[i, i] <- 1
    }
    colnames(dummy_matrix) <- conditions
    rownames(dummy_matrix) <- conditions

    mock_input$DDA_DIA <- "PTM"
    mock_input$PTMTMT = "No"
    mock_input$filetype = "sample"
    mock_input$MBi = TRUE
    mock_input$log = "2"
    mock_input$norm = "equalizeMedians"

    stub(dataComparison,"loadpage_input",mock_input,2)
    stub(dataComparison,"qc_input",mock_input)
    
    mockery::stub(
      where = preprocessData,
      what = "MSstatsShiny::lf_summarization_loop",
      how = function(...){
        mockery::stub(MSstatsShiny::lf_summarization_loop, "qc_input", mock_input,depth=2);
        mockery::stub(MSstatsShiny::lf_summarization_loop, "show_modal_progress_line", NULL);
        mockery::stub(MSstatsShiny::lf_summarization_loop, "update_modal_progress", NULL);
        mockery::stub(MSstatsShiny::lf_summarization_loop, "remove_modal_progress", NULL);
        MSstatsShiny::lf_summarization_loop(...)})

    mockery::stub(
      where = dataComparison,
      what = "MSstatsShiny::lf_model",
      how = function(...){
        mockery::stub(MSstatsShiny::lf_model, "show_modal_progress_line", NULL);
        mockery::stub(MSstatsShiny::lf_model, "update_modal_progress", NULL);
        mockery::stub(MSstatsShiny::lf_model, "remove_modal_progress", NULL);
        MSstatsShiny::lf_model(...)})

    output <- dataComparison(mock_input,mock_input,mock_input,dummy_matrix,preprocessData(mock_input,mock_input,getData(mock_input)))
    expected_names <- c("PTM.Model","PROTEIN.Model","ADJUSTED.Model")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

# test_that("dataComparison statmodel TMT", {
#   suppressWarnings({
#     dummy_matrix <- matrix(0, nrow = 4, ncol = 4)
#     conditions <- c("0.125", "0.667", "1", "0.5")
#     for (i in 2:4) {
#       dummy_matrix[i, 1] <- -1
#       dummy_matrix[1, i] <- 1
#       dummy_matrix[i, i] <- 1
#     }
#     colnames(dummy_matrix) <- conditions
#     rownames(dummy_matrix) <- conditions
# 
#     mock_input$DDA_DIA <- "TMT"
#     mock_input$filetype = "sample"
#     mock_input$summarization = "Median"
#     mock_input$global_norm = TRUE
#     mock_input$reference_norm = TRUE
#     mock_input$remove_norm_channel =TRUE
#     mock_input$summ = TRUE
# 
#     stub(dataComparison,"loadpage_input",mock_input,2)
#     stub(dataComparison,"qc_input",mock_input)
# 
#     mockery::stub(
#       where = dataComparison,
#       what = "preprocessData",
#       how = function(...){
#         mockery::stub(preprocessData, "loadpage_input", mock_input,depth=2);
#         mockery::stub(where=preprocessData, what="getData", how=function(...){
#           mockery::stub(getData, "remove_modal_spinner", NULL);
#           getData(...)});
#         mockery::stub(where=preprocessData, what="MSstatsShiny::tmt_summarization_loop", how=function(...){
#           mockery::stub(MSstatsShiny::tmt_summarization_loop, "remove_modal_progress", NULL);
#           mockery::stub(MSstatsShiny::tmt_summarization_loop, "show_modal_progress_line", NULL);
#           mockery::stub(MSstatsShiny::tmt_summarization_loop, "qc_input", mock_input);
#           MSstatsShiny::tmt_summarization_loop(...)});
#         preprocessData(...)})
# 
#     mockery::stub(
#         where = dataComparison,
#         what = "MSstatsShiny::tmt_model",
#         how = function(...){
#           mockery::stub(MSstatsShiny::tmt_model, "show_modal_progress_line", NULL);
#           mockery::stub(MSstatsShiny::tmt_model, "update_modal_progress", NULL);
#           mockery::stub(MSstatsShiny::tmt_model, "remove_modal_progress", NULL);
#           mockery::stub(MSstatsShiny::tmt_model, "input", mock_input);
#           MSstatsShiny::tmt_model(...)})
# 
#     output <- dataComparison(mock_input,mock_input,mock_input,dummy_matrix)
#     expected_names <- c("ComparisonResult","ModelQC","FittedModel")
#     expect_type(output,"list")
#     expect_identical(names(output), expected_names)
#   })
# })

test_that("dataComparison statmodel Other", {
  suppressWarnings({
    dummy_matrix <- matrix(0, nrow = 6, ncol = 6)
    conditions <- c("C1", "C2", "C3", "C4","C5","C6")
    for (i in 2:6) {
      dummy_matrix[i, 1] <- -1
      dummy_matrix[1, i] <- 1
      dummy_matrix[i, i] <- 1
    }
    colnames(dummy_matrix) <- conditions
    rownames(dummy_matrix) <- conditions

    mock_input$DDA_DIA <- "DDA"
    mock_input$filetype = "sample"
    mock_input$norm = "equalizeMedians"
    mock_input$log = "2"

    stub(dataComparison,"loadpage_input",mock_input,2)
    stub(dataComparison,"qc_input",mock_input)

    mockery::stub(
      where = dataComparison,
      what = "MSstatsShiny::lf_model",
      how = function(...){
        mockery::stub(MSstatsShiny::lf_model, "show_modal_progress_line", NULL);
        mockery::stub(MSstatsShiny::lf_model, "update_modal_progress", NULL);
        mockery::stub(MSstatsShiny::lf_model, "remove_modal_progress", NULL);
        MSstatsShiny::lf_model(...)})
    
    mockery::stub(
      where = preprocessData,
      what = "MSstatsShiny::lf_summarization_loop",
      how = function(...){
        mockery::stub(MSstatsShiny::lf_summarization_loop, "qc_input", mock_input,depth=2);
        mockery::stub(MSstatsShiny::lf_summarization_loop, "show_modal_progress_line", NULL);
        mockery::stub(MSstatsShiny::lf_summarization_loop, "update_modal_progress", NULL);
        mockery::stub(MSstatsShiny::lf_summarization_loop, "remove_modal_progress", NULL);
        MSstatsShiny::lf_summarization_loop(...)})

    output <- dataComparison(mock_input,mock_input,mock_input,dummy_matrix,preprocessData(mock_input,mock_input,getData(mock_input)))
    expected_names <- c("ComparisonResult","ModelQC","FittedModel")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})
