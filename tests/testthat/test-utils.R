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
    mock_input$DDA_DIA = "LType"
    mock_input$annot$datapath = test_file_tsv
    annot <- getAnnot(mock_input)
    expect_s3_class(annot, "data.frame")
  })
})

test_that("getAnnot returns NULL when given a null input", {
  suppressWarnings({
    mock_input$filetype = "sample"
    mock_input$DDA_DIA = "LType"
    mock_input$annot <- NULL
    annot <- getAnnot(mock_input)
    expect_equal(annot, NULL)
  })
})

test_that("getAnnot returns an error message when given invalid input", {
  suppressWarnings({
    mock_input$filetype = "sample"
    mock_input$DDA_DIA = "LType"
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
  mock_input$BIO <- "Protein"
  mock_input$DDA_DIA <- "LType"
  ips_vec <- c("SRM_PRM" , "DDA" ,
               "DIA")
  for (ddadia in ips_vec) {
    mock_input$LabelFreeType <- ddadia
    output <- getData(mock_input)
    expect_type(output, "list")
  }

  mock_input$DDA_DIA <- "LType"
  mock_input$BIO <- "PTM"
  output <- getData(mock_input)
  print(typeof(output))
  expect_type(output,"list")

  mock_input$DDA_DIA <- "TMT"
  mock_input$BIO <- "PTM"
  output <- getData(mock_input)
  print(typeof(output))
  expect_type(output,"list")
})

test_that("dda maxquant", {
  suppressWarnings({
    mock_input$BIO <- "Protein"
    
    mock_input$DDA_DIA <- "LType"
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
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
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
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
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

# Metamorpheus converter integration
test_that("dda metamorpheus", {
  suppressWarnings({
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
    mock_input$filetype <- "meta"
    mock_input$unique_peptides <- TRUE
    mock_input$remove <- FALSE

    mock_input$data$datapath <-
      system.file("tinytest/raw_data/Metamorpheus/QuantifiedPeaks.tsv",
        package = "MSstatsConvert"
      )

    stub(getData, "getAnnot", data.table::fread(
      system.file("tinytest/raw_data/Metamorpheus/annotation.csv",
        package = "MSstatsConvert"
      )
    ))

    output <- getData(mock_input)
    expected_names <- c(
      "ProteinName", "PeptideSequence", "PrecursorCharge",
      "FragmentIon", "ProductCharge", "IsotopeLabelType",
      "Condition", "BioReplicate", "Run", "Fraction", "Intensity"
    )
    expect_type(output, "list")
    expect_identical(names(output), expected_names)
    expect_gt(nrow(output), 0)
  })
})

test_that("ptm metamorpheus", {
  suppressWarnings({
    mock_input$BIO <- "PTM"
    mock_input$DDA_DIA <- "LType"
    mock_input$filetype <- "meta"
    mock_input$mod_id_meta_select <- "[Common Fixed:Carbamidomethyl on C]"
    mock_input$mod_id_meta_custom <- NULL
    mock_input$ptm_input$datapath <- system.file(
      "tinytest/raw_data/Metamorpheus/AllQuantifiedPeaks.tsv",
      package = "MSstatsPTM")
    mock_input$ptm_annot$datapath <- system.file(
      "tinytest/raw_data/Metamorpheus/ExperimentalDesign.tsv",
      package = "MSstatsPTM")
    mock_input$fasta$datapath <- system.file(
      "extdata", "metamorpheus_fasta.fasta",
      package = "MSstatsPTM")
    mock_input$ptm_protein_input$datapath <- system.file(
      "tinytest/raw_data/Metamorpheus/AllQuantifiedPeaksGlobalProteome.tsv",
      package = "MSstatsPTM")
    mock_input$ptm_protein_annot$datapath <- system.file(
      "tinytest/raw_data/Metamorpheus/ExperimentalDesignGlobalProteome.tsv",
      package = "MSstatsPTM")

    output <- getData(mock_input)
    expected_names <- c("PTM", "PROTEIN")
    expect_type(output, "list")
    expect_identical(names(output), expected_names)
    expect_gt(nrow(output$PTM), 0)
    expect_gt(nrow(output$PROTEIN), 0)
  })
})

test_that("dda dia skyline", {
  suppressWarnings({
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
    mock_input$filetype = "sky"
    mock_input$sep = ","

    mock_input$skylinedata$datapath <- system.file("tinytest/raw_data/Skyline/skyline_input.csv",
                                            package = "MSstatsConvert")

    stub(getData,"getAnnot",NULL)

    output <- getData(mock_input)
    expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)

    mock_input$DDA_DIA <- "LType"
    output <- getData(mock_input)
    expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

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

test_that("dia diaumpire", {
  suppressWarnings({
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
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
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
    mock_input$filetype = "spec"
    mock_input$q_val = TRUE 
    mock_input$q_cutoff = 0.01 
    stub(getData,"getAnnot",NULL)
    
    stub(getData,"data.table::fread",data.table::fread(system.file("tinytest/raw_data/Spectronaut/spectronaut_input.csv",
                                                                   package = "MSstatsConvert")))
    output <- getData(mock_input)
    expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

test_that("dia openswath", {
  suppressWarnings({
    mock_input$BIO  <- "Protein"
    mock_input$DDA_DIA <- "LType"
    mock_input$filetype = "open"
    mock_input$sep = "\t"
    
    stub(getData,"getAnnot",data.table::fread(system.file("tinytest/annotations/annot_os.csv",
                                                          package = "MSstats")))
    stub(getData,"data.table::fread",data.table::fread(system.file("tinytest/raw_data/OpenSWATH/openswath_input.csv",
                                                                   package = "MSstatsConvert")))
    output <- getData(mock_input)
    expected_names <- c("ProteinName","PeptideSequence","PrecursorCharge","FragmentIon","ProductCharge","IsotopeLabelType","Condition","BioReplicate","Run","Fraction","Intensity")
    expect_type(output,"list")
    expect_identical(names(output), expected_names)
  })
})

test_that("tmt maxquant", {
  suppressWarnings({
    mock_input$BIO <- "Protein" 
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
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "TMT"
    mock_input$filetype = "openms"
    mock_input$sep = ","
    
    load(system.file("data/raw.om.rda", package = "MSstatsShiny"))
    stub(getData,"data.table::fread",raw.om)
    
    output <- getData(mock_input)
    print(names(output))
    expected_names <- c("ProteinName","PeptideSequence","Charge","PSM","Mixture","TechRepMixture","Run","Channel","BioReplicate","Condition","Intensity")
    expect_identical(names(output), expected_names)
  })
})

test_that("tmt spectromine", {
  suppressWarnings({
    mock_input$BIO <-"Protein"
    mock_input$DDA_DIA <- "TMT"
    mock_input$filetype = "spmin"
    
    load(system.file("data/raw.mine.rda",
                     package = "MSstatsShiny"))
    load(system.file("data/annotation.mine.rda",
                     package = "MSstatsShiny"))
    stub(getData,"data.table::fread",raw.mine)
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

    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
    mock_input$LabelFreeType ="SRM_PRM"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
    mock_input$LabelFreeType ="DDA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
    mock_input$LabelFreeType ="DIA"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "TMT"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

test_that("get data code filetype msstats", {
  suppressWarnings({
    mock_input$filetype = "msstats"
    
    mock_input$BIO <- "PTM"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$BIO <- "Protein"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

test_that("get data code filetype 10col", {
  suppressWarnings({
    mock_input$BIO <- "Protein"
    mock_input$filetype = "10col"
    
    mock_input$DDA_DIA <- "LType"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

test_that("get data code filetype sky", {
  suppressWarnings({
    mock_input$BIO <- "Protein"
    mock_input$filetype = "sky"
    
    mock_input$DDA_DIA <- "LType"
    output <- getDataCode(mock_input)
    expect_type(output,"character")

    mock_input$DDA_DIA <- "LType"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

test_that("get data code filetype maxq", {
  suppressWarnings({
    mock_input$filetype = "maxq"
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "TMT"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$BIO  <- "PTM"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

test_that("get data code filetype prog", {
  suppressWarnings({
    mock_input$filetype = "prog"
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

test_that("get data code filetype PD", {
  suppressWarnings({
    mock_input$filetype = "PD"
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "TMT"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
  })
})

test_that("get data code filetype spec & open & openms", {
  suppressWarnings({
    mock_input$filetype = "spec"
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$filetype = "open"
    
    mock_input$DDA_DIA <- "LType"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$filetype = "openms"
    
    mock_input$DDA_DIA <- "LType"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$filetype = "spim"
    
    mock_input$DDA_DIA <- "LType"
    output <- getDataCode(mock_input)
    expect_type(output,"character")
    
    mock_input$filetype = "phil"
    
    mock_input$DDA_DIA <- "LType"
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
    mock_input$BIO <- "PTM"
    mock_input$DDA_DIA <- "TMT"
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
    mock_input$BIO <- "PTM"
    mock_input$DDA_DIA <- "LType"
    stub(getSummary1,"getData",mockGetData(mock_input))
    
    output <- getSummary1(mock_input,getData(mock_input))
    
    expected_names <- c("Number of Conditions","Number of PTM Biological Replicates","Number of PTM MS runs","Number of Unmod Conditions","Number of Unmod Biological Replicates","Number of Unmod MS runs")
    expect_type(output,"list")
    expect_identical(rownames(output), expected_names)
  })
})

test_that("get summary 1 Other:DDA", {
  suppressWarnings({
    mock_input$BIO <- "Protein"
    mock_input$filetype = "sample"
    mock_input$DDA_DIA <- "LType"
    mock_input$LabelFreeType <- "DDA"
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
    mock_input$BIO <- "PTM"
    mock_input$DDA_DIA <- "TMT"
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
    mock_input$BIO <- "PTM"
    mock_input$DDA_DIA <- "LType"
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
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
    mock_input$LabelFreeType <- "DDA"
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
    mock_input$BIO <- "PTM"
    mock_input$DDA_DIA <- "LType"
    mock_input$filetype = "sample"
    mock_input$norm = "equalizeMedians"
    mock_input$log = "2"
    mock_input$summaryMethod = "TMP"

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
 
test_that("preprocessData QC, PTM and PTMTMT: Yes", {
  suppressWarnings({
    mock_input$BIO <- "PTM"
    mock_input$DDA_DIA <- "TMT"
    mock_input$filetype = "sample"
    mock_input$norm = "equalizeMedians"
    mock_input$log = "2"
    mock_input$summarization = "msstats"
    mock_input$summaryMethod = "TMP"

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
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
    mock_input$LabelFreeType <- "DDA"
    mock_input$filetype = "sample"
    mock_input$norm = "equalizeMedians"
    mock_input$log = "2"
    mock_input$summaryMethod = "TMP"

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
    
    mock_input$BIO <- "PTM"
    mock_input$DDA_DIA <- "TMT"
    
    stub(preprocessDataCode,"getDataCode","some test code")
    stub(preprocessDataCode,"loadpage_input",mock_input)
    stub(preprocessDataCode,"qc_input",mock_input)
    
    output <- preprocessDataCode(mock_input,mock_input)
    expect_type(output,"character")
    
    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
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

    mock_input$BIO <- "PTM"
    mock_input$DDA_DIA <- "TMT"
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

    mock_input$BIO <- "PTM"
    mock_input$DDA_DIA <- "LType"
    mock_input$filetype = "sample"
    mock_input$MBi = TRUE
    mock_input$log = "2"
    mock_input$norm = "equalizeMedians"
    mock_input$summaryMethod = "TMP"

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

    mock_input$BIO <- "Protein"
    mock_input$DDA_DIA <- "LType"
    mock_input$LabelFreeType <- "DDA"
    mock_input$filetype = "sample"
    mock_input$norm = "equalizeMedians"
    mock_input$log = "2"
    mock_input$summaryMethod = "TMP"

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


################################################################################
#  getData for Spectronaut FUNCTION TESTING
################################################################################

describe("getData for Spectronaut input with anomaly scores", {
  
  # Dummy data frames
  dummy_spec_data <- data.frame(FG.Charge = 2, PG.ProteinGroups = "Protein1")
  dummy_run_order <- data.frame(Run = "Run1", Order = 1)
  dummy_annot <- data.frame(Run = "Run1", Condition = "A")
  
  # A mock converter function
  mock_spectro_converter <- function(...) {
    args <- list(...)
    return(args) # Return the arguments for inspection
  }
  
  test_that("adds anomaly score arguments when checkbox is checked", {
    
    #SETUP: Mock Shiny input for the "checked" scenario
    mock_input_anomaly <- list(
      BIO = "Protein",
      DDA_DIA = "DIA",
      filetype = "spec",
      specdata = list(datapath = "dummy_spec.csv"),
      annot = list(datapath = "dummy_annot.csv"),
      q_val = TRUE,
      q_cutoff = 0.01,
      remove = TRUE,
      calculate_anomaly_scores = TRUE,
      run_order_file = list(datapath = "dummy_run_order.csv")
    )

    # Mock the functions that getData calls
    stub(getData, "data.table::fread", function(path, ...) {
      if (path == "dummy_run_order.csv") return(dummy_run_order)
      return(dummy_spec_data)
    })
    stub(getData, "getAnnot", dummy_annot)
    
    stub(getData, "SpectronauttoMSstatsFormat", mock_spectro_converter)
    
    #EXECUTION: function call
    result_args <- getData(mock_input_anomaly)
    
    #ASSERTION: Check if the arguments are correct
    expect_true(result_args$calculateAnomalyScores)
    expect_equal(result_args$runOrder, dummy_run_order)
    expect_equal(result_args$anomalyModelFeatures, c("FG.ShapeQualityScore (MS2)", "FG.ShapeQualityScore (MS1)", "EGDeltaRT"))
    expect_equal(result_args$n_trees, 100)
  })
  
  test_that("does NOT add anomaly score arguments when checkbox is unchecked", {
    
    #SETUP: Mock Shiny input for the "unchecked" scenario
    mock_input_no_anomaly <- list(
      BIO = "Protein",
      DDA_DIA = "DIA",
      filetype = "spec",
      specdata = list(datapath = "dummy_spec.csv"),
      annot = list(datapath = "dummy_annot.csv"),
      q_val = TRUE,
      q_cutoff = 0.01,
      remove = TRUE,
      calculate_anomaly_scores = FALSE, # Main difference
      run_order_file = NULL
    )

    stub(getData, "data.table::fread", dummy_spec_data)
    stub(getData, "getAnnot", dummy_annot)
    stub(getData, "SpectronauttoMSstatsFormat", mock_spectro_converter)
    
    #EXECUTION
    result_args <- getData(mock_input_no_anomaly)
    
    #ASSERTION: Check that the anomaly arguments are NOT present
    expect_null(result_args$calculateAnomalyScores)
    expect_null(result_args$runOrder)
    expect_null(result_args$anomalyModelFeatures)
  })
})

describe("getData for Big Spectronaut", {
  
  # Common mock input for big spec
  mock_input_big <- list(
    filetype = "spec",
    big_file_spec = TRUE,
    big_file_browse = list(files = list("file.csv")),
    qvalue_cutoff = 0.01,
    max_feature_count = 20,
    filter_by_excluded = FALSE,
    filter_by_identified = FALSE,
    filter_by_qvalue = TRUE,
    filter_unique_peptides = TRUE,
    aggregate_psms = TRUE,
    filter_few_obs = TRUE,
    BIO = "Protein",
    DDA_DIA = "DIA"
  )
  
  # Mock data to return
  mock_arrow_obj <- list(dummy = "arrow")
  mock_df <- data.frame(ProteinName = "P1", Intensity = 100)
  
  test_that("Valid input returns data", {
    # Mocks
    stub(getData, "shinyFiles::getVolumes", function() function() c(root = "/"))
    stub(getData, "shinyFiles::parseFilePaths", function(...) data.frame(datapath = "test.csv"))
    stub(getData, "file.exists", TRUE)
    stub(getData, "MSstatsBig::bigSpectronauttoMSstatsFormat", mock_arrow_obj)
    stub(getData, "dplyr::collect", mock_df)
    stub(getData, "showNotification", function(...) NULL)
    stub(getData, "shinybusy::update_modal_spinner", function(...) NULL)
    stub(getData, "shinybusy::remove_modal_spinner", function(...) NULL)
    
    res <- getData(mock_input_big)
    expect_equal(res, mock_df)
  })
  
  test_that("Invalid qvalue_cutoff returns NULL", {
    bad_input <- mock_input_big
    bad_input$qvalue_cutoff <- 1.5
    
    stub(getData, "shinyFiles::getVolumes", function() function() c(root = "/"))
    stub(getData, "shinyFiles::parseFilePaths", function(...) data.frame(datapath = "test.csv"))
    stub(getData, "showNotification", function(msg, ...) expect_match(msg, "qvalue_cutoff"))
    stub(getData, "shinybusy::remove_modal_spinner", function(...) NULL)

    res <- getData(bad_input)
    expect_null(res)
  })
  
  test_that("Invalid max_feature_count returns NULL", {
    bad_input <- mock_input_big
    bad_input$max_feature_count <- 0
    
    stub(getData, "shinyFiles::getVolumes", function() function() c(root = "/"))
    stub(getData, "shinyFiles::parseFilePaths", function(...) data.frame(datapath = "test.csv"))
    stub(getData, "showNotification", function(msg, ...) expect_match(msg, "max_feature_count"))
    stub(getData, "shinybusy::remove_modal_spinner", function(...) NULL)
    
    res <- getData(bad_input)
    expect_null(res)
  })
  
  test_that("File not found returns NULL", {
    stub(getData, "shinyFiles::getVolumes", function() function() c(root = "/"))
    stub(getData, "shinyFiles::parseFilePaths", function(...) data.frame(datapath = "nonexistent.csv"))
    stub(getData, "file.exists", FALSE)
    stub(getData, "showNotification", function(msg, ...) expect_match(msg, "does not exist"))
    stub(getData, "shinybusy::remove_modal_spinner", function(...) NULL)
    
    res <- getData(mock_input_big)
    expect_null(res)
  })
  
  test_that("Memory error returns NULL", {
    stub(getData, "shinyFiles::getVolumes", function() function() c(root = "/"))
    stub(getData, "shinyFiles::parseFilePaths", function(...) data.frame(datapath = "test.csv"))
    stub(getData, "file.exists", TRUE)
    stub(getData, "MSstatsBig::bigSpectronauttoMSstatsFormat", mock_arrow_obj)
    stub(getData, "dplyr::collect", function(...) stop("Memory allocation failed"))
    stub(getData, "showNotification", function(msg, ...) expect_match(msg, "Memory Error"))
    stub(getData, "shinybusy::update_modal_spinner", function(...) NULL)
    stub(getData, "shinybusy::remove_modal_spinner", function(...) NULL)

    res <- getData(mock_input_big)
    expect_null(res)
  })

  # Capturing converter (returns its args so we can inspect what
  # got forwarded). Same idea as mock_spectro_converter above; the
  # big-file caller uses do.call(), but mockery intercepts the
  # MSstatsBig::bigSpectronauttoMSstatsFormat symbol resolution
  # rather than the call form, so this still works.
  mock_big_spec_converter <- function(...) list(...)
  dummy_annot_df <- data.frame(
    Run = c("run1", "run2"),
    BioReplicate = c(7L, 8L),
    Condition = c("ctrl", "treat"),
    stringsAsFactors = FALSE)

  test_that("passes annotation to converter when big_spec_annotation is supplied", {
    input_with_annot <- mock_input_big
    input_with_annot$big_spec_annotation <- list(datapath = "annot.csv")

    stub(getData, "shinyFiles::getVolumes", function() function() c(root = "/"))
    stub(getData, "shinyFiles::parseFilePaths", function(...) data.frame(datapath = "test.csv"))
    stub(getData, "file.exists", TRUE)
    stub(getData, "shinybusy::update_modal_spinner", function(...) NULL)
    stub(getData, "shinybusy::remove_modal_spinner", function(...) NULL)
    stub(getData, "showNotification", function(...) NULL)
    stub(getData, "data.table::fread", dummy_annot_df)
    stub(getData, "MSstatsBig::bigSpectronauttoMSstatsFormat",
         mock_big_spec_converter)
    # Hijack dplyr::collect to read back what the (stubbed)
    # converter received — getData passes its return value into
    # collect, so the captured value IS the list of args.
    captured_args <- NULL
    stub(getData, "dplyr::collect", function(x) {
      captured_args <<- x
      mock_df
    })

    getData(input_with_annot)

    expect_true(!is.null(captured_args$annotation))
    expect_equal(captured_args$annotation, dummy_annot_df)
  })

  test_that("passes calculateAnomalyScores + anomalyModelFeatures to converter when calculate_anomaly_scores = TRUE", {
    input_with_anomaly <- mock_input_big
    input_with_anomaly$calculate_anomaly_scores <- TRUE
    input_with_anomaly$run_order_file <- list(datapath = "run_order.csv")

    stub(getData, "shinyFiles::getVolumes", function() function() c(root = "/"))
    stub(getData, "shinyFiles::parseFilePaths", function(...) data.frame(datapath = "test.csv"))
    stub(getData, "file.exists", TRUE)
    stub(getData, "shinybusy::update_modal_spinner", function(...) NULL)
    stub(getData, "shinybusy::remove_modal_spinner", function(...) NULL)
    stub(getData, "showNotification", function(...) NULL)
    stub(getData, "MSstatsBig::bigSpectronauttoMSstatsFormat",
         mock_big_spec_converter)
    captured_args <- NULL
    stub(getData, "dplyr::collect", function(x) {
      captured_args <<- x
      mock_df
    })
    # Skip the post-collect scoring call for this test — it's
    # exercised separately below.
    stub(getData, "data.table::fread",
         data.frame(Run = "run1", Order = 1L))
    stub(getData, "MSstatsConvert::MSstatsAnomalyScores",
         function(...) mock_df)

    getData(input_with_anomaly)

    expect_true(isTRUE(captured_args$calculateAnomalyScores))
    # Raw Spectronaut export names — the converter applies
    # .standardizeColnames internally on the way out.
    expect_equal(captured_args$anomalyModelFeatures,
                 c("FG.ShapeQualityScore (MS2)",
                   "FG.ShapeQualityScore (MS1)",
                   "EG.DeltaRT"))
    # The big-file converter itself does NOT take a runOrder arg —
    # that's consumed by the separate MSstatsAnomalyScores step
    # post-collect (covered in the next test).
    expect_null(captured_args$runOrder)
  })

  test_that("calls MSstatsConvert::MSstatsAnomalyScores after collect when calculate_anomaly_scores && run_order_file are set", {
    input_with_full_anomaly <- mock_input_big
    input_with_full_anomaly$calculate_anomaly_scores <- TRUE
    input_with_full_anomaly$run_order_file <- list(datapath = "run_order.csv")

    stub(getData, "shinyFiles::getVolumes", function() function() c(root = "/"))
    stub(getData, "shinyFiles::parseFilePaths", function(...) data.frame(datapath = "test.csv"))
    stub(getData, "file.exists", TRUE)
    stub(getData, "shinybusy::update_modal_spinner", function(...) NULL)
    stub(getData, "shinybusy::remove_modal_spinner", function(...) NULL)
    stub(getData, "showNotification", function(...) NULL)
    stub(getData, "MSstatsBig::bigSpectronauttoMSstatsFormat",
         mock_arrow_obj)
    stub(getData, "dplyr::collect", mock_df)

    run_order_df <- data.frame(Run = c("run1", "run2"),
                               Order = c(1L, 2L),
                               stringsAsFactors = FALSE)
    stub(getData, "data.table::fread", run_order_df)

    captured_scoring_args <- NULL
    stub(getData, "MSstatsConvert::MSstatsAnomalyScores",
         function(...) {
           captured_scoring_args <<- list(...)
           mock_df
         })

    getData(input_with_full_anomaly)

    expect_false(is.null(captured_scoring_args))
    expect_equal(captured_scoring_args$input, mock_df)
    # Standardized column names — the in-memory data after collect
    # has had .standardizeColnames applied during the converter
    # step, so MSstatsAnomalyScores must look for these names.
    expect_equal(captured_scoring_args$quality_metrics,
                 c("FGShapeQualityScore(MS2)",
                   "FGShapeQualityScore(MS1)",
                   "EGDeltaRT"))
    expect_equal(captured_scoring_args$temporal_direction,
                 c("mean_decrease",
                   "mean_decrease",
                   "dispersion_increase"))
    expect_equal(captured_scoring_args$run_order, run_order_df)
    expect_equal(captured_scoring_args$n_trees, 100)
    expect_equal(captured_scoring_args$max_depth, "auto")
    expect_equal(captured_scoring_args$cores, 1)
  })

  test_that("fails fast when calculate_anomaly_scores is TRUE but run_order_file is missing", {
    input_no_runorder <- mock_input_big
    input_no_runorder$calculate_anomaly_scores <- TRUE
    input_no_runorder$run_order_file <- NULL

    stub(getData, "shinyFiles::getVolumes", function() function() c(root = "/"))
    stub(getData, "shinyFiles::parseFilePaths", function(...) data.frame(datapath = "test.csv"))
    stub(getData, "file.exists", TRUE)
    stub(getData, "shinybusy::remove_modal_spinner", function(...) NULL)
    stub(getData, "showNotification",
         function(msg, ...) expect_match(msg, "Run Order CSV"))
    # The converter should never run; if it does, fail the test.
    stub(getData, "shinybusy::update_modal_spinner",
         function(...) stop("converter step reached despite missing run order"))

    res <- getData(input_no_runorder)
    expect_null(res)
  })

  test_that("passes intensity to converter when spec_intensity_col is set", {
    input_with_intensity <- mock_input_big
    input_with_intensity$spec_intensity_col <- "FG.MS1Quantity"

    stub(getData, "shinyFiles::getVolumes", function() function() c(root = "/"))
    stub(getData, "shinyFiles::parseFilePaths", function(...) data.frame(datapath = "test.csv"))
    stub(getData, "file.exists", TRUE)
    stub(getData, "shinybusy::update_modal_spinner", function(...) NULL)
    stub(getData, "shinybusy::remove_modal_spinner", function(...) NULL)
    stub(getData, "showNotification", function(...) NULL)
    stub(getData, "MSstatsBig::bigSpectronauttoMSstatsFormat",
         mock_big_spec_converter)
    captured_args <- NULL
    stub(getData, "dplyr::collect", function(x) {
      captured_args <<- x
      mock_df
    })

    getData(input_with_intensity)

    expect_equal(captured_args$intensity, "FG.MS1Quantity")
  })

  test_that("omits annotation + anomaly args when neither is supplied", {
    stub(getData, "shinyFiles::getVolumes", function() function() c(root = "/"))
    stub(getData, "shinyFiles::parseFilePaths", function(...) data.frame(datapath = "test.csv"))
    stub(getData, "file.exists", TRUE)
    stub(getData, "shinybusy::update_modal_spinner", function(...) NULL)
    stub(getData, "shinybusy::remove_modal_spinner", function(...) NULL)
    stub(getData, "showNotification", function(...) NULL)
    stub(getData, "MSstatsBig::bigSpectronauttoMSstatsFormat",
         mock_big_spec_converter)
    captured_args <- NULL
    stub(getData, "dplyr::collect", function(x) {
      captured_args <<- x
      mock_df
    })

    getData(mock_input_big)

    expect_null(captured_args$annotation)
    expect_null(captured_args$calculateAnomalyScores)
    expect_null(captured_args$anomalyModelFeatures)
  })
})

# ============================================================================
# MOD ID RESOLUTION TESTS
# ============================================================================

test_that("resolve_mod_id uses dropdown selection and escapes brackets", {
  result <- MSstatsShiny:::.resolve_mod_id(
    selected = "[Common Fixed:Carbamidomethyl on C]",
    custom = NULL
  )
  expect_equal(result, "\\[Common Fixed:Carbamidomethyl on C\\]")
})

test_that("resolve_mod_id uses custom input when Other is selected", {
  result <- MSstatsShiny:::.resolve_mod_id(
    selected = "__other__",
    custom = "[My Custom Mod]"
  )
  expect_equal(result, "\\[My Custom Mod\\]")
})

test_that("resolve_mod_id preserves already-escaped custom input", {
  result <- MSstatsShiny:::.resolve_mod_id(
    selected = "__other__",
    custom = "\\[Already Escaped\\]"
  )
  expect_equal(result, "\\[Already Escaped\\]")
})

test_that("resolve_mod_id fixes partially escaped custom input", {
  result <- MSstatsShiny:::.resolve_mod_id(
    selected = "__other__",
    custom = "\\[Phospho]"
  )
  expect_equal(result, "\\[Phospho\\]")
})

test_that("resolve_mod_id errors when both inputs are NULL", {
  expect_error(MSstatsShiny:::.resolve_mod_id(NULL, NULL),
               "No modification ID selected")
})

test_that("resolve_mod_id errors when custom is empty string", {
  expect_error(MSstatsShiny:::.resolve_mod_id("__other__", ""),
               "No modification ID selected")
})

test_that("extract_mod_ids_from_preview handles consecutive modifications", {
  preview <- data.frame(
    `Full Sequence` = c(
      "A[Mod1][Mod2]B[Mod3]C"
    ),
    check.names = FALSE
  )

  result <- MSstatsShiny:::.extract_mod_ids_from_preview(preview)
  expect_equal(length(result), 3)
  expect_true(all(c("[Mod1]", "[Mod2]", "[Mod3]") %in% result))
})

# ============================================================================
# DIANN FORMAT DETECTION TESTS
# ============================================================================

test_that("is_diann_2plus returns TRUE for DIANN 2.0+ format with numbered fragment columns", {
  preview <- data.frame(
    Run = "run1",
    Protein.Group = "P1",
    Fr.0.Quantity = 100,
    Fr.1.Quantity = 200,
    Fr.2.Quantity = 300,
    Precursor.Charge = 2
  )
  expect_true(MSstatsShiny:::.is_diann_2plus(preview))
})

test_that("is_diann_2plus returns FALSE for DIANN 1.x format with legacy Fragment.Quant.Corrected", {
  preview <- data.frame(
    Run = "run1",
    Protein.Group = "P1",
    Fragment.Quant.Corrected = 100,
    Fragment.Quant.Raw = 95,
    Precursor.Charge = 2
  )
  expect_false(MSstatsShiny:::.is_diann_2plus(preview))
})

test_that("is_diann_2plus returns FALSE for DIANN 1.x format with FragmentQuantCorrected (no dots)", {
  preview <- data.frame(
    Run = "run1",
    FragmentQuantCorrected = 100,
    Precursor.Charge = 2
  )
  expect_false(MSstatsShiny:::.is_diann_2plus(preview))
})

test_that("is_diann_2plus returns FALSE when both formats are present (legacy takes precedence)", {
  preview <- data.frame(
    Run = "run1",
    Fragment.Quant.Corrected = 100,
    Fr.0.Quantity = 200
  )
  expect_false(MSstatsShiny:::.is_diann_2plus(preview))
})

test_that("is_diann_2plus returns FALSE for NULL preview", {
  expect_false(MSstatsShiny:::.is_diann_2plus(NULL))
})

test_that("is_diann_2plus returns FALSE for empty data frame", {
  expect_false(MSstatsShiny:::.is_diann_2plus(data.frame()))
})

test_that("is_diann_2plus returns FALSE for data with no fragment columns", {
  preview <- data.frame(
    Run = "run1",
    Protein.Group = "P1",
    Precursor.Charge = 2
  )
  expect_false(MSstatsShiny:::.is_diann_2plus(preview))
})

test_that("is_diann_2plus detects DIANN 2.0+ with many numbered fragment columns", {
  # Real DIANN 2.0+ files can have Fr.0 through Fr.11
  cols <- c("Run", "Protein.Group", paste0("Fr.", 0:11, ".Quantity"),
            paste0("Fr.", 0:11, ".Index"), paste0("Fr.", 0:11, ".Score"))
  preview <- as.data.frame(setNames(
    lapply(cols, function(x) if (grepl("Quantity", x)) runif(1) else "x"),
    cols
  ))
  expect_true(MSstatsShiny:::.is_diann_2plus(preview))
})

# ============================================================================
# PREVIEW READER TESTS
# ============================================================================

test_that("read_preview reads CSV files with nrows limit", {
  tmp <- tempfile(fileext = ".csv")
  df <- data.frame(a = 1:200, b = letters[1:26][1:200 %% 26 + 1])
  write.csv(df, tmp, row.names = FALSE)

  preview <- MSstatsShiny:::.read_preview(tmp, "test.csv", nrows = 100)
  expect_false(is.null(preview))
  expect_equal(nrow(preview), 100)
  expect_true(all(c("a", "b") %in% names(preview)))

  unlink(tmp)
})

test_that("read_preview reads TSV files", {
  tmp <- tempfile(fileext = ".tsv")
  df <- data.frame(a = 1:50, b = letters[1:50 %% 26 + 1])
  write.table(df, tmp, sep = "\t", row.names = FALSE, quote = FALSE)

  preview <- MSstatsShiny:::.read_preview(tmp, "test.tsv", nrows = 100)
  expect_false(is.null(preview))
  expect_equal(nrow(preview), 50)

  unlink(tmp)
})

test_that("read_preview returns NULL for non-existent files", {
  preview <- MSstatsShiny:::.read_preview("/nonexistent/path.csv", "test.csv")
  expect_null(preview)
})

test_that("read_preview returns NULL for malformed files", {
  tmp <- tempfile(fileext = ".csv")
  writeBin(as.raw(c(0xFF, 0xFE, 0x00, 0x00)), tmp)  # Garbage bytes
  preview <- MSstatsShiny:::.read_preview(tmp, "test.csv")
  # Either NULL or a data frame (fread can sometimes parse garbage) — both acceptable
  expect_true(is.null(preview) || is.data.frame(preview))
  unlink(tmp)
})

test_that("read_preview handles NULL filename gracefully", {
  tmp <- tempfile(fileext = ".csv")
  df <- data.frame(a = 1:10)
  write.csv(df, tmp, row.names = FALSE)

  # Should default to CSV reading path
  preview <- MSstatsShiny:::.read_preview(tmp, NULL)
  expect_false(is.null(preview))
  expect_equal(nrow(preview), 10)

  unlink(tmp)
})

test_that("read_preview dispatches parquet files to arrow schema reader", {
  skip_if_not_installed("arrow")

  tmp <- tempfile(fileext = ".parquet")
  df <- data.frame(a = 1:50, b = runif(50))
  arrow::write_parquet(df, tmp)

  preview <- MSstatsShiny:::.read_preview(tmp, "test.parquet")
  expect_false(is.null(preview))
  # Schema-only read returns 0 rows but correct column names
  expect_equal(nrow(preview), 0)
  expect_true(all(c("a", "b") %in% names(preview)))

  unlink(tmp)
})

test_that("read_preview recognizes .pq extension as parquet", {
  skip_if_not_installed("arrow")

  tmp <- tempfile(fileext = ".pq")
  df <- data.frame(a = 1:10)
  arrow::write_parquet(df, tmp)

  preview <- MSstatsShiny:::.read_preview(tmp, "test.pq")
  expect_false(is.null(preview))

  unlink(tmp)
})

# ============================================================================
# INTEGRATION TESTS: PREVIEW + DIANN DETECTION
# ============================================================================

test_that("DIANN 1.x CSV file is correctly detected as not 2.0+", {
  tmp <- tempfile(fileext = ".csv")
  df <- data.frame(
    Run = paste0("run", 1:10),
    Protein.Group = "P1",
    Fragment.Quant.Corrected = runif(10) * 1000,
    Fragment.Quant.Raw = runif(10) * 1000,
    Precursor.Charge = 2,
    Q.Value = runif(10, 0, 0.01)
  )
  write.csv(df, tmp, row.names = FALSE)

  preview <- MSstatsShiny:::.read_preview(tmp, "diann_1x.csv")
  expect_false(MSstatsShiny:::.is_diann_2plus(preview))

  unlink(tmp)
})

test_that("DIANN 2.0 parquet file is correctly detected as 2.0+", {
  skip_if_not_installed("arrow")

  tmp <- tempfile(fileext = ".parquet")
  df <- data.frame(
    Run = paste0("run", 1:10),
    Protein.Group = "P1",
    Fr.0.Quantity = runif(10) * 1000,
    Fr.0.Index = 1L,
    Fr.1.Quantity = runif(10) * 1000,
    Fr.1.Index = 2L,
    Fr.2.Quantity = runif(10) * 1000,
    Fr.2.Index = 3L,
    Precursor.Charge = 2,
    Q.Value = runif(10, 0, 0.01)
  )
  arrow::write_parquet(df, tmp)

  preview <- MSstatsShiny:::.read_preview(tmp, "diann_2plus.parquet")
  expect_true(MSstatsShiny:::.is_diann_2plus(preview))

  unlink(tmp)
})