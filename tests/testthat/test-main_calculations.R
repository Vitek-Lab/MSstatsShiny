library(testthat)
library(mockery)

test_that("lf_summarization_loop calls the correct summarizer for 'linear' method", {
  # 1. Create mock objects for the summarization functions
  mock_linear <- mock()
  mock_tmp <- mock()
  
  # 2. Stub all the heavy preprocessing functions to return simple data frames
  #    and the summarization functions to use our mocks.
  stub(lf_summarization_loop, "makePeptidesDictionary", list())
  stub(lf_summarization_loop, "MSstatsPrepareForDataProcess", data.frame(PROTEIN = "p1"))
  stub(lf_summarization_loop, "MSstatsNormalize", data.frame(PROTEIN = "p1"))
  stub(lf_summarization_loop, "MSstatsMergeFractions", data.frame(PROTEIN = "p1"))
  stub(lf_summarization_loop, "MSstatsHandleMissing", data.frame(PROTEIN = "p1"))
  stub(lf_summarization_loop, "MSstatsSelectFeatures", data.frame(PROTEIN = "p1"))
  stub(lf_summarization_loop, "getProcessed", data.frame(PROTEIN = "p1"))
  stub(lf_summarization_loop, "MSstatsPrepareForSummarization", data.frame(PROTEIN = "p1", LABEL = "L"))
  stub(lf_summarization_loop, "MSstatsSummarizationOutput", "done")
  stub(lf_summarization_loop, "MSstatsSummarizeSingleLinear", mock_linear)
  stub(lf_summarization_loop, "MSstatsSummarizeSingleTMP", mock_tmp)
  
  # 3. Run the function with minimal input
  qc_input <- list(summaryMethod = "linear", features_used = "all", MBi = TRUE, censInt = "NA", remove50 = FALSE)
  lf_summarization_loop(data.frame(), qc_input, list(), busy_indicator = FALSE)
  
  # 4. Assert that the correct function was called and the other was not.
  expect_called(mock_linear, 1)
  expect_called(mock_tmp, 0)
})

test_that("lf_summarization_loop calls the correct summarizer for 'TMP' method", {
  mock_linear <- mock()
  mock_tmp <- mock()
  
  stub(lf_summarization_loop, "makePeptidesDictionary", list())
  stub(lf_summarization_loop, "MSstatsPrepareForDataProcess", data.frame(PROTEIN = "p1"))
  stub(lf_summarization_loop, "MSstatsNormalize", data.frame(PROTEIN = "p1"))
  stub(lf_summarization_loop, "MSstatsMergeFractions", data.frame(PROTEIN = "p1"))
  stub(lf_summarization_loop, "MSstatsHandleMissing", data.frame(PROTEIN = "p1"))
  stub(lf_summarization_loop, "MSstatsSelectFeatures", data.frame(PROTEIN = "p1"))
  stub(lf_summarization_loop, "getProcessed", data.frame(PROTEIN = "p1"))
  stub(lf_summarization_loop, "MSstatsPrepareForSummarization", data.frame(PROTEIN = "p1", LABEL = "L"))
  stub(lf_summarization_loop, "MSstatsSummarizationOutput", "done")
  stub(lf_summarization_loop, "MSstatsSummarizeSingleLinear", mock_linear)
  stub(lf_summarization_loop, "MSstatsSummarizeSingleTMP", mock_tmp)
  
  qc_input <- list(summaryMethod = "TMP", features_used = "all", MBi = TRUE, censInt = "NA", remove50 = FALSE)
  lf_summarization_loop(data.frame(), qc_input, list(), busy_indicator = FALSE)
  
  expect_called(mock_tmp, 1)
  expect_called(mock_linear, 0)
})