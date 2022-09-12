## Simple test to ensure package can find app directory (appDir)
expect_silent(launch_MSstatsShiny(FALSE))

## Label Free DIA Testing ------------------------------------------------------
data("example_dia_skyline", package = "MSstatsShiny")
data("example_skyline_annotation", package = "MSstatsShiny")

## Convert to MSstats data
testdata <- MSstats::SkylinetoMSstatsFormat(example_dia_skyline,
                                 annotation = example_skyline_annotation,
                                 filter_with_Qvalue = TRUE, 
                                 qvalue_cutoff = 0.01, 
                                 fewMeasurements="remove", 
                                 removeProtein_with1Feature = TRUE,
                                 use_log_file = FALSE)

## Shiny inputs
input = list()
input$norm = "equalizeMedians"
input$log = 2
input$names = NULL
input$features_used	= "all"
code_n_feat=3
input$censInt = "NA"
input$features_used	= "all"
input$MBi = TRUE
input$remove50 = FALSE
input$maxQC = 0.999
input$null = FALSE
input$null1 = FALSE
input$DDA_DIA = "LF"

## Test that calculation is correct
data("dia_skyline_summarized", package = "MSstatsShiny")
summarization_lf_test = lf_summarization_loop(testdata, busy_indicator = FALSE)
expect_equal(dia_skyline_summarized, summarization_lf_test)

## Test modeling function
comparison <- matrix(c(1, -1, 0, 0, 0, 0, 0, 0, 0, 0),nrow=1)
row.names(comparison) = "1 vs 128"
colnames(comparison) = c("1", "128", "16", "2", "256", 
                         "32", "4", "512", "64", "8")
model_lf_test = lf_model(summarization_lf_test, comparison, 
                         busy_indicator = FALSE)
data("dia_skyline_model", package = "MSstatsShiny")
expect_equal(dia_skyline_model, model_lf_test)


## TMT Testing -----------------------------------------------------------------
data(raw.pd, package = "MSstatsTMT")
data(annotation.pd, package = "MSstatsTMT")

testdata <- MSstatsTMT::PDtoMSstatsTMTFormat(input = raw.pd, 
                               annotation = annotation.pd,
                               use_log_file = FALSE
)

## Shiny inputs
input = list()
input$summarization = "msstats"
input$norm = "equalizeMedians"
input$log = 2
input$names = NULL
input$features_used	= "all"
code_n_feat=3
input$censInt = "NA"
input$features_used	= "all"
input$MBi = TRUE
input$remove50 = FALSE
input$maxQC = 0.999
input$null = FALSE
input$null1 = FALSE
input$DDA_DIA = "LF"
input$global_norm = TRUE
input$reference_norm = TRUE
input$remove_norm_channel = TRUE
input$maxQC1 = NULL

## Test that calculation is correct
summarization_tmt_test = tmt_summarization_loop(testdata, busy_indicator = FALSE)
data("tmt_pd_summarized", package = "MSstatsShiny")
expect_equal(summarization_tmt_test, tmt_pd_summarized)

## Test modeling function
comparison=matrix(c(-1,0,0,1),nrow=1)
row.names(comparison) = "1-0.125"
colnames(comparison) = c("0.125", "0.5", "0.667", "1")
input$moderated = FALSE

model_tmt_test = tmt_model(summarization_tmt_test, comparison, 
                         busy_indicator = FALSE)
data("tmt_pd_model", package = "MSstatsShiny")
expect_equal(tmt_pd_model$ComparisonResult, model_tmt_test$ComparisonResult)
expect_equal(tmt_pd_model$ModelQC, model_tmt_test$ModelQC)
