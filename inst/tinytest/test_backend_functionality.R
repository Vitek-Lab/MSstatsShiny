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
summarization_lf_test = lf_summarization_loop(testdata, input, 
                                              busy_indicator = FALSE)
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

testdata <- MSstatsTMT::PDtoMSstatsTMTFormat(raw.pd, 
                               annotation.pd,
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
input$moderated = FALSE

## Test that calculation is correct
summarization_tmt_test = tmt_summarization_loop(testdata, input, 
                                                busy_indicator = FALSE)
data("tmt_pd_summarized", package = "MSstatsShiny")
expect_equal(summarization_tmt_test$ProteinLevelData, 
             tmt_pd_summarized$ProteinLevelData)

## Test modeling function
comparison=matrix(c(-1,0,0,1),nrow=1)
row.names(comparison) = "1-0.125"
colnames(comparison) = c("0.125", "0.5", "0.667", "1")

model_tmt_test = tmt_model(summarization_tmt_test, input, comparison, 
                         busy_indicator = FALSE)
data("tmt_pd_model", package = "MSstatsShiny")
expect_equal(tmt_pd_model$ComparisonResult, model_tmt_test$ComparisonResult)
expect_equal(tmt_pd_model$ModelQC, model_tmt_test$ModelQC)

## Adjustment functions --------------------------------------------------------
model = MSstatsPTM::groupComparisonPTM(MSstatsPTM::summary.data, 
                                       data.type = "LabelFree")
expect_equal(colnames(apply_adj(model$PTM.Model, model$PROTEIN.Model)), 
             c("Protein", "Label", "log2FC", "SE", "Tvalue", "DF", "pvalue", 
               "adj.pvalue", "GlobalProtein"))

## Smaller functions -----------------------------------------------------------
expect_equal(xy_str(list(x=5.0,y=2.0)), "x=5 y=2\n")

expect_error(radioTooltip())

input = list(maxQC1=10,null=FALSE, null1=FALSE, DDA_DIA="TMT")
expect_equal(QC_check(input),10)

## Plotting function -----------------------------------------------------------
data("dia_skyline_model")
expect_silent(groupComparisonPlots2(dia_skyline_model$ComparisonResult, 
                                    type="VolcanoPlot", address=FALSE))

expect_silent(groupComparisonPlots2(dia_skyline_model$ComparisonResult, 
                                    type="ComparisonPlot", address=FALSE, 
                                    which.Protein = "AQUA4SWATH_HMLangeA"))
## expect error bc only one comparison in data
expect_error(groupComparisonPlots2(dia_skyline_model$ComparisonResult, 
                                    type="Heatmap", address=FALSE))
expect_error(groupComparisonPlots2(tmt_pd_model$ComparisonResult, 
                                   type="Heatmap", address=FALSE))

expect_silent(groupComparisonPlots2(dia_skyline_model$ComparisonResult, 
                                    type="VolcanoPlot", address=FALSE,
                                    sig=.01))

## plot limits
expect_silent(groupComparisonPlots2(dia_skyline_model$ComparisonResult, 
                                    type="VolcanoPlot", address=FALSE,
                                    ylimUp=TRUE))
expect_silent(groupComparisonPlots2(dia_skyline_model$ComparisonResult, 
                                    type="VolcanoPlot", address=FALSE,
                                    ylimDown=TRUE))
expect_silent(groupComparisonPlots2(dia_skyline_model$ComparisonResult, 
                                    type="VolcanoPlot", address=FALSE,
                                    xlimUp=TRUE))
expect_silent(groupComparisonPlots2(dia_skyline_model$ComparisonResult, 
                                     type="VolcanoPlot", address=FALSE,
                                     x.axis.size = 20))
expect_silent(groupComparisonPlots2(dia_skyline_model$ComparisonResult, 
                                    type="VolcanoPlot", address=FALSE,
                                    y.axis.size = 20))
expect_silent(groupComparisonPlots2(dia_skyline_model$ComparisonResult, 
                                    type="VolcanoPlot", address=FALSE,
                                    dot.size = 20))
expect_silent(groupComparisonPlots2(dia_skyline_model$ComparisonResult, 
                                    type="VolcanoPlot", address=FALSE,
                                    ProteinName = FALSE))
expect_silent(groupComparisonPlots2(dia_skyline_model$ComparisonResult, 
                                    type="VolcanoPlot", address=FALSE,
                                    which.Comparison = "1 vs 128"))
