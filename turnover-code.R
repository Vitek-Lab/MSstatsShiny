library(MSstatsConvert)
library(MSstats)
input = data.table::fread(paste0(file_path, file_name))
annotation = input %>% group_by(R.FileName, R.Fraction, R.Condition) %>% summarize()
annotation = annotation %>% rename(Run = R.FileName,
                                   Fraction = R.Fraction,
                                   Condition = R.Condition)
annotation$BioReplicate = annotation$Condition

msstats_input = SpectronauttoMSstatsFormat(
  input_proper, 
  annotation = annotation,
  intensity = "FG.MS1Quantity", 
  peptideSequenceColumn = "FG.LabeledSequence",
  heavyLabels = c("L[Leu6]"),
  filter_with_Qvalue = TRUE,
  removeFewMeasurements = FALSE,
  calculateAnomalyScores = FALSE
)

quant_data = dataProcess(msstats_input, 
                         normalization = "globalStandards", 
                         nameStandards = "unlabeled",
                         numberOfCores = 1
)