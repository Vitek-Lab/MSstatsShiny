# QC Download Data tab: enable and serve the feature- and protein-level CSV
# downloads (regular, PTM, and unmodified-protein).

#' Register the QC Download Data tab handlers.
#' @noRd
register_qc_downloads <- function(input, output, session, loadpage_input, preprocess_data) {

  observeEvent(input$run,{

    if(loadpage_input()$BIO=="PTM"){
      enable("prep_feature_level_data_csv_ptm")
      enable("prep_protein_level_data_csv_ptm")
      enable("prep_feature_level_data_csv_global_proteome")
      enable("prep_protein_level_data_csv_global_proteome")
    } else {
      enable("prep_feature_level_data_csv")
      enable("prep_protein_level_data_csv")
    }

  })

  output$prep_feature_level_data_csv = downloadHandler(
    filename = function() {
      paste("Feature_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$FeatureLevelData, file, row.names = FALSE)
    }
  )

  output$prep_feature_level_data_csv_ptm = downloadHandler(
    filename = function() {
      paste("PTM_Feature_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$PTM$FeatureLevelData, file, row.names = FALSE)
    }
  )

  output$prep_feature_level_data_csv_global_proteome = downloadHandler(
    filename = function() {
      paste("Global_Proteome_Feature_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$PROTEIN$FeatureLevelData, file, row.names = FALSE)
    }
  )

  output$prep_protein_level_data_csv = downloadHandler(
    filename = function() {
      paste("Protein_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$ProteinLevelData, file, row.names = FALSE)
    }
  )

  output$prep_protein_level_data_csv_ptm = downloadHandler(
    filename = function() {
      paste("PTM_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$PTM$ProteinLevelData, file, row.names = FALSE)
    }
  )

  output$prep_protein_level_data_csv_global_proteome = downloadHandler(
    filename = function() {
      paste("Global_Proteome_Protein_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$PROTEIN$ProteinLevelData, file, row.names = FALSE)
    }
  )
}
