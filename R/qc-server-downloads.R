# QC Download Data tab: enable and serve the feature- and protein-level CSV
# downloads (regular, PTM, and unmodified-protein).

#' Register the QC Download Data tab handlers.
#' @noRd
register_qc_downloads <- function(input, output, session, loadpage_input, preprocess_data) {

  observeEvent(input$run,{

    if(loadpage_input()$BIO=="PTM"){
      enable("prepr_csv_ptm")
      enable("summ_csv_ptm")
      enable("prepr_csv_prot")
      enable("summ_csv_prot")
    } else {
      enable("prepr_csv")
      enable("summ_csv")
    }

  })

  output$prepr_csv = downloadHandler(
    filename = function() {
      paste("Feature_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      if(loadpage_input()$DDA_DIA=='TMT'){

        write.csv(preprocess_data()$FeatureLevelData, file, row.names = FALSE)

      }
      else{

        write.csv(preprocess_data()$FeatureLevelData, file, row.names = FALSE)
      }

    }
  )

  output$prepr_csv_ptm = downloadHandler(
    filename = function() {
      paste("PTM_Feature_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$PTM$FeatureLevelData, file, row.names = FALSE)
    }
  )

  output$prepr_csv_prot = downloadHandler(
    filename = function() {
      paste("Protein_Feature_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$PROTEIN$FeatureLevelData, file, row.names = FALSE)
    }
  )

  output$summ_csv = downloadHandler(
    filename = function() {
      paste("Protein_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$ProteinLevelData, file, row.names = FALSE)
    }
  )

  output$summ_csv_ptm = downloadHandler(
    filename = function() {
      paste("PTM_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$PTM$ProteinLevelData, file, row.names = FALSE)
    }
  )

  output$summ_csv_prot = downloadHandler(
    filename = function() {
      paste("Protein_level_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(preprocess_data()$PROTEIN$ProteinLevelData, file, row.names = FALSE)
    }
  )
}
