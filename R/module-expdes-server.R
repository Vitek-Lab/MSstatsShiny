#' Expdes Server module for future experiments
#'
#' This function sets up the Expdes server to process data based on user
#' selected inputs
#'
#' @param input input object to capture different ui element values
#' @param output to render and create elements
#' @param session session current module
#' @param parent_session session of the main calling module
#' @param loadpage_input input object from loadpage UI
#' @param qc_input input object from QC UI
#' @param statmodel_input input object from Statmodel UI
#' @param data_comparison function for group comparisons
#' 
#' @return list object with user selected options and matrix build
#'
#' @export
#' @examples
#' NA
#' 
expdesServer <- function(input, output, session,parent_session, loadpage_input, qc_input,statmodel_input,data_comparison) {
  # toggle input elements and plot
  observe({
    if (input$param == "sample") {
      disable("nsample")
      sample_x = TRUE
    }
    else {
      sample_x = input$nsample
      enable("nsample")
    }

    if (input$param == "npower") {
      disable("power")
      power_x = TRUE
    }
    else {
      power_x = input$power
      enable("power")
    }
    FDR_x = input$FDR
    FCR_x = input$desirFC
    future_exp = function(){
      exp = designSampleSize(data=data_comparison()$FittedModel,
                             desiredFC = input$desirFC,
                             FDR = FDR_x,
                             numSample = sample_x,
                             power= power_x)
    }

    output$result_plot = renderPlotly({
      designSampleSizePlots(future_exp(), isPlotly = TRUE)
    })

    #download

    output$download_future = downloadHandler(
      filename = "future_exp.pdf",
      content = function(file) {
        pdf(file)
        designSampleSizePlots(future_exp())
        dev.off()
      })

    # hover

    output$info = renderText({
      paste0(
        "hover: ", MSstatsShiny::xy_str(input$plot_hover)
      )
    })
  })

}
