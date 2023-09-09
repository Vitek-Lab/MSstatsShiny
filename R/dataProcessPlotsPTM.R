#' Visualization for explanatory data analysis
#'
#' To illustrate the quantitative data and quality control of MS runs,
#' dataProcessPlotsPTM takes the quantitative data from dataSummarizationPTM or
#' dataSummarizationPTM_TMT to plot the following :
#' (1) profile plot (specify "ProfilePlot" in option type), to identify the
#' potential sources of variation for each protein;
#' (2) quality control plot (specify "QCPlot" in option type), to evaluate the
#' systematic bias between MS runs.
#'
#' @export
#' @import ggplot2
#' @importFrom grDevices dev.off hcl pdf
#' @importFrom gridExtra grid.arrange
#' @importFrom stringr str_match
#' @importFrom data.table data.table as.data.table melt dcast `:=` setnames copy rbindlist
#' @importFrom checkmate assertCharacter assertNumeric assertChoice
#' @importFrom MSstats theme_msstats
#' 
#' @param data name of the list with PTM and (optionally) Protein data, which
#' can be the output of the MSstatsPTM 
#' \code{\link[MSstatsPTM]{dataSummarizationPTM}} or 
#' \code{\link[MSstatsPTM]{dataSummarizationPTM_TMT}} functions.
#' @param type choice of visualization. "ProfilePlot" represents profile plot of
#'  log intensities across MS runs.
#' "QCPlot" represents box plots of log intensities across channels and MS runs.
#' @param ylimUp upper limit for y-axis in the log scale.
#' FALSE(Default) for Profile Plot and QC Plot uses the upper limit as rounded
#' off maximum of log2(intensities) after normalization + 3..
#' @param ylimDown lower limit for y-axis in the log scale. FALSE(Default) for
#' Profile Plot and QC Plot uses 0..
#' @param x.axis.size size of x-axis labeling for "Run" and "channel in Profile
#' Plot and QC Plot.
#' @param y.axis.size size of y-axis labels. Default is 10.
#' @param text.size size of labels represented each condition at the top of
#' Profile plot and QC plot. Default is 4.
#' @param text.angle angle of labels represented each condition at the top of
#' Profile plot and QC plot. Default is 0.
#' @param legend.size size of legend above Profile plot. Default is 7.
#' @param dot.size.profile size of dots in Profile plot. Default is 2.
#' @param ncol.guide number of columns for legends at the top of plot. Default
#' is 5.
#' @param width width of the saved pdf file. Default is 10.
#' @param height height of the saved pdf file. Default is 10.
#' @param ptm.title title of overall PTM QC plot
#' @param protein.title title of overall Protein QC plot
#' @param which.PTM PTM list to draw plots. List can be names of
#' PTMs or order numbers of PTMs.
#' Default is "all", which generates all plots for each protein. For QC plot,
#' "allonly" will generate one QC plot with all proteins.
#' @param which.Protein List of proteins to plot. Will plot all PTMs associated 
#' with listed Proteins. Default is NULL which will default to which.PTM.
#' @param originalPlot TRUE(default) draws original profile plots, without
#' normalization.
#' @param summaryPlot TRUE(default) draws profile plots with protein
#' summarization for each channel and MS run.
#' @param address the name of folder that will store the results. Default folder
#'  is the current working directory.
#' The other assigned folder has to be existed under the current working
#' directory.
#' An output pdf file is automatically created with the default name of
#' "ProfilePlot.pdf" or "QCplot.pdf".
#' The command address can help to specify where to store the file as well as
#' how to modify the beginning of the file name.
#' If address=FALSE, plot will be not saved as pdf file but showed in window.
#' @return plot or pdf
#' @examples
#' 
#' # QCPlot
#' dataProcessPlotsPTM(summary.data,
#'                     type = 'QCPLOT',
#'                     which.PTM = "allonly",
#'                     address = FALSE)
#'                     
#' #ProfilePlot
#' dataProcessPlotsPTM(summary.data,
#'                     type = 'PROFILEPLOT',
#'                     which.PTM = "Q9UQ80_K376",
#'                     address = FALSE)
dataProcessPlotsPTM = function(data,
                               type = 'PROFILEPLOT',
                               ylimUp = FALSE,
                               ylimDown = FALSE,
                               x.axis.size = 10,
                               y.axis.size = 10,
                               text.size = 4,
                               text.angle = 90,
                               legend.size = 7,
                               dot.size.profile = 2,
                               ncol.guide = 5,
                               width = 10,
                               height = 12,
                               ptm.title = "All PTMs",
                               protein.title = "All Proteins",
                               which.PTM = "all", 
                               which.Protein = NULL,
                               originalPlot = TRUE,
                               summaryPlot = TRUE,
                               address = "") {
  
  type = toupper(type)
  label = .check.dataProcess.plotting.data(data, type, ylimUp, ylimDown, 
                                           x.axis.size,
                                           y.axis.size, text.size, text.angle, legend.size,
                                           dot.size.profile, ncol.guide, width, height,
                                           ptm.title, protein.title, which.PTM, 
                                           originalPlot, summaryPlot, address)
  
  data.table.list = .format.data.process.plots(data, label)
  
  ## Filter for all PTMs in one protein
  if (!is.null(which.Protein)){
    
    data.table.list[[1]] = data.table.list[[1]][PROTEINNAME %in% which.Protein]
    data.table.list[[2]] = data.table.list[[2]][
      GLOBALPROTEIN %in% which.Protein]
    data.table.list[[3]] = data.table.list[[3]][PROTEINNAME %in% which.Protein]
    data.table.list[[4]] = data.table.list[[4]][
      GLOBALPROTEIN %in% which.Protein]
    
    if (sum(nrow(data.table.list[[1]]), nrow(data.table.list[[2]]), 
            nrow(data.table.list[[3]]), nrow(data.table.list[[4]])) == 0){
      msg = paste0("The protein ", which.Protein, " specified in Which.Protein",
                   " is not in the global protein run. Please specify ",
                   "individual peptides only for this Protein.")
      stop(msg)
    }
  }
  
  ## Profile plot ##
  ## ---------------
  if (type == "PROFILEPLOT") {
    if (label == 'TMT'){
      return(.profile.tmt(data.table.list, type, ylimUp, ylimDown, 
                   x.axis.size, y.axis.size,text.size,text.angle, 
                   legend.size, dot.size.profile, ncol.guide, width, 
                   height, which.PTM, originalPlot, summaryPlot, 
                   address))
    } else if (label == 'LabelFree'){
      return(.profile.lf(data.table.list, type, ylimUp, ylimDown, 
                  x.axis.size, y.axis.size,text.size,text.angle, 
                  legend.size, dot.size.profile, ncol.guide, width, 
                  height, which.PTM, originalPlot, 
                  summaryPlot, address))
    }
  }
  
  ## QC plot (Quality control plot) ##
  ## ---------------------------------
  if (type == "QCPLOT") {
    if (label == 'TMT'){
     return( .qc.tmt(data.table.list, type, ylimUp, ylimDown, width, height, 
              x.axis.size, y.axis.size, text.size, text.angle,
              which.PTM, address, ptm.title, protein.title))
    } else if (label == 'LabelFree'){
      return(.qc.lf(data.table.list, type, ylimUp, ylimDown, width, height, 
             x.axis.size, y.axis.size, text.size,
             which.PTM, address, ptm.title, protein.title))
    } 
  }
}