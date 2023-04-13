#' MSstatsShiny: An R-shiny based package for detecting differencially abundant 
#' proteins, integrated with the MSstats family of packages.
#'
#' A set of tools for detecting differentially abundant proteins in
#' shotgun mass spectrometry-based proteomic experiments. The package can handle
#' a variety of acquisition types, including label free, DDA, DIA, and TMT. The 
#' package includes tools to convert raw data from different spectral processing
#' tools, summarize feature intensities, and fit a linear mixed effects model. 
#' The GUI supports different biological queries including those targeting the 
#' global proteome and post translational modifications. Additionally the 
#' package includes functionality to plot a variety of data visualizations.
#'
#' @section functions :
#' \itemize{
#'   \item \code{\link{launch_MSstatsShiny}} : Main function to launch the 
#'   application.
#'   \item \code{\link{groupComparisonPlots2}} : Generates MSstatsShiny plots.
#'   \item \code{\link{lf_summarization_loop}} : Summarization for LF 
#'   experiments.
#'   \item \code{\link{tmt_summarization_loop}} : Summarization for TMT 
#'   experiments.
#'   \item \code{\link{lf_model}} : Modeling for LF experiments.
#'   \item \code{\link{tmt_model}} : Modeling for TMT experiments.
#' }
#' 
#' @importFrom shiny reactiveValues isolate renderText observeEvent fluidPage HTML headerPanel div img mainPanel sidebarPanel sidebarLayout tagList h1 h2 h3 h4 h5 h6 a br hr p radioButtons icon conditionalPanel fileInput checkboxInput actionButton column uiOutput numericInput tags textOutput tabsetPanel tabPanel wellPanel fluidRow selectInput downloadButton fluidPage renderUI selectizeInput observe req reactive sliderInput validate need eventReactive downloadHandler plotOutput tableOutput renderPlot renderTable updateTabsetPanel textInput updateSelectInput insertUI verbatimTextOutput renderPrint nearPoints titlePanel reactiveFileReader hideTab showTab navbarPage navbarMenu shinyUI downloadLink shinyApp
#' @importFrom shinyBS tipify bsTooltip
#' @importFrom shinyjs disabled hidden useShinyjs runjs enable disable toggleState onclick hide show js toggleClass refresh extendShinyjs
#' @importFrom shinybusy use_busy_spinner remove_modal_spinner show_modal_spinner
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom data.table copy
#' @importFrom htmltools attachDependencies
#' @importFrom uuid UUIDgenerate
#' @importFrom Hmisc describe
#' @importFrom dplyr `%>%` filter summarise n_distinct group_by ungroup select n mutate
#' @importFrom tidyr unite
#' @importFrom MSstatsConvert MSstatsLogsSettings
#' @importFrom MSstatsPTM dataProcessPlotsPTM groupComparisonPlotsPTM MaxQtoMSstatsPTMFormat
#' @importFrom utils capture.output head packageVersion read.csv read.delim write.csv
#' @importFrom stats aggregate
#' @importFrom methods is
#' 
#' @docType package
#' @name MSstatsShiny
NULL