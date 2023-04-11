#' Run MSstatsShiny Application
#' 
#' Main function to run MSstatsShiny. All other functions in this package are 
#' run automatically.
#' 
#' @export
#' @return Running Shiny Application
#' 
#' @param launch_app One of TRUE or FALSE indicating whether or not to run 
#' application. Default is TRUE.
#' @param port (optional) Specify port the application should list to.
#' @param host (optional) The IPv4 address that the application should listen on.
#' @param testMode One of TRUE or FALSE indicating whether or not to run the
#' application in test mode. Default is FALSE.
#' @examples
#' 
#' ## To run app set launch_app=TRUE
#' \dontrun{
#' launch_MSstatsShiny(launch_app=FALSE,testMode=FALSE)
#' }
launch_MSstatsShiny = function(launch_app=TRUE, 
                               port=getOption("shiny.port"), 
                               host=getOption("shiny.host", "127.0.0.1"),testMode = FALSE){
  
  # appDir = system.file("MSstatsShiny", package = "MSstatsShiny")
  # if (appDir == "") {
  #   stop("Could not find application folder. Try re-installing `MSstatsShiny`.", 
  #        call. = FALSE)
  # }
  # 
  # if (launch_app){
  #   runApp(appDir, port=port, host=host)
  # }
  print(getwd())
  print(list.files('.'))
  path = "R/"
  shiny_test_mode <- testMode
  if (!is.null(shiny_test_mode) && shiny_test_mode) {
    path = "../../R/"
  }
  # source(paste(path,"module-home-ui.R",sep=""))
  # source(paste(path,"module-loadpage-ui.R",sep=""))
  # source(paste(path,"module-loadpage-server.R",sep=""))
  # source(paste(path,"module-expdes-ui.R",sep=""))
  # source(paste(path,"module-expdes-server.R",sep=""))
  # source(paste(path,"module-statmodel-ui.R",sep=""))
  # source(paste(path,"module-statmodel-server.R",sep=""))
  # source(paste(path,"module-qc-ui.R",sep=""))
  # source(paste(path,"module-qc-server.R",sep=""))
  # source(paste(path,"module-mssstats-help-ui.R",sep=""))
  # source(paste(path,"module-help-ui.R",sep=""))
  # source(paste(path,"module-statstmt-help-ui.R",sep=""))
  # source(paste(path,"utils.R",sep=""))
  # appDir = system.file("MSstatsShiny", package = "MSstatsShiny")
  # print(appDir)
  # appDir = "Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library/MSstatsShiny/R"
  # print('----')
  # runApp(path,port=port, host=host)
  # addResourcePath(prefix = "www", directoryPath = "./www")
  shinyApp(uiObject(),server,options = list(host=host,port=port))
  # runApp(app,host=host,port=port)
  # addResourcePath("prefix", "www")
  # runApp(shinyApp(uiObject(),server),host=host,port=port)
}
