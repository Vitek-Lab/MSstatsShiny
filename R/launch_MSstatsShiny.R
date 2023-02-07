#' Run MSstatsShiny Application
#' 
#' Main function to run MSstatsShiny. All other functions in this package are 
#' run automatically.
#' 
#' @export
#' @importFrom shiny runApp
#' @return Running Shiny Application
#' 
#' @param launch_app One of TRUE or FALSE indicating whether or not to run 
#' application. Default is TRUE.
#' @param port (optional) Specify port the application should list to.
#' @param host (optional) The IPv4 address that the application should listen on.
#' @examples
#' 
#' ## To run app set launch_app=TRUE
#' launch_MSstatsShiny(launch_app=FALSE)
#' 
launch_MSstatsShiny = function(launch_app=TRUE, 
                               port=getOption("shiny.port"), 
                               host=getOption("shiny.host", "127.0.0.1")){
  
  # appDir = system.file("MSstatsShiny", package = "MSstatsShiny")
  # if (appDir == "") {
  #   stop("Could not find application folder. Try re-installing `MSstatsShiny`.", 
  #        call. = FALSE)
  # }
  # 
  # if (launch_app){
  #   runApp(appDir, port=port, host=host)
  # }
  pkgload::load_all()
  runApp("R",port=port, host=host)
  
}