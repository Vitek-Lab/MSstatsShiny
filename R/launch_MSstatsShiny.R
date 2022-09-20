#' Run MSstatsShiny Application
#' 
#' Main function to run MSstatsShiny. All other functions in this package are 
#' run automatically.
#' 
#' @export
#' @importFrom shiny runApp
#' @return Running Shiny Application
#' 
#' @param launch_app One of TRUE or FALSE indicating wheter or not to run 
#' application. Default is TRUE.
#' @examples
#' 
#' ## To run app set launch_app=TRUE
#' launch_MSstatsShiny(launch_app=FALSE)
#' 
launch_MSstatsShiny = function(launch_app=TRUE){
  
  appDir = system.file("MSstatsShiny", package = "MSstatsShiny")
  if (appDir == "") {
    stop("Could not find application folder. Try re-installing `MSstatsShiny`.", 
         call. = FALSE)
  }
  
  if (launch_app){
    runApp(appDir, display.mode = "normal")
  }
  
}