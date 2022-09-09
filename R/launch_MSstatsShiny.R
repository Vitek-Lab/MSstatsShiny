#' Run MSstatsShiny Application
#' 
#' Main function to run MSstatsShiny. All other functions in this package are 
#' run automatically.
#' 
#' @export
#' @importFrom shiny runApp
#' @return Running Shiny Application
#' @examples
#' 
#' #launch_MSstatsShiny()
launch_MSstatsShiny = function(){
  
  appDir <- system.file("MSstatsShiny", package = "MSstatsShiny")
  if (appDir == "") {
    stop("Could not find application folder. Try re-installing `MSstatsShiny`.", 
         call. = FALSE)
  }
  
  runApp(appDir, display.mode = "normal")
  
}