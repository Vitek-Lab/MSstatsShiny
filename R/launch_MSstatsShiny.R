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
#' \dontrun{
#' ## To run app set launch_app=TRUE
#' launch_MSstatsShiny(launch_app=FALSE,testMode=FALSE)
#' }
launch_MSstatsShiny = function(launch_app=TRUE, 
                               port=getOption("shiny.port"), 
                               host=getOption("shiny.host", "127.0.0.1"),
                               testMode = FALSE){
  shinyApp(uiObject(),server,options = list(host=host,port=port))
}
