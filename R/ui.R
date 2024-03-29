## Uncomment out these lines to deploy on Shiny Server
# library(MSstatsShiny)
# library(shiny)
# library(shinyjs)

jsCode = '
shinyjs.init = function() {
$(document).keypress(function(e) { alert("Key pressed: " + e.which); });
  alert("fooo");
  console.log("initttttt");
  $("#tablist li a").addClass("disabled");

  $(".nav").on("click", ".disabled", function (e) {
    e.preventDefault();
    return false;
  });
}

shinyjs.enableTab = function(value) {
  $("#tablist li a[data-value=" + value + "]").removeClass("disabled");
}
'

if(!exists('currentTab') || is.null(currentTab)){
  currentTab = "Homepage"
}

css = "
.disabled {
background: #eee !important;
cursor: default !important;
color: black !important;
}
"

#' @title UI function for the MSstatsShiny app
#' @description This functions generates the UI object for MSstatsShiny app. Responsible
#' for generating 5 nav pages homepage, data upload page, data processing page, statistical
#' inference and future experiments.
#' @return UI object for shinyUI 
#' @rdname uiObject
#' @importFrom shiny NS span
#' @export
#' @examples 
#' NA
#' 
uiObject <- function() {
  ui = navbarPage(
    title = "MSstatsShiny",
    id = "tablist",
    selected = currentTab,
    
    
    tabPanel("Homepage", icon = icon("home"), homeUI("home")),
    tabPanel("1. Data Uploading",value = "Uploaddata", icon = icon("paper-plane"), loadpageUI("loadpage")),
    tabPanel("2. Data Processing", value = "DataProcessing", icon = icon("gears"), qcUI("qc")),
    tabPanel("3. Statistical Inference", value = "StatsModel", icon = icon("magic"), statmodelUI("statmodel")),
    tabPanel("4. Future Experiments", value = "Future", icon = icon("flask"), expdesUI("expdes")),
    navbarMenu("Help", icon = icon("ambulance"),
               tabPanel("Shiny Help", helpUI("help")),
               tabPanel("MSstats Vignette", msstatsHelpUI("msstatsHelp")),
               tabPanel("MSstatsTMT Vignette", msstatsTmtHelpUI("msstatsTmtHelp"))
    ),
    inverse = TRUE,
    collapsible = TRUE,
    windowTitle = "MSstatsShiny"
  )
  return(ui)
}

uiObject()