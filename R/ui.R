library(shiny)
library(shinyBS)
library(shinyjs)
library(shinybusy)
library(DT)
library(htmltools)
library(uuid)
library(Hmisc)
library(dplyr)
library(data.table)
library(tidyr)
library(MSstats)
library(MSstatsTMT)
library(MSstatsConvert)
library(MSstatsPTM)

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
#' @examples 
#' \dontrun{
#' library(shiny)
#' shinyUI(uiObject())
#' }
#' @rdname uiObject
#' @export 
#' @import shiny shinyBS shinyjs shinybusy DT htmltools Hmisc uuid dplyr data.table tidyr MSstats MSstatsTMT MSstatsConvert MSstatsPTM
uiObject <- function() {
  ui = navbarPage(
    title = "MSstatsShiny",
    id = "tablist",
    selected = currentTab,
    
    tags$head(
      tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    }
                    h1 {
                    color: #000000;
                    }
                    "))
    ),
    
    
    useShinyjs(),
    extendShinyjs(text = jsCode,functions = c("init","enableTab")),
    tags$style(css),
    
    
    tabPanel("Homepage", icon = icon("home"), homeUI("home")),
    tabPanel("1. Data Uploading",value = "Uploaddata", icon = icon("send"), loadpageUI("loadpage")),
    tabPanel("2. Data Processing", value = "DataProcessing", icon = icon("gears"), qcUI("qc")),
    tabPanel("3. Statistical Inference", value = "StatsModel", icon = icon("magic"), statmodelUI("statmodel")),
    tabPanel("4. Future Experiments", value = "Future", icon = icon("flask"), expdesUI("expdes")),
    navbarMenu("Help", icon = icon("ambulance"),
               tabPanel("Shiny Help", helpUI("help")),
               tabPanel("MSstats Vignette", msstatsHelpUI("msstatsHelp")),
               tabPanel("MSstatsTMT Vignette", msstatsTmtHelpUI("msstatsTmtHelp"))
    ),
    inverse = T,
    collapsible = T,
    windowTitle = "MSstatsShiny"
  )
  return(ui)
}

uiObject()
# shinyUI(uiObject())