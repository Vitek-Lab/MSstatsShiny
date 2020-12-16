rm()
#options(shiny.autoreload = TRUE)
#options(shiny.maxRequestSize=2000*1024^2)
library(shiny)
library(shinyBS)
library(shinyjs)
#library(STRINGdb)
library(ggplot2)
library(tidyverse)
library(data.table)
library(MSstatsTMT)
library(knitr)
library(readxl)

if (FALSE) require("V8")
#library(MSnbase)

#####################################

# Global functions #

radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
                                                 $(document).ready(function() {
                                                 setTimeout(function() {
                                                 $('input', $('#", id, "')).each(function(){
                                                 if(this.getAttribute('value') == '", choice, "') {
                                                 opts = $.extend(", options, ", {html: true});
                                                 $(this.parentElement).tooltip('destroy');
                                                 $(this.parentElement).tooltip(opts);
                                                 }
                                                 })
                                                 }, 500)
                                                 });
                                                 ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}


# shinyjs.disableTab = function() {
#   vartabs = $('tablist').find('li:not(.active) a');
#   tabs.bind('click.tab', function(e) {
#     e.preventDefault();
#     return false;
#   });
#   tabs.addClass('disabled');
# }
# 
# shinyjs.enableTab = function(param) {
#   vartabs = $('tablist').find('li:not(.active):nth-child(' + param + ') a');
#   tab.unbind('click.tab');
#   tab.removeClass('disabled');
# }



 ####################################

source("panels/home-ui.R", local = T)
source("panels/loadpage-ui.R", local = T)
source("panels/qc-ui.R", local = T)
source("panels/pq-ui.R", local = T)
source("panels/statmodel-ui.R", local = T)

source("panels/expdes-ui.R", local = T)
#source("panels/analysis-ui.R", local = T)
#source("panels/clust-ui.R", local = T)
source("panels/report-ui.R", local = T)
source("panels/help-ui.R", local = T)

#########################################################################

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
  currentTab <- "Homepage"
}

css <- "
.disabled {
background: #eee !important;
cursor: default !important;
color: black !important;
}
"

ui <- navbarPage(
  title = "MSstats-Shiny",
  id = "tablist",
  selected = currentTab,
  
  tags$head(
    tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    }
                    "))
    ),
  
  useShinyjs(),
  extendShinyjs(text = jsCode,functions = c("init","enableTab")),
  tags$style(css),
  
  
  tabPanel("Homepage", icon = icon("home"), home),
  tabPanel("Upload data",value = "Uploaddata", icon = icon("send"), loadpage),
  tabPanel("Data processing",value = "DataProcessing", icon = icon("gears"), qc),
  tabPanel("Protein quantification", value = "PQ",icon = icon("calculator"), pq),
  tabPanel("Statistical model", value = "StatsModel", icon = icon("magic"), statmodel),
  tabPanel("Future experiments", value = "Future", icon = icon("flask"), expdes),
  tabPanel("Download logfile", icon = icon("download"), report),
  tabPanel("Help", icon = icon("ambulance"), help),
  inverse = T,
  collapsible = T,
  windowTitle = "Shiny-MSstats"
)

shinyUI(ui)