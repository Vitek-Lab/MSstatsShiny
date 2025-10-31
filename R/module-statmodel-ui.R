#' Statmodel UI module for statistical inference UI.
#'
#' This function sets up the Statmodel UI where it consists of several, 
#' options for users to select and upload files.
#'
#' @param id namespace prefix for the module
#' 
#' @return This function returns nothing, as it sets up the Statmodel UI
#'
#' @export
#' @examples
#' NA
#' 
statmodelUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      create_custom_styles(),
      use_busy_spinner(spin = "fading-circle"),
      create_header_section(),
      
      sidebarPanel(
        create_contrast_section(ns),
        create_modeling_section(ns),
        create_visualization_section(ns)
      ),
      
      fluidRow(
        column(7, 
               create_results_section(ns)
        )
      )
    )
  )
}