# ============================================================================
# Style and Header Functions
# ============================================================================

#' Create custom CSS styles for buttons
#' @noRd
create_custom_styles <- function() {
  tags$head(
    tags$style(HTML('#statmodel-submit4{background-color:orange}')),
    tags$style(HTML('#statmodel-clear4{background-color:orange}')),
    tags$style(HTML('#statmodel-submit3{background-color:orange}')),
    tags$style(HTML('#statmodel-clear3{background-color:orange}')),
    tags$style(HTML('#statmodel-submit1{background-color:orange}')),
    tags$style(HTML('#statmodel-clear1{background-color:orange}')),
    tags$style(HTML('#statmodel-submit2{background-color:orange}')),
    tags$style(HTML('#statmodel-clear2{background-color:orange}')),
    tags$style(HTML('#statmodel-calculate{background-color:orange}')),
    tags$style(HTML('#statmodel-plotresults{background-color:orange}')),
    tags$style(HTML('#statmodel-viewresults{background-color:orange}')),
    tags$style(HTML('#statmodel-submit{background-color:orange}')),
    tags$style(HTML('#statmodel-clear{background-color:orange}')),
    tags$link(rel = "stylesheet", type = "text/css", href = "assets/style.css")
  )
}

#' Create header section with instructions
#' @noRd
create_header_section <- function() {
  tagList(
    headerPanel("Statistical modeling and inference"),
    p("In this tab a statistical model is built in three steps:"),
    p("(i) Create a contrast matrix with the correct Group comparisons,"), 
    p("(ii) generate the model and "),
    p("(iii) view result plots."),
    p("More info ", a("here", href="https://www.rdocumentation.org/packages/MSstats/versions/3.4.0/topics/groupComparisonPlots"))
  )
}