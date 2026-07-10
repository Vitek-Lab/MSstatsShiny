#' QC Server module for data processing
#'
#' This function sets up the QC server to process data based on user
#' selected inputs
#'
#' @param input input object to capture different ui element values
#' @param output to render and create elements
#' @param session session current module
#' @param parent_session session of the main calling module
#' @param loadpage_input input object from loadpage UI
#' @param get_data stored function that returns the data from loadpage
#'
#' @return input object with user selected options
#'
#' @export
#' @examples
#' NA
#'
qcServer <- function(input, output, session, parent_session, loadpage_input, get_data,
                     app_template = NULL, get_condition_metadata = NULL) {

  # ---- Core summarization reactives ----

  preprocess_data = eventReactive(input$run, {
    preprocessData(input, loadpage_input(), get_data())
  })

  # For protein turnover, re-level GROUP factor using TimeVal ordering from loadpage
  ordered_preprocess_data <- reactive({
    data <- preprocess_data()
    if (is.null(data)) return(data)
    if (!is.null(get_condition_metadata) && !is.null(get_condition_metadata())) {
      meta <- get_condition_metadata()
      meta_with_time <- meta[!is.na(meta$TimeVal), ]
      if (nrow(meta_with_time) > 0) {
        ordered_conditions <- meta_with_time$Condition[order(as.numeric(meta_with_time$TimeVal))]
        all_groups <- unique(as.character(data$FeatureLevelData$GROUP))
        remaining <- setdiff(all_groups, ordered_conditions)
        final_levels <- c(ordered_conditions, remaining)
        final_levels <- final_levels[final_levels %in% all_groups]
        data$FeatureLevelData$GROUP <- factor(data$FeatureLevelData$GROUP, levels = final_levels)
        if (!is.null(data$ProteinLevelData)) {
          data$ProteinLevelData$GROUP <- factor(data$ProteinLevelData$GROUP, levels = final_levels)
        }
      }
    }
    data
  })

  # ---- Run caption and "Next step" navigation to the statistical model page ----

  cap = eventReactive(input$run, {
    text_output = "Protein abundance have been estimated, use the tabs below to download and plot the results."
  })

  observeEvent(input$run, {
    output$submit.button = renderUI({
      ns <- session$ns
      actionButton(inputId = ns("proceed6"),label = "Next step")
    })


    })

  output$caption = renderText({
    cap()
  })

  enable("proceed6")
  observeEvent(preprocess_data(),{
    enable("proceed6")
  })

  onclick("proceed6", {
    updateTabsetPanel(session = parent_session, inputId = "tablist", selected = "StatsModel")
  })

  # ---- Tab-specific server logic ----

  register_qc_visibility_observers(input, session, loadpage_input, app_template)
  register_qc_sidebar_options(input, output, session, loadpage_input, get_data, app_template)
  register_qc_plots(input, output, session, loadpage_input, get_data,
                    preprocess_data, ordered_preprocess_data)
  register_qc_summary(input, output, session, loadpage_input, preprocess_data, app_template)
  register_qc_downloads(input, output, session, loadpage_input, preprocess_data)
  turnover_ratios <- register_qc_turnover(input, output, session, app_template, get_data,
                                          get_condition_metadata, preprocess_data)

  return(
    list(
      input = input,
      preprocessData = preprocess_data,
      turnoverRatios = turnover_ratios
    )
  )
}
