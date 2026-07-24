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

  turnover_ratios <- register_qc_turnover(input, output, session, app_template, get_data,
                                          get_condition_metadata, preprocess_data)

  data_upload = register_qc_data_upload(input, output, session, loadpage_input,
                                        app_template, get_data, preprocess_data,
                                        get_condition_metadata, turnover_ratios)

  effective_preprocess_data <- data_upload$effective_preprocess_data

  # Re-level GROUP by TimeVal order for protein turnover.
  ordered_preprocess_data <- reactive({
    data <- effective_preprocess_data()
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
  # Keyed on the effective (computed OR uploaded) data so upload users, who never
  # fire input$run, still get the caption and the Next-step button.

  cap = eventReactive(effective_preprocess_data(), {
    text_output = "Data is ready. Click 'Next step' to continue to the Statistical Inference Page."
  })

  observeEvent(effective_preprocess_data(), {
    output$submit.button = renderUI({
      ns <- session$ns
      actionButton(inputId = ns("proceed6"),label = "Next step")
    })
  }, ignoreNULL = TRUE)

  output$caption = renderText({
    cap()
  })

  enable("proceed6")
  observeEvent(effective_preprocess_data(),{
    enable("proceed6")
  })

  onclick("proceed6", {
    updateTabsetPanel(session = parent_session, inputId = "tablist", selected = "StatsModel")
  })

  # ---- Tab-specific server logic ----

  register_qc_visibility_observers(input, session, loadpage_input, app_template)
  register_qc_sidebar_options(input, output, session, loadpage_input, get_data, app_template)
  register_qc_plots(input, output, session, loadpage_input, get_data,
                    effective_preprocess_data, ordered_preprocess_data)
  register_qc_summary(input, output, session, loadpage_input, effective_preprocess_data, app_template)
  register_qc_downloads(input, output, session, loadpage_input, effective_preprocess_data)

  return(
    list(
      input = input,
      preprocessData = effective_preprocess_data,
      turnoverRatios = data_upload$effective_turnover_ratios
    )
  )
}
