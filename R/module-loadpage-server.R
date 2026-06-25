#' Loadpage Server module for data selection and upload server.
#'
#' This function sets up the loadpage server where it consists of several
#' options for users to select and upload files. After the Phase 2 split,
#' the orchestrator below keeps only:
#'
#' \itemize{
#'   \item the module signature and `condition_metadata` reactiveVal,
#'   \item the shinyFiles browser block (it produces the
#'         `local_big_file_path` / `local_big_diann_path` reactives that the
#'         proceed-validation helper consumes, so it must remain co-located
#'         with the module's reactive scope),
#'   \item the six helper registrations in order
#'         (`register_loadpage_preview`,
#'         `register_loadpage_visibility_observers`,
#'         `register_loadpage_converter_ui`,
#'         `register_loadpage_proceed_validation`,
#'         `register_loadpage_data_loaders`,
#'         `register_loadpage_summary`),
#'   \item the final public `return(list(input, getData,
#'         getConditionMetadata))`.
#' }
#'
#' Each helper lives in its own file (`R/loadpage-server-*.R`); see those
#' files for the moved code blocks.
#'
#' @param id namespace prefix for the module
#' @param parent_session session of the main calling module
#' @param is_web_server boolean indicating if the app is running on a web server
#' @param app_template reactive (or NULL) returning the selected template
#'
#' @return input object with user selected options
#'
#' @export
#' @examples
#' NA
#'
loadpageServer <- function(id, parent_session, is_web_server = FALSE, app_template = NULL) {
  moduleServer(id, function(input, output, session) {

    condition_metadata <- reactiveVal(NULL)

    # == shinyFiles LOGIC FOR LOCAL FILE BROWSER ===============================
    # Stays in the orchestrator because the two `local_big_*_path` reactives
    # it produces are consumed by `register_loadpage_proceed_validation()`
    # below; lifting them into a helper would require an extra round-trip.
    if (!is_web_server) {
      volumes <- shinyFiles::getVolumes()()

      shinyFiles::shinyFileChoose(input, "big_file_browse",  roots = volumes, session = session)
      shinyFiles::shinyFileChoose(input, "big_diann_browse", roots = volumes, session = session)

      local_file_info <- reactive({
        req(is.list(input$big_file_browse))
        shinyFiles::parseFilePaths(volumes, input$big_file_browse)
      })

      local_diann_file_info <- reactive({
        req(is.list(input$big_diann_browse))
        shinyFiles::parseFilePaths(volumes, input$big_diann_browse)
      })

      local_big_file_path <- reactive({
        path_info <- local_file_info()
        if (nrow(path_info) > 0) path_info$datapath else NULL
      })

      local_big_diann_path <- reactive({
        path_info <- local_diann_file_info()
        if (nrow(path_info) > 0) path_info$datapath else NULL
      })

      output$specdata_big_path <- renderPrint({
        req(nrow(local_file_info()) > 0)
        cat(local_file_info()$name)
      })

      output$dianndata_big_path <- renderPrint({
        req(nrow(local_diann_file_info()) > 0)
        cat(local_diann_file_info()$name)
      })
    } else {
      local_big_file_path  <- reactive({ NULL })
      local_big_diann_path <- reactive({ NULL })
    }

    # == HELPER REGISTRATION (6 helpers, all in R/loadpage-server-*.R) =========
    #
    # Order matters only insofar as Shiny reactivity is set up at module-mount
    # time. We follow the file's original top-to-bottom layout: preview ->
    # visibility -> converter UI -> proceed validation -> data loaders ->
    # summary. The visibility + converter helpers are independent of the
    # data-loaders' return value; only the summary helper consumes it.

    register_loadpage_preview(input, output, session)

    register_loadpage_visibility_observers(input, output, session)

    register_loadpage_converter_ui(
      input, output, session,
      is_web_server = is_web_server,
      app_template  = app_template
    )

    register_loadpage_proceed_validation(
      input, session,
      local_big_file_path  = local_big_file_path,
      local_big_diann_path = local_big_diann_path
    )

    data_reactives <- register_loadpage_data_loaders(input, output, session)

    register_loadpage_summary(
      input, output, session, parent_session,
      app_template       = app_template,
      data_reactives     = data_reactives,
      condition_metadata = condition_metadata
    )

    return(list(
      input = input,
      getData = data_reactives$get_data,
      getConditionMetadata = condition_metadata
    ))
  })
}
