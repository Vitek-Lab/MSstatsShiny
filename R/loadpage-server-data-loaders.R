# ============================================================================
# Loadpage — data-loading reactives + download MSstats handler + summaries
# ============================================================================
#
# Extracted from R/module-loadpage-server.R by the Phase 2 server split.
# Pure cut-and-paste: no behavior change, no reactivity timing change, no
# input-ID renames. Owns:
#   - 11 single-file wrapper reactives (`get_annot`, `get_annot1/2/3`,
#     `get_evidence`, `get_evidence2`, `get_global`, `get_proteinGroups`,
#     `get_proteinGroups2`, `get_FragSummary`, `get_peptideSummary`,
#     `get_protSummary`, `get_maxq_ptm_sites`)
#   - the lynchpin `get_data` eventReactive (triggered on `proceed1`)
#   - the download_msstats_format downloadHandler + its enable/disable
#     observers
#   - `get_data_code` (triggered on `calculate`)
#   - `get_summary1`, `get_summary2` (triggered on `proceed1`)
#
# Returns a named list of reactives the summary helper and the orchestrator
# (for the public return value) read.


#' Register the loadpage data-loading reactives + download handler.
#'
#' @param input   the Shiny module's `input` object
#' @param output  the Shiny module's `output` object
#' @param session the Shiny module's `session`
#' @return        named list with `get_data`, `get_annot`, `get_summary1`,
#'                `get_summary2`, `get_data_code` (and the other single-file
#'                wrappers if the orchestrator or any future helper needs
#'                them)
#' @noRd
register_loadpage_data_loaders <- function(input, output, session) {

  get_annot <- eventReactive(input$proceed1, {
    getAnnot(input)
  })

  get_annot1 <- reactive({
    getAnnot1(input)
  })

  get_annot2 <- reactive({
    getAnnot2(input)
  })

  get_annot3 <- reactive({
    getAnnot3(input)
  })

  get_evidence <- reactive({
    getEvidence(input)
  })

  get_evidence2 <- reactive({
    getEvidence2(input)
  })

  get_global <- reactive({
    getGlobal(input)
  })

  get_proteinGroups <- reactive({
    getProteinGroups(input)
  })

  get_proteinGroups2 <- reactive({
    getProteinGroups2(input)
  })

  get_FragSummary <- reactive({
    getFragSummary(input)
  })

  get_peptideSummary <- reactive({
    getPeptideSummary(input)
  })

  get_protSummary <- reactive({
    getProtSummary(input)
  })

  get_maxq_ptm_sites <- reactive({
    getMaxqPtmSites(input)
  })

  get_data <- eventReactive(input$proceed1, {
    tryCatch(
      getData(input),
      error = function(e) {
        tryCatch(remove_modal_spinner(), error = function(e2) NULL)
        showNotification(
          paste("Failed to load data:", conditionMessage(e)),
          type = "error", duration = 12)
        NULL
      }
    )
  })

  observeEvent(input$proceed1, {
    shinyjs::disable("download_msstats_format")
  })

  observeEvent(get_data(), {
    req(get_data())
    shinyjs::enable("download_msstats_format")
  })

  output$download_msstats_format <- downloadHandler(
    filename = function() {
      data <- get_data()
      if (inherits(data, "data.frame")) {
        paste0("MSstats_format-", Sys.Date(), ".csv")
      } else {
        paste0("MSstats_format-", Sys.Date(), ".zip")
      }
    },
    content = function(file) {
      tryCatch({
        data <- get_data()
        if (inherits(data, "data.frame")) {
          data.table::fwrite(data, file)
        } else {
          tmp_dir <- tempfile("msstats_format_")
          dir.create(tmp_dir)
          on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
          tmp_files <- character()
          for (nm in names(data)) {
            tbl <- data[[nm]]
            if (is.null(tbl)) next
            if (NROW(tbl) == 0L) next
            tmp_path <- file.path(tmp_dir, paste0(nm, ".csv"))
            data.table::fwrite(tbl, tmp_path)
            tmp_files <- c(tmp_files, tmp_path)
          }
          if (length(tmp_files) == 0L) {
            stop("No non-empty tables available to export.")
          }
          utils::zip(zipfile = file, files = tmp_files, flags = "-j")
        }
      }, error = function(e) {
        writeLines(paste("Failed to export MSstats format:", conditionMessage(e)), file)
      })
    }
  )

  get_data_code <- eventReactive(input$calculate, {
    getDataCode(input)
  })

  get_summary1 <- eventReactive(input$proceed1, {
    getSummary1(input, get_data(), get_annot())
  })

  get_summary2 <- eventReactive(input$proceed1, {
    getSummary2(input, get_data())
  })

  list(
    get_annot = get_annot,
    get_annot1 = get_annot1,
    get_annot2 = get_annot2,
    get_annot3 = get_annot3,
    get_evidence = get_evidence,
    get_evidence2 = get_evidence2,
    get_global = get_global,
    get_proteinGroups = get_proteinGroups,
    get_proteinGroups2 = get_proteinGroups2,
    get_FragSummary = get_FragSummary,
    get_peptideSummary = get_peptideSummary,
    get_protSummary = get_protSummary,
    get_maxq_ptm_sites = get_maxq_ptm_sites,
    get_data = get_data,
    get_data_code = get_data_code,
    get_summary1 = get_summary1,
    get_summary2 = get_summary2
  )
}
