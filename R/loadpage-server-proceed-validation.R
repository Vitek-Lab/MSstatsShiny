# ============================================================================
# Loadpage — `proceed1` button enable/disable cascade
# ============================================================================
#
# Extracted from R/module-loadpage-server.R by the Phase 2 server split.
# Pure cut-and-paste: the deeply nested `observe()` block that gates the
# Upload Data button against the active (BIO, DDA_DIA, filetype, file-upload
# state) combination is preserved verbatim. The two big-file path reactives
# (`local_big_file_path`, `local_big_diann_path`) originate in the
# shinyFiles block — they stay in the orchestrator and are passed in as
# function arguments here.


#' Register the `proceed1` enable cascade.
#'
#' @param input                the Shiny module's `input` object
#' @param session              the Shiny module's `session` (used implicitly
#'                             by `enable` / `disable` via the current
#'                             reactive domain)
#' @param local_big_file_path  reactive returning the local path of the
#'                             Spectronaut big-file selection (NULL when not
#'                             in big-file mode or on the web server)
#' @param local_big_diann_path reactive returning the local path of the
#'                             DIANN big-file selection
#' @noRd
register_loadpage_proceed_validation <- function(input, session,
                                                  local_big_file_path,
                                                  local_big_diann_path) {
  observe({
    disable("proceed1")
    if (((input$BIO == "Protein") || (input$BIO == "Peptide"))) {
      if (input$DDA_DIA == "LType") {
        if ((!is.null(input$filetype) && length(input$filetype) > 0)) {
          if (input$filetype == "sample") {
            if (!is.null(input$LabelFreeType)) {
              enable("proceed1")
            }
          } else if (input$filetype == "msstats") {
            if (!is.null(input$msstatsdata)) {
              enable("proceed1")
            }
          } else if (input$filetype == "sky") {
            if (!is.null(input$skylinedata)) {
              enable("proceed1")
            }
          } else if (input$filetype == "maxq") {
            if (!is.null(input$evidence) && !is.null(input$pGroup)) { # && !is.null(input$annot1)
              enable("proceed1")
            }
          } else if (input$filetype == "prog" || input$filetype == "PD" || input$filetype == "open" || input$filetype == "phil" || input$filetype == "meta") {
            if (!is.null(input$data)) {
              enable("proceed1")
            }
          } else if (input$filetype == "openms") {
            if (!is.null(input$data)) {
              enable("proceed1")
            }
          } else if (input$filetype == "spec") {
            spec_regular_file_ok <- !isTRUE(input$big_file_spec) && !is.null(input$specdata)
            spec_big_file_ok <- isTRUE(input$big_file_spec) && length(local_big_file_path()) > 0
            if (spec_regular_file_ok || spec_big_file_ok) {
              enable("proceed1")
            }
          } else if (input$filetype == "ump") {
            if (!is.null(input$fragSummary) && !is.null(input$peptideSummary) && !is.null(input$protSummary)) {  #&& !is.null(input$annot2)
              enable("proceed1")
            }
          } else if (input$filetype == "diann") {
            diann_regular_file_ok <- !isTRUE(input$big_file_diann) && !is.null(input$dianndata)
            diann_big_file_ok <- isTRUE(input$big_file_diann) && length(local_big_diann_path()) > 0
            if (diann_regular_file_ok || diann_big_file_ok) {
              enable("proceed1")
            }
          }
        }
      } else if (input$DDA_DIA == "TMT") {
        if ((!is.null(input$filetype) && length(input$filetype) > 0)) {
          if (input$filetype == "sample" || input$filetype == "msstats") {
            enable("proceed1")
          }
          if (input$filetype == "maxq") {
            if (!is.null(input$evidence) && !is.null(input$pGroup)) { # && !is.null(input$annot1)
              enable("proceed1")
            }
          } else if (input$filetype == "PD") {
            if (!is.null(input$data)) {
              enable("proceed1")
            }
          } else if (input$filetype == "openms") {
            if (!is.null(input$data)) {
              enable("proceed1")
            }
          } else if (input$filetype == "spmin" || input$filetype == "phil") {
            if (!is.null(input$data)) {
              enable("proceed1")
            }
          }
        }
      }

    }
    else if ((input$BIO == "PTM")) {
      if (input$DDA_DIA == "LType" || input$DDA_DIA == "TMT") {
        if ((!is.null(input$filetype) && length(input$filetype) > 0)) {
          if (input$filetype == "sample") {
            enable("proceed1")
          } else if (input$filetype == "msstats") {
            if (!is.null(input$msstatsptmdata)) {
              enable("proceed1")
            }
          } else if (input$filetype == "sky" || input$filetype == "maxq" || input$filetype == "spec" || input$filetype == "PD" || input$filetype == "meta") {
            if (!is.null(input$ptm_input) && !is.null(input$fasta)) { # && !is.null(input$ptm_annot)
              enable("proceed1")
            }
          }
          else if (input$filetype == "phil") {
            if (!is.null(input$ptmdata)) { # && !is.null(input$annotation)
              enable("proceed1")
            }
          }
        }
      }
    }
  })

  invisible(NULL)
}
