# Loadpage converter-options panel: pure visibility predicates + their server-side show/hide observers, the TMT which.proteinid renderUI, and the Spectronaut/DIANN converter renderUIs.


# ----------------------------------------------------------------------------
# DIANN-cluster visibility predicates.
# ----------------------------------------------------------------------------

#' @noRd
loadpage_show_diann_lf_options <- function(filetype, dda_dia, big_file_diann) {
  isTRUE(filetype == "diann") &&
    isTRUE(dda_dia == "LType") &&
    !isTRUE(big_file_diann)
}

#' @noRd
loadpage_show_diann_intensity_column <- function(diann_2plus) {
  !isTRUE(diann_2plus)
}

#' @noRd
loadpage_show_diann_mbr <- function(q_val, filetype) {
  isTRUE(q_val) && isTRUE(filetype == "diann")
}

#' @noRd
loadpage_show_diann_anomaly <- function(filetype, big_file_diann) {
  isTRUE(filetype == "diann") && !isTRUE(big_file_diann)
}

#' @noRd
loadpage_show_diann_anomaly_run_order <- function(diann_calculate_anomaly_scores) {
  isTRUE(diann_calculate_anomaly_scores)
}

#' @noRd
loadpage_show_big_diann_anomaly_run_order <- function(big_diann_calculate_anomaly_scores) {
  isTRUE(big_diann_calculate_anomaly_scores)
}


# ----------------------------------------------------------------------------
# Shared / cross-converter visibility predicates. These gate panels shown under
# more than one converter — e.g. the q-value filter applies to Skyline,
# Spectronaut, and small-file DIANN — so they live here rather than under any
# single converter's section.
# ----------------------------------------------------------------------------

#' @noRd
loadpage_show_qval_filter <- function(filetype, big_file_diann) {
  isTRUE(filetype == "sky") ||
    isTRUE(filetype == "spec") ||
    (isTRUE(filetype == "diann") && !isTRUE(big_file_diann))
}

#' @noRd
loadpage_show_qval_cutoff <- function(q_val) {
  isTRUE(q_val)
}


# ----------------------------------------------------------------------------
# Visibility predicates for the remaining converter and upload panels. Each
# encodes the full ancestor chain so a nested panel hides when an ancestor does.
# ----------------------------------------------------------------------------

#' Sample dataset description (parameterized for DDA / DIA / SRM_PRM).
#' @noRd
loadpage_show_sample_dataset_description <- function(filetype, label_free_type, label_free_mode) {
  isTRUE(filetype == "sample") && isTRUE(label_free_type == label_free_mode)
}

#' The LabelFreeType radio (DDA / DIA / SRM_PRM picker) is itself shown only
#' for the sample-dataset label-free workflow.
#' @noRd
loadpage_show_sample_dataset_label_free_type_selector <- function(bio, filetype, dda_dia) {
  !isTRUE(bio == "PTM") &&
    isTRUE(filetype == "sample") &&
    isTRUE(dda_dia == "LType")
}

#' Generic `data` fileInput section (used by non-PTM 10col / prog / PD /
#' open / openms / spmin / phil / meta converters).
#' @noRd
loadpage_show_standard_quant_upload <- function(filetype, bio) {
  if (isTRUE(bio == "PTM")) return(FALSE)
  isTRUE(filetype %in% c("10col", "prog", "PD", "open", "openms",
                          "spmin", "phil", "meta"))
}

#' Generic `annot` fileInput section (Skyline / Progenesis / PD /
#' OpenSWATH / SpectroMine / FragPipe / Metamorpheus always, Spectronaut and
#' DIANN only outside their big-file paths).
#' @noRd
loadpage_show_standard_annot_upload <- function(filetype, bio, big_file_spec, big_file_diann) {
  if (isTRUE(bio == "PTM")) return(FALSE)
  if (isTRUE(filetype %in% c("sky", "prog", "PD", "open", "spmin", "phil", "meta"))) return(TRUE)
  if (isTRUE(filetype == "spec")  && !isTRUE(big_file_spec))  return(TRUE)
  if (isTRUE(filetype == "diann") && !isTRUE(big_file_diann)) return(TRUE)
  FALSE
}

#' Pre-formatted MSstats CSV upload — label-free path only.
#' @noRd
loadpage_show_msstats_label_free_upload <- function(filetype, bio, dda_dia) {
  isTRUE(filetype == "msstats") &&
    !isTRUE(bio == "PTM") &&
    !isTRUE(dda_dia == "TMT")
}

#' Pre-formatted MSstatsPTM CSV upload — PTM path only. (A
#' `|| (BIO=='PTM' && DDA_DIA=='TMT')` term would be redundant — tautological
#' whenever `BIO=='PTM'` — so the predicate omits it.)
#' @noRd
loadpage_show_msstats_ptm_upload <- function(filetype, bio) {
  isTRUE(filetype == "msstats") && isTRUE(bio == "PTM")
}

#' @noRd
loadpage_show_skyline_upload <- function(filetype, bio) {
  isTRUE(filetype == "sky") && !isTRUE(bio == "PTM")
}

#' @noRd
loadpage_show_ptm_fragpipe_upload <- function(filetype, bio) {
  isTRUE(filetype == "phil") && isTRUE(bio == "PTM")
}

#' @noRd
loadpage_show_maxquant_upload <- function(filetype, bio, dda_dia) {
  isTRUE(filetype == "maxq") &&
    !isTRUE(bio == "PTM") &&
    isTRUE(dda_dia %in% c("TMT", "LType"))
}

#' Shared PTM uploads block (PTM Input / Annot / FASTA / Unmod Protein).
#' A `|| (BIO=='PTM' && DDA_DIA=='TMT')` term would be redundant (tautological
#' whenever `BIO=='PTM'`), so the predicate is `BIO=='PTM' && filetype ∈ ...`.
#' @noRd
loadpage_show_ptm_uploads <- function(filetype, bio) {
  isTRUE(bio == "PTM") &&
    isTRUE(filetype %in% c("maxq", "PD", "spec", "sky", "meta"))
}

#' MaxQuant-specific PTM `proteinGroups.txt` upload.
#' @noRd
loadpage_show_ptm_maxquant_pgroup <- function(filetype, bio) {
  isTRUE(filetype == "maxq") && isTRUE(bio == "PTM")
}

#' Metamorpheus-specific PTM extras (`ptm_protein_annot` + dynamic mod-ID
#' selector).
#' @noRd
loadpage_show_ptm_metamorpheus_extras <- function(filetype, bio) {
  isTRUE(filetype == "meta") && isTRUE(bio == "PTM")
}

#' FASTA-column-name text input (same gate as `loadpage_show_ptm_uploads`).
#' @noRd
loadpage_show_ptm_fasta_id_column <- function(filetype, bio) {
  loadpage_show_ptm_uploads(filetype, bio)
}

#' @noRd
loadpage_show_ptm_mod_id_maxq <- function(filetype, bio) {
  isTRUE(filetype == "maxq") && isTRUE(bio == "PTM")
}

#' @noRd
loadpage_show_ptm_mod_id_pd <- function(filetype, bio) {
  isTRUE(filetype == "PD") && isTRUE(bio == "PTM")
}

#' @noRd
loadpage_show_ptm_mod_id_spec <- function(filetype, bio) {
  isTRUE(filetype == "spec") && isTRUE(bio == "PTM")
}

#' @noRd
loadpage_show_dia_umpire_upload <- function(filetype) {
  isTRUE(filetype == "ump")
}

#' Label-free options block (`unique_peptides`, `remove`). Suppressed for
#' the sample-data and big-file workflows.
#' @noRd
loadpage_show_label_free_options <- function(filetype, dda_dia, big_file_spec, big_file_diann) {
  if (is.null(filetype) || !nzchar(filetype)) return(FALSE)
  isTRUE(dda_dia == "LType") &&
    !isTRUE(filetype == "sample") &&
    !(isTRUE(filetype == "spec")  && isTRUE(big_file_spec))  &&
    !(isTRUE(filetype == "diann") && isTRUE(big_file_diann))
}

#' OpenSWATH M-score filter section (parent of `mscore_cutoff`).
#' @noRd
loadpage_show_openswath_mscore <- function(filetype) {
  isTRUE(filetype == "open")
}

#' M-score numeric cutoff — nested under the M-score section. The predicate
#' AND-includes the parent's `filetype == 'open'` clause, NOT only the
#' immediate `m_score` driver, so the cutoff hides when the converter
#' changes even if the checkbox happens to still be TRUE.
#' @noRd
loadpage_show_openswath_mscore_cutoff <- function(filetype, m_score) {
  isTRUE(filetype == "open") && isTRUE(m_score)
}

#' TMT options section visibility — used inside the renderUI below to decide
#' whether to emit any UI at all.
#' @noRd
loadpage_show_tmt_options <- function(filetype, dda_dia) {
  isTRUE(dda_dia == "TMT") && isTRUE(filetype %in% c("PD", "maxq"))
}

#' Default `which.proteinid` value for the active TMT converter. Returns
#' NULL when neither PD nor MaxQuant is selected.
#' @noRd
loadpage_default_proteinid_for_filetype <- function(filetype) {
  if (isTRUE(filetype == "PD"))   return("Protein.Accessions")
  if (isTRUE(filetype == "maxq")) return("Proteins")
  NULL
}

#' Compute the seed value for the TMT `which.proteinid` renderUI on a rebuild.
#'
#' Distinguishes "user never changed the default" from "user typed a custom
#' value" so converter switches apply the new converter's default in the
#' former case and carry the typed value in the latter. Pure — no Shiny
#' reactivity; the caller passes in `isolate(input[[which_proteinid]])` and
#' the previous filetype from a reactiveVal tracker.
#'
#' Rules (in order):
#'   1. `preserved_value` is NULL — first build, or rebuild after leaving TMT
#'      entirely (the renderUI returned NULL, textInput unmounted) → apply
#'      the incoming converter's default.
#'   2. Converter has NOT changed (`outgoing_filetype == incoming_filetype`)
#'      → carry `preserved_value` verbatim. Covers re-renders that fire
#'      without an actual converter switch.
#'   3. Converter changed AND `preserved_value` equals the OUTGOING
#'      converter's default → user never typed → apply the incoming default.
#'   4. Converter changed AND `preserved_value` differs from the outgoing
#'      default → user typed a custom value → carry it.
#'
#' Edge case: `outgoing_filetype` is NULL but `preserved_value` is non-NULL
#' (unusual race / pre-fill). Conservative: carry `preserved_value` rather
#' than clobber it with the incoming default.
#'
#' @param incoming_filetype  the current `input$filetype` (PD or maxq)
#' @param outgoing_filetype  the filetype from the previous renderUI call,
#'   tracked in a reactiveVal; NULL on first build
#' @param preserved_value    `isolate(input[[which_proteinid]])` — the value
#'   of the textInput from the previous build, NULL when unmounted
#' @noRd
loadpage_seed_proteinid <- function(incoming_filetype,
                                     outgoing_filetype,
                                     preserved_value) {
  incoming_default <- loadpage_default_proteinid_for_filetype(incoming_filetype)

  # Rule 1: first build / textInput was unmounted.
  if (is.null(preserved_value)) {
    return(incoming_default)
  }

  # Rule 2: no converter change.
  if (!is.null(outgoing_filetype) &&
      isTRUE(outgoing_filetype == incoming_filetype)) {
    return(preserved_value)
  }

  # Rule 3: converter switch with the outgoing default — user never typed.
  outgoing_default <- loadpage_default_proteinid_for_filetype(outgoing_filetype)
  if (!is.null(outgoing_default) &&
      isTRUE(preserved_value == outgoing_default)) {
    return(incoming_default)
  }

  # Rule 4 (and unknown-outgoing edge case): user typed something → carry.
  preserved_value
}


# ----------------------------------------------------------------------------
# Unified registration helper.
# ----------------------------------------------------------------------------

#' Register every loadpage visibility observer plus the single TMT
#' `which.proteinid` renderUI exception. Call once from `loadpageServer`'s
#' `moduleServer` scope.
#'
#' Panels are shown/hidden with `shinyjs` rather than rebuilt with `renderUI`:
#' `getData` / `getDataCode` read input IDs by literal string, so a `renderUI`
#' rebuild would destroy a hidden input and feed `getData` NULL at proceed
#' time, and would also reset values the user typed and drop uploaded files.
#' show/hide keeps the inputs mounted so their state is preserved.
#'
#' @param input   the Shiny module's `input` object
#' @param output  the Shiny module's `output` object (for the TMT renderUI)
#' @param session the Shiny module's `session` (for `session$ns`)
#' @noRd
register_loadpage_visibility_observers <- function(input, output, session) {
  # --- DIANN cluster ---------------------------------------------------------
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$diann_lf_options_panel,
      condition = loadpage_show_diann_lf_options(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$dda_dia]],
        input[[NAMESPACE_LOADPAGE$big_file_diann]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$diann_intensity_column_panel,
      condition = loadpage_show_diann_intensity_column(
        input[[NAMESPACE_LOADPAGE$diann_2plus]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$qval_filter_panel,
      condition = loadpage_show_qval_filter(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$big_file_diann]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$qval_cutoff_panel,
      condition = loadpage_show_qval_cutoff(
        input[[NAMESPACE_LOADPAGE$q_val]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$qval_mbr_panel,
      condition = loadpage_show_diann_mbr(
        input[[NAMESPACE_LOADPAGE$q_val]],
        input[[NAMESPACE_LOADPAGE$filetype]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$diann_anomaly_panel,
      condition = loadpage_show_diann_anomaly(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$big_file_diann]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$diann_anomaly_run_order_panel,
      condition = loadpage_show_diann_anomaly_run_order(
        input[[NAMESPACE_LOADPAGE$diann_calculate_anomaly_scores]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$big_diann_anomaly_run_order_panel,
      condition = loadpage_show_big_diann_anomaly_run_order(
        input[[NAMESPACE_LOADPAGE$big_diann_calculate_anomaly_scores]]
      )
    )
  })

  # --- Sample-dataset descriptions + LabelFreeType picker -------------------
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$sample_dda_description_panel,
      condition = loadpage_show_sample_dataset_description(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$label_free_type]],
        "DDA"
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$sample_dia_description_panel,
      condition = loadpage_show_sample_dataset_description(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$label_free_type]],
        "DIA"
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$sample_srm_prm_description_panel,
      condition = loadpage_show_sample_dataset_description(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$label_free_type]],
        "SRM_PRM"
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$label_free_type_selection_panel,
      condition = loadpage_show_sample_dataset_label_free_type_selector(
        input[[NAMESPACE_LOADPAGE$bio]],
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$dda_dia]]
      )
    )
  })

  # --- Non-PTM converter uploads ---------------------------------------------
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$standard_quant_upload_panel,
      condition = loadpage_show_standard_quant_upload(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$bio]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$standard_annot_upload_panel,
      condition = loadpage_show_standard_annot_upload(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$bio]],
        input[[NAMESPACE_LOADPAGE$big_file_spec]],
        input[[NAMESPACE_LOADPAGE$big_file_diann]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$msstats_regular_upload_panel,
      condition = loadpage_show_msstats_label_free_upload(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$bio]],
        input[[NAMESPACE_LOADPAGE$dda_dia]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$msstats_ptm_upload_panel,
      condition = loadpage_show_msstats_ptm_upload(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$bio]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$skyline_upload_panel,
      condition = loadpage_show_skyline_upload(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$bio]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$ptm_fragpipe_upload_panel,
      condition = loadpage_show_ptm_fragpipe_upload(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$bio]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$maxquant_upload_panel,
      condition = loadpage_show_maxquant_upload(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$bio]],
        input[[NAMESPACE_LOADPAGE$dda_dia]]
      )
    )
  })

  # --- PTM converter cluster -------------------------------------------------
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$ptm_uploads_panel,
      condition = loadpage_show_ptm_uploads(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$bio]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$ptm_maxquant_pgroup_panel,
      condition = loadpage_show_ptm_maxquant_pgroup(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$bio]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$ptm_metamorpheus_extras_panel,
      condition = loadpage_show_ptm_metamorpheus_extras(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$bio]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$ptm_fasta_id_column_panel,
      condition = loadpage_show_ptm_fasta_id_column(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$bio]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$ptm_mod_id_maxq_panel,
      condition = loadpage_show_ptm_mod_id_maxq(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$bio]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$ptm_mod_id_pd_panel,
      condition = loadpage_show_ptm_mod_id_pd(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$bio]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$ptm_mod_id_spec_panel,
      condition = loadpage_show_ptm_mod_id_spec(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$bio]]
      )
    )
  })

  # --- DIA-Umpire + label-free options + OpenSWATH M-score -------------------
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$dia_umpire_upload_panel,
      condition = loadpage_show_dia_umpire_upload(
        input[[NAMESPACE_LOADPAGE$filetype]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$label_free_options_panel,
      condition = loadpage_show_label_free_options(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$dda_dia]],
        input[[NAMESPACE_LOADPAGE$big_file_spec]],
        input[[NAMESPACE_LOADPAGE$big_file_diann]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$openswath_mscore_panel,
      condition = loadpage_show_openswath_mscore(
        input[[NAMESPACE_LOADPAGE$filetype]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_LOADPAGE$openswath_mscore_cutoff_panel,
      condition = loadpage_show_openswath_mscore_cutoff(
        input[[NAMESPACE_LOADPAGE$filetype]],
        input[[NAMESPACE_LOADPAGE$m_score]]
      )
    )
  })

  # --- TMT which.proteinid renderUI (the duplicate-ns()-id case) -------------
  #
  # Two `conditionalPanel`s previously declared the same ns("which.proteinid")
  # with different defaults (PD: "Protein.Accessions", MaxQuant: "Proteins").
  # Mounting both as hidden divs would collide deterministically. The fix:
  # one uiOutput slot driven by a renderUI that emits the textInput. The seed
  # logic must distinguish "user is still on the converter's default" (in
  # which case switching converter should apply the new converter's default)
  # from "user typed a custom value" (in which case the typed value carries
  # across the switch). The reactiveVal `last_tmt_filetype` below holds the
  # filetype from the previous renderUI evaluation so the seeding helper can
  # compare `preserved` against the OUTGOING converter's default.
  last_tmt_filetype <- reactiveVal(NULL)

  output[[NAMESPACE_LOADPAGE$tmt_options_ui]] <- renderUI({
    filetype <- input[[NAMESPACE_LOADPAGE$filetype]]
    dda_dia  <- input[[NAMESPACE_LOADPAGE$dda_dia]]
    if (!loadpage_show_tmt_options(filetype, dda_dia)) return(NULL)

    preserved <- isolate(input[[NAMESPACE_LOADPAGE$which_proteinid]])
    outgoing  <- isolate(last_tmt_filetype())

    seed_value <- loadpage_seed_proteinid(
      incoming_filetype = filetype,
      outgoing_filetype = outgoing,
      preserved_value   = preserved
    )

    # Update the tracker for the NEXT evaluation. Set after computing the
    # seed so this evaluation still sees the previous filetype as `outgoing`.
    last_tmt_filetype(filetype)

    tagList(
      h4("Select the options for pre-processing"),
      textInput(session$ns(NAMESPACE_LOADPAGE$which_proteinid),
                label = h5("Enter the column name corresponding to the protein name"),
                value = seed_value)
    )
  })

  invisible(NULL)
}


# Spectronaut/DIANN converter renderUIs + the file-type availability observer (radio-disable + opacity; UI state, so no `loadpage_show_*` predicate).


#' Register the Spectronaut and DIANN converter renderUIs and the file-type
#' availability observer.
#'
#' @param input          the Shiny module's `input` object
#' @param output         the Shiny module's `output` object
#' @param session        the Shiny module's `session` (for `session$ns`)
#' @param is_web_server  TRUE if running as the web app (`launch_MSstatsShiny()`),
#'                       FALSE for local-server / shinyFiles mode
#' @param app_template   reactive returning the selected template name; may be
#'                       NULL when the orchestrator passes no template
#' @noRd
register_loadpage_converter_ui <- function(input, output, session,
                                            is_web_server = FALSE,
                                            app_template = NULL) {

  output$spectronaut_header_ui <- renderUI({
    req(input$filetype == 'spec', input$BIO != 'PTM')
    create_spectronaut_header()
  })

  output$spectronaut_file_selection_ui <- renderUI({
    req(input$filetype == 'spec', input$BIO != 'PTM')

    ui_elements <- tagList()

    if (!is_web_server) {
      ui_elements <- tagList(ui_elements, create_spectronaut_mode_selector(session$ns, isTRUE(input$big_file_spec)))

      if (isTRUE(input$big_file_spec)) {
        ui_elements <- tagList(ui_elements, create_spectronaut_large_file_ui(session$ns))
      } else {
        ui_elements <- tagList(ui_elements, create_spectronaut_standard_ui(session$ns))
      }
    } else {
      ui_elements <- tagList(ui_elements, create_spectronaut_standard_ui(session$ns))
    }

    ui_elements
  })

  output$spectronaut_intensity_ui <- renderUI({
    req(input$filetype == 'spec', input$BIO != 'PTM')

    default_intensity <- if (!is.null(app_template) &&
                             app_template() == TEMPLATES$protein_turnover) {
      "FG.MS1Quantity"
    } else {
      "F.NormalizedPeakArea"
    }

    textInput(session$ns("spec_intensity_col"),
              label = h5("Intensity column",
                         class = "icon-wrapper",
                         icon("question-circle", lib = "font-awesome"),
                         div("Spectronaut export column to use as the intensity measure (e.g. F.NormalizedPeakArea, F.PeakArea, FG.MS1Quantity). Leave at the default unless you have a specific reason to override it.",
                             class = "icon-tooltip")),
              value = default_intensity)
  })

  output$spectronaut_turnover_ui <- renderUI({
    req(input$filetype == 'spec', input$BIO != 'PTM')
    req(!is.null(app_template) && app_template() == TEMPLATES$protein_turnover)

    ns <- session$ns
    tagList(
      tags$hr(),
      h4("Protein Turnover Options"),
      textInput(ns("spec_peptide_seq_col"),
                "Peptide sequence column",
                value = "FG.LabeledSequence"),
      textInput(ns("spec_heavy_labels"),
                "Heavy labels (comma-separated)",
                value = "L[Leu6]")
    )
  })

  output$diann_turnover_ui <- renderUI({
    req(input$filetype == 'diann', input$DDA_DIA == 'LType')
    req(!is.null(app_template) && app_template() == TEMPLATES$protein_turnover)

    ns <- session$ns
    textInput(ns("diann_labeled_aa"),
              h5("SILAC-labeled amino acids", class = "icon-wrapper",
                 icon("question-circle", lib = "font-awesome"),
                 div("Comma-separated single-letter codes of SILAC-labeled amino acids (e.g. K for lysine, or K,R for lysine and arginine).", class = "icon-tooltip")),
              value = "K")
  })

  output$diann_header_ui <- renderUI({
    req(input$filetype == 'diann', input$BIO != 'PTM')
    create_diann_header()
  })

  output$diann_file_selection_ui <- renderUI({
    req(input$filetype == 'diann', input$BIO != 'PTM')

    ui_elements <- tagList()

    if (!is_web_server) {
      ui_elements <- tagList(ui_elements, create_diann_mode_selector(session$ns, isTRUE(input$big_file_diann)))

      if (isTRUE(input$big_file_diann)) {
        ui_elements <- tagList(ui_elements, create_diann_large_file_ui(session$ns))
      } else {
        ui_elements <- tagList(ui_elements, create_diann_standard_ui(session$ns))
      }
    } else {
      ui_elements <- tagList(ui_elements, create_diann_standard_ui(session$ns))
    }

    ui_elements
  })

  output$diann_options_ui <- renderUI({
    req(input$filetype == 'diann', input$BIO != 'PTM')

    if (!is_web_server && isTRUE(input$big_file_diann)) {
      mbr_def <- if (is.null(input$big_diann_MBR)) TRUE else input$big_diann_MBR
      quantcol_def <- if (is.null(input$big_diann_quantification_column) ||
                          !nzchar(input$big_diann_quantification_column)) {
        "FragmentQuantCorrected"
      } else {
        input$big_diann_quantification_column
      }
      global_qv_def <- if (is.null(input$big_diann_global_qvalue_cutoff)) 0.01 else input$big_diann_global_qvalue_cutoff
      qv_def <- if (is.null(input$big_diann_qvalue_cutoff)) 0.01 else input$big_diann_qvalue_cutoff
      pg_qv_def <- if (is.null(input$big_diann_pg_qvalue_cutoff)) 0.01 else input$big_diann_pg_qvalue_cutoff

      max_feature_def <- if (is.null(input$big_diann_max_feature_count)) 100 else input$big_diann_max_feature_count
      unique_peps_def <- if (is.null(input$big_diann_filter_unique_peptides)) FALSE else input$big_diann_filter_unique_peptides
      agg_psms_def <- if (is.null(input$big_diann_aggregate_psms)) FALSE else input$big_diann_aggregate_psms
      few_obs_def <- if (is.null(input$big_diann_filter_few_obs)) FALSE else input$big_diann_filter_few_obs
      backend_def <- if (is.null(input$big_diann_backend) || !nzchar(input$big_diann_backend)) "arrow" else input$big_diann_backend
      calculate_anomaly_def <- if (is.null(input$big_diann_calculate_anomaly_scores)) FALSE else input$big_diann_calculate_anomaly_scores

      tagList(
        create_diann_large_filter_options(session$ns, mbr_def, quantcol_def,
                                          global_qv_def, qv_def, pg_qv_def),
        create_diann_large_bottom_ui(session$ns, max_feature_def,
                                     unique_peps_def, agg_psms_def, few_obs_def,
                                     backend_def),
        create_diann_large_annotation_ui(session$ns, calculate_anomaly_def)
      )
    } else {
      NULL
    }
  })

  output$spectronaut_options_ui <- renderUI({
    req(input$filetype == 'spec', input$BIO != 'PTM')

    if (!is_web_server && isTRUE(input$big_file_spec)) {
      qval_def <- if (is.null(input$filter_by_qvalue)) TRUE else input$filter_by_qvalue
      excluded_def <- if (is.null(input$filter_by_excluded)) FALSE else input$filter_by_excluded
      identified_def <- if (is.null(input$filter_by_identified)) FALSE else input$filter_by_identified
      cutoff_def <- if (is.null(input$qvalue_cutoff)) 0.01 else input$qvalue_cutoff

      max_feature_def <- if (is.null(input$max_feature_count)) 20 else input$max_feature_count
      unique_peps_def <- if (is.null(input$filter_unique_peptides)) FALSE else input$filter_unique_peptides
      agg_psms_def <- if (is.null(input$aggregate_psms)) FALSE else input$aggregate_psms
      few_obs_def <- if (is.null(input$filter_few_obs)) FALSE else input$filter_few_obs
      calculate_anomaly_def <- if (is.null(input$calculate_anomaly_scores)) FALSE else input$calculate_anomaly_scores

      tagList(
        create_spectronaut_large_filter_options(session$ns, excluded_def, identified_def, qval_def),
        if (qval_def) create_spectronaut_qvalue_cutoff_ui(session$ns, cutoff_def),
        create_spectronaut_large_bottom_ui(session$ns, max_feature_def, unique_peps_def, agg_psms_def, few_obs_def),
        create_spectronaut_large_annotation_ui(session$ns, calculate_anomaly_def)
      )
    } else {
      NULL
    }
  })

  # File-type availability — disable converter radios that don't fit the
  # current (BIO, DDA_DIA) combo, and dim them via the `runjs` opacity hack.
  # UI state only, not predicate-driven visibility (no `loadpage_show_*`
  # predicate).
  observe({
    if ((input$BIO == "Protein" || input$BIO == "Peptide") && input$DDA_DIA == "LType") {
      runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
      enable("filetype")
      disable(selector = "[type=radio][value=spmin]")
      runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")

    } else if ((input$BIO == "Protein" || input$BIO == "Peptide") && input$DDA_DIA == "TMT") {
      runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
      enable("filetype")
      disable(selector = "[type=radio][value=sky]")
      disable(selector = "[type=radio][value=prog]")
      disable(selector = "[type=radio][value=spec]")
      disable(selector = "[type=radio][value=open]")
      disable(selector = "[type=radio][value=ump]")
      disable(selector = "[type=radio][value=diann]")
      disable(selector = "[type=radio][value=meta]")
      runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")

    } else if (input$BIO == "PTM" && input$DDA_DIA == "LType") {
      runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
      enable("filetype")
      # disable(selector = "[type=radio][value=sky]")
      disable(selector = "[type=radio][value=prog]")
      disable(selector = "[type=radio][value=PD]")
      disable(selector = "[type=radio][value=openms]")
      disable(selector = "[type=radio][value=spmin]")
      disable(selector = "[type=radio][value=open]")
      disable(selector = "[type=radio][value=ump]")
      disable(selector = "[type=radio][value=phil]")
      disable(selector = "[type=radio][value=diann]")

      runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
    } else if (input$BIO == "PTM" && input$DDA_DIA == "TMT") {
      runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
      enable("filetype")
      disable(selector = "[type=radio][value=prog]")
      disable(selector = "[type=radio][value=openms]")
      disable(selector = "[type=radio][value=spec]")
      disable(selector = "[type=radio][value=open]")
      disable(selector = "[type=radio][value=ump]")
      disable(selector = "[type=radio][value=spmin]")
      disable(selector = "[type=radio][value=diann]")
      disable(selector = "[type=radio][value=sky]")
      disable(selector = "[type=radio][value=meta]")

      runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
    }
  })

  invisible(NULL)
}

