# QC sidebar: pure visibility predicates for the processing-option panels, the
# server-side shinyjs toggles + protein-turnover template setup that drive them,
# and the dynamic sidebar option inputs (standard names, top-N feature slider,
# run-level summarization method).

# ----------------------------------------------------------------------------
# Visibility predicates. Pure functions of their driver values so they can be
# unit-tested directly; each gates on the complete condition for its panel, not
# just the immediate driver.
# ----------------------------------------------------------------------------

#' Show the TMT-branch processing panels (peptide normalization, summarization
#' method, reference-channel normalization).
#' @noRd
qc_show_tmt <- function(dda_dia, bio) {
  isTRUE(dda_dia == "TMT") || (isTRUE(bio == "PTM") && isTRUE(dda_dia == "TMT"))
}

#' Show the label-free-branch processing panels (normalization, feature subset,
#' censoring, imputation, summary method).
#' @noRd
qc_show_lf <- function(dda_dia, bio) {
  isTRUE(dda_dia == "LType") || (isTRUE(bio == "PTM") && !isTRUE(dda_dia == "TMT"))
}

#' Show the maximum-censored-quantile inputs: TMT branch with MSstats summarization.
#' @noRd
qc_show_maxqc_msstats <- function(dda_dia, bio, summarization) {
  qc_show_tmt(dda_dia, bio) && isTRUE(summarization == "msstats")
}

#' Show the global-standards picker: label-free non-PTM with globalStandards normalization.
#' @noRd
qc_show_standards <- function(norm, bio, dda_dia) {
  isTRUE(norm == "globalStandards") &&
    (!isTRUE(bio == "PTM") && !isTRUE(dda_dia == "TMT"))
}

#' Show the top-N feature-count slider: label-free branch with the topN subset selected.
#' @noRd
qc_show_features_topn <- function(dda_dia, bio, features_used) {
  qc_show_lf(dda_dia, bio) && isTRUE(features_used == "topN")
}

#' Show the model-based imputation checkbox: label-free branch with a censoring assumption set.
#' @noRd
qc_show_mbi <- function(dda_dia, bio, cens_int) {
  qc_show_lf(dda_dia, bio) && (isTRUE(cens_int == "NA") || isTRUE(cens_int == "0"))
}

#' Show the log-transformation panel: label-free branch, but hidden for the
#' protein-turnover template.
#' @noRd
qc_show_log_section <- function(dda_dia, bio, template) {
  qc_show_lf(dda_dia, bio) && !isTRUE(template == TEMPLATES$protein_turnover)
}

#' Show the feature-subset radio: label-free branch, but hidden for the
#' protein-turnover template (which always summarizes on all features).
#' @noRd
qc_show_feature_subset <- function(dda_dia, bio, template) {
  qc_show_lf(dda_dia, bio) && !isTRUE(template == TEMPLATES$protein_turnover)
}

#' Show the feature-weighting checkbox: protein-turnover template only.
#' @noRd
qc_show_feature_weights <- function(template) {
  isTRUE(template == TEMPLATES$protein_turnover)
}

#' Show the profile-plot options (summary toggle, feature legend).
#' @noRd
qc_show_profileplot_options <- function(type1) {
  isTRUE(type1 == "ProfilePlot")
}

#' Show the quality-metric selectors.
#' @noRd
qc_show_qualitymetrics_options <- function(type1) {
  isTRUE(type1 == "QualityMetricsPlot")
}

#' Show the non-PTM CSV download buttons.
#' @noRd
qc_show_nonptm_downloads <- function(bio) {
  !isTRUE(bio == "PTM")
}

#' Show the PTM CSV download buttons.
#' @noRd
qc_show_ptm_downloads <- function(bio) {
  isTRUE(bio == "PTM")
}

# ----------------------------------------------------------------------------
# Visibility + enablement observers.
# ----------------------------------------------------------------------------

#' Register the QC sidebar visibility observers.
#'
#' Each panel is a permanently-mounted, initially-hidden div toggled here with
#' shinyjs. show/hide (not renderUI) is required because preprocessData /
#' preprocessDataCode read these input ids by literal string at run time: a
#' renderUI would destroy a hidden input, feeding the consumer NULL and resetting
#' the user's value, whereas a hidden-but-mounted input keeps reporting its value.
#'
#' @noRd
register_qc_visibility_observers <- function(input, session, loadpage_input, app_template) {

  # TMT-branch panels.
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$global_norm_panel,
      condition = qc_show_tmt(loadpage_input()$DDA_DIA, loadpage_input()$BIO)
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$summarization_panel,
      condition = qc_show_tmt(loadpage_input()$DDA_DIA, loadpage_input()$BIO)
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$reference_norm_panel,
      condition = qc_show_tmt(loadpage_input()$DDA_DIA, loadpage_input()$BIO)
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$maxqc_msstats_panel,
      condition = qc_show_maxqc_msstats(
        loadpage_input()$DDA_DIA, loadpage_input()$BIO,
        input[[NAMESPACE_QC$summarization]]
      )
    )
  })

  # Label-free-branch panels.
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$norm_panel,
      condition = qc_show_lf(loadpage_input()$DDA_DIA, loadpage_input()$BIO)
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$lf_options_panel,
      condition = qc_show_lf(loadpage_input()$DDA_DIA, loadpage_input()$BIO)
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$features_topn_panel,
      condition = qc_show_features_topn(
        loadpage_input()$DDA_DIA, loadpage_input()$BIO,
        input[[NAMESPACE_QC$features_used]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$mbi_panel,
      condition = qc_show_mbi(
        loadpage_input()$DDA_DIA, loadpage_input()$BIO,
        input[[NAMESPACE_QC$cens_int]]
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$standards_panel,
      condition = qc_show_standards(
        input[[NAMESPACE_QC$norm]], loadpage_input()$BIO, loadpage_input()$DDA_DIA
      )
    )
  })

  # Log-transformation panel. One observer gates this single container on both
  # the label-free branch and the (non-)turnover template; a second toggle on the
  # same div would race with this one.
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$log_section,
      condition = qc_show_log_section(
        loadpage_input()$DDA_DIA, loadpage_input()$BIO, app_template()
      )
    )
  })

  observe({
    shinyjs::toggle(
      NAMESPACE_QC$feature_subset_panel,
      condition = qc_show_feature_subset(
        loadpage_input()$DDA_DIA, loadpage_input()$BIO, app_template()
      )
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$feature_weights_panel,
      condition = qc_show_feature_weights(app_template())
    )
  })

  # Plot-tab option panels.
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$profileplot_options_panel,
      condition = qc_show_profileplot_options(input[[NAMESPACE_QC$qc_page_plot_type]])
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$qualitymetrics_options_panel,
      condition = qc_show_qualitymetrics_options(input[[NAMESPACE_QC$qc_page_plot_type]])
    )
  })

  # Download-tab panels.
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$nonptm_downloads_panel,
      condition = qc_show_nonptm_downloads(loadpage_input()$BIO)
    )
  })
  observe({
    shinyjs::toggle(
      NAMESPACE_QC$ptm_downloads_panel,
      condition = qc_show_ptm_downloads(loadpage_input()$BIO)
    )
  })

  # Protein-turnover template: switch the Turnover Ratios tab, hide the
  # template-specific sections, and restrict normalization / feature-subset choices.
  observeEvent(app_template(), {
    req(!is.null(app_template))
    if (app_template() == TEMPLATES$protein_turnover) {
      showTab(inputId = "qc_tabs", target = "Turnover Ratios", session = session)
      shinyjs::hide(NAMESPACE_QC$censoring_section)
      shinyjs::hide(NAMESPACE_QC$standards_type_section)
      updateSelectInput(session, NAMESPACE_QC$norm,
                        choices = c("none" = "FALSE", "global standards" = "globalStandards"),
                        selected = "FALSE")
      updateRadioButtons(session, NAMESPACE_QC$features_used,
                         choices = c("Use all features" = "all"),
                         selected = "all")
    } else {
      hideTab(inputId = "qc_tabs", target = "Turnover Ratios", session = session)
      shinyjs::show(NAMESPACE_QC$censoring_section)
      shinyjs::show(NAMESPACE_QC$standards_type_section)
      updateSelectInput(session, NAMESPACE_QC$norm,
                        choices = c("none" = "FALSE", "equalize medians" = "equalizeMedians",
                                    "quantile" = "quantile", "global standards" = "globalStandards"),
                        selected = "equalizeMedians")
      updateRadioButtons(session, NAMESPACE_QC$features_used,
                         choices = c("Use all features" = "all", "Use top N features" = "topN",
                                     "Remove uninformative features & outliers" = "highQuality"),
                         selected = "all")
    }
  }, ignoreNULL = TRUE)

  # Enable the censoring assumptions and grey out the option the active converter
  # cannot report.
  observe({
    if(!is.null(loadpage_input()$filetype)) {
      runjs("$('[type=radio][name=censInt]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
      enable("censInt")
      if (loadpage_input()$filetype == "sky" || loadpage_input()$filetype == "prog" || loadpage_input()$filetype == "spec") {
        disable(selector = "[type=radio][value=NA]")
        runjs("$.each($('[type=radio][name=censInt]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
      }
      else if (loadpage_input()$filetype == "maxq" || loadpage_input()$filetype == "PD" || loadpage_input()$filetype == "open") {
        disable(selector = "[type=radio][value=0]")
        runjs("$.each($('[type=radio][name=censInt]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
      }
    }
  })

  # Enable the maximum-censored-quantile numeric only when its "do not apply
  # cutoff" checkbox is unchecked.
  observe ({
    shinyjs::toggleState("maxQC", input$null == FALSE)
  })
}

# ----------------------------------------------------------------------------
# Dynamic sidebar option inputs.
# ----------------------------------------------------------------------------

#' Register the QC sidebar option renderUIs: standard-name picker, top-N feature
#' slider, and run-level summarization method.
#' @noRd
register_qc_sidebar_options <- function(input, output, session, loadpage_input, get_data, app_template) {

  output$Names = renderUI({
    ns <- session$ns

    if (!is.null(app_template) && !is.null(app_template()) &&
        app_template() == TEMPLATES$protein_turnover) {
      return(selectizeInput(ns("names"), "Standard name",
                            choices = "unlabeled", selected = "unlabeled",
                            multiple = TRUE))
    }

    if (input$standards == "Proteins") {
      selectizeInput(ns("names"), "choose standard", unique(get_data()$ProteinName), multiple = TRUE)
    }
    else if (input$standards == "Peptides") {
      selectizeInput(ns("names"), "choose standard", unique(get_data()$PeptideSequence), multiple = TRUE)
    }

  })

  output$features = renderUI({
    ns <- session$ns
    req(get_data())
    max_feat <- reactive({
      df <- if (loadpage_input()$BIO == "PTM") as.data.frame(get_data()$PTM)
            else                                as.data.frame(get_data())
      feat_cols <- c("PeptideSequence", "PrecursorCharge", "FragmentIon", "ProductCharge")
      if (!all(c("ProteinName", feat_cols) %in% names(df))) return(100)   # fallback
      feature <- do.call(paste, c(df[feat_cols], sep = "_"))
      counts  <- tapply(feature, df$ProteinName, function(x) length(unique(x)))
      # Fragment-level converters (e.g. DIANN) can produce hundreds of
      # features per protein; cap the slider at 100 for usability. topN
      # caps naturally when N exceeds a protein's available features.
      min(max(counts, na.rm = TRUE), 100)
    })
    sliderInput(ns("n_feat"), "Number of top features to use", 1,
                as.numeric(max_feat()), 1)
  })

  output$summaryMethodUI <- renderUI({
    ns <- session$ns

    # Default choices
    choices <- c("TMP" = "TMP")
    tooltip_text <- "Run-level summarization method. TMP is Tukey's Median Polish. "
    selected <- "TMP"

    # Conditionally add MSstats+ if anomaly score calculation is checked
    # (Spectronaut, regular DIANN, or big-file DIANN).
    if (.anomaly_scores_enabled(loadpage_input())) {
      choices <- c(choices, "MSstats+" = "linear")
      tooltip_text <- paste0(tooltip_text, "MSstats+ uses a weighted linear model.")
      selected = "linear"
    }

    radioButtons(
      ns("summaryMethod"),
      label = h4(
        "Summarization",
        class = "icon-wrapper",
        icon("question-circle", lib = "font-awesome"),
        div(tooltip_text, class = "icon-tooltip")
      ),
      choices = choices,
      selected = selected
    )
  })
}
