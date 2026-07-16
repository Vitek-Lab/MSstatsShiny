test_that("loadpageUI returns a valid tagList with fluidPage structure", {
  # Test basic function execution and structure
  result <- loadpageUI("test")

  # Should return a tagList
  expect_s3_class(result, "shiny.tag.list")

  result_html = as.character(result)
  expect_true(grepl("div", result_html))

  # Should not be NULL or empty
  expect_true(length(result) > 0)
})

test_that("metabolomics template is registered and appears in the picker choices", {
  expect_equal(TEMPLATES$metabolomics, "metabolomics")
  expect_equal(TEMPLATE_LABELS$metabolomics,
               "Metabolite Differential Abundance Analysis")

  # The home picker builds its choices from the constants via
  # setNames(unlist(TEMPLATES), unlist(TEMPLATE_LABELS)).
  choices = setNames(unlist(TEMPLATES, use.names = FALSE),
                     unlist(TEMPLATE_LABELS, use.names = FALSE))
  expect_true("metabolomics" %in% choices)
  expect_equal(names(choices)[choices == "metabolomics"],
               "Metabolite Differential Abundance Analysis")
})

test_that("loadpageUI generates correct namespaced input IDs", {
  # Test that all input elements use proper namespacing
  result <- loadpageUI("testmodule")
  html_output <- as.character(result)
  
  # Check for key namespaced input IDs that should be present
  expected_ids <- c(
    "testmodule-BIO",           # Biological question radio buttons
    "testmodule-DDA_DIA",       # Label type radio buttons  
    "testmodule-filetype",      # File type radio buttons
    "testmodule-proceed1"       # Upload button
  )
  
  for(id in expected_ids) {
    expect_true(
      grepl(id, html_output, fixed = TRUE),
      info = paste("Missing namespaced ID:", id)
    )
  }
})

test_that("loadpageUI contains all required radio button choices", {
  # Test that essential radio button options are present
  result <- loadpageUI("test")
  html_output <- as.character(result)
  
  # Check biological question options
  bio_options <- c("Protein", "Peptide", "PTM")
  for(option in bio_options) {
    expect_true(grepl(option, html_output), 
                info = paste("Missing biological option:", option))
  }
  
  # Check label type options
  label_options <- c("Label-Free", "TMT")
  for(option in label_options) {
    expect_true(grepl(option, html_output),
                info = paste("Missing label option:", option))
  }
  
  # Check file type options (sample a few key ones)
  file_options <- c("MaxQuant", "Skyline", "MSstats Format")
  for(option in file_options) {
    expect_true(grepl(option, html_output),
                info = paste("Missing file type option:", option))
  }
})

test_that("loadpageUI mounts hidden visibility containers for migrated workflows", {
  # The Phase 1 + Phase 2 refactor moved conditional UI off `conditionalPanel`
  # and onto server-side `shinyjs::show/hide`. Each migrated panel is now
  # wrapped in `shinyjs::hidden(div(id = ns(NAMESPACE_LOADPAGE$<panel>), ...))`,
  # so the static UI contains the namespaced container divs (mounted, hidden)
  # in place of the old JS condition strings. The driver inputs / file inputs
  # inside live alongside, ready for the server's toggle observers.
  result <- loadpageUI("test")
  html_output <- as.character(result)

  expected_panel_ids <- c(
    # Sample dataset descriptions (Phase 2 — 3 mutually exclusive panels)
    "test-sample_dda_description_panel",
    "test-sample_dia_description_panel",
    "test-sample_srm_prm_description_panel",
    # LabelFreeType selector (Phase 2)
    "test-label_free_type_selection_panel",
    # Non-PTM uploads (Phase 2)
    "test-standard_quant_upload_panel",
    "test-standard_annot_upload_panel",
    "test-msstats_regular_upload_panel",
    "test-skyline_upload_panel",
    "test-maxquant_upload_panel",
    "test-dia_umpire_upload_panel",
    # PTM cluster (Phase 2)
    "test-msstats_ptm_upload_panel",
    "test-ptm_fragpipe_upload_panel",
    "test-ptm_uploads_panel",
    "test-ptm_maxquant_pgroup_panel",
    "test-ptm_metamorpheus_extras_panel",
    "test-ptm_fasta_id_column_panel",
    "test-ptm_mod_id_maxq_panel",
    "test-ptm_mod_id_pd_panel",
    "test-ptm_mod_id_spec_panel",
    # Label-free options + OpenSWATH (Phase 2)
    "test-label_free_options_panel",
    "test-openswath_mscore_panel",
    "test-openswath_mscore_cutoff_panel",
    # Phase 1 DIANN panels (already mounted as hidden divs)
    "test-diann_lf_options_panel",
    "test-diann_intensity_column_panel",
    "test-qval_filter_panel",
    "test-qval_cutoff_panel",
    "test-qval_mbr_panel"
  )
  for (id in expected_panel_ids) {
    expect_true(
      grepl(paste0('id="', id, '"'), html_output, fixed = TRUE),
      info = paste("Missing hidden visibility container div id:", id)
    )
  }
})

test_that("loadpageUI exposes the TMT renderUI slot in place of duplicate-id panels", {
  # The two pre-existing TMT `conditionalPanel`s both declared
  # `ns("which.proteinid")` with different per-converter defaults — mounting
  # both as hidden divs would collide on a single ns() id. Phase 2 consolidated
  # them into a single `output[[tmt_options_ui]]` renderUI; the static UI
  # exposes a `uiOutput(ns("tmt_options_ui"))` slot instead.
  result <- loadpageUI("test")
  html_output <- as.character(result)
  expect_true(grepl('id="test-tmt_options_ui"', html_output, fixed = TRUE),
              info = "TMT options uiOutput slot not found in rendered UI")
  # And the static UI must NOT contain the literal `which.proteinid` input
  # node, since it is rendered server-side now.
  expect_false(grepl('id="test-which.proteinid"', html_output, fixed = TRUE),
               info = paste("Static UI must not mount a `which.proteinid` input;",
                            "it is emitted server-side via the tmt_options_ui",
                            "renderUI. A static occurrence would re-introduce",
                            "the duplicate-ns()-id collision Phase 2 fixed."))
})

test_that("Spectronaut regular-path anomaly UI is a server-rendered slot, not a static conditionalPanel", {
  # `calculate_anomaly_scores` + `run_order_file` are also declared by the
  # big-file Spectronaut helper (`create_spectronaut_large_annotation_ui`,
  # emitted by `output$spectronaut_options_ui`). To keep the two copies from
  # colliding on a shared ns() id, the regular path now emits them from
  # `output$spectronaut_anomaly_ui` (renderUI), which mounts only on the
  # regular path (filetype == 'spec' && !big_file_spec). The static
  # quality-filtering options therefore expose only the renderUI slot.
  options <- create_quality_filtering_options(NS("test"))
  options_html <- as.character(options)

  # The renderUI slot is present...
  expect_true(grepl('id="test-spectronaut_anomaly_ui"', options_html, fixed = TRUE),
              info = "spectronaut_anomaly_ui renderUI slot missing")
  # ...and the regular-path anomaly inputs are NOT mounted statically (they are
  # emitted server-side), so there is no duplicate-ns()-id with the big-file copy.
  expect_false(grepl("test-calculate_anomaly_scores", options_html, fixed = TRUE),
               info = "calculate_anomaly_scores must be server-rendered, not static")
  expect_false(grepl("test-run_order_file", options_html, fixed = TRUE),
               info = "run_order_file must be server-rendered, not static")
  # The old regular-path anomaly conditionalPanel JS condition must be gone.
  expect_false(grepl("input[&#39;loadpage-filetype&#39;] == &#39;spec&#39;",
                     options_html, fixed = TRUE),
               info = "regular-path anomaly conditionalPanel should be gone")
})

test_that("create_spectronaut_anomaly_ui emits the checkbox and (only when ticked) the run-order fileInput", {
  # Default (unticked): checkbox present, run-order fileInput ABSENT. The
  # fileInput is emitted by the renderUI only when the checkbox is ticked (no
  # conditionalPanel) — that renderUI gating is what keeps the regular and
  # big-file copies (same ns() ids) from coexisting in the DOM.
  html <- as.character(create_spectronaut_anomaly_ui(NS("test")))
  expect_true(grepl("test-calculate_anomaly_scores", html, fixed = TRUE))
  expect_true(grepl("Calculate Anomaly Scores", html, fixed = TRUE))
  expect_false(grepl("test-run_order_file", html, fixed = TRUE),
               info = "run-order fileInput must be absent when the checkbox is unticked")
  expect_false(grepl("data-display-if", html, fixed = TRUE),
               info = "no conditionalPanel — the nesting is renderUI-gated now")

  # Ticked: checkbox pre-checked (seed) + run-order fileInput present. Ids are
  # the SAME literals the big-file helper uses (no rename).
  checked <- as.character(create_spectronaut_anomaly_ui(NS("test"), TRUE))
  expect_true(grepl("test-run_order_file", checked, fixed = TRUE))
  expect_true(grepl("checked", checked, fixed = TRUE))
})

test_that("create_diann_anomaly_ui emits the checkbox and (only when ticked) the run-order fileInput", {
  # DIANN regular path, migrated from Phase 1 show/hide to the same renderUI-
  # gated pattern as the Spectronaut regular helper. Uses the diann_* ids
  # (unchanged, distinct from the big-file big_diann_* ids).
  html <- as.character(create_diann_anomaly_ui(NS("test")))
  expect_true(grepl("test-diann_calculate_anomaly_scores", html, fixed = TRUE))
  expect_true(grepl("Calculate Anomaly Scores", html, fixed = TRUE))
  expect_false(grepl("test-diann_run_order_file", html, fixed = TRUE),
               info = "run-order fileInput must be absent when the checkbox is unticked")
  expect_false(grepl("data-display-if", html, fixed = TRUE),
               info = "no conditionalPanel — the nesting is renderUI-gated now")

  checked <- as.character(create_diann_anomaly_ui(NS("test"), TRUE))
  expect_true(grepl("test-diann_run_order_file", checked, fixed = TRUE))
  expect_true(grepl("checked", checked, fixed = TRUE))
})

test_that("create_spectronaut_large_annotation_ui gates the run-order fileInput on the checkbox (renderUI, no conditionalPanel)", {
  # Big-file Spectronaut path: the run-order fileInput is emitted only when the
  # checkbox is ticked (was a conditionalPanel before). Shares the regular
  # path's ns() ids; renderUI mounting on mutually exclusive big_file_spec keeps
  # them from colliding.
  html <- as.character(create_spectronaut_large_annotation_ui(NS("test")))
  expect_true(grepl("test-big_spec_annotation", html, fixed = TRUE))
  expect_true(grepl("test-calculate_anomaly_scores", html, fixed = TRUE))
  expect_false(grepl("test-run_order_file", html, fixed = TRUE),
               info = "run-order fileInput must be absent when the checkbox is unticked")
  expect_false(grepl("data-display-if", html, fixed = TRUE),
               info = "no conditionalPanel — the nesting is renderUI-gated now")

  checked <- as.character(create_spectronaut_large_annotation_ui(NS("test"), TRUE))
  expect_true(grepl("test-run_order_file", checked, fixed = TRUE))
})

test_that("loadpageUI properly handles file input elements and validation", {
  # Test that file inputs are properly configured
  result <- loadpageUI("test")
  html_output <- as.character(result)
  
  # Should contain file input elements
  expect_true(grepl('type="file"', html_output),
              "No file input elements found")
  
  # Upload button should be disabled initially (using shinyjs)
  expect_true(grepl("proceed1", html_output),
              "Upload button not found")
  
  # The upload description renders server-side now (output$upload_description
  # fills this uiOutput slot; see STEP 3). The static UI carries only the
  # placeholder; the "User Guide" / "250 MB" text is asserted directly against
  # create_header_content() below.
  expect_true(grepl('id="test-upload_description"', html_output, fixed = TRUE),
              "Upload-description uiOutput slot not found")
})

# Test suite for loadpage UI module
test_that("loadpageUI creates proper structure", {
  # Test the main UI structure
  ui_output <- loadpageUI("test")
  
  expect_s3_class(ui_output, "shiny.tag.list")
  expect_true(any(grepl("container-fluid", as.character(ui_output))))
  expect_true(any(grepl("Upload data", as.character(ui_output))))
})

# Tests for create_header_content()
test_that("create_header_content includes required elements", {
  header <- create_header_content()
  header_html <- as.character(header)
  
  # Check for key text content
  expect_true(grepl("MSstats Pipeline", header_html))
  expect_true(grepl("User Guide", header_html))
  expect_true(grepl("MSstatsPTM", header_html))
  expect_true(grepl("CSV/TSV format", header_html))
  expect_true(grepl("250 MB", header_html))
  
  # Check for external links
  expect_true(grepl("msstats.org", header_html))
  expect_true(grepl("bioconductor.org", header_html))
  expect_true(grepl('target="_blank"', header_html))
})

# Tests for create_metabolomics_header_content()
test_that("create_metabolomics_header_content includes metabolomics-specific text", {
  header <- create_metabolomics_header_content()
  header_html <- as.character(header)

  # The metabolomics renderUI branch (output$upload_description) renders this.
  expect_true(grepl("metabolomics pipeline", header_html))
  expect_true(grepl("MZmine feature quant", header_html))
  expect_true(grepl("SIRIUS", header_html))
  expect_true(grepl("MSstats format", header_html))
  # Keeps the CSV/TSV + size note.
  expect_true(grepl("CSV/TSV", header_html))
  expect_true(grepl("250 MB", header_html))
})

# Tests for create_sample_dataset_descriptions()
test_that("create_sample_dataset_descriptions creates hidden divs with namespaced container IDs", {
  # Phase 2: the helper now requires `ns` and returns three hidden divs (not
  # conditionalPanels). Visibility is toggled server-side by
  # `register_loadpage_visibility_observers` on the
  # `filetype == 'sample' && LabelFreeType == <mode>` predicate.
  descriptions <- create_sample_dataset_descriptions(NS("test"))
  descriptions_html <- as.character(descriptions)

  # Three hidden container divs, one per LabelFreeType mode
  expect_true(grepl('id="test-sample_dda_description_panel"',
                    descriptions_html, fixed = TRUE),
              info = "DDA description hidden container missing")
  expect_true(grepl('id="test-sample_dia_description_panel"',
                    descriptions_html, fixed = TRUE),
              info = "DIA description hidden container missing")
  expect_true(grepl('id="test-sample_srm_prm_description_panel"',
                    descriptions_html, fixed = TRUE),
              info = "SRM/PRM description hidden container missing")

  # And none of them should still be conditionalPanels.
  expect_false(grepl("shiny-panel-conditional", descriptions_html, fixed = TRUE),
               info = paste("Sample-dataset descriptions must be hidden divs,",
                            "not conditionalPanels"))

  # The publication content must be preserved verbatim
  expect_true(grepl("DDA acquisition", descriptions_html))
  expect_true(grepl("DIA acquisition", descriptions_html))
  expect_true(grepl("SRM/PRM acquisition", descriptions_html))
  expect_true(grepl("Choi, M. et al", descriptions_html))
  expect_true(grepl("Selevsek, N. et al", descriptions_html))
  expect_true(grepl("Picotti, P. et al", descriptions_html))
})

# Tests for create_css_styling()
test_that("create_css_styling includes required CSS", {
  css <- create_css_styling()
  
  # Check that it's a proper tag structure (not character conversion)
  expect_s3_class(css, "shiny.tag")
  expect_equal(css$name, "head")
  
  # Check the children elements contain the expected content
  css_children <- css$children
  expect_true(length(css_children) >= 3)  # Should have 3 children (2 styles + 1 link)
  
  # Convert children to string to check content
  children_html <- paste(css_children, collapse = " ")
  expect_true(grepl("background-color:orange", children_html))
  expect_true(grepl("proceed1", children_html))
  expect_true(grepl("reset1", children_html))
  expect_true(grepl("style.css", children_html))
  
  # Check that it's a head tag with proper structure
  expect_true(any(sapply(css_children, function(x) {
    if (is.list(x) && !is.null(x$name)) {
      return(x$name == "style" || x$name == "link")
    }
    FALSE
  })))
})

# Tests for create_main_selection_controls()
test_that("create_main_selection_controls creates proper radio buttons", {
  controls <- create_main_selection_controls(NS("test"))
  controls_html <- as.character(controls)
  
  # Check for biological question options
  expect_true(grepl("Biological Question", controls_html))
  expect_true(grepl("Protein", controls_html))
  expect_true(grepl("Peptide", controls_html))
  expect_true(grepl("PTM", controls_html))
  
  # Check for label type options
  expect_true(grepl("Label Type", controls_html))
  expect_true(grepl("Label-Free", controls_html))
  expect_true(grepl("TMT", controls_html))
  
  # Check for file type options
  # The "Type of File" header is a static h4 (no template-dependent numbering).
  expect_true(grepl("Type of File", controls_html))
  expect_true(grepl("MSstats Format", controls_html))
  expect_true(grepl("Skyline", controls_html))
  expect_true(grepl("MaxQuant", controls_html))
  expect_true(grepl("FragPipe", controls_html))
  expect_true(grepl("DIANN", controls_html))
  
  # Check for proper namespace
  expect_true(grepl("test-BIO", controls_html))
  expect_true(grepl("test-DDA_DIA", controls_html))
  expect_true(grepl("test-filetype", controls_html))
})

# Tests for create_label_free_type_selection()
test_that("create_label_free_type_selection wraps the LabelFreeType radio in a hidden container", {
  # Phase 2: the conditionalPanel was replaced with
  # `shinyjs::hidden(div(id = ns(NAMESPACE_LOADPAGE$label_free_type_selection_panel), ...))`.
  # The BIO / filetype / DDA_DIA gating is now in
  # `loadpage_show_sample_dataset_label_free_type_selector()` (server-side).
  selection <- create_label_free_type_selection(NS("test"))
  selection_html <- as.character(selection)

  expect_true(grepl('id="test-label_free_type_selection_panel"', selection_html, fixed = TRUE),
              info = "Hidden container div missing")
  expect_false(grepl("shiny-panel-conditional", selection_html, fixed = TRUE),
               info = "LabelFreeType selector should no longer be a conditionalPanel")
  # Contents preserved
  expect_true(grepl("Type of Label-Free type", selection_html))
  expect_true(grepl("DDA", selection_html))
  expect_true(grepl("DIA", selection_html))
  expect_true(grepl("SRM/PRM", selection_html))
  # The LabelFreeType radio input ID must remain literal (no renames).
  expect_true(grepl("test-LabelFreeType", selection_html, fixed = TRUE),
              info = paste("LabelFreeType radio input ID missing or renamed;",
                           "Phase 2 explicitly forbids input-ID renames"))
})

# Tests for create_standard_uploads()
test_that("create_standard_uploads wraps the data fileInput in a hidden container", {
  # Phase 2: the conditionalPanel JS condition is gone; the panel is now a
  # hidden div with the data fileInput mounted inside. The list of converter
  # filetypes that should show this panel is encoded in
  # `loadpage_show_standard_quant_upload()` (truth-tabled in
  # test-loadpage-server-rendering.R).
  uploads <- create_standard_uploads(NS("test"))
  uploads_html <- as.character(uploads)

  expect_true(grepl('id="test-standard_quant_upload_panel"', uploads_html, fixed = TRUE),
              info = "Hidden container div missing")
  expect_false(grepl("shiny-panel-conditional", uploads_html, fixed = TRUE),
               info = "Standard quant upload should no longer be a conditionalPanel")
  # Contents preserved — header text + the fileInput
  expect_true(grepl("Upload quantification dataset", uploads_html))
  expect_true(grepl("shiny-input-file", uploads_html))
  expect_true(grepl("test-data", uploads_html, fixed = TRUE),
              info = "`data` fileInput input ID missing or renamed")
})

# Tests for create_msstats_uploads()
test_that("create_msstats_uploads creates different inputs for regular and PTM", {
  uploads <- create_msstats_uploads(NS("test"))
  uploads_html <- as.character(uploads)
  
  # Check for regular MSstats format
  expect_true(grepl("Upload data in MSstats Format", uploads_html))
  expect_true(grepl("test-msstatsdata", uploads_html))
  
  # Check for PTM MSstats format
  expect_true(grepl("Upload PTM data in MSstats Format", uploads_html))
  expect_true(grepl("test-msstatsptmdata", uploads_html))
  expect_true(grepl("Upload unmodified data in MSstats Format", uploads_html))
  expect_true(grepl("test-unmod", uploads_html))
})

# Tests for create_ptm_fragpipe_uploads()
test_that("create_ptm_fragpipe_uploads creates comprehensive PTM options", {
  uploads <- create_ptm_fragpipe_uploads(NS("test"))
  uploads_html <- as.character(uploads)
  
  # Check for required uploads
  expect_true(grepl("Upload PTM msstats dataset", uploads_html))
  expect_true(grepl("Upload PTM annotation file", uploads_html))
  expect_true(grepl("Upload global profiling msstats dataset", uploads_html))
  expect_true(grepl("Upload global profiling annotation file", uploads_html))
  
  # Check for processing options
  expect_true(grepl("modification id column", uploads_html))
  expect_true(grepl("localization_cutoff", uploads_html))
  expect_true(grepl("Remove unlocalized peptides", uploads_html))
  
  # Check default values
  expect_true(grepl("STY", uploads_html))
  expect_true(grepl("\\.75", uploads_html))
})

# Tests for create_maxquant_uploads()
test_that("create_maxquant_uploads creates proper file inputs", {
  uploads <- create_maxquant_uploads(NS("test"))
  uploads_html <- as.character(uploads)
  
  expect_true(grepl("Upload evidence.txt File", uploads_html))
  expect_true(grepl("Upload proteinGroups.txt File", uploads_html))
  expect_true(grepl("Upload annotation File", uploads_html))
  
  expect_true(grepl("test-evidence", uploads_html))
  expect_true(grepl("test-pGroup", uploads_html))
  expect_true(grepl("test-annot1", uploads_html))
})

# Tests for create_ptm_uploads()
test_that("create_ptm_uploads creates PTM-specific inputs", {
  uploads <- create_ptm_uploads(NS("test"))
  uploads_html <- as.character(uploads)
  
  # Check for PTM specific uploads
  expect_true(grepl("Upload PTM Input File", uploads_html))
  expect_true(grepl("Upload fasta File", uploads_html))
  expect_true(grepl("Upload Unmodified Protein Input File", uploads_html))
  
  # Check for modification labels
  expect_true(grepl("Modification Label", uploads_html))
  expect_true(grepl("FASTA file column name", uploads_html))
  
  # Check default values
  expect_true(grepl("uniprot_iso", uploads_html))
})

# Tests for create_processing_options()
test_that("create_processing_options creates TMT and label-free options", {
  options <- create_processing_options(NS("test"))
  options_html <- as.character(options)
  
  # Check for processing options structure
  expect_true(grepl("Select the options for pre-processing", options_html))
  expect_true(grepl("Use unique peptides", options_html))
  expect_true(grepl("Remove proteins with 1", options_html))
})

# Tests for create_quality_filtering_options()
test_that("create_quality_filtering_options creates filtering controls", {
  options <- create_quality_filtering_options(NS("test"))
  options_html <- as.character(options)

  expect_true(grepl("Filter with Q-value", options_html))
  expect_true(grepl("Filter with M-score", options_html))
  expect_true(grepl("Q-value cutoff", options_html))
  expect_true(grepl("M-score cutoff", options_html))
  expect_true(grepl("MBR Enabled", options_html))
  # DIANN regular-path anomaly UI is now a server-rendered slot (renderUI),
  # parallel to Spectronaut's — the inputs are emitted server-side, not static.
  expect_true(grepl('id="test-diann_anomaly_ui"', options_html, fixed = TRUE),
              info = "diann_anomaly_ui renderUI slot missing")
  expect_false(grepl("test-diann_calculate_anomaly_scores", options_html, fixed = TRUE),
               info = "diann_calculate_anomaly_scores must be server-rendered, not static")
  expect_false(grepl("test-diann_run_order_file", options_html, fixed = TRUE),
               info = "diann_run_order_file must be server-rendered, not static")
})

# Test order preservation in main selection controls
test_that("main selection controls maintain proper order", {
  controls <- create_main_selection_controls(NS("test"))
  controls_html <- as.character(controls)
  
  # Find positions of each section
  bio_pos <- regexpr("Biological Question", controls_html)
  label_pos <- regexpr("Label Type", controls_html)
  file_pos <- regexpr("Type of File", controls_html)

  # Verify correct order
  expect_true(bio_pos < label_pos)
  expect_true(label_pos < file_pos)

  # Headers are present (section numbering was removed per review).
  expect_true(grepl("Biological Question", controls_html))
  expect_true(grepl("Label Type", controls_html))
  expect_true(grepl("Type of File", controls_html))
})

# Test tooltip content is preserved
test_that("tooltips contain proper explanatory text", {
  controls <- create_main_selection_controls(NS("test"))
  controls_html <- as.character(controls)
  
  # Check for tooltip explanations
  expect_true(grepl("Select the biological question of interest", controls_html))
  expect_true(grepl("Label-free will process all label-free acquisitions", controls_html))
  expect_true(grepl("Choose the spectral processing tool used", controls_html))
  
  # Check for icon-wrapper and icon-tooltip classes
  expect_true(grepl("icon-wrapper", controls_html))
  expect_true(grepl("icon-tooltip", controls_html))
})

# Test file input configurations
test_that("file inputs have proper accept attributes", {
  # Test CSV-only inputs
  annot_input <- create_standard_annotation_uploads(NS("test"))
  annot_html <- as.character(annot_input)
  expect_true(grepl('accept=.*csv', annot_html))
})

test_that("create_mzmine_uploads renders the hidden panel, four input ids, and tooltips", {
  uploads_html <- as.character(create_mzmine_uploads(NS("test")))

  # Hidden panel container (NAMESPACE_LOADPAGE$mzmine_upload_panel).
  expect_true(grepl('id="test-mzmine_upload_panel"', uploads_html, fixed = TRUE))
  # The four namespaced file-input ids that getData()/proceed validation read.
  expect_true(grepl('id="test-mzmine_input"', uploads_html, fixed = TRUE))
  expect_true(grepl('id="test-mzmine_annotation"', uploads_html, fixed = TRUE))
  expect_true(grepl('id="test-mzmine_annotations"', uploads_html, fixed = TRUE))
  expect_true(grepl('id="test-sirius_annotations"', uploads_html, fixed = TRUE))

  # Fields carry help tooltips (icon-wrapper + icon-tooltip markup) and the
  # visible label text survives being wrapped in the h5 tooltip container.
  expect_true(grepl("icon-tooltip", uploads_html, fixed = TRUE))
  expect_true(grepl("MZmine feature quant table", uploads_html, fixed = TRUE))
  expect_true(grepl("SIRIUS structure identifications", uploads_html, fixed = TRUE))
})

# Tests for Spectronaut specific UI components
test_that("create_spectronaut_uploads creates UI outputs", {
  uploads <- create_spectronaut_uploads(NS("test"))
  uploads_html <- as.character(uploads)
  
  expect_true(grepl("spectronaut_header_ui", uploads_html))
  expect_true(grepl("spectronaut_file_selection_ui", uploads_html))
  expect_true(grepl("spectronaut_options_ui", uploads_html))
})

test_that("create_diann_uploads exposes the diann renderUI slots", {
  uploads <- create_diann_uploads(NS("test"))
  uploads_html <- as.character(uploads)

  expect_true(grepl("diann_header_ui", uploads_html))
  expect_true(grepl("diann_file_selection_ui", uploads_html))
  expect_true(grepl("diann_options_ui", uploads_html))
})

test_that("DIANN large-file helper functions create correct UI elements", {
  # Header
  header <- create_diann_header()
  expect_true(grepl("Upload MSstats report from DIANN", as.character(header)))

  # Mode selector
  mode_sel <- create_diann_mode_selector(NS("test"))
  mode_html <- as.character(mode_sel)
  expect_true(grepl("Large file mode", mode_html))
  expect_true(grepl("checkbox", mode_html))
  expect_true(grepl("test-big_file_diann", mode_html))

  # Standard UI
  std_ui <- create_diann_standard_ui(NS("test"))
  std_html <- as.character(std_ui)
  expect_true(grepl("file", std_html))
  expect_true(grepl("test-dianndata", std_html))

  # Large file UI
  large_ui <- create_diann_large_file_ui(NS("test"))
  large_html <- as.character(large_ui)
  expect_true(grepl("Browse for local file", large_html))
  expect_true(grepl("dianndata_big_path", large_html))
  expect_true(grepl("test-big_diann_browse", large_html))

  # Filter options
  filter_opts <- create_diann_large_filter_options(NS("test"))
  opts_html <- as.character(filter_opts)
  expect_true(grepl("MBR Enabled", opts_html))
  expect_true(grepl("Quantification column", opts_html))
  expect_true(grepl("Global Q-value cutoff", opts_html))
  expect_true(grepl("Protein group Q-value cutoff", opts_html))
  expect_true(grepl("FragmentQuantCorrected", opts_html))

  # Bottom UI
  bottom_ui <- create_diann_large_bottom_ui(NS("test"))
  bottom_html <- as.character(bottom_ui)
  expect_true(grepl("Max feature count", bottom_html))
  expect_true(grepl("Use unique peptides", bottom_html))
  expect_true(grepl("Aggregate PSMs", bottom_html))
  expect_true(grepl("Filter features with few observations", bottom_html))
  expect_true(grepl("Backend", bottom_html))
  expect_true(grepl("arrow", bottom_html))

  # Annotation + anomaly UI. The run-order fileInput is now renderUI-gated on
  # the checkbox (no show/hide panel): absent by default, present only when
  # calculate_anomaly_def = TRUE.
  annot_ui <- create_diann_large_annotation_ui(NS("test"))
  annot_html <- as.character(annot_ui)
  expect_true(grepl("Annotation file", annot_html))
  expect_true(grepl("test-big_diann_annotation", annot_html))
  expect_true(grepl("Calculate Anomaly Scores", annot_html))
  expect_true(grepl("test-big_diann_calculate_anomaly_scores", annot_html))
  expect_false(grepl("test-big_diann_run_order_file", annot_html, fixed = TRUE),
               info = "run-order fileInput must be absent when the checkbox is unticked")
  annot_html_checked <- as.character(create_diann_large_annotation_ui(NS("test"), TRUE))
  expect_true(grepl("test-big_diann_run_order_file", annot_html_checked, fixed = TRUE),
              info = "run-order fileInput must appear when calculate_anomaly_def = TRUE")
})

test_that("DIANN big-file gating now lives in the server predicate, not a JS condition", {
  # The DIANN big-file gate (`!big_file_diann`) lives in server-side visibility
  # code, not a JS `conditionalPanel` condition: `loadpage_show_qval_filter` /
  # `loadpage_show_standard_annot_upload` drive show/hide panels, and the DIANN
  # regular-path anomaly UI is now a renderUI slot (`diann_anomaly_ui`). So the
  # JS-encoded `loadpage-big_file_diann` string is not emitted statically. We
  # assert the gated containers / slots are present and the JS string is gone.
  result <- loadpageUI("test")
  html_output <- as.character(result)

  for (panel_id in c("test-standard_annot_upload_panel",
                     "test-qval_filter_panel",
                     "test-diann_anomaly_ui")) {
    expect_true(
      grepl(paste0('id="', panel_id, '"'), html_output, fixed = TRUE),
      info = paste("Big-file-gated panel container missing:", panel_id)
    )
  }
  expect_false(
    grepl("loadpage-big_file_diann", html_output, fixed = TRUE),
    info = paste("Static UI should no longer encode a `big_file_diann` JS",
                 "condition string; gating moved to server predicates in",
                 "R/loadpage-server-converter-options-panel.R")
  )
})

test_that("Spectronaut helper functions create correct UI elements", {
  # Header
  header <- create_spectronaut_header()
  expect_true(grepl("Upload MSstats scheme output from Spectronaut", as.character(header)))
  
  # Mode selector
  mode_sel <- create_spectronaut_mode_selector(NS("test"))
  expect_true(grepl("Large file mode", as.character(mode_sel)))
  expect_true(grepl("checkbox", as.character(mode_sel)))
  
  # Standard UI
  std_ui <- create_spectronaut_standard_ui(NS("test"))
  expect_true(grepl("file", as.character(std_ui)))
  expect_true(grepl("specdata", as.character(std_ui)))
  
  # Large file UI
  large_ui <- create_spectronaut_large_file_ui(NS("test"))
  large_ui_html <- as.character(large_ui)
  expect_true(grepl("Browse for local file", large_ui_html))
  expect_true(grepl("specdata_big_path", large_ui_html))
  
  # Filter options
  filter_opts <- create_spectronaut_large_filter_options(NS("test"))
  opts_html <- as.character(filter_opts)
  expect_true(grepl("Filter by excluded", opts_html))
  expect_true(grepl("Filter by identified", opts_html))
  expect_true(grepl("Filter by q-value", opts_html))
  
  # Q-value cutoff
  qval_ui <- create_spectronaut_qvalue_cutoff_ui(NS("test"))
  expect_true(grepl("Q-value cutoff", as.character(qval_ui)))
  expect_true(grepl("0.01", as.character(qval_ui)))
  
  # Bottom UI
  bottom_ui <- create_spectronaut_large_bottom_ui(NS("test"))
  bottom_html <- as.character(bottom_ui)
  expect_true(grepl("Max feature count", bottom_html))
  expect_true(grepl("Use unique peptides", bottom_html))
  expect_true(grepl("Aggregate PSMs", bottom_html))
  expect_true(grepl("Filter features with few observations", bottom_html))
})

# Tests for Metamorpheus specific UI components
test_that("Metamorpheus converter option exists in filetype choices", {
  result <- loadpageUI("test")
  html_output <- as.character(result)

  # Check that the display label "Metamorpheus" is present
  expect_true(grepl("Metamorpheus", html_output),
              info = "Metamorpheus label not found in UI")

  # Check that the radio button value "meta" is present
  expect_true(grepl("value=\"meta\"", html_output),
              info = "Metamorpheus radio button value 'meta' not found in UI")
})

test_that("Metamorpheus PTM upload fields exist in UI", {
  result <- loadpageUI("test")
  html_output <- as.character(result)

  # Modification IDs section is now a dynamic uiOutput (rendered server-side)
  expect_true(grepl("mod_id_meta_ui", html_output),
              info = "Metamorpheus PTM modification ID uiOutput placeholder not found in UI")
  expect_true(grepl("ptm_protein_annot", html_output),
              info = "Metamorpheus PTM protein annotation upload not found in UI")
})

test_that("create_meta_mod_id_selector shows dropdown when mods are available", {
  ui <- MSstatsShiny:::create_meta_mod_id_selector(
    shiny::NS("test"),
    c("[Mod A]", "[Mod B]")
  )
  html <- as.character(ui)
  expect_true(grepl("mod_id_meta_select", html))
  expect_true(grepl("Mod A", html))
  expect_true(grepl("Mod B", html))
  expect_true(grepl("__other__", html))
})

test_that("create_meta_mod_id_selector shows text input when no mods available", {
  ui <- MSstatsShiny:::create_meta_mod_id_selector(
    shiny::NS("test"),
    character(0)
  )
  html <- as.character(ui)
  expect_true(grepl("mod_id_meta_custom", html))
  expect_false(grepl("mod_id_meta_select", html))
})