library(shiny)
# Mock data setup helper
create_mock_data <- function(type = "DDA", bio = "protein") {
  # Create mock preprocessed data
  mock_conditions <- factor(c("Group1", "Group1", "Group2", "Group2", "Group3", "Group3"))
  
  if (bio == "PTM") {
    list(
      PTM = list(
        ProteinLevelData = data.frame(
          Protein = rep(c("P1", "P2"), each = 6),
          Condition = rep(mock_conditions, 2),
          GROUP = rep(mock_conditions, 2),
          Abundance = rnorm(12)
        )
      )
    )
  } else {
    list(
      ProteinLevelData = data.frame(
        Protein = rep(c("P1", "P2"), each = 6),
        Condition = rep(mock_conditions, 2),
        GROUP = rep(mock_conditions, 2),
        Abundance = rnorm(12)
      )
    )
  }
}

create_mock_raw_data <- function() {
  data.frame(
    Protein = rep(c("P1", "P2", "P3"), each = 10),
    Run = rep(paste0("Run", 1:10), 3),
    Intensity = rnorm(30, mean = 1000, sd = 100)
  )
}

# Test suite
test_that("statmodelServer initializes correctly", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "protein",
          DDA_DIA = "DDA",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        list(c("P1", "P2", "P3"))
      }),
      preprocess_data = reactive({
        create_mock_data("DDA", "protein")
      })
    ),
    {
      # Test that reactive values are initialized
      expect_null(contrast$matrix)
      expect_null(contrast$row)
      expect_null(comp_list$dList)
      expect_null(significant$result)
    }
  )
})

test_that("condition_list() returns correct groups for DDA data", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "protein",
          DDA_DIA = "DDA",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        list(c("P1", "P2", "P3"))
      }),
      preprocess_data = reactive({
        create_mock_data("DDA", "protein")
      })
    ),
    {
      # Test choices reactive
      expected_choices <- c("Group1", "Group2", "Group3")
      expect_equal(as.character(condition_list()), expected_choices)
    }
  )
})

test_that("condition_list() returns correct groups for TMT data", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "protein",
          DDA_DIA = "TMT",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        list(c("P1", "P2", "P3"))
      }),
      preprocess_data = reactive({
        create_mock_data("TMT", "protein")
      })
    ),
    {
      # Test choices reactive for TMT
      expected_choices <- c("Group1", "Group2", "Group3")
      expect_equal(as.character(condition_list()), expected_choices)
    }
  )
})

test_that("condition_list() returns correct groups for PTM data", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "PTM",
          DDA_DIA = "DDA",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        list(c("P1", "P2", "P3"))
      }),
      preprocess_data = reactive({
        create_mock_data("DDA", "PTM")
      })
    ),
    {
      # Test choices reactive for PTM
      expected_choices <- c("Group1", "Group2", "Group3")
      expect_equal(as.character(condition_list()), expected_choices)
    }
  )
})

test_that("matrix_build creates correct pairwise comparison", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "protein",
          DDA_DIA = "DDA",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        list(c("P1", "P2", "P3"))
      }),
      preprocess_data = reactive({
        create_mock_data("DDA", "protein")
      })
    ),
    {
      # Set up custom comparison
      inputs <- list()
      inputs[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_custom_pairwise
      inputs[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1]] = "Group1"
      inputs[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2]] = "Group2"
      inputs[[NAMESPACE_STATMODEL$comparisons_submit]] <- 1
      do.call(session$setInputs, inputs)
      
      # Get the built matrix
      mat <- matrix_build()
      
      # Verify matrix structure
      expect_equal(nrow(mat), 1)
      expect_equal(ncol(mat), 3)
      expect_equal(colnames(mat), c("Group1", "Group2", "Group3"))
      expect_equal(rownames(mat), "Group1 vs Group2")
      
      # Verify matrix values
      expect_equal(mat[1, 1], 1)
      expect_equal(mat[1, 2], -1)
      expect_equal(mat[1, 3], 0)
    }
  )
})

test_that("matrix_build creates all pairwise comparisons", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "protein",
          DDA_DIA = "DDA",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        list(c("P1", "P2", "P3"))
      }),
      preprocess_data = reactive({
        create_mock_data("DDA", "protein")
      })
    ),
    {
      # Set up all pairwise comparisons
      inputs <- list()
      inputs[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_all_pairwise
      inputs[[NAMESPACE_STATMODEL$comparisons_submit]] <- 1
      do.call(session$setInputs, inputs)
      
      mat <- matrix_build()
      
      # With 3 groups, expect 3 pairwise comparisons
      expect_equal(nrow(mat), 3)
      expect_equal(ncol(mat), 3)
      
      # Verify row names contain comparisons
      expect_true(all(grepl("vs", rownames(mat))))
    }
  )
})

test_that("matrix_build creates all vs one comparisons", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "protein",
          DDA_DIA = "DDA",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        list(c("P1", "P2", "P3"))
      }),
      preprocess_data = reactive({
        create_mock_data("DDA", "protein")
      })
    ),
    {
      # Set up all vs one comparison
      inputs <- list()
      inputs[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_all_vs_one
      inputs[[NAMESPACE_STATMODEL$comparisons_all_vs_one_choice]] = "Group3"
      inputs[[NAMESPACE_STATMODEL$comparisons_submit]] <- 1
      do.call(session$setInputs, inputs)
      
      mat <- matrix_build()
      
      # With 3 groups, expect 2 comparisons (all others vs Group3)
      expect_equal(nrow(mat), 2)
      expect_equal(ncol(mat), 3)
      
      # Verify all comparisons are against Group3
      expect_true(all(grepl("Group3", rownames(mat))))
    }
  )
})

test_that("matrix_build creates custom non-pairwise comparison", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "protein",
          DDA_DIA = "DDA",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        list(c("P1", "P2", "P3"))
      }),
      preprocess_data = reactive({
        create_mock_data("DDA", "protein")
      })
    ),
    {
      # Set up custom non-pairwise comparison
      inputs <- list()
      inputs[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_custom_nonpairwise
      inputs[[NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_name]] = "CustomComparison"
      inputs[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, 1)]] = 1
      inputs[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, 2)]] = 1
      inputs[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, 3)]] = -2
      inputs[[NAMESPACE_STATMODEL$comparisons_submit]] <- 1
      do.call(session$setInputs, inputs)
      
      mat <- matrix_build()
      
      # Verify matrix structure
      expect_equal(nrow(mat), 1)
      expect_equal(rownames(mat), "CustomComparison")
      
      # Verify weights sum to 0
      expect_equal(sum(mat[1, ]), 0)
      
      # Verify individual weights
      expect_equal(mat[1, 1], 1)
      expect_equal(mat[1, 2], 1)
      expect_equal(mat[1, 3], -2)
    }
  )
})

test_that("check_cond validates same group selection", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "protein",
          DDA_DIA = "DDA",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        list(c("P1", "P2", "P3"))
      }),
      preprocess_data = reactive({
        create_mock_data("DDA", "protein")
      })
    ),
    {
      # Set up invalid comparison (same groups)
      inputs <- list()
      inputs[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_custom_pairwise
      inputs[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1]] = "Group1"
      inputs[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2]] = "Group1"
      inputs[[NAMESPACE_STATMODEL$comparisons_submit]] <- 1
      do.call(session$setInputs, inputs)
      
      # Should throw validation error
      expect_error(check_cond(), "Please select different groups")
    }
  )
})

test_that("check_cond validates contrast weights sum to zero", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "protein",
          DDA_DIA = "DDA",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        list(c("P1", "P2", "P3"))
      }),
      preprocess_data = reactive({
        create_mock_data("DDA", "protein")
      })
    ),
    {
      # Set up invalid weights (don't sum to 0)
      inputs <- list()
      inputs[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_custom_nonpairwise
      inputs[[NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_name]] = "BadComparison"
      inputs[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, 1)]] = 1
      inputs[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, 2)]] = 1
      inputs[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, 3)]] = 1
      inputs[[NAMESPACE_STATMODEL$comparisons_submit]] <- 1
      do.call(session$setInputs, inputs)
      
      # Should throw validation error
      expect_error(check_cond(), "The contrast weights should sum up to 0")
    }
  )
})

test_that("contrast_mode change resets matrix", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "protein",
          DDA_DIA = "DDA",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        list(c("P1", "P2", "P3"))
      }),
      preprocess_data = reactive({
        create_mock_data("DDA", "protein")
      })
    ),
    {
      # Build a matrix
      inputs <- list()
      inputs[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_custom_pairwise
      inputs[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1]] = "Group1"
      inputs[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2]] = "Group2"
      inputs[[NAMESPACE_STATMODEL$comparisons_submit]] <- 1
      do.call(session$setInputs, inputs)
      matrix_build()
      
      # Change comparison type
      inputs2 <- list()
      inputs2[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_all_pairwise
      do.call(session$setInputs, inputs2)
      
      # Verify matrix is reset
      expect_null(contrast$matrix)
      expect_null(comp_list$dList)
    }
  )
})

test_that("matrix doesn't add duplicate rows", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "protein",
          DDA_DIA = "DDA",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        list(c("P1", "P2", "P3"))
      }),
      preprocess_data = reactive({
        create_mock_data("DDA", "protein")
      })
    ),
    {
      # Add same comparison twice
      inputs <- list()
      inputs[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_custom_pairwise
      inputs[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1]] = "Group1"
      inputs[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2]] = "Group2"
      inputs[[NAMESPACE_STATMODEL$comparisons_submit]] <- 1
      do.call(session$setInputs, inputs)
      matrix_build()
      
      inputs[[NAMESPACE_STATMODEL$comparisons_submit]] <- 2
      do.call(session$setInputs, inputs)
      mat <- matrix_build()
      
      # Should still have only 1 row
      expect_equal(nrow(mat), 1)
    }
  )
})

test_that("Rownames reactive returns correct comparison names", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "protein",
          DDA_DIA = "DDA",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        list(c("P1", "P2", "P3"))
      }),
      preprocess_data = reactive({
        create_mock_data("DDA", "protein")
      })
    ),
    {
      # Build matrix with multiple comparisons
      inputs <- list()
      inputs[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_all_pairwise
      inputs[[NAMESPACE_STATMODEL$comparisons_submit]] <- 1
      do.call(session$setInputs, inputs)
      matrix_build()
      
      # Get rownames
      rnames <- Rownames()
      
      # Should have rownames for all comparisons
      expect_true(length(rnames) > 0)
      expect_true(all(grepl("vs", rnames)))
    }
  )
})

# Additional edge case tests
test_that("handles empty comparison list correctly", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "protein",
          DDA_DIA = "DDA",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        list(c("P1", "P2", "P3"))
      }),
      preprocess_data = reactive({
        create_mock_data("DDA", "protein")
      })
    ),
    {
      # Before any comparisons are made
      expect_null(contrast$matrix)
      expect_null(comp_list$dList)
    }
  )
})

# ============================================================================
# RESPONSE CURVE RATIO SCALE CHECKBOX TESTS
# ============================================================================

test_that("Ratio scale checkbox input can be toggled", {
  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(
          BIO = "protein",
          DDA_DIA = "DDA",
          filetype = "standard",
          proceed1 = 0
        )
      }),
      qc_input = reactive({
        list(normalization = "equalizeMedians")
      }),
      get_data = reactive({
        create_mock_raw_data()
      }),
      preprocess_data = reactive({
        create_mock_data("DDA", "protein")
      })
    ),
    {
      # Set ratio scale checkbox to TRUE
      session$setInputs(
        !!NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale := TRUE
      )
      expect_true(
        isTRUE(input[[NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale]]),
        info = "ratio_response should be TRUE when checkbox is checked"
      )

      # Set ratio scale checkbox to FALSE
      session$setInputs(
        !!NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale := FALSE
      )
      expect_false(
        isTRUE(input[[NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale]]),
        info = "ratio_response should be FALSE when checkbox is unchecked"
      )
    }
  )
})

# ============================================================================
# DOWNLOAD PLOT HANDLER TESTS
# ============================================================================

test_that("get_download_plot_filename returns ResponseCurvePlot for response curves", {
  filename <- MSstatsShiny:::get_download_plot_filename(CONSTANTS_STATMODEL$plot_type_response_curve)
  expect_true(grepl("ResponseCurvePlot", filename))
  expect_true(grepl("\\.zip$", filename))
})

test_that("get_download_plot_filename returns SummaryPlot for non-response-curve types", {
  for (plot_type in c(CONSTANTS_STATMODEL$plot_type_volcano_plot,
                      CONSTANTS_STATMODEL$plot_type_heatmap,
                      CONSTANTS_STATMODEL$plot_type_comparison_plot)) {
    filename <- MSstatsShiny:::get_download_plot_filename(plot_type)
    expect_true(grepl("SummaryPlot", filename),
                info = paste("Expected SummaryPlot for", plot_type))
    expect_true(grepl("\\.zip$", filename))
  }
})

test_that("get_download_plot_filename returns QQPlot for QQ plot type", {
  filename <- MSstatsShiny:::get_download_plot_filename(CONSTANTS_STATMODEL$plot_type_qq_plot)
  expect_true(grepl("^QQPlot-", filename))
  expect_true(grepl("\\.zip$", filename))
})

test_that("default_template_plot_type_choices excludes Heatmap when n_comparisons < 2", {
  for (n in c(0, 1)) {
    choices <- MSstatsShiny:::default_template_plot_type_choices(n)
    expect_true("Volcano Plot" %in% names(choices), info = paste("n =", n))
    expect_true("Comparison Plot" %in% names(choices), info = paste("n =", n))
    expect_true("QQ Plot" %in% names(choices), info = paste("n =", n))
    expect_false("Heatmap" %in% names(choices), info = paste("n =", n))
  }
})

test_that("default_template_plot_type_choices includes Heatmap when n_comparisons >= 2", {
  choices <- MSstatsShiny:::default_template_plot_type_choices(2)
  expect_true("Volcano Plot" %in% names(choices))
  expect_true("Heatmap" %in% names(choices))
  expect_true("Comparison Plot" %in% names(choices))
  expect_true("QQ Plot" %in% names(choices))
})

test_that("default_template_plot_type_choices binds QQ Plot to the QQPlots constant", {
  choices <- MSstatsShiny:::default_template_plot_type_choices(0)
  expect_equal(unname(choices[["QQ Plot"]]), CONSTANTS_STATMODEL$plot_type_qq_plot)
  expect_equal(CONSTANTS_STATMODEL$plot_type_qq_plot, "QQPlots")
})

test_that("default_template_plot_type_choices defaults to no-Heatmap when n_comparisons omitted", {
  expect_identical(MSstatsShiny:::default_template_plot_type_choices(),
                   MSstatsShiny:::default_template_plot_type_choices(0))
})

test_that("default_template_plot_type_choices omits QQ Plot when include_qq = FALSE", {
  for (n in c(0, 1, 2)) {
    choices <- MSstatsShiny:::default_template_plot_type_choices(n, include_qq = FALSE)
    expect_false("QQ Plot" %in% names(choices),
                 info = paste("n =", n, "include_qq = FALSE"))
    expect_true("Volcano Plot" %in% names(choices), info = paste("n =", n))
    expect_true("Comparison Plot" %in% names(choices), info = paste("n =", n))
  }
  expect_false("QQ Plot" %in% names(
    MSstatsShiny:::default_template_plot_type_choices(2, include_qq = FALSE)))
})

test_that("default_template_plot_type_choices includes QQ Plot when include_qq = TRUE", {
  for (n in c(0, 1, 2)) {
    choices <- MSstatsShiny:::default_template_plot_type_choices(n, include_qq = TRUE)
    expect_true("QQ Plot" %in% names(choices),
                info = paste("n =", n, "include_qq = TRUE"))
  }
})

test_that("zip_and_copy_plot creates a valid zip from PDF files", {
  # Create a real temp PDF to zip
  temp_pdf <- tempfile("test_plot_", fileext = ".pdf")
  pdf(temp_pdf)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf), add = TRUE)

  dest_file <- tempfile("download_", fileext = ".zip")
  on.exit(unlink(dest_file), add = TRUE)

  result <- MSstatsShiny:::zip_and_copy_plot(temp_pdf, dest_file)
  expect_true(result)
  expect_true(file.exists(dest_file))
  expect_gt(file.size(dest_file), 0)

  # Verify zip contains a PDF
  contents <- utils::unzip(dest_file, list = TRUE)
  expect_true(any(grepl("\\.pdf$", contents$Name)))
})

test_that("zip_and_copy_plot returns FALSE for empty file list", {
  dest_file <- tempfile("download_", fileext = ".zip")
  fn <- MSstatsShiny:::zip_and_copy_plot
  mockery::stub(fn, "showNotification", function(...) NULL)
  result <- fn(character(0), dest_file)
  expect_false(result)
})

test_that("zip_and_copy_plot handles multiple PDFs", {
  temp_pdfs <- vapply(1:3, function(i) {
    path <- tempfile(paste0("test_plot_", i, "_"), fileext = ".pdf")
    pdf(path); plot(1:10); dev.off()
    path
  }, character(1))
  on.exit(unlink(temp_pdfs), add = TRUE)

  dest_file <- tempfile("download_", fileext = ".zip")
  on.exit(unlink(dest_file), add = TRUE)

  result <- MSstatsShiny:::zip_and_copy_plot(temp_pdfs, dest_file)
  expect_true(result)

  contents <- utils::unzip(dest_file, list = TRUE)
  expect_equal(sum(grepl("\\.pdf$", contents$Name)), 3)
})

test_that("create_download_plot_handler is invoked with all 6 arguments", {
  handler_called <- FALSE
  handler_args <- NULL

  mockery::stub(statmodelServer, "create_download_plot_handler", function(...) {
    handler_called <<- TRUE
    handler_args <<- list(...)
  })

  testServer(
    statmodelServer,
    args = list(
      parent_session = MockShinySession$new(),
      loadpage_input = reactive({
        list(BIO = "protein", DDA_DIA = "DDA", filetype = "standard", proceed1 = 0)
      }),
      qc_input = reactive({ list(normalization = "equalizeMedians") }),
      get_data = reactive({ create_mock_raw_data() }),
      preprocess_data = reactive({ create_mock_data("DDA", "protein") })
    ),
    {
      expect_true(handler_called,
                  info = "create_download_plot_handler should be called during server init")
      expect_equal(length(handler_args), 9,
                   info = "create_download_plot_handler should receive 9 arguments")
    }
  )
})
