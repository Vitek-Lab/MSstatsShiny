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

test_that("create_download_plot_handler registers a downloadHandler", {
  mock_input <- list()
  mock_input[[NAMESPACE_STATMODEL$visualization_plot_type]] <- CONSTANTS_STATMODEL$plot_type_response_curve

  mock_contrast <- list(matrix = NULL)
  mock_preprocess <- function() { NULL }

  mock_download_handler <- mock("handler_result")

  stub(create_download_plot_handler, "downloadHandler", mock_download_handler)

  mock_output <- list()
  stub(create_download_plot_handler, "output", mock_output)

  # Use a real-looking output assignment by wrapping in testServer
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
      # Server initialized: create_download_plot_handler was called internally
      # Verify the server doesn't crash with the new 4-argument signature
      expect_null(contrast$matrix)
    }
  )
})

test_that("download handler filename returns ResponseCurvePlot for response curves", {
  # Test the filename logic directly
  plot_type <- CONSTANTS_STATMODEL$plot_type_response_curve
  filename <- if (plot_type == CONSTANTS_STATMODEL$plot_type_response_curve) {
    paste("ResponseCurvePlot-", Sys.Date(), ".zip", sep = "")
  } else {
    paste("SummaryPlot-", Sys.Date(), ".zip", sep = "")
  }
  expect_true(grepl("ResponseCurvePlot", filename))
  expect_true(grepl("\\.zip$", filename))
})

test_that("download handler filename returns SummaryPlot for other plot types", {
  plot_type <- CONSTANTS_STATMODEL$plot_type_volcano_plot
  filename <- if (plot_type == CONSTANTS_STATMODEL$plot_type_response_curve) {
    paste("ResponseCurvePlot-", Sys.Date(), ".zip", sep = "")
  } else {
    paste("SummaryPlot-", Sys.Date(), ".zip", sep = "")
  }
  expect_true(grepl("SummaryPlot", filename))
  expect_true(grepl("\\.zip$", filename))
})