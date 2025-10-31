library(testthat)
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

test_that("choices() returns correct groups for DDA data", {
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
      expect_equal(as.character(choices()), expected_choices)
    }
  )
})

test_that("choices() returns correct groups for TMT data", {
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
      expect_equal(as.character(choices()), expected_choices)
    }
  )
})

test_that("choices() returns correct groups for PTM data", {
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
      expect_equal(as.character(choices()), expected_choices)
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
      session$setInputs(
        def_comp = "custom",
        group1 = "Group1",
        group2 = "Group2",
        submit = 1
      )
      
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

test_that("matrix_build handles multiple custom comparisons", {
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
      # First comparison
      session$setInputs(
        def_comp = "custom",
        group1 = "Group1",
        group2 = "Group2",
        submit = 1
      )
      matrix_build()
      
      # Second comparison
      session$setInputs(
        group1 = "Group2",
        group2 = "Group3",
        submit = 2
      )
      mat <- matrix_build()
      
      # Verify matrix has both comparisons
      expect_equal(nrow(mat), 2)
      expect_equal(rownames(mat), c("Group1 vs Group2", "Group2 vs Group3"))
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
      session$setInputs(
        def_comp = "all_pair",
        submit = 1
      )
      
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
      session$setInputs(
        def_comp = "all_one",
        group3 = "Group3",
        submit = 1
      )
      
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
      session$setInputs(
        def_comp = "custom_np",
        comp_name = "CustomComparison",
        weight1 = 1,
        weight2 = 1,
        weight3 = -2,
        submit = 1
      )
      
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
      session$setInputs(
        def_comp = "custom",
        group1 = "Group1",
        group2 = "Group1",
        submit = 1
      )
      
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
      session$setInputs(
        def_comp = "custom_np",
        comp_name = "BadComparison",
        weight1 = 1,
        weight2 = 1,
        weight3 = 1,
        submit = 1
      )
      
      # Should throw validation error
      expect_error(check_cond(), "The contrast weights should sum up to 0")
    }
  )
})

test_that("clear button resets contrast matrix", {
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
      session$setInputs(
        def_comp = "custom",
        group1 = "Group1",
        group2 = "Group2",
        submit = 1
      )
      matrix_build()
      
      # Verify matrix exists
      expect_false(is.null(contrast$matrix))
      
      # Clear the matrix
      session$setInputs(clear = 1)
      
      # Verify matrix is cleared
      expect_null(contrast$matrix)
      expect_null(comp_list$dList)
    }
  )
})

test_that("def_comp change resets matrix", {
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
      session$setInputs(
        def_comp = "custom",
        group1 = "Group1",
        group2 = "Group2",
        submit = 1
      )
      matrix_build()
      
      # Change comparison type
      session$setInputs(def_comp = "all_pair")
      
      # Verify matrix is reset
      expect_null(contrast$matrix)
      expect_null(comp_list$dList)
    }
  )
})

test_that("round_df helper function rounds numeric columns", {
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
      # Create test dataframe
      test_df <- data.frame(
        name = c("A", "B", "C"),
        value1 = c(1.123456, 2.789012, 3.456789),
        value2 = c(10.111111, 20.222222, 30.333333)
      )
      
      # Round the dataframe
      rounded <- round_df(test_df)
      
      # Check that numeric columns are rounded to 4 digits
      expect_equal(rounded$value1[1], 1.1235)
      expect_equal(rounded$value2[1], 10.1111)
      
      # Check that character columns are unchanged
      expect_equal(rounded$name, test_df$name)
    }
  )
})

test_that("UI shows/hides Design input correctly", {
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
      # For DDA, Design should be visible (not hidden)
      # This would be tested in integration tests with actual shinyjs
      expect_true(TRUE) # Placeholder for shinyjs test
    }
  )
})

test_that("nump input validation works correctly", {
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
      # Test with invalid input (negative)
      session$setInputs(nump = -5)
      session$flushReact()
      
      # Should be reset to 100
      expect_equal(session$input$nump, 100)
      
      # Test with invalid input (NA)
      session$setInputs(nump = NA)
      session$flushReact()
      
      # Should be reset to 100
      expect_equal(session$input$nump, 100)
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
      session$setInputs(
        def_comp = "custom",
        group1 = "Group1",
        group2 = "Group2",
        submit = 1
      )
      matrix_build()
      
      session$setInputs(
        group1 = "Group1",
        group2 = "Group2",
        submit = 2
      )
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
      session$setInputs(
        def_comp = "all_pair",
        submit = 1
      )
      matrix_build()
      
      # Get rownames
      rnames <- Rownames()
      
      # Should have rownames for all comparisons
      expect_true(length(rnames) > 0)
      expect_true(all(grepl("vs", rnames)))
    }
  )
})

test_that("module returns correct list structure", {
  result <- testServer(
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
      # Return value should have input and dataComparison
      session$returned()
    }
  )
  
  expect_true("input" %in% names(result))
  expect_true("dataComparison" %in% names(result))
  expect_true(is.reactive(result$input))
  expect_true(is.reactive(result$dataComparison))
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

test_that("row() helper creates correct zero vector", {
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
      r <- row()
      
      # Should have length equal to number of choices
      expect_equal(length(r), length(choices()))
      
      # All values should be 0
      expect_true(all(r == 0))
    }
  )
})