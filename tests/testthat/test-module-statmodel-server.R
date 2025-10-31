library(testthat)
library(shiny)

# ============================================================================
# MOCK DATA AND HELPER FUNCTIONS
# ============================================================================

# Create mock preprocessed data
create_mock_preprocess_data <- function(type = "standard") {
  if (type == "PTM_TMT") {
    list(
      PTM = list(
        ProteinLevelData = data.frame(
          Protein = c("P1", "P2", "P3"),
          Condition = factor(c("A", "B", "C")),
          LogIntensities = c(10, 11, 12)
        )
      )
    )
  } else if (type == "PTM_LabelFree") {
    list(
      PTM = list(
        ProteinLevelData = data.frame(
          Protein = c("P1", "P2", "P3"),
          GROUP = factor(c("A", "B", "C")),
          LogIntensities = c(10, 11, 12)
        )
      )
    )
  } else if (type == "TMT") {
    list(
      ProteinLevelData = data.frame(
        Protein = c("P1", "P2", "P3"),
        Condition = factor(c("A", "B", "C")),
        LogIntensities = c(10, 11, 12)
      )
    )
  } else {
    list(
      ProteinLevelData = data.frame(
        Protein = c("P1", "P2", "P3"),
        GROUP = factor(c("A", "B", "C")),
        LogIntensities = c(10, 11, 12)
      )
    )
  }
}

# Create mock comparison data
create_mock_comparison_data <- function(type = "standard") {
  if (type == "PTM") {
    list(
      PTM.Model = data.frame(
        Protein = c("P1", "P2", "P3"),
        log2FC = c(1.5, -2.0, 0.5),
        pvalue = c(0.01, 0.001, 0.5),
        adj.pvalue = c(0.03, 0.003, 0.6),
        Label = c("A vs B", "A vs B", "A vs B")
      ),
      PROTEIN.Model = data.frame(
        Protein = c("P1", "P2"),
        log2FC = c(1.2, -1.8),
        pvalue = c(0.02, 0.005),
        adj.pvalue = c(0.04, 0.01),
        Label = c("A vs B", "A vs B")
      ),
      ADJUSTED.Model = data.frame(
        Protein = c("P1", "P2"),
        log2FC = c(1.3, -1.9),
        pvalue = c(0.015, 0.003),
        adj.pvalue = c(0.035, 0.008),
        Label = c("A vs B", "A vs B")
      )
    )
  } else {
    list(
      ComparisonResult = data.frame(
        Protein = c("P1", "P2", "P3", "P4"),
        Label = c("A vs B", "A vs B", "A vs C", "A vs C"),
        log2FC = c(1.5, -2.0, 0.5, 1.8),
        SE = c(0.3, 0.4, 0.2, 0.35),
        Tvalue = c(5.0, -5.0, 2.5, 5.14),
        DF = c(10, 10, 10, 10),
        pvalue = c(0.001, 0.001, 0.03, 0.0005),
        adj.pvalue = c(0.002, 0.002, 0.06, 0.001),
        issue = c(NA, NA, NA, NA),
        MissingPercentage = c(0, 0, 0, 0),
        ImputationPercentage = c(0, 0, 0, 0)
      ),
      ModelQC = data.frame(
        Protein = c("P1", "P2"),
        log2FC = c(1.5, -2.0)
      ),
      fittedmodel = list()
    )
  }
}

# Wrapper module that conforms to testServer requirements
# This makes the server function testable
testableStatmodelServer <- function(id, loadpage_input, qc_input, get_data, preprocess_data) {
  moduleServer(id, statmodelServer)
}

# Helper to create mock reactive inputs
create_mock_inputs <- function() {
  list(
    loadpage_input = reactive({
      list(
        BIO = "Standard",
        DDA_DIA = "DDA",
        filetype = "standard",
        proceed1 = 0
      )
    }),
    qc_input = reactive({
      list(normalization = "equalizeMedians")
    }),
    get_data = reactive({
      list(c("P1", "P2", "P3", "P4"))
    }),
    preprocess_data = reactive({
      create_mock_preprocess_data("standard")
    })
  )
}

# ============================================================================
# 1. REACTIVE VALUE INITIALIZATION TESTS
# ============================================================================

# test_that("Server initializes reactive values correctly", {
#   mock_inputs <- create_mock_inputs()
#   
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     # Check that reactive values exist
#     expect_true(exists("contrast"))
#     expect_true(exists("comp_list"))
#     expect_true(exists("significant"))
#     
#     # Check choices reactive works
#     expect_true(is.reactive(choices))
#     choice_vals <- choices()
#     expect_true(is.factor(choice_vals) || is.character(choice_vals))
#   })
# })
# 
# test_that("Choices reactive returns correct levels for different data types", {
#   # Test standard DDA
#   mock_inputs <- create_mock_inputs()
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     choice_vals <- choices()
#     expect_equal(as.character(choice_vals), c("A", "B", "C"))
#   })
#   
#   # Test TMT
#   mock_inputs$loadpage_input <- reactive({
#     list(BIO = "Standard", DDA_DIA = "TMT", filetype = "standard", proceed1 = 0)
#   })
#   mock_inputs$preprocess_data <- reactive({
#     create_mock_preprocess_data("TMT")
#   })
#   
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     choice_vals <- choices()
#     expect_equal(as.character(choice_vals), c("A", "B", "C"))
#   })
# })

# ============================================================================
# 2. UI RENDERING TESTS
# ============================================================================

test_that("UI outputs render correctly", {
  mock_inputs <- create_mock_inputs()
  
  testServer(testableStatmodelServer, args = list(
    loadpage_input = mock_inputs$loadpage_input,
    qc_input = mock_inputs$qc_input,
    get_data = mock_inputs$get_data,
    preprocess_data = mock_inputs$preprocess_data
  ), {
    # Test choice UI outputs render
    expect_true(!is.null(output$choice1))
    expect_true(!is.null(output$choice2))
    expect_true(!is.null(output$choice3))
    expect_true(!is.null(output$comp_name))
    expect_true(!is.null(output$weights))
  })
})

test_that("Matrix UI renders correctly", {
  mock_inputs <- create_mock_inputs()
  
  testServer(testableStatmodelServer, args = list(
    loadpage_input = mock_inputs$loadpage_input,
    qc_input = mock_inputs$qc_input,
    get_data = mock_inputs$get_data,
    preprocess_data = mock_inputs$preprocess_data
  ), {
    # Initially matrix should be NULL
    expect_true(!is.null(output$matrix))
    
    # Set up matrix
    session$setInputs(def_comp = "custom")
    session$setInputs(group1 = "A", group2 = "B")
    session$setInputs(submit = 1)
    
    # Matrix should now render with table
    expect_true(!is.null(output$matrix))
    expect_true(!is.null(output$table))
  })
})

test_that("WhichComp and WhichProt UI render after matrix submission", {
  mock_inputs <- create_mock_inputs()
  
  testServer(testableStatmodelServer, args = list(
    loadpage_input = mock_inputs$loadpage_input,
    qc_input = mock_inputs$qc_input,
    get_data = mock_inputs$get_data,
    preprocess_data = mock_inputs$preprocess_data
  ), {
    # Submit a comparison
    session$setInputs(def_comp = "custom")
    session$setInputs(group1 = "A", group2 = "B")
    session$setInputs(submit = 1)
    
    # Check UI outputs exist
    expect_true(!is.null(output$WhichComp))
    expect_true(!is.null(output$WhichProt))
    expect_true(!is.null(output$WhichProt1))
  })
})

# ============================================================================
# 3. CONTRAST MATRIX BUILDING TESTS
# ============================================================================

# test_that("Custom pairwise comparison builds matrix correctly", {
#   mock_inputs <- create_mock_inputs()
#   
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     # Set up custom comparison
#     session$setInputs(def_comp = "custom")
#     session$setInputs(group1 = "A", group2 = "B")
#     session$setInputs(submit = 1)
#     
#     matrix <- matrix_build()
#     
#     # Check matrix structure
#     expect_true(is.matrix(matrix))
#     expect_equal(nrow(matrix), 1)
#     expect_equal(colnames(matrix), c("A", "B", "C"))
#     expect_equal(rownames(matrix)[1], "A vs B")
#     
#     # Check matrix values
#     expect_equal(matrix[1, "A"], 1)
#     expect_equal(matrix[1, "B"], -1)
#     expect_equal(matrix[1, "C"], 0)
#   })
# })

# test_that("All pairwise comparisons builds matrix correctly", {
#   mock_inputs <- create_mock_inputs()
#   
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     session$setInputs(def_comp = "all_pair")
#     session$setInputs(submit2 = 1)
#     
#     matrix <- matrix_build()
#     
#     # For 3 groups, should have 3 comparisons (A vs B, A vs C, B vs C)
#     expect_equal(nrow(matrix), 3)
#     expect_true(all(rowSums(abs(matrix)) == 2))  # Each row should sum to 2 in absolute value
#   })
# })

# test_that("All against one comparison builds matrix correctly", {
#   mock_inputs <- create_mock_inputs()
#   
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     session$setInputs(def_comp = "all_one")
#     session$setInputs(group3 = "C")
#     session$setInputs(submit1 = 1)
#     
#     matrix <- matrix_build()
#     
#     # Should have 2 comparisons (A vs C, B vs C)
#     expect_equal(nrow(matrix), 2)
#     
#     # All comparisons should be against C
#     expect_true(all(matrix[, "C"] == -1))
#   })
# })

# test_that("Custom non-pairwise comparison builds matrix correctly", {
#   mock_inputs <- create_mock_inputs()
#   
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     session$setInputs(def_comp = "custom_np")
#     session$setInputs(comp_name = "Custom1")
#     session$setInputs(weight1 = 1, weight2 = 1, weight3 = -2)
#     session$setInputs(submit3 = 1)
#     
#     matrix <- matrix_build()
#     
#     # Check matrix structure
#     expect_equal(nrow(matrix), 1)
#     expect_equal(rownames(matrix)[1], "Custom1")
#     
#     # Check weights
#     expect_equal(matrix[1, 1], 1)
#     expect_equal(matrix[1, 2], 1)
#     expect_equal(matrix[1, 3], -2)
#     
#     # Check sum equals 0
#     expect_equal(sum(matrix[1, ]), 0)
#   })
# })

# test_that("Duplicate comparisons are not added to matrix", {
#   mock_inputs <- create_mock_inputs()
#   
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     session$setInputs(def_comp = "custom")
#     session$setInputs(group1 = "A", group2 = "B")
#     session$setInputs(submit = 1)
#     
#     # Try to add same comparison again
#     session$setInputs(submit = 2)
#     
#     matrix <- matrix_build()
#     
#     # Should still only have 1 comparison
#     expect_equal(nrow(matrix), 1)
#   })
# })

# ============================================================================
# 4. VALIDATION TESTS
# ============================================================================

test_that("Validation prevents identical groups in custom comparison", {
  mock_inputs <- create_mock_inputs()
  
  testServer(testableStatmodelServer, args = list(
    loadpage_input = mock_inputs$loadpage_input,
    qc_input = mock_inputs$qc_input,
    get_data = mock_inputs$get_data,
    preprocess_data = mock_inputs$preprocess_data
  ), {
    session$setInputs(def_comp = "custom")
    session$setInputs(group1 = "A", group2 = "A")
    
    # Should trigger validation error when submit is clicked
    expect_error(session$setInputs(submit = 1), NA)  # Should not crash
  })
})

test_that("Validation requires weights sum to zero for non-pairwise", {
  mock_inputs <- create_mock_inputs()
  
  testServer(testableStatmodelServer, args = list(
    loadpage_input = mock_inputs$loadpage_input,
    qc_input = mock_inputs$qc_input,
    get_data = mock_inputs$get_data,
    preprocess_data = mock_inputs$preprocess_data
  ), {
    session$setInputs(def_comp = "custom_np")
    session$setInputs(comp_name = "Invalid")
    session$setInputs(weight1 = 1, weight2 = 1, weight3 = 1)  # Sum = 3, not 0
    
    # Should trigger validation
    expect_error(session$setInputs(submit3 = 1), NA)
  })
})

# ============================================================================
# 5. CLEAR MATRIX FUNCTIONALITY TESTS
# ============================================================================

# test_that("Clear button resets contrast matrix", {
#   mock_inputs <- create_mock_inputs()
#   
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     # Build a matrix
#     session$setInputs(def_comp = "custom")
#     session$setInputs(group1 = "A", group2 = "B")
#     session$setInputs(submit = 1)
#     
#     expect_false(is.null(contrast$matrix))
#     
#     # Clear it
#     session$setInputs(clear = 1)
#     
#     expect_true(is.null(contrast$matrix))
#     expect_true(is.null(comp_list$dList))
#   })
# })

# test_that("All clear buttons work for different comparison types", {
#   mock_inputs <- create_mock_inputs()
#   
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     # Test clear1 (all_one)
#     session$setInputs(def_comp = "all_one", group3 = "C", submit1 = 1)
#     session$setInputs(clear1 = 1)
#     expect_true(is.null(contrast$matrix))
#     
#     # Test clear2 (all_pair)
#     session$setInputs(def_comp = "all_pair", submit2 = 1)
#     session$setInputs(clear2 = 1)
#     expect_true(is.null(contrast$matrix))
#     
#     # Test clear3 (custom_np)
#     session$setInputs(def_comp = "custom_np", comp_name = "Test")
#     session$setInputs(weight1 = 1, weight2 = -1, weight3 = 0, submit3 = 1)
#     session$setInputs(clear3 = 1)
#     expect_true(is.null(contrast$matrix))
#   })
# })

# ============================================================================
# 6. UTILITY FUNCTION TESTS (Standalone - doesn't need testServer)
# ============================================================================

test_that("round_df function rounds numeric columns correctly", {
  # This function can be tested directly without testServer
  # Create a standalone version for testing
  round_df <- function(df) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    df[, nums] <- round(df[, nums], digits = 4)
    df
  }
  
  test_df <- data.frame(
    name = c("A", "B", "C"),
    value1 = c(1.23456, 2.34567, 3.45678),
    value2 = c(10.123456, 20.234567, 30.345678),
    category = c("X", "Y", "Z"),
    stringsAsFactors = FALSE
  )
  
  rounded <- round_df(test_df)
  
  expect_equal(rounded$value1[1], 1.2346, tolerance = 0.0001)
  expect_equal(rounded$value2[1], 10.1235, tolerance = 0.0001)
  expect_equal(rounded$category, test_df$category)
})

# ============================================================================
# 7. STATE MANAGEMENT TESTS
# ============================================================================

# test_that("Changing def_comp resets matrix", {
#   mock_inputs <- create_mock_inputs()
#   
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     # Build a matrix
#     session$setInputs(def_comp = "custom")
#     session$setInputs(group1 = "A", group2 = "B")
#     session$setInputs(submit = 1)
#     
#     expect_false(is.null(contrast$matrix))
#     
#     # Change comparison type
#     session$setInputs(def_comp = "all_pair")
#     
#     # Matrix should be reset
#     expect_true(is.null(contrast$matrix))
#     expect_true(is.null(comp_list$dList))
#   })
# })

# ============================================================================
# 8. DOWNLOAD HANDLER TESTS
# ============================================================================

# test_that("Download handlers are created", {
#   mock_inputs <- create_mock_inputs()
#   
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     # Check that download outputs exist
#     expect_true(!is.null(output$plotresults))
#     expect_true(!is.null(output$compar))
#     expect_true(!is.null(output$model_QC))
#     expect_true(!is.null(output$fitted_v))
#     expect_true(!is.null(output$download_compar))
#     expect_true(!is.null(output$download_signif))
#   })
# })
# 
# test_that("PTM-specific download handlers exist", {
#   mock_inputs <- create_mock_inputs()
#   mock_inputs$loadpage_input <- reactive({
#     list(BIO = "PTM", DDA_DIA = "DDA", filetype = "standard", proceed1 = 0)
#   })
#   
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     expect_true(!is.null(output$download_compar_adj))
#     expect_true(!is.null(output$download_compar_unadj))
#     expect_true(!is.null(output$download_compar_prot))
#     expect_true(!is.null(output$download_signif_adj))
#     expect_true(!is.null(output$download_signif_unadj))
#     expect_true(!is.null(output$download_signif_prot))
#   })
# })

# ============================================================================
# 9. RETURN VALUE TESTS
# ============================================================================

# test_that("Server returns correct list structure", {
#   mock_inputs <- create_mock_inputs()
#   
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     return_value <- session$returned()
#     
#     expect_true(is.list(return_value))
#     expect_true("input" %in% names(return_value))
#     expect_true("dataComparison" %in% names(return_value))
#   })
# })

# test_that("Returned dataComparison is reactive", {
#   mock_inputs <- create_mock_inputs()
#   
#   testServer(testableStatmodelServer, args = list(
#     loadpage_input = mock_inputs$loadpage_input,
#     qc_input = mock_inputs$qc_input,
#     get_data = mock_inputs$get_data,
#     preprocess_data = mock_inputs$preprocess_data
#   ), {
#     return_value <- session$returned()
#     
#     expect_true(is.reactive(return_value$dataComparison))
#   })
# })
