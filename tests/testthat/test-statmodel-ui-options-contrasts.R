test_that("create_response_matrix_panel returns a conditionalPanel", {
  result <- create_response_matrix_panel(NS("statmodel"))
  
  expect_equal(result$name, "div")
  expect_true("shiny-panel-conditional" %in% result$attribs$class)
})

test_that("create_response_matrix_panel conditionalPanel has correct condition attribute", {
  result <- create_response_matrix_panel(NS("statmodel"))
  
  expected_condition <- "input['statmodel-contrast_mode'] == 'response_curve'"
  expect_equal(result$attribs$`data-display-if`, expected_condition)
})

test_that("create_response_matrix_panel panel contains h5 header with correct text", {
  result <- create_response_matrix_panel(NS("statmodel"))
  
  children <- result$children
  h5_element <- children[[1]]
  
  expect_equal(h5_element$name, "h5")
  expect_equal(h5_element$children[[1]], "Add Condition to Response Mapping:")
})

test_that("create_response_matrix_panel panel contains uiOutput with namespaced ID", {
  ns <- NS("statmodel")
  result <- create_response_matrix_panel(ns)
  
  ui_output <- result$children[[2]]
  
  expect_equal(ui_output$name, "div")
  expect_equal(ui_output$attribs$id, "statmodel-choice3")
})

test_that("create_response_matrix_panel textInput has correct properties", {
  ns <- NS("statmodel")
  result <- create_response_matrix_panel(ns)
  
  text_input <- result$children[[3]]

  expect_true(grepl("X-Axis Label:", as.character(text_input)))
  expect_true(grepl("e.g., Dosage, Time", as.character(text_input)))
})

test_that("create_response_matrix_panel numericInput has correct properties", {
  ns <- NS("statmodel")
  result <- create_response_matrix_panel(ns)
  
  numeric_input <- result$children[[4]]

  expect_true(grepl("Response:", as.character(numeric_input)))
})

test_that("create_response_matrix_panel panel contains submit actionButton with correct properties", {
  ns <- NS("statmodel")
  result <- create_response_matrix_panel(ns)
  
  submit_button <- result$children[[5]]
  
  expect_equal(submit_button$name, "button")
  expect_true(grepl("Add Entry", as.character(submit_button)))
})

test_that("create_response_matrix_panel panel contains clear actionButton with correct properties", {
  ns <- NS("statmodel")
  result <- create_response_matrix_panel(ns)
  
  clear_button <- result$children[[6]]
  
  expect_equal(clear_button$name, "button")
  expect_true(grepl("Clear All Data", as.character(clear_button)))
})

test_that("create_response_matrix_panel all child elements are present in correct order", {
  result <- create_response_matrix_panel(NS("statmodel"))
  
  expect_length(result$children, 6)
  expect_true(grepl("statmodel-choice3", as.character(result$children[[2]])))
  expect_true(grepl("statmodel-response_curve_xaxis", as.character(result$children[[3]])))
  expect_true(grepl("statmodel-response_curve_amount", as.character(result$children[[4]])))
  expect_true(grepl("statmodel-submit4", as.character(result$children[[5]])))
  expect_true(grepl("statmodel-clear4", as.character(result$children[[6]])))
})