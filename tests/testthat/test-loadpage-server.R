test_that(".get_data_source_type correctly identifies the data loading path", {
  # This test checks the helper function that determines whether to use the
  # standard data loader or the special loader for large Spectronaut files.
  # This approach is simpler and more robust than a complex shiny::testServer test.
  
  # Case 1: Spectronaut file with the 'big file' checkbox checked
  expect_equal(.get_data_source_type("spec", TRUE), "big_spectronaut")
  
  # Case 2: Spectronaut file without the 'big file' checkbox
  expect_equal(.get_data_source_type("spec", FALSE), "standard")
  
  # Case 3: A non-Spectronaut file (should always be standard)
  expect_equal(.get_data_source_type("maxq", FALSE), "standard")
  
  # Case 4: A non-Spectronaut file where the Spectronaut checkbox might be TRUE
  # (though the UI should prevent this, the logic should be robust)
  expect_equal(.get_data_source_type("maxq", TRUE), "standard")
  
  # Case 5: Input is NULL (initial state)
  expect_equal(.get_data_source_type("spec", NULL), "standard")
})
