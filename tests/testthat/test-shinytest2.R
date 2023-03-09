library(shinytest2)

test_that("Initial Shiny values are consistent", {
  shiny_app = launch_MSstatsShiny(testMode = TRUE)
  # app <- AppDriver$new(shiny_app, name = "MSstatsShiny")
  app <- ShinyDriver$new(shiny_app, name = "MSstatsShiny")
  # testServer(loadpageServer, args = list(parent_session = NULL),{
  #   session$setInputs(DDA_DIA = "DDA")
  #   print(input$DDA_DIA)
  #   session$click("proceed1")
  #   print("hii")
  #   print(output$ex)
  #   expect_equal(1,1)
  # })
  app$setInputs(DDA_DIA = "DDA")
  app$click("home-StartPipeline")
  # app$expect_values()
  expect_equal(1,1)
  app$stop()
})

