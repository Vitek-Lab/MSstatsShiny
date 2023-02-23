library(shinytest2)

test_that("Initial Shiny values are consistent", {
  shiny_app = launch_MSstatsShiny(testMode = TRUE)
  app <- AppDriver$new(shiny_app, name = "MSstatsShiny")

  # app$expect_values()
})
