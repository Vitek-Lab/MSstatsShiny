# Tests for the copy-to-clipboard wrapper used on the protein-name dropdowns.
# copyable_select() is a pure tag transform, so it is tested directly rather
# than through testServer(). The browser-side behaviour it enables (what text
# reaches the clipboard, the disabled state) lives in copy-select.js and is
# only observable in a real browser - see the manual walkthrough in the PR.

html_of <- function(tag) as.character(htmltools::renderTags(tag)$html)

test_that("copyable_select wraps the input without altering it", {
  inner <- shiny::selectizeInput("qc-qm_protein", "Show plot for",
                                 choices = c("", "P1"))
  wrapped <- MSstatsShiny:::copyable_select(inner)

  # the original input survives verbatim, id and all
  expect_true(grepl('id="qc-qm_protein"', html_of(wrapped), fixed = TRUE))
  expect_true(grepl('<option value="P1">P1</option>', html_of(wrapped),
                    fixed = TRUE))
  # and picks up the wrapper plus exactly one button
  expect_true(grepl('class="copyable-select"', html_of(wrapped), fixed = TRUE))
  expect_equal(
    lengths(regmatches(html_of(wrapped),
                       gregexpr("copyable-select-btn", html_of(wrapped)))),
    1L
  )
})

test_that("copyable_select carries the JS/CSS dependency", {
  wrapped <- MSstatsShiny:::copyable_select(
    shiny::selectInput("p", "l", c("A", "B")))
  deps <- htmltools::renderTags(wrapped)$dependencies
  names <- vapply(deps, function(d) d$name, character(1))

  expect_true("msstatsshiny-copyable-select" %in% names)
  dep <- deps[[which(names == "msstatsshiny-copyable-select")]]
  expect_equal(dep$script, "copy-select.js")
  expect_equal(dep$stylesheet, "copy-select.css")
})

test_that("the tooltip is used as both the accessible name and the live region", {
  wrapped <- MSstatsShiny:::copyable_select(
    shiny::selectInput("p", "l", c("A", "B")), "Copy analyte name")
  html <- html_of(wrapped)

  expect_true(grepl('aria-label="Copy analyte name"', html, fixed = TRUE))
  # role/aria-live are what let the flashed "Copied" reach a screen reader
  expect_true(grepl('role="status"', html, fixed = TRUE))
  expect_true(grepl('aria-live="polite"', html, fixed = TRUE))
})

test_that("copyable_select rejects anything but exactly one select", {
  # Two call sites build a tagList around the protein selector; wrapping at
  # that level would bind the button to the wrong input, so it must error.
  expect_error(
    MSstatsShiny:::copyable_select(
      shiny::tagList(shiny::selectInput("metric", "l", c("m1", "m2")),
                     shiny::selectizeInput("prot", "l", c("P1")))),
    "exactly one select input, got 2"
  )
  expect_error(MSstatsShiny:::copyable_select(NULL),
               "exactly one select input, got 0")
})

test_that("count_select_tags walks nested tag structures", {
  expect_equal(MSstatsShiny:::count_select_tags(shiny::selectInput("a", "l", 1:2)), 1L)
  expect_equal(MSstatsShiny:::count_select_tags(shiny::tags$div()), 0L)
  expect_equal(MSstatsShiny:::count_select_tags("not a tag"), 0L)
  expect_equal(
    MSstatsShiny:::count_select_tags(
      shiny::tags$div(shiny::tags$div(shiny::selectInput("a", "l", 1:2)),
                      shiny::selectizeInput("b", "l", 1:2))),
    2L
  )
})
