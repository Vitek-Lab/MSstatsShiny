# ============================================================================
# Copy-to-clipboard wrapper for protein-name dropdowns
# ============================================================================

#' HTML dependency carrying the copy-button assets.
#'
#' Delivered as a dependency rather than as `tags$link` / `tags$script` in the
#' page headers for two reasons: every call site renders inside `renderUI()`,
#' and `expdesUI()` does not load `assets/style.css` at all, so a header-tag
#' approach would silently skip the experimental design dropdown.
#'
#' @noRd
copyable_select_dependency <- function() {
  htmltools::htmlDependency(
    name = "msstatsshiny-copyable-select",
    version = as.character(utils::packageVersion("MSstatsShiny")),
    src = c(file = system.file("assets", package = "MSstatsShiny")),
    script = "copy-select.js",
    stylesheet = "copy-select.css"
  )
}

#' Count `select` elements in a tag tree.
#'
#' Used only to enforce the one-select contract of [copyable_select()].
#'
#' @noRd
count_select_tags <- function(x) {
  if (inherits(x, "shiny.tag")) {
    return(as.integer(identical(x$name, "select")) + count_select_tags(x$children))
  }
  if (is.list(x)) {
    return(sum(vapply(x, count_select_tags, integer(1))))
  }
  0L
}

#' Add a copy-to-clipboard button to a select input.
#'
#' Wraps an already-built [shiny::selectInput()] or [shiny::selectizeInput()]
#' so the user can copy the selected protein name out of the dropdown.
#' Selectize renders the current selection as a `div`, which browsers will not
#' let the user select as text, so a button is the only reliable affordance.
#'
#' This is a pure wrapper: the input keeps its id, choices, label and options
#' untouched. The button finds its select by DOM traversal, so the namespaced
#' `inputId` does not need to be repeated here.
#'
#' Wrap one select, not a `tagList` of several. The button has no way to know
#' which of several selects it belongs to, so that case is rejected rather
#' than silently binding to the first one.
#'
#' @param select_tag A select input tag, as returned by `selectInput()` or
#'   `selectizeInput()`.
#' @param tooltip Hover text for the button, and the text announced to screen
#'   readers as the button's name.
#'
#' @return A `div` containing `select_tag`, the copy button, and the JS/CSS
#'   dependency.
#'
#' @noRd
copyable_select <- function(select_tag, tooltip = "Copy name") {
  n_selects <- count_select_tags(select_tag)
  if (n_selects != 1L) {
    stop("copyable_select() needs exactly one select input, got ", n_selects,
         ". Wrap the individual selectInput()/selectizeInput(), not a tagList.",
         call. = FALSE)
  }

  button <- tags$button(
    type = "button",
    class = "copyable-select-btn",
    `aria-label` = tooltip,
    icon("copy", lib = "font-awesome"),
    # span, not div: a button's content model is phrasing content only.
    # role/aria-live let the flashed result ("Copied") reach screen readers,
    # which the static aria-label above cannot.
    tags$span(tooltip,
              class = "copyable-select-tip",
              role = "status",
              `aria-live` = "polite")
  )

  htmltools::attachDependencies(
    div(class = "copyable-select", select_tag, button),
    copyable_select_dependency()
  )
}
