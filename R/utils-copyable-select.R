# Copy buttons for protein-name dropdowns. The JS and CSS ship as an
# htmltools::htmlDependency attached to each wrapped dropdown, so they load
# wherever the dropdown is rendered, including inside renderUI().

#' The copy button's JS/CSS assets, served from inst/assets.
#' @importFrom htmltools htmlDependency
#' @noRd
copy_button_assets <- function() {
  htmlDependency(
    name = "msstatsshiny-copyable-select",
    version = as.character(utils::packageVersion("MSstatsShiny")),
    src = c(file = system.file("assets", package = "MSstatsShiny")),
    script = "copy-select.js",
    stylesheet = "copy-select.css"
  )
}

#' Count the select elements anywhere inside a tag tree.
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

#' Add a copy-to-clipboard button next to a select input.
#'
#' @param select_tag A single `selectInput()` or `selectizeInput()`, left
#'   unchanged.
#' @param tooltip Hover text and accessible name for the button.
#' @return A `div` holding `select_tag` and the button.
#' @importFrom htmltools attachDependencies
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
    # aria-live announces "Copied" to screen readers
    tags$span(tooltip,
              class = "copyable-select-tip",
              role = "status",
              `aria-live` = "polite")
  )

  attachDependencies(
    div(class = "copyable-select", select_tag, button),
    copy_button_assets()
  )
}
