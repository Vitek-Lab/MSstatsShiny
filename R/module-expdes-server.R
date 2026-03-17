# ============================================================================
# Dose Response TPR Simulation Functions
# ============================================================================

# Hardcoded concentration ladders: control (0) and highest dose always present,
# intermediate doses filled from a biologically motivated set.
CONC_MAP <- list(
  "2" = c(0, 3000),
  "3" = c(0, 1000, 3000),
  "4" = c(0, 1, 1000, 3000),
  "5" = c(0, 1, 100, 1000, 3000),
  "6" = c(0, 1, 100, 300, 1000, 3000),
  "7" = c(0, 1, 10, 100, 300, 1000, 3000),
  "8" = c(0, 1, 10, 30, 100, 300, 1000, 3000),
  "9" = c(0, 1, 3, 10, 30, 100, 300, 1000, 3000)
)

#' Run TPR simulation across a grid of concentration counts and replicate counts
#'
#' @param rep_range integer vector of length 2, c(min, max) for replicate sweep
#' @param n_proteins integer, number of proteins to simulate
#'
#' @return data.frame with columns: Interaction, TPR, N_rep, NumConcs
#'
#' @importFrom MSstatsResponse futureExperimentSimulation
#' @importFrom dplyr filter mutate select if_else
run_tpr_simulation <- function(rep_range, n_proteins = 1000) {
  k_grid <- as.integer(names(CONC_MAP))
  rep_grid <- seq(rep_range[1], rep_range[2])

  run_one <- function(n_rep, k_conc, seed = 123) {
    set.seed(seed + n_rep * 100 + k_conc)
    concs_k <- CONC_MAP[[as.character(k_conc)]]

    sim_args <- list(
      N_proteins = n_proteins,
      N_rep = n_rep,
      Concentrations = concs_k,
      IC50_Prediction = FALSE
    )

    temp_res <- do.call(futureExperimentSimulation, sim_args)
    temp_res$Hit_Rates_Data |>
      dplyr::filter(Category %in% c("TPR (Strong)", "TPR (Weak)")) |>
      dplyr::mutate(
        N_rep = n_rep,
        NumConcs = length(concs_k),
        Interaction = dplyr::if_else(Category == "TPR (Strong)", "Strong", "Weak")
      ) |>
      dplyr::select(Interaction, TPR = Percent, N_rep, NumConcs)
  }

  grid_df <- expand.grid(N_rep = rep_grid, k_conc = k_grid)
  results <- do.call(rbind, lapply(seq_len(nrow(grid_df)), function(i) {
    tryCatch(
      run_one(n_rep = grid_df$N_rep[i], k_conc = grid_df$k_conc[i]),
      error = function(e) {
        cat(file = stderr(), paste("Simulation failed for N_rep=", grid_df$N_rep[i],
            ", k_conc=", grid_df$k_conc[i], ":", conditionMessage(e), "\n"))
        NULL
      }
    )
  }))

  return(results)
}

#' Plot TPR power curves for dose response proteomics experiments
#'
#' @param simulation_results data.frame from run_tpr_simulation
#'
#' @return plotly object with two panels (Strong and Weak interaction)
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_x_continuous
#'   scale_y_continuous scale_color_manual scale_linetype_manual labs
#'   theme_bw theme element_text
#' @importFrom plotly ggplotly subplot
plot_tpr_power_curve <- function(simulation_results) {
  k_grid <- sort(unique(simulation_results$NumConcs))
  rep_levels <- sort(unique(simulation_results$N_rep))

  ltypes <- c("dotted", "dotdash", "dashed", "longdash", "solid")
  ltype_values <- setNames(ltypes[seq_along(rep_levels)], as.character(rep_levels))

  make_panel <- function(data, color, show_legend = FALSE) {
    p <- ggplot2::ggplot(data,
      ggplot2::aes(x = NumConcs, y = TPR, linetype = factor(N_rep))) +
      ggplot2::geom_line(linewidth = 1.2, color = color) +
      ggplot2::geom_point(size = 2, color = color) +
      ggplot2::scale_x_continuous(breaks = k_grid) +
      ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      ggplot2::scale_linetype_manual(values = ltype_values) +
      ggplot2::labs(
        x = "Number of concentrations",
        y = "True Positive Rate (%)",
        linetype = "Replicates"
      ) +
      ggplot2::theme_bw(base_size = 14) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
      )

    if (!show_legend) {
      p <- p + ggplot2::theme(legend.position = "none")
    }
    p
  }

  results_strong <- simulation_results[simulation_results$Interaction == "Strong", ]
  results_weak <- simulation_results[simulation_results$Interaction == "Weak", ]

  # Only show legend on the second panel to avoid duplication
  p_strong <- plotly::ggplotly(make_panel(results_strong, "#1b9e77", FALSE))
  p_weak <- plotly::ggplotly(make_panel(results_weak, "#d95f02", TRUE))

  plotly::subplot(p_strong, p_weak,
    nrows = 1, shareY = TRUE, titleX = TRUE, titleY = TRUE,
    margin = 0.05
  ) |> plotly::layout(
    margin = list(t = 60),
    annotations = list(
      list(text = "<b>Strong interaction detection power</b>", x = 0.22, y = 1.08,
           xref = "paper", yref = "paper", showarrow = FALSE,
           font = list(size = 15), xanchor = "center"),
      list(text = "<b>Weak interaction detection power</b>", x = 0.78, y = 1.08,
           xref = "paper", yref = "paper", showarrow = FALSE,
           font = list(size = 15), xanchor = "center")
    )
  )
}

# ============================================================================
# Expdes Server Module
# ============================================================================

#' Expdes Server module for future experiments
#'
#' @param input input object to capture different ui element values
#' @param output to render and create elements
#' @param session session current module
#' @param parent_session session of the main calling module
#' @param loadpage_input input object from loadpage UI
#' @param qc_input input object from QC UI
#' @param statmodel_input input object from Statmodel UI
#' @param data_comparison function for group comparisons
#' @param preprocess_data function returning preprocessed data
#'
#' @return list object with user selected options and matrix build
#'
#' @export
#' @examples
#' NA
#'
expdesServer <- function(input, output, session, parent_session, loadpage_input,
                         qc_input, statmodel_input, data_comparison,
                         preprocess_data = NULL, statmodel_contrast = NULL) {
  ns <- session$ns

  prepared_response_data <- reactive({
    req(is_response_curve())
    req(preprocess_data)
    req(statmodel_contrast)
    req(statmodel_contrast$matrix)
    matrix <- statmodel_contrast$matrix
    protein_level_data <- merge(preprocess_data()$ProteinLevelData, matrix, by = "GROUP")
    prepare_dose_response_fit(protein_level_data)
  })

  is_response_curve <- reactive({
    !is.null(statmodel_input()) &&
      !is.null(statmodel_input()[[NAMESPACE_STATMODEL$comparison_mode]]) &&
      statmodel_input()[[NAMESPACE_STATMODEL$comparison_mode]] ==
        CONSTANTS_STATMODEL$comparison_mode_response_curve
  })

  # Render sidebar controls based on analysis mode
  output[[NAMESPACE_EXPDES$sidebar_controls]] <- renderUI({
    if (is_response_curve()) {
      # Dose response mode: protein selector + replicate range
      protein_choices <- character(0)
      tryCatch({
        protein_choices <- unique(prepared_response_data()$protein)
      }, error = function(e) {})

      tagList(
        h4("Dose response power analysis"),
        selectizeInput(ns(NAMESPACE_EXPDES$protein_select),
                       label = h5("Select protein (strong interaction)"),
                       choices = protein_choices,
                       options = list(placeholder = "Search protein..."),
                       multiple = FALSE),
        sliderInput(ns(NAMESPACE_EXPDES$rep_range),
                    "Replicates per dose",
                    min = 1, max = 5, value = c(1, 5), step = 1),
        actionButton(ns(NAMESPACE_EXPDES$run_simulation),
                     "Run simulation",
                     icon = icon("play"),
                     style = "color: #000; background-color: #75ba82; border-color: #000; margin-top: 15px;")
      )
    } else {
      # Standard mode: existing controls
      tagList(
        h4("Choose parameter to estimate"),
        p("This section is not currently compatible with TMT experiments."),
        radioButtons(ns("param"), "parameters:",
                     c("Sample size" = "sample", "Power" = "npower")),
        sliderInput(ns("nsample"), "Number of samples", 0, 50, 4, 1),
        sliderInput(ns("power"), "Power", 0, 1, 0.8, 0.1),
        sliderInput(ns("FDR"), "False discovery rate", 0, 1, 0.05, 0.01),
        sliderInput(ns("desirFC"), "Desired fold change", 0, 5, c(1.25, 1.75), 0.01)
      )
    }
  })

  # ---- Dose response simulation ----
  simulation_results <- reactiveVal(NULL)

  observeEvent(input[[NAMESPACE_EXPDES$run_simulation]], {
    req(input[[NAMESPACE_EXPDES$protein_select]])
    req(input[[NAMESPACE_EXPDES$rep_range]])

    show_modal_spinner(text = "Running simulations... This may take a minute.")

    tryCatch({
      results <- run_tpr_simulation(
        rep_range = input[[NAMESPACE_EXPDES$rep_range]],
        n_proteins = 1000
      )
      simulation_results(results)
      remove_modal_spinner()
    }, error = function(e) {
      remove_modal_spinner()
      showNotification(conditionMessage(e), type = "error", duration = 8)
    })
  })

  # ---- Standard sample size logic ----
  observe({
    if (!is_response_curve()) {
      req(input$param)

      if (input$param == "sample") {
        disable("nsample")
        sample_x <- TRUE
      } else {
        sample_x <- input$nsample
        enable("nsample")
      }

      if (input$param == "npower") {
        disable("power")
        power_x <- TRUE
      } else {
        power_x <- input$power
        enable("power")
      }

      future_exp <- function() {
        designSampleSize(
          data = data_comparison()$FittedModel,
          desiredFC = input$desirFC,
          FDR = input$FDR,
          numSample = sample_x,
          power = power_x
        )
      }

      output[[NAMESPACE_EXPDES$result_plot]] <- renderPlotly({
        designSampleSizePlots(future_exp(), isPlotly = TRUE)
      })

      output[[NAMESPACE_EXPDES$download_future]] <- downloadHandler(
        filename = "future_exp.pdf",
        content = function(file) {
          pdf(file)
          designSampleSizePlots(future_exp())
          dev.off()
        }
      )
    }
  })

  # ---- Dose response plot + download ----
  observe({
    if (is_response_curve()) {
      output[[NAMESPACE_EXPDES$result_plot]] <- renderPlotly({
        req(simulation_results())
        plot_tpr_power_curve(simulation_results())
      })

      output[[NAMESPACE_EXPDES$download_future]] <- downloadHandler(
        filename = paste0("tpr_power_curve-", Sys.Date(), ".pdf"),
        content = function(file) {
          results <- simulation_results()
          if (is.null(results)) {
            showNotification("Please run the simulation first.", type = "error")
            return(NULL)
          }

          k_grid <- sort(unique(results$NumConcs))
          rep_levels <- sort(unique(results$N_rep))
          ltypes <- c("dotted", "dotdash", "dashed", "longdash", "solid")
          ltype_values <- setNames(ltypes[seq_along(rep_levels)], as.character(rep_levels))

          make_panel <- function(data, title, color) {
            ggplot2::ggplot(data,
              ggplot2::aes(x = NumConcs, y = TPR, linetype = factor(N_rep))) +
              ggplot2::geom_line(linewidth = 1.2, color = color) +
              ggplot2::geom_point(size = 2, color = color) +
              ggplot2::scale_x_continuous(breaks = k_grid) +
              ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
              ggplot2::scale_linetype_manual(values = ltype_values) +
              ggplot2::labs(title = title, x = "Number of concentrations",
                           y = "True Positive Rate (%)", linetype = "Replicates") +
              ggplot2::theme_bw(base_size = 14) +
              ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
          }

          p_strong <- make_panel(
            results[results$Interaction == "Strong", ],
            "Strong interaction detection power", "#1b9e77")
          p_weak <- make_panel(
            results[results$Interaction == "Weak", ],
            "Weak interaction detection power", "#d95f02")

          pdf(file, width = 16, height = 6)
          print(p_strong)
          print(p_weak)
          dev.off()
        }
      )
    }
  })
}