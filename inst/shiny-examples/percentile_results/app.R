library(dz)
library(tidyverse)
library(shiny)

# Analyze results
results_direc <- "C:/Users/krose/Desktop/experiment results"
direcs <- list.files(results_direc, full.names = F)

load_results_files <- function(direc) {
  results <- lapply(
    list.files(paste(results_direc, direc, sep = "/"), full.names = T),
    function(x) readRDS(x)
  )

  if (length(results) == 2) {
    names(results) <- c("failed", "results")
  } else {
    names(results <- c("results"))
  }

  return(results)
}

rslt <- lapply(direcs, load_results_files)
names(rslt) <- sub("dot", "\\.", direcs)

all_results <- do.call(
  dplyr::bind_rows,
  lapply(seq_along(rslt), function(x) {
    rslt[[x]]$results |> dplyr::mutate(top_percentile = names(rslt)[x])
  })
)

(ar <- all_results |>
    dplyr::mutate(ur_realized_score = sapply(all_results$`list(ur)`, function(x) do.call(sum, x$total_realized_score)),
                  ur_score = sapply(all_results$`list(ur)`, function(x) do.call(sum, x$total_score)),
                  ur_candidate_outside = sapply(all_results$`list(ur)`, function(x) do.call(paste, lapply(x$candidate_outside, function(x) round(x)))),
                  sr_score = sapply(all_results$`list(sr)`, function(x) do.call(sum, x$total_score)),
                  k = sapply(all_results$`list(ur)`, function(x) length(x$zones)),
                  L = sapply(all_results$`list(ur)`, function(x) x$L * length(x$zones)),
                  L_remaining = sapply(all_results$`list(ur)`, function(x) do.call(paste, lapply(x$L_remaining, function(x) round(x, 1))))))

# Define UI for application that draws a histogram
ui <- fluidPage(

    fluidRow(
      column(4,
        plotOutput("score_plot", click = "plot_click")
      ),
      column(8,
        fluidRow(tableOutput('table')),
        plotOutput("ur_plot")
      )
    )
)

server <- function(input, output) {

    vals <- reactiveValues(
      ur_plot = NULL,
      tab_data = ar[1,] |> select(sr_score, ur_score, ur_realized_score, ur_candidate_outside)
    )

    output$table <- renderTable({vals$tab_data})

    output$score_plot <- renderPlot({
      ar |>
        ggplot(aes(y = ur_score, x = factor(L), color = factor(k)), group = paste(k,top_percentile)) +
        geom_point(position = position_dodge(width = .5)) +
        facet_wrap(~top_percentile, labeller = "label_both", ncol = 1) +
        theme_bw()
    }, res = 100, width = 400, height = 900)

    output$ur_plot <- renderPlot({vals$ur_plot}, res = 110, width = 900, height = 800)

    observeEvent(input$plot_click, {
        vals$ur_plot <- (nearPoints(ar, input$plot_click, threshold = 10, maxpoints = 1, addDist = FALSE, xvar = "L", yvar = "ur_score") |>
          pull(`list(ur)`))[[1]] |> plot(test_instances$p7_chao)

        print(nearPoints(ar, input$plot_click, threshold = 10, maxpoints = 1, addDist = FALSE, xvar = "L", yvar = "ur_score", allRows = T)$selected_)

        vals$tab_data <- nearPoints(ar, input$plot_click, threshold = 10, maxpoints = 1, addDist = FALSE, xvar = "L", yvar = "ur_score") |> select(sr_score, ur_score, ur_realized_score, ur_candidate_outside, L_remaining)
    })
}

shinyApp(ui = ui, server = server)
