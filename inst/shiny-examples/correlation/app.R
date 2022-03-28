library(dz)
library(shiny)
set.seed(123)

# filter out the the terminal point, as we will use
# node 1 as both source and sink.
inst <- test_instances$p7_chao
inst$points <- inst$points |> dplyr::filter(id != nrow(inst$points))
plot(inst)

# find shortest path distance matrix
tri <- (deldir::deldir(inst$points$x, inst$points$y))$delsgs
tri$dist <- sqrt((tri$x1 - tri$x2)^2 + (tri$y1 - tri$y2)^2)

g <- igraph::graph_from_data_frame(
  tri |> dplyr::select(ind1, ind2, weight = dist),
  directed = FALSE,
  vertices = inst$points |> dplyr::select(id, score)
)

dst <- igraph::distances(g, algorithm = "dijkstra")

# generate information matrix
eps <- matrix(
  data = runif(nrow(inst$points)^2, min = -1, max = 1),
  nrow = nrow(inst$points)
)

info <- (eps / dst)
info[!is.finite(info)] <- 0
info[dst > 20] <- 0

# visualize information
plot_info <- function(node_id = 1, delaunay = T) {
  # Instantiate the ggplot object
  p <- ggplot2::ggplot()

  # Add delaunay edges
  tri <- deldir::deldir(inst$points$x, inst$points$y)
  p <- p +
    ggplot2::geom_segment(
      data = tri$delsgs,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    )

  # Add points and title to the plot
  p <- p +
    ggplot2::geom_point(
      data = inst$points,# |> dplyr::filter(point_type == "intermediate"),
      ggplot2::aes(x, y, size = abs(info[node_id,]), shape = point_type, alpha = .3)
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(id == node_id),
      ggplot2::aes(x, y), color = "red"
    ) +
    ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(shape = "none") +
    # ggplot2::scale_size(limits = c(0,1)) +
    ggplot2::theme(legend.position = "none")

  return(p)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Dynamic Zoning: Instances"),

    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "radius", label = "Radius",
                        min = 1, max = 100, value = 20)
        ),

        mainPanel(
           plotOutput("inst_plot", click = "plot_click")
        )
    )
)

server <- function(input, output) {

    vals <- reactiveValues(
      node_id = 1
    )

    observeEvent(input$plot_click, {
        vals$node_id = (nearPoints(inst$points, input$plot_click, threshold = 10, maxpoints = 1,
                             addDist = TRUE, xvar = "x", yvar = "y")) |> dplyr::pull(id)
    })

    output$inst_plot <- renderPlot({

      plot_info(node_id = vals$node_id)

    }, res = 110, width = 700, height = 700)
}

shinyApp(ui = ui, server = server)
