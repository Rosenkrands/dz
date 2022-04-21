library(dz)
library(shiny)

inst <- test_instances$p7_chao

sp <- function(id1, id2, graph = g){
  # handle identical ids
  if (id1 == id2) {
    warning("Trying to calculate the shortest path from one node to itself, returning 0")
    return(0)
  }

  # Find vertices that make up the path
  short_vert <- igraph::shortest_paths(
    graph = graph,
    from = as.character(id1),
    to = as.character(id2),
    output = "vpath"
  )$vpath[[1]] |>
    names() |>
    as.integer()

  # return the path not including the first point
  return(short_vert |> tail(-1))
}

get_SDR <- function(current_node, L_remaining, score, graph = g, dst = dst) {
  # current_node = 1

  # The shortest paths to all node
  paths <- igraph::shortest_paths(
    graph = graph, from = as.character(current_node), to = igraph::V(graph)
  )$vpath

  # The gathered profit from a path
  s <- do.call(
    c,
    lapply(
      paths,
      function(x) score[x |> names() |> as.integer()] |> sum()
    )
  )

  # The distance of a path
  d <- dst[current_node, ]

  # can we get to a node and back to source
  feasible <- d + dst[,1] <= L_remaining # TODO: we should maybe adjust L_remaining here to discourage paths that are close to L_remaining

  # set the infeasible nodes to 0 including the current node
  r <- s/d * feasible; r[is.na(r) | !is.finite(r)] <- 0

  # return SDR for the feasible nodes
  r[(r > 0) & names(r) != "1"]
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    shinyjs::useShinyjs(),

    sidebarLayout(
        sidebarPanel(
          # h2("Parameters"),

          h3("Agents"),
          # sliderInput(inputId = "k", label = "Number of agents",
                      # min = 2, max = 6, value = 3),
          sliderInput(inputId = "L", label = "Range constraint",
                      min = 0, max = 200, value = 100),

          h3("Information"),
          sliderInput(inputId = "r", label = "Sphere of influence",
                      min = 0, max = 100, value = 20),

          h3("Variance"),
          sliderInput(inputId = "bounds", label = "Bounds on score variance",
                      min = 0, max = 500, value = c(10, 100)),

          actionButton("btn","Show instance"),
          br(), br(), textOutput("text")
        ),

        mainPanel(
          plotOutput("clustering_plot")
        )
    )
)

server <- function(input, output) {
    reactive({set.seed(input$seed)})

    vals <- reactiveValues(
      p = NULL,
      p_wo_route = NULL,
      label = "Show instance",
      score = NULL,
      realized_score = NULL,
      unexpected = NULL,
      g = NULL,
      dst = NULL,
      route = NULL,
      L_remaining = NULL,
      route_concluded = NULL,
      current_node = NULL,
      candidates = NULL,
      sdr = NULL,
      node_id = NULL,
      candidate_points = NULL,
      node_id_point = NULL,
      route_to_node_id = NULL
    )

    info <- reactive({generate_information(test_instances$p7_chao, r = input$r)})

    variances <- reactive({
      generate_variances(
        test_instances$p7_chao,
        bounds = c("min" = input$bounds[1], "max" = input$bounds[2])
      )
    })

    p_inst <- reactive({
      p_inst <- prepare_instance(test_instances$p7_chao, variances(), info())

      vals$score = p_inst$points$score
      vals$realized_score = p_inst$points$realized_score
      vals$unexpected = p_inst$points$unexpected
      vals$g = test_instances$p7_chao$g
      vals$dst = test_instances$p7_chao$dst

      p_inst
    })


    observeEvent(input$btn, {
      if (vals$label == "Show instance") {
        vals$p <- plot(p_inst())
        vals$p_wo_route <- plot(p_inst())
        vals$label = "Show feasible nodes"
        updateActionButton(inputId = "btn", label = vals$label)

        vals$route <- c(1); vals$current_node <- tail(vals$route, 1)
        vals$L_remaining <- input$L
        vals$route_concluded <- F
      } else if (vals$label == "Show feasible nodes") {
        vals$sdr <- get_SDR(vals$current_node, vals$L_remaining, vals$score, graph = vals$g, dst = vals$dst)
        vals$candidates <- vals$sdr |> names() |> as.integer()

        vals$candidate_points <- ggplot2::geom_point(
          data = inst$points[vals$candidates,],
          ggplot2::aes(x,y), size = vals$sdr, color = "green", alpha = .5
        )
        vals$p <- vals$p + vals$candidate_points

        vals$label = "Sample a node"
        updateActionButton(inputId = "btn", label = vals$label)
      } else if (vals$label == "Sample a node") {
        if (length(vals$candidates) > 1) { # there are multiple candidates
          vals$node_id <- sample(vals$candidates, 1, prob = vals$sdr)
        } else if (length(vals$candidates) == 1) { # there is only one candidate
          vals$node_id <- vals$candidates[1]
        } else if (length(vals$candidates) < 1) { # there are no feasible candidates
          vals$node_id <- 1; vals$route_concluded <- T
        }

        vals$node_id_point <- ggplot2::geom_point(
          data = inst$points |> dplyr::filter(id == vals$node_id),
          ggplot2::aes(x,y), color = "red", size = 5, shape = 13, stroke = 1
        )

        vals$p <- vals$p + vals$node_id_point

        vals$label = "Find path to node"
        updateActionButton(inputId = "btn", label = vals$label)
      } else if (vals$label == "Find path to node") {
        # Find path to next and append to route
        path_to_next <- sp(vals$current_node, vals$node_id, graph = vals$g)
        vals$route <- append(vals$route, path_to_next)

        # Generate route segements based on the route
        route_segments <- tibble::tibble(route = vals$route) |>
          dplyr::mutate(id_start = dplyr::lag(route), id_end = route) |>
          dplyr::filter(!is.na(id_start)) |>
          dplyr::select(-route) |>
          dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                            by = c("id_start" = "id")) |>
          dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                            by = c("id_end" = "id"), suffix = c("","end")) |>
          dplyr::group_by(x,y,xend,yend)

        vals$route_to_node_id <- ggplot2::geom_segment(
          data = route_segments,
          ggplot2::aes(x=x, y=y, xend=xend, yend=yend)
        );

        vals$p <- vals$p_wo_route + vals$node_id_point + vals$route_to_node_id
        vals$p_wo_route <- vals$p_wo_route + vals$route_to_node_id

        # p <- p + route_to_node_id

        # Update variables
        vals$score[path_to_next] <- 0
        vals$L_remaining <- vals$L_remaining - vals$dst[vals$current_node, vals$node_id]

        # check if anything was unexpected and update the correlated scores
        for (j in path_to_next) { # we need to consider all nodes in the shortest path
          if (vals$unexpected[j]) {
            related_nodes <- which(info()[j,] != 0) # find the nodes that are related
            for (k in related_nodes) { # update score
              # TODO: What if the score have already been gather do we want to add new score or ignore points that are already visited?
              vals$score[k] <- vals$score[k] + info()[j,k]
            }
            vals$unexpected[j] <- F
          }
        }

        vals$current_node <- tail(vals$route, 1)
        print(vals$route)
        if (!vals$route_concluded) {
          vals$label = "Show feasible nodes"
          updateActionButton(inputId = "btn", label = vals$label)
        } else {
          vals$label = "Press to restart!"
          updateActionButton(inputId = "btn", label = vals$label)
        }
      } else if (vals$label == "Press to restart!") {
        vals$label = "Show feasible nodes"
        updateActionButton(inputId = "btn", label = vals$label)

        vals$p <- plot(p_inst())
        vals$p_wo_route <- plot(p_inst())
        vals$label = "Show feasible nodes"
        updateActionButton(inputId = "btn", label = vals$label)

        vals$route <- c(1); vals$current_node <- tail(vals$route, 1)
        vals$L_remaining <- input$L
        vals$route_concluded <- F
      }
    })

    output$clustering_plot <- renderPlot({
      vals$p
    }, res = 110, width = 1000, height = 800)
}

shinyApp(ui = ui, server = server)
