library(dz)
library(shiny)

cat <- message

# Define UI for application that draws a histogram
ui <- fluidPage(
    shinyjs::useShinyjs(),

    sidebarLayout(
        sidebarPanel(
          # h2("Parameters"),

          h3("Agents"),
          sliderInput(inputId = "k", label = "Number of agents",
                      min = 2, max = 6, value = 3),
          sliderInput(inputId = "L", label = "Range constraint",
                      min = 0, max = 200, value = 100),

          h3("Information"),
          sliderInput(inputId = "r", label = "Sphere of influence",
                      min = 0, max = 100, value = 20),

          h3("Variance"),
          sliderInput(inputId = "bounds", label = "Bounds on score variance",
                      min = 0, max = 500, value = c(10, 100)),

          h3("Objective"),
          sliderInput(inputId = "alpha", label = "Alpha",
                      min = 0, max = 1, value = 1),

          h3("Misc."),

          numericInput(inputId = "num_routes", label = "Number of initial routes", value = 100),

          fluidRow(
            column(
              6,
              selectInput(inputId = "dispute_obj", label = "Resolving diputes",
                          choices = c("most_frequent", "highest_score"))
            ),
            column(
              6,
              numericInput(inputId = "seed", label = "Seed for reproducibility", value = 1)
            )
          ),

          actionButton("btn_init","Create an initial route"),
          actionButton("btn_rb_clust","RB-Clustering"),
          actionButton("btn_ls_clust","LS-Clustering"),
          br(), br(), textOutput("text")
        ),

        mainPanel(
          plotOutput("clustering_plot")
        )
    )
)

server <- function(input, output) {
    reactive({set.seed(input$seed)})

    info <- reactive({generate_information(test_instances$p7_chao, r = input$r)})

    variances <- reactive({
      generate_variances(
        test_instances$p7_chao,
        bounds = c("min" = input$bounds[1], "max" = input$bounds[2])
      )
    })

    vals <- reactiveValues(
      clust = NULL
    )

    observeEvent(input$btn_init, {
      inst <- test_instances$p7_chao
      init_route <- initial_route(inst, L = input$L, variances(), info())
      vals$clust <- init_route
    })

    observeEvent(input$btn_rb_clust, {
      set.seed(input$seed)
      withCallingHandlers({
        shinyjs::html("text", "")
        withProgress(
          vals$clust <- rb_clustering(
            p_inst = prepare_instance(test_instances$p7_chao, variances(), info()),
            L = input$L,
            k = input$k,
            num_routes = input$num_routes,
            info = info(),
            dispute_obj = input$dispute_obj,
            shiny = T
          ),
          message = "Constructing initial routes..."
        )
      },
      message = function(m) {
        shinyjs::html(id = "text", html = m$message, add = FALSE)
      })
    })

    observeEvent(input$btn_ls_clust, {
      withCallingHandlers({
        shinyjs::html("text", "")
        vals$clust <- clustering(
          inst = test_instances$p7_chao,
          k = input$k,
          L = input$L,
          eps = 0,
          info = info(),
          cluster_method = "local_search",
          variances = variances(),
          alpha = input$alpha
        )
      },
      message = function(m) {
        shinyjs::html(id = "text", html = m$message, add = FALSE)
      })
    })

    output$clustering_plot <- renderPlot({
      if (is.null(vals$clust)) {return()}

      if (class(vals$clust) %in% c("rb_clustering","initial_route")) {
        plot(vals$clust)
      } else {
        plot(vals$clust, delaunay = T)
      }
    }, res = 110, width = 800, height = 800)
}

shinyApp(ui = ui, server = server)
