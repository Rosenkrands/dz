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
                      min = 2, max = 6, value = 4),
          sliderInput(inputId = "L", label = "Range constraint",
                      min = 0, max = 100, value = 30),

          h3("Information"),
          sliderInput(inputId = "r", label = "Sphere of influence",
                      min = 0, max = 100, value = 20),

          h3("Variance"),
          sliderInput(inputId = "bounds", label = "Bounds on score variance",
                      min = 0, max = 500, value = c(10, 100)),

          h3("Objective"),
          sliderInput(inputId = "alpha", label = "Alpha",
                      min = 0, max = 1, value = 1),

          actionButton("btn","Perform clustering!"),
          br(), br(), textOutput("text")
        ),

        mainPanel(
          plotOutput("clustering_plot")
        )
    )
)

server <- function(input, output) {
    print(isolate(input$bounds))

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

    observeEvent(input$btn, {
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

      plot(vals$clust, delaunay = T)
    }, res = 110, width = 800, height = 800)
}

shinyApp(ui = ui, server = server)
