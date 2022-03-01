library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Dynamic Zoning: Instances"),

    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "instance",
                        label = "Select instance",
                        choices = names(dz::test_instances),
                        selected = names(dz::test_instances)[1]),
            checkboxInput(inputId = "delaunay", label = "Delaunay triangulation"),
            checkboxInput(inputId = "voronoi", label = "Voronoi tesselation")
        ),

        mainPanel(
           plotOutput("instancePlot")
        )
    )
)

server <- function(input, output) {

    output$instancePlot <- renderPlot({
        plot(dz::test_instances[[input$instance]], delaunay = input$delaunay, voronoi = input$voronoi)
    }, res = 110, width = 1000, height = 600)
}

shinyApp(ui = ui, server = server)
