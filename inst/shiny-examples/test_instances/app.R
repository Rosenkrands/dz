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
                        selected = names(dz::test_instances)[1])
        ),

        mainPanel(
           plotOutput("instancePlot")
        )
    )
)

server <- function(input, output) {

    output$instancePlot <- renderPlot({
        plot(dz::test_instances[[input$instance]])
    })
}

shinyApp(ui = ui, server = server)
