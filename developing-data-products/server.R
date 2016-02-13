library(shiny)

# Define a server for the Shiny app
shinyServer(function(input, output) {
    data_input <- reactive({
        get_all_asset_dailies(input$treasury, input$dates)
    })
    
    output$plot <- renderPlot({
        data <- data_input()
        # Render a barplot
        plot_treasury(data)
    })
})