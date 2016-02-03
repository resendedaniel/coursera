library(shiny)

# Define a server for the Shiny app
shinyServer(function(input, output) {
    
    # Fill in the spot we created for a plot
    output$phonePlot <- renderPlot({
        
        # Render a barplot
        plot_treasury(input$treasury)
    })
})