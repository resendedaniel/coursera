library(shiny)
source('api.R')
ids <- get_treasuries_id()
treasuries <- list()
# Define the overall UI
shinyUI(
    
    # Use a fluid Bootstrap layout
    fluidPage(    
        
        # Give the page a title
        titlePanel("Brazilian treasuries"),
        
        # Generate a row with a sidebar
        sidebarLayout(      
            
            # Define the sidebar with one input
            sidebarPanel(
                selectInput("treasury", "Treasury:", 
                            choices=ids,
                            multiple=TRUE),
                hr(),
                helpText("Data from verios.com")
            ),
            
            # Create a spot for the barplot
            mainPanel(
                plotOutput("phonePlot")  
            )
            
        )
    )
)