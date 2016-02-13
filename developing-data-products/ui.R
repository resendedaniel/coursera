library(shiny)
source('api.R')
ids <- get_treasuries_id()
ids <- ids[!grepl("\ ", ids)]
ids <- sort(ids)

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
                selectInput("treasury",
                            "Treasuries id", 
                            choices=ids,
                            multiple=TRUE),
                dateRangeInput("dates",
                               label = "Date range", 
                               start = as.Date("2003-01-1")),
                helpText("Data from verios.com.br")
            ),
            
            # Create a spot for the plot
            mainPanel(
                plotOutput("plot")  
            )
            
        )
    )
)