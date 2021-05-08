#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Data preprocessing ----
# Non-reactive functions to set up the dataset.
df <- readRDS("data/lu_data.Rda")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Liberty University Resilience Room"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput("team", "Team:", df$TEAM),
            
            uiOutput("athleteSelection"),
            
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           
           tableOutput("teamTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Rows for the team chosen
    selectedTeam <- reactive(which(df$TEAM == input$team))
    
    output$athleteSelection <- renderUI({
        selectInput("athlete", "Athlete:", choices = unique(df$NAME[selectedTeam()]))
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- na.omit(df$Total_Problems_TScore)
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$teamTable <- renderTable(head(df[selectedTeam(),]))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
