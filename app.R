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
# Compute ASEBA statistics for all athletes in dataset.
# Three categories: Adaptive Functioning, Syndrome, and DSM-Oriented scale scores.
# Build each as a separate dataframe.

# Adaptive Functioning Scale Scores
afCats <- c("Friends", "Spouse/Partner", "Family", "Job", "Education",
                "Mean Adaptive", "Personal Strengths")
afCols <- c("Friends_TScore", "Spouse_Partner_TScore", "Family_TScore",
           "Job_TScore", "Education_TScore", "Mean_Adaptive_TScore",
           "Personal_Strengths_TScore")
afT_ScoreAverage <- colMeans(df[,afCols], na.rm = T)
adaptiveFunctioning <- data.frame(afCats, afT_ScoreAverage)
colnames(adaptiveFunctioning) <- c("Category", "All Athlete T Score")

# Syndrome Scale Scores
ssCats <- c("Anxious/Depressed", "Withdrawn", "Somatic Complaints",
            "Thought Problems", "Attention Problems", "Aggressive Behavior",
            "Rule-Breaking Behavior", "Intrusive")
ssCols <- c("Anxious__Depressed_TScore", "Withdrawn_TScore",
            "Somatic_Complaints_TScore", "Thought_Problems_TScore",
            "Attention_Problems_TScore", "Aggressive_Behavior_TScore",
            "Rule_Breaking_Behavior_TScore", "Intrusive_TScore")
ssT_ScoreAverage <- colMeans(df[,ssCols], na.rm = T)
syndrome <- data.frame(ssCats, ssT_ScoreAverage)
colnames(syndrome) <- c("Category", "All Athlete T Score")

dsmCats <- c("Depressive Problems", "Anxiety Problems", "Somatic Problems",
             "Avoidant Personality Problems", "AD/H Problems",
             "Antisocial Personality")
dsmCols <- c("Depressive_Problems_TScore", "Anxiety_Problems_TScore",
             "Somatic_Problems_TScore", "Avoidant_Personality_Problems_TScore",
             "AD_H_Problems_TScore", "Antisocial_Personality_TScore")
dsmT_ScoreAverage <- colMeans(df[,dsmCols], na.rm = T)
dsmOriented <- data.frame(dsmCats, dsmT_ScoreAverage)
colnames(dsmOriented) <- c("Category", "All Athlete T Score")

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
           tableOutput("adaptiveFunctioningTable"),
           tableOutput("syndromeTable"),
           tableOutput("dsmOrientedTable"),
           tableOutput("teamTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Rows for the team chosen
    selectedTeamIdx <- reactive(which(df$TEAM == input$team))
    selectedTeam <- reactive(df[selectedTeamIdx(),])
    
    output$athleteSelection <- renderUI({
        selectInput("athlete", "Athlete:",
                    choices = unique(df$NAME[selectedTeamIdx()]))
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- na.omit(df$Total_Problems_TScore)
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    # Tables of T-scores for all athletes.
    output$adaptiveFunctioningTable <- renderTable(adaptiveFunctioning)
    output$dsmOrientedTable <- renderTable(dsmOriented)
    output$syndromeTable <- renderTable(syndrome)
    
    output$teamTable <- renderTable(head(selectedTeam()))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
