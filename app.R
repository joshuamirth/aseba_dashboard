library(shiny)
library(fmsb) # for radar charts

##############################################################################
# Data pre-processing.
##############################################################################
# Import the data set from the database. (Currently just
# a flat R data frame.)
df <- readRDS("data/lu_data.Rda")

# Compute ASEBA statistics for all athletes in dataset. Three categories:
# Adaptive Functioning, Syndrome, and DSM-Oriented scale scores. Build each as a
# separate dataframe.

# Adaptive Functioning
afCats <- c("Friends", "Spouse/Partner", "Family", "Job", "Education",
                "Mean Adaptive", "Personal Strengths")
afCols <- c("Friends_TScore", "Spouse_Partner_TScore", "Family_TScore",
           "Job_TScore", "Education_TScore", "Mean_Adaptive_TScore",
           "Personal_Strengths_TScore")
afT_ScoreAverage <- colMeans(df[,afCols], na.rm = T)

# Syndromes
ssCats <- c("Anxious/Depressed", "Withdrawn", "Somatic Complaints",
            "Thought Problems", "Attention Problems", "Aggressive Behavior",
            "Rule-Breaking Behavior", "Intrusive")
ssCols <- c("Anxious__Depressed_TScore", "Withdrawn_TScore",
            "Somatic_Complaints_TScore", "Thought_Problems_TScore",
            "Attention_Problems_TScore", "Aggressive_Behavior_TScore",
            "Rule_Breaking_Behavior_TScore", "Intrusive_TScore")
ssT_ScoreAverage <- colMeans(df[,ssCols], na.rm = T)

# DSM-Oriented
dsmCats <- c("Depressive Problems", "Anxiety Problems", "Somatic Problems",
             "Avoidant Personality Problems", "AD/H Problems",
             "Antisocial Personality")
dsmCols <- c("Depressive_Problems_TScore", "Anxiety_Problems_TScore",
             "Somatic_Problems_TScore", "Avoidant_Personality_Problems_TScore",
             "AD_H_Problems_TScore", "Antisocial_Personality_TScore")
dsmT_ScoreAverage <- colMeans(df[,dsmCols], na.rm = T)

# Function to create table of individual, team, and all test score values.
buildStatsTable <- function(athlete, team, cols, cats, tScoreAverage, statType="T_Score"){
    tScoreAthlete <- as.numeric(athlete[cols])
    tScoreTeam <- colMeans(team[,cols], na.rm=T)
    if(statType == "T_Score"){
        tableFrame <- data.frame(cats, tScoreAverage, tScoreTeam, tScoreAthlete)
        colnames(tableFrame) <- c("Category", "All Athletes T Score Average",
                            "Teammates' T Score Average", "Individual T Score")    
    } else {
        tableFrame <- data.frame(cats, 100*pnorm(tScoreAverage, 50, 10),
            100*pnorm(tScoreTeam, 50, 10), 100*pnorm(tScoreAthlete, 50, 10))
            colnames(tableFrame) <- c("Category", "All Athletes Percentile Average",
                            "Teammates' Percentile Average", "Individual Percentile")    
    }
    return(tableFrame)
}

buildRadarChart <- function(tableFrame){
    radarData <- data.frame(rbind(
        rep(100, nrow(tableFrame)),
        rep(0, nrow(tableFrame)),
        t(tableFrame[,-1])
    ))   
    radarchart(radarData)
}

##############################################################################
# UI
##############################################################################
ui <- fluidPage(

    # Application title
    titlePanel("Liberty University Resilience Room"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput("team", "Team:", df$TEAM),
            
            uiOutput("athleteSelection"),
            
            radioButtons("statType", "Display Style:", c("T Score" = "T_Score", "Percentile" = "Percentile"))
           
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("adaptiveFunctioningTable"),
           tableOutput("syndromeTable"),
           tableOutput("dsmOrientedTable"),
           plotOutput("adaptiveFunctioningPlot"),
           plotOutput("syndromePlot"),
           plotOutput("dsmOrientedPlot"),
        )
        
    )
)

##############################################################################
# Server
##############################################################################
server <- function(input, output) {

    # Rows for the team and athlete chosen.
    selectedTeamIdx <- reactive(which(df$TEAM == input$team))
    # TODO: currently selecting last index at which an athlete occurs. There are
    # athletes with multiple occurrences. Decide how to handle that. The best
    # option might be a third drop-down menu to select date of assessment to
    # view. Another option might be to always show most recent (which is
    # probably, but not necessarily, the last index as it is currently.)
    selectedAthleteIdx <- reactive(which.max(df$NAME == input$athlete))
    selectedTeam <- reactive(df[selectedTeamIdx(),])
    selectedAthlete <- reactive(df[selectedAthleteIdx(),])
   
    # List of athletes associated to selected team.
    output$athleteSelection <- renderUI({
        selectInput("athlete", "Athlete:",
                    choices = unique(df$NAME[selectedTeamIdx()]))
    })
    
    # dataframes for selected athlete.
    adaptiveFunctioning <- reactive(buildStatsTable(selectedAthlete(),
                                                    selectedTeam(),
                                                    afCols, afCats,
                                                    afT_ScoreAverage,
                                                    statType = input$statType))
    dsmOriented <- reactive(buildStatsTable(selectedAthlete(),
                                   selectedTeam(),
                                   dsmCols,
                                   dsmCats,
                                   dsmT_ScoreAverage,
                                   statType = input$statType))
    syndrome <- reactive(buildStatsTable(selectedAthlete(),
                                         selectedTeam(),
                                         ssCols, ssCats,
                                         ssT_ScoreAverage,
                                         statType = input$statType))
    
    # Tables of T-scores/Percentiles compared with all athletes and teammates.
    output$adaptiveFunctioningTable <- renderTable(adaptiveFunctioning())
    output$dsmOrientedTable <- renderTable(dsmOriented())
    output$syndromeTable <- renderTable(syndrome())
    
    # Radar plot of comparison data
    output$adaptiveFunctioningPlot <- renderPlot({
        buildRadarChart(adaptiveFunctioning())
    })
    output$dsmOrientedPlot <- renderPlot({
        buildRadarChart(dsmOriented())
    })
    output$syndromePlot <- renderPlot({
        buildRadarChart(syndrome())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
