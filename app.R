#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

##############################################################################
# Data pre-processing.
##############################################################################
# Import the data set from the database. (Currently just
# a flat R data frame.)
df <- readRDS("data/lu_data.Rda")

# Compute ASEBA statistics for all athletes in dataset. Three categories:
# Adaptive Functioning, Syndrome, and DSM-Oriented scale scores. Build each as a
# separate dataframe.

# Adaptive Functioning Scale Scores
afCats <- c("Friends", "Spouse/Partner", "Family", "Job", "Education",
                "Mean Adaptive", "Personal Strengths")
afCols <- c("Friends_TScore", "Spouse_Partner_TScore", "Family_TScore",
           "Job_TScore", "Education_TScore", "Mean_Adaptive_TScore",
           "Personal_Strengths_TScore")
afT_ScoreAverage <- colMeans(df[,afCols], na.rm = T)

# Function to create table of individual, team, and all adaptive functioning values.
buildAdaptiveFunctioning <- function(athlete, team){
    afT_ScoreAthlete <- as.numeric(athlete[afCols])
    afT_ScoreTeam <- colMeans(team[,afCols], na.rm=T)
    adaptiveFunctioning <- data.frame(afCats, afT_ScoreAverage, afT_ScoreTeam, afT_ScoreAthlete)
    colnames(adaptiveFunctioning) <- c("Category", "All Athletes T Score Average",
                            "Teammates' T Score Average", "Individual T Score")
    return(adaptiveFunctioning)
}

# Syndrome Scale Scores
ssCats <- c("Anxious/Depressed", "Withdrawn", "Somatic Complaints",
            "Thought Problems", "Attention Problems", "Aggressive Behavior",
            "Rule-Breaking Behavior", "Intrusive")
ssCols <- c("Anxious__Depressed_TScore", "Withdrawn_TScore",
            "Somatic_Complaints_TScore", "Thought_Problems_TScore",
            "Attention_Problems_TScore", "Aggressive_Behavior_TScore",
            "Rule_Breaking_Behavior_TScore", "Intrusive_TScore")
ssT_ScoreAverage <- colMeans(df[,ssCols], na.rm = T)

# Function to create table of individual, team, and all syndrome values.
buildSyndrome <- function(athlete, team){
    ssT_ScoreAthlete <- as.numeric(athlete[ssCols])
    ssT_ScoreTeam <- colMeans(team[,ssCols], na.rm=T)
    syndrome <- data.frame(ssCats, ssT_ScoreAverage, ssT_ScoreTeam, ssT_ScoreAthlete)
    colnames(syndrome) <- c("Category", "All Athletes T Score Average",
                            "Teammates' T Score Average", "Individual T Score")
    return(syndrome)
}

# DSM-Oriented Scale Scores
dsmCats <- c("Depressive Problems", "Anxiety Problems", "Somatic Problems",
             "Avoidant Personality Problems", "AD/H Problems",
             "Antisocial Personality")
dsmCols <- c("Depressive_Problems_TScore", "Anxiety_Problems_TScore",
             "Somatic_Problems_TScore", "Avoidant_Personality_Problems_TScore",
             "AD_H_Problems_TScore", "Antisocial_Personality_TScore")
dsmT_ScoreAverage <- colMeans(df[,dsmCols], na.rm = T)

# Function to create table of individual, team, and all DSM Oriented values.
buildDSMOriented <- function(athlete, team){
    dsmT_ScoreAthlete <- as.numeric(athlete[dsmCols])
    dsmT_ScoreTeam <- colMeans(team[,dsmCols], na.rm=T)
    dsmOriented <- data.frame(dsmCats, dsmT_ScoreAverage, dsmT_ScoreTeam, dsmT_ScoreAthlete)
    colnames(dsmOriented) <- c("Category", "All Athletes T Score Average",
                            "Teammates' T Score Average", "Individual T Score")
    return(dsmOriented)
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
           
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("adaptiveFunctioningTable"),
           tableOutput("syndromeTable"),
           tableOutput("dsmOrientedTable"),
        )
        
    )
)

##############################################################################
# Server
##############################################################################
server <- function(input, output) {

    # Rows for the team chosen
    selectedTeamIdx <- reactive(which(df$TEAM == input$team))
    selectedAthleteIdx <- reactive(which.min(df$NAME == input$athlete))
    selectedTeam <- reactive(df[selectedTeamIdx(),])
    selectedAthlete <- reactive(df[selectedAthleteIdx(),])
    
    output$athleteSelection <- renderUI({
        selectInput("athlete", "Athlete:",
                    choices = unique(df$NAME[selectedTeamIdx()]))
    })
    
    # Tables of T-scores for all athletes.
    output$adaptiveFunctioningTable <- renderTable(
        buildAdaptiveFunctioning(selectedAthlete(), selectedTeam())
    )
    output$dsmOrientedTable <- renderTable(
        buildDSMOriented(selectedAthlete(), selectedTeam())
    )
    output$syndromeTable <- renderTable(
        buildSyndrome(selectedAthlete(), selectedTeam())
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
