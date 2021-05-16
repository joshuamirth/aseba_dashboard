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
        colnames(tableFrame) <- c("Category", "All Athletes (T)",
                            "Teammates (T)", "Individual (T)")    
    } else {
        tableFrame <- data.frame(cats, 100*pnorm(tScoreAverage, 50, 10),
            100*pnorm(tScoreTeam, 50, 10), 100*pnorm(tScoreAthlete, 50, 10))
            colnames(tableFrame) <- c("Category", "All Athletes (%)",
                            "Teammates (%)", "Individual (%)")    
    }
    return(tableFrame)
}

# TODO: Dates may not be in order. Want last date - first date.
# TODO: behaves poorly before data selection made.
buildDeltaTable <- function(athlete, cols, cats, dates, statType="T_Score"){
    tScoreAthlete <- t(as.matrix(athlete[,cols]))
    deltas <- tScoreAthlete[,ncol(tScoreAthlete)] - tScoreAthlete[,1]
    deltaFrame <- data.frame(cats, cbind(tScoreAthlete, deltas))
    colVec <- append(dates, "Category", after=0)
    colVec <- append(colVec, "Delta")
    colnames(deltaFrame) <- colVec
    return(deltaFrame)
}

# Radar chart commands. Separate function so they can all by styled.
# TODO: add better formatting.
buildRadarChart <- function(tableFrame){
    radarData <- data.frame(rbind(
        rep(100, nrow(tableFrame)),
        rep(0, nrow(tableFrame)),
        t(tableFrame[,-1])   # first column is labels
    ))   
    radarData[is.na(radarData)] <- 0
    radarchart(radarData, vlabels=tableFrame[,1])
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
            
            uiOutput("dateSelection"),
            
            radioButtons("statType", "Display Style:",
                         c("T Score" = "T_Score","Percentile" = "Percentile")),
            width=3
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                h3("Adaptive Functioning Scores"),
                column(6,
                    tableOutput("adaptiveFunctioningTable"),
                ),
                column(6,
                    plotOutput("adaptiveFunctioningPlot"),
                )
            ),
            fluidRow(
                h3("Syndrome Scores"),
                column(6,
                    tableOutput("syndromeTable"),
                ),
                column(6,
                    plotOutput("syndromePlot"),
                )
            ),
            fluidRow(
                h3("DSM-Oriented Scores"),
                column(6,
                    tableOutput("dsmOrientedTable"),
                ),
                column(6,
                    plotOutput("dsmOrientedPlot"),
                )
            ),
            
           tableOutput("athleteDeltaTable"),
           
           # tableOutput("dsmOrientedTable"),
           # plotOutput("syndromePlot"),
           # plotOutput("dsmOrientedPlot"),
           
           width=9
        )
        
    )
)

##############################################################################
# Server
##############################################################################
server <- function(input, output) {

    # Rows for the team and athlete chosen.
    selectedTeamIdx <- reactive(which(df$TEAM == input$team))
    selectedTeam <- reactive(df[selectedTeamIdx(),])
    selectedAthleteAllIdx <- reactive(which(selectedTeam()$NAME == input$athlete))
    selectedAthlete <- reactive(selectedTeam()[selectedAthleteAllIdx(),])
    selectedDateIdx <- reactive(which(selectedAthlete()$DateOnForm == input$date))
    selectedAssessment <- reactive(selectedAthlete()[selectedDateIdx(),])
    
    # List of athletes associated to selected team.
    output$athleteSelection <- renderUI({
        selectInput("athlete", "Athlete:",
                    choices = unique(df$NAME[selectedTeamIdx()]))
    })
    
    output$dateSelection <- renderUI({
        selectInput("date", "Assessment Date:",
                    choices = selectedTeam()$DateOnForm[selectedAthleteAllIdx()])
    })
    
    # dataframes for selected athlete.
    adaptiveFunctioning <- reactive(buildStatsTable(selectedAssessment(),
                                                    selectedTeam(),
                                                    afCols, afCats,
                                                    afT_ScoreAverage,
                                                    statType = input$statType))
    dsmOriented <- reactive(buildStatsTable(selectedAssessment(),
                                   selectedTeam(),
                                   dsmCols,
                                   dsmCats,
                                   dsmT_ScoreAverage,
                                   statType = input$statType))
    syndrome <- reactive(buildStatsTable(selectedAssessment(),
                                         selectedTeam(),
                                         ssCols, ssCats,
                                         ssT_ScoreAverage,
                                         statType = input$statType))
    
    # TODO: create table of athlete deltas.
    athleteDelta <- reactive(buildDeltaTable(
        selectedAthlete(), ssCols, ssCats,
        selectedTeam()$DateOnForm[selectedAthleteAllIdx()]
    ))

    output$athleteDeltaTable <- renderTable(athleteDelta())
    
    # Tables of T-scores/Percentiles compared with all athletes and teammates.
    output$adaptiveFunctioningTable <- renderTable(adaptiveFunctioning())
    output$dsmOrientedTable <- renderTable(dsmOriented())
    output$syndromeTable <- renderTable(syndrome())
    
    # Radar plot of comparison data
    
    # TODO: figure out how to handle the errors that occur when no data is yet
    # selected (it doesn't cause issues, so at least suppress the output.)
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
