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

# DSM-Oriented
dsmCats <- c("Depressive Problems", "Anxiety Problems", "Somatic Problems",
             "Avoidant Personality Problems", "AD/H Problems",
             "Antisocial Personality")
dsmCols <- c("Depressive_Problems_TScore", "Anxiety_Problems_TScore",
             "Somatic_Problems_TScore", "Avoidant_Personality_Problems_TScore",
             "AD_H_Problems_TScore", "Antisocial_Personality_TScore")
dsmT_ScoreAverage <- colMeans(df[,dsmCols], na.rm = T)

# Syndromes
ssCats <- c("Anxious/Depressed", "Withdrawn", "Somatic Complaints",
            "Thought Problems", "Attention Problems", "Aggressive Behavior",
            "Rule-Breaking Behavior", "Intrusive")
ssCols <- c("Anxious__Depressed_TScore", "Withdrawn_TScore",
            "Somatic_Complaints_TScore", "Thought_Problems_TScore",
            "Attention_Problems_TScore", "Aggressive_Behavior_TScore",
            "Rule_Breaking_Behavior_TScore", "Intrusive_TScore")
ssT_ScoreAverage <- colMeans(df[,ssCols], na.rm = T)


##############################################################################
# General functions
##############################################################################

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

# Build table comparing assessments at two different dates.
buildDeltaTable <- function(assessment, comparison, cols, cats, statType="T_Score"){
    if(statType == "T_Score"){
        assessmentData <- as.numeric(assessment[cols]) # cast to avoid no data selected error.
        comparisonData <- as.numeric(comparison[cols])
        deltaLabel <- "Delta (T)"
    } else {
        assessmentData <- 100*pnorm(as.numeric(assessment[cols]), 50, 10)
        comparisonData <- 100*pnorm(as.numeric(comparison[cols]), 50, 10)
        deltaLabel <- "Delta (%)"
    }
    delta <- assessmentData - comparisonData
    deltaTable <- data.frame(cats, t(rbind(comparisonData, assessmentData, delta)))
    colnames(deltaTable) <- c("Category", comparison$DateOnForm, assessment$DateOnForm, deltaLabel)
    return(deltaTable)
}

# Clinical (borderline) percentiles are below 3 (7) or above 97 (97) depending
# on portion of assessment. Constants are the equivalent T-Scores.
LOW_BORDER = qnorm(.07, 50, 10)
LOW_CLINIC= qnorm(.03, 50, 10)
HI_BORDER = qnorm(.93, 50, 10)
HI_CLINIC = qnorm(.97, 50, 10)

buildConcernList <- function(athlete){
    afBorderIdx <- which(
        (athlete[,afCols] < LOW_BORDER) & (athlete[,afCols] > LOW_CLINIC),
        arr.ind=T)[,2]
    dsmBorderIdx <- which(
        (athlete[,dsmCols] > HI_BORDER) & (athlete[,dsmCols] < HI_CLINIC),
        arr.ind=T)[,2]
    ssBorderIdx <- which(
        (athlete[,ssCols] > HI_BORDER) & (athlete[,ssCols] < HI_CLINIC),
        arr.ind=T)[,2]
    if(length(afBorderIdx) + length(dsmBorderIdx) + length(ssBorderIdx) > 0){
        borderline <- c(afCats[afBorderIdx],
                        dsmCats[dsmBorderIdx],
                        ssCats[ssBorderIdx])
    } else {
        borderline <- "None"
    }
    return(borderline)
}

buildClinicalList <- function(athlete){
    afClinicIdx <- which(athlete[,afCols] < LOW_CLINIC, arr.ind=T)[,2]
    dsmClinicIdx <- which(athlete[,dsmCols] > HI_CLINIC, arr.ind=T)[,2]
    ssClinicIdx <- which(athlete[,ssCols] > HI_CLINIC, arr.ind=T)[,2]
    if(length(afClinicIdx) + length(dsmClinicIdx) + length(ssClinicIdx) > 0){
        clinical <- c(afCats[afClinicIdx],
                      dsmCats[dsmClinicIdx],
                      ssCats[ssClinicIdx])
    } else {
        clinical <- "None"
    }
    return(clinical)
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
    radarchart(radarData,
               pcol = 1:3,
               vlabels=tableFrame[,1])
    legend("bottomleft",
           legend=c("All Athletes", "Teammates", "Individual"),
           bty = "n",
           pch = 20,
           col = 1:3)
}

##############################################################################
# UI
##############################################################################
ui <- fluidPage(

    # Application title
    titlePanel("Liberty University Athletes"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("team", "Team:", df$TEAM),
            uiOutput("athleteSelection"),
            uiOutput("dateSelection"),
            uiOutput("dateSelectionDelta"),
            radioButtons("statType", "Display Style:",
                         c("T Score" = "T_Score","Percentile" = "Percentile")),
            width=3
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                h3("Areas of Concern:"),
                textOutput("clinical"),
                textOutput("borderline"),
            ),
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
            fluidRow(
                h2("Assessment Comparison"),
                tableOutput("adaptiveFunctioningDeltaTable"),
                tableOutput("syndromeDeltaTable"), 
                tableOutput("dsmOrientedDeltaTable"), 
            ),
           
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
    comparisonDateIdx <- reactive(which(selectedAthlete()$DateOnForm == input$compdate))
    comparisonAssessment <- reactive(selectedAthlete()[comparisonDateIdx(),])
    
    # List of athletes associated to selected team.
    output$athleteSelection <- renderUI({
        selectInput("athlete", "Athlete:",
                    choices = unique(df$NAME[selectedTeamIdx()]))
    })
    
    # List of dates associated to selected athlete.
    output$dateSelection <- renderUI({
        selectInput("date", "Assessment Date:",
                    choices = selectedTeam()$DateOnForm[selectedAthleteAllIdx()])
    })
    
    output$dateSelectionDelta <- renderUI({
        selectInput("compdate", "Comparison Assessment Date:",
                    choices = selectedTeam()$DateOnForm[selectedAthleteAllIdx()])
    })   
    
    # Data for selected athlete.
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
    
    clinicalConcerns <- reactive(buildClinicalList(selectedAthlete()))
    borderlineConcerns <- reactive(buildConcernList(selectedAthlete()))
    output$clinical <- renderText(
        paste("Clinical:", paste(clinicalConcerns(), collapse=", ")))
    output$borderline <- renderText(
        paste("Borderline:", paste(borderlineConcerns(), collapse=", ")))
    
    # Create table of athlete deltas.
    adaptiveFunctioningDelta <- reactive(buildDeltaTable(
        selectedAssessment(),
        comparisonAssessment(),
        afCols, afCats,
        statType=input$statType
    ))
    syndromeDelta <- reactive(buildDeltaTable(
        selectedAssessment(),
        comparisonAssessment(),
        ssCols, ssCats,
        statType=input$statType
    ))
    dsmOrientedDelta <- reactive(buildDeltaTable(
        selectedAssessment(),
        comparisonAssessment(),
        dsmCols, dsmCats,
        statType=input$statType
    ))
    
    # Tables of T-scores/Percentiles compared with all athletes and teammates.
    output$adaptiveFunctioningTable <- renderTable(adaptiveFunctioning())
    output$syndromeTable <- renderTable(syndrome())
    output$dsmOrientedTable <- renderTable(dsmOriented())
    
    # Tables of assessment deltas
    output$adaptiveFunctioningDeltaTable <- renderTable(adaptiveFunctioningDelta())
    output$syndromeDeltaTable <- renderTable(syndromeDelta())
    output$dsmOrientedDeltaTable <- renderTable(dsmOrientedDelta())
    
    # Radar plots
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
