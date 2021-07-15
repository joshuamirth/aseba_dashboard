library(shiny)
library(fmsb)        # for radar charts.
library(formattable) # For conditional formatting of tables.

##############################################################################
# Data pre-processing.
##############################################################################
# Import the data set from the database. (Currently just a flat R data frame.)
# df <- readRDS("data/example.Rda")
df <- readRDS("data/lu_data.Rda")
# Correct database values. Spouse/Partner score often reported as zero when
# respondent does not have a spouse/partner. Better to treat these as zeros.
df$Spouse_Partner_TScore[which(df$Spouse_Partner_TScore < .01)] = NA
# Dates missing from some assessments. Replace with unique identifier (even
# though not meaningful).
missingDates <- which(df$DateOnForm == "")
df$DateOnForm[missingDates] <- df$FormId[missingDates]

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
        tableFrame <- data.frame(tScoreAverage, tScoreTeam, tScoreAthlete)
        colnames(tableFrame) <- c("All Athletes (T)",
                                  "Teammates (T)", "Individual (T)")    
    } else {
        tableFrame <- data.frame(100*pnorm(tScoreAverage, 50, 10),
            100*pnorm(tScoreTeam, 50, 10), 100*pnorm(tScoreAthlete, 50, 10))
            colnames(tableFrame) <- c("All Athletes (%)",
                                      "Teammates (%)", "Individual (%)")    
    }
    # print(length(rownames(tableFrame)))
    rownames(tableFrame) <- cats
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
    deltaTable <- data.frame(t(rbind(comparisonData, assessmentData, delta)))
    # TODO: Improve this mild hack with adding "A" and "B" to column names.
    # Formatted tables do not behave well with duplicate column names so need to
    # force these to be different at all times.
    colnames(deltaTable) <- c(paste("A:", comparison$DateOnForm), paste("B:", assessment$DateOnForm), deltaLabel)
    rownames(deltaTable) <- cats
    return(deltaTable)
}

# Clinical (borderline) percentiles are below 3 (7) or above 97 (97) depending
# on portion of assessment. Constants are the equivalent T-Scores.
LO_BORDER = qnorm(.07, 50, 10)
LO_CLINIC= qnorm(.03, 50, 10)
HI_BORDER = qnorm(.93, 50, 10)
HI_CLINIC = qnorm(.97, 50, 10)

buildConcernList <- function(athlete){
    afBorderIdx <- which(
        (athlete[,afCols] < LO_BORDER) & (athlete[,afCols] > LO_CLINIC),
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
    afClinicIdx <- which(athlete[,afCols] < LO_CLINIC, arr.ind=T)[,2]
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
# Set color parameters.
myRed = "#FF1E1EFF"
myBlue = "#0007A3FF"
myLBlue = "#B1CBEB"
myGrey = "#99999980"
myPink = "#FF9494FF"
myLtGreen = "#99ff99"
myGreen = "#00cc00"
pcol = c(NA, myRed, myBlue)          # Line colors
col = c(myGrey, myRed, myBlue)  # Legend colors
plty = c(2,1,1)                 # Line styles
pfcol = c(myGrey, NA, NA)  # Fill colors

buildRadarChart <- function(tableFrame){
    op <- par(mar = c(3, 1, 0.5, 1)) # Plot margins
    radarData <- data.frame(rbind(
        rep(100, nrow(tableFrame)),
        rep(0, nrow(tableFrame)),
        t(tableFrame)   # first column is labels
    ))   
    radarData[is.na(radarData)] <- 0
    radarchart(radarData,
               pcol = pcol,
               plty = plty,
               pfcol = pfcol,
               vlabels=rownames(tableFrame),
               vlcex=0.8)
    legend("topleft",
           legend=c("All Athletes", "Teammates", "Individual"),
           bty = "n",
           pch = 20,
           col = col,
           inset=0,
           cex=1,
           pt.cex = 2)
    par(op)
}

##############################################################################
# Formatting and CSS
##############################################################################
# Conditional formatting for the main data tables.

getFormatList <- function(nTeammates=1, high=TRUE){
    # Set number of standard deviations for borderline and clinical values. The
    # "official" values are 1.5 and 1.85 for an individual. At the aggregate
    # level that makes almost everything clinical(!) so for visual clarity we
    # increase these parameters.
    TEAM_CLINIC <- 3.0
    TEAM_BORDER <- 2.0
    
    teamStdDev <- 10/sqrt(nTeammates)
    
    teamHiBorder <- 50 + TEAM_BORDER*teamStdDev
    teamHiClinic <- 50 + TEAM_CLINIC*teamStdDev 
    teamLoBorder <- 50 - TEAM_BORDER*teamStdDev
    teamLoClinic <- 50 - TEAM_CLINIC*teamStdDev
    
    teamHiBorderPerc <- 100*pnorm(teamHiBorder, 50, 10)
    teamHiClinicPerc <- 100*pnorm(teamHiClinic, 50, 10)
    teamLoBorderPerc <- 100*pnorm(teamLoBorder, 50, 10)
    teamLoClinicPerc <- 100*pnorm(teamLoClinic, 50, 10)
    
    if(high){
        hiBorderColor = myPink
        hiClinicColor = myRed
        loBorderColor = myLtGreen
        loClinicColor = myGreen
    } else {
        hiBorderColor = myLtGreen
        hiClinicColor = myGreen
        loBorderColor = myPink
        loClinicColor = myRed
    }
    
    concernFormat <- list(
        `All Athletes (%)` = formatter("span", x ~ percent(x / 100)),
        `Teammates (%)` = formatter("span", x ~ percent(x / 100),
                            style = x ~ style(display = "block",
                                padding = "0 4px",
                                `border-radius` = "4px",
                                `background-color` = ifelse(x > teamHiClinicPerc, hiClinicColor,
                                                     ifelse(x > teamHiBorderPerc, hiBorderColor,
                                                     ifelse(x > teamLoBorderPerc, "white",
                                                     ifelse(x > teamLoClinicPerc, loBorderColor, loClinicColor)))))),       
        `Individual (%)` = formatter("span", x ~ percent(x / 100),
                            style = x ~ style(display = "block",
                                padding = "0 4px",
                                `border-radius` = "4px",
                                `background-color` = ifelse(x > 97, hiClinicColor,
                                                     ifelse(x > 93, hiBorderColor,
                                                     ifelse(x > 7, "white",
                                                     ifelse(x > 3, loBorderColor, loClinicColor)))))),
        `All Athletes (T)` = formatter("span", x ~ digits(x, 2)),
        `Teammates (T)` = formatter("span", x ~ digits(x, 2),
                            style = x ~ style(display = "block",
                                padding = "0 4px",
                                `border-radius` = "4px",
                                `background-color` = ifelse(x > teamHiClinic, hiClinicColor,
                                                     ifelse(x > teamHiBorder, hiBorderColor,
                                                     ifelse(x > teamLoBorder, "white",
                                                     ifelse(x > teamLoClinic, loBorderColor, loClinicColor)))))),
        `Individual (T)` = formatter("span", x ~ digits(x, 2),
                             style = x ~ style(display = "block",
                                 padding = "0 4px",
                                 `border-radius` = "4px",
                                 `background-color` = ifelse(x > HI_CLINIC, hiClinicColor,
                                                      ifelse(x > HI_BORDER, hiBorderColor,
                                                      ifelse(x > LO_BORDER, "white",
                                                      ifelse(x > LO_CLINIC, loBorderColor, loClinicColor))))))
    )
    return(concernFormat)
}

# TODO: Improve colors for delta (gradient?).
deltaFormatPos <- list(
    area(col = 1:2) ~ function(x) digits(x, 1),
    `Delta (%)` = formatter("span", x ~ percent(x/100),
                            style = x ~ style(display = "block",
                                              padding = "0 4px",
                                              `border-radius` = "4px",
                                              `background-color` = ifelse(x > 0, myLBlue, myGrey))),
    `Delta (T)` = formatter("span", x ~ digits(x, 1),
                            style = x ~ style(display = "block",
                                              padding = "0 4px",
                                              `border-radius` = "4px",
                                              `background-color` = ifelse(x > 0, myLBlue, myGrey)))
)

deltaFormatNeg <- list(
    area(col = 1:2) ~ function(x) digits(x, 1),
    `Delta (%)` = formatter("span", x ~ percent(x/100),
                            style = x ~ style(display = "block",
                                              padding = "0 4px",
                                              `border-radius` = "4px",
                                              `background-color` = ifelse(x < 0, myLBlue, myGrey))),
    `Delta (T)` = formatter("span", x ~ digits(x, 1),
                            style = x ~ style(display = "block",
                                              padding = "0 4px",
                                              `border-radius` = "4px",
                                              `background-color` = ifelse(x < 0, myLBlue, myGrey)))
)

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
                         c("Percentile" = "Percentile", "T Score" = "T_Score")),
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
                    formattableOutput("adaptiveFunctioningTable"),
                    # tableOutput("adaptiveFunctioningTable"),
                ),
                column(6,
                    plotOutput("adaptiveFunctioningPlot"),
                )
            ),
            fluidRow(
                h3("Syndrome Scores"),
                column(6,
                    formattableOutput("syndromeTable"),
                ),
                column(6,
                    plotOutput("syndromePlot"),
                )
            ),
            fluidRow(
                h3("DSM-Oriented Scores"),
                column(6,
                    formattableOutput("dsmOrientedTable"),
                ),
                column(6,
                    plotOutput("dsmOrientedPlot"),
                )
            ),
            fluidRow(
                h2("Assessment Comparison"),
                column(9,
                formattableOutput("adaptiveFunctioningDeltaTable"),
                formattableOutput("syndromeDeltaTable"), 
                formattableOutput("dsmOrientedDeltaTable"), 
                ),
                column(3)
            ),
           
           width=9
        )
        
    )
)

##############################################################################
# Server
##############################################################################
server <- function(input, output, session) {

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
    
    # Stats for team.
    nTeammates <- reactive(nrow(selectedTeam()))
    
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
    
    clinicalConcerns <- reactive(buildClinicalList(selectedAssessment()))
    borderlineConcerns <- reactive(buildConcernList(selectedAssessment()))
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
    output$adaptiveFunctioningTable <- renderFormattable({formattable(adaptiveFunctioning(), getFormatList(nTeammates=nTeammates(), high=FALSE))})
    output$syndromeTable <- renderFormattable({formattable(syndrome(), getFormatList(nTeammates=nTeammates()))})
    output$dsmOrientedTable <- renderFormattable({formattable(dsmOriented(), getFormatList(nTeammates=nTeammates()))})
    
    # Tables of assessment deltas
    output$adaptiveFunctioningDeltaTable <- renderFormattable({formattable(adaptiveFunctioningDelta(), deltaFormatPos)})
    output$syndromeDeltaTable <- renderFormattable({formattable(syndromeDelta(), deltaFormatNeg)})
    output$dsmOrientedDeltaTable <- renderFormattable({formattable(dsmOrientedDelta(), deltaFormatNeg)})
    
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
