filename <- "csv_data_file_name"
infile <- paste(filename, "csv", sep=".")
df <- read.csv(infile)

# tsv_rda <- function(filename){
#   infile <- paste(filename,".csv",se="")
#   df <- read.delim(infile, skip=1)
#   outfile <- paste(filename, ".Rda", sep="")
#   saveRDS(df, outfile)
# }

reqd_cols = c(
  "NAME",
  "TEAM",
  "DateOnForm",
  "FormId",
  "Friends_TScore",
  "Spouse_Partner_TScore",
  "Family_TScore",
  "Job_TScore",
  "Education_TScore",
  "Mean_Adaptive_TScore",
  "Personal_Strengths_TScore",
  "Depressive_Problems_TScore",
  "Anxiety_Problems_TScore",
  "Somatic_Problems_TScore",
  "Avoidant_Personality_Problems_TScore",
  "AD_H_Problems_TScore",
  "Antisocial_Personality_TScore",
  "Anxious__Depressed_TScore",
  "Withdrawn_TScore",
  "Somatic_Complaints_TScore",
  "Thought_Problems_TScore",
  "Attention_Problems_TScore",
  "Aggressive_Behavior_TScore",
  "Rule_Breaking_Behavior_TScore",
  "Intrusive_TScore"
)

# See which columns are missing, if any.
print("Missing columns:")
for (i in reqd_cols) {
  if (! i %in% colnames(df)){
    print(i)
  }
}

# Merge firstname and lastname columns into NAME
df$NAME <- paste(df$asr_firstname, df$asr_lastname, sep=" ")

# Add TEAM column.
teams = read.csv("data/athlete_teams.csv")
teams = unique(teams)
colnames(teams) <- c("TEAM", "NAME")
fulldf = merge(x=df, y=teams, by="NAME")

# Sort by team and name
fulldf <- fulldf[order(fulldf$TEAM, fulldf$NAME),]

# Remove "empty" reports
tscores <- c(26, 123, 3) # columns containing T scores.
empties <- as.vector(rowSums(is.na(fulldf[,t_scores])))
gooddf <- fulldf[empties < 15,]

# Write to .Rda file
outfile = paste(filename, "Rda", sep=".")
saveRDS(gooddf, file=outfile)