
library(dplyr)

# Load team statistics
teams <- read.csv("March_Madness_2024_2026_Key_Stats.csv")

# Calculate Strength Index
teams <- teams %>%
  mutate(Strength_Index = SOS + PPG_Diff)

# Function to predict the winner
predict_winner <- function(team1, team2, year = 2026) {
  
  t1 <- teams %>%
    filter(Year == year, Team == team1)
  
  t2 <- teams %>%
    filter(Year == year, Team == team2)
  
  # Check that both teams exist
  if (nrow(t1) != 1 || nrow(t2) != 1 || team1 == team2) {
    stop("Enter two different, exact team names.")
  }
  
  # Compare Strength Index
  index1 <- t1$Strength_Index
  index2 <- t2$Strength_Index
  
  if (index1 > index2) {
    winner <- team1
  } else if (index2 > index1) {
    winner <- team2
  } else {
    winner <- "Tie"
  }
  
  # Display results
  cat("Team 1:", team1, "\n")
  cat("Strength Index:", round(index1, 2), "\n\n")
  
  cat("Team 2:", team2, "\n")
  cat("Strength Index:", round(index2, 2), "\n\n")
  
  cat("Predicted winner:", winner, "\n")
  
  invisible(data.frame(
    Team1 = team1,
    Team2 = team2,
    Predicted_Winner = winner
  ))
}

# CHANGE ONLY THESE TWO NAMES
predict_winner("Duke", "Siena")

# Find exact team names
sort(unique(teams$Team))
