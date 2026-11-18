
library(dplyr)

# Load team statistics and historical tournament results
teams <- read.csv("March_Madness_2024_2026_Key_Stats.csv")
games <- read.csv("Tournament_Matchups_2024_2026.csv")

# Five statistics used in the model
stats <- c("Strength_Index", "EFG_O", "EFG_D", "TOR", "ORB")

# Calculate Strength Index
teams <- teams %>%
  mutate(Strength_Index = SOS + PPG_Diff)

# Prepare historical data from 2024 and 2025
a <- teams %>%
  select(Year, Team, all_of(stats))

b <- a

names(a) <- c("Season", "Team1", paste0("a_", stats))
names(b) <- c("Season", "Team2", paste0("b_", stats))

training <- games %>%
  filter(Season %in% c(2024, 2025)) %>%    # Change TRAINING year, Season %in% c(2024, 2025) is 2026, Season == 2024 for 2025 
  left_join(a, by = c("Season", "Team1")) %>%
  left_join(b, by = c("Season", "Team2"))

# Calculate differences between teams
for (s in stats) {
  training[[paste0("d_", s)]] <-
    training[[paste0("a_", s)]] -
    training[[paste0("b_", s)]]
}

# Record whether Team 1 won
training$win <- as.integer(training$Winner == training$Team1)

x <- paste0("d_", stats)

# Check for missing statistics
if (anyNA(training[, x])) {
  stop("Some historical team statistics did not match.")
}

# Reverse matchups to reduce team-order bias
reverse <- training
reverse[, x] <- -reverse[, x]
reverse$win <- 1 - reverse$win

training <- bind_rows(training, reverse)

# Train logistic regression model
model <- glm(
  reformulate(x, response = "win"),
  data = training,
  family = binomial()
)

# Function to predict a winner. Change year to test other seasons, 2026=2026 testing 2025=2025...
predict_winner <- function(team1, team2, year = 2026) {
  
  t1 <- teams %>%
    filter(Year == year, Team == team1)
  
  t2 <- teams %>%
    filter(Year == year, Team == team2)
  
  # Check that both teams exist
  if (nrow(t1) != 1 || nrow(t2) != 1 || team1 == team2) {
    stop("Enter two different, exact team names.")
  }
  
  # Calculate differences in the five statistics
  differences <- setNames(
    as.list(
      as.numeric(t1[1, stats]) -
        as.numeric(t2[1, stats])
    ),
    x
  )
  
  # Predict probability of Team 1 winning
  p <- as.numeric(
    predict(
      model,
      newdata = as.data.frame(differences),
      type = "response"
    )
  )
  
  # Select predicted winner
  if (p >= 0.5) {
    winner <- team1
  } else {
    winner <- team2
  }
  
  # Display results
  cat("Team 1:", team1, "\n")
  cat("Team 2:", team2, "\n\n")
  
  cat("Predicted winner:", winner, "\n")
  cat("Estimated win probability:",
      round(100 * max(p, 1 - p), 1), "%\n")
  
  invisible(data.frame(
    Team1 = team1,
    Team2 = team2,
    Predicted_Winner = winner,
    Probability = max(p, 1 - p)
  ))
}


#lines 25 and 61 have comments explaining how to test and train different years before the prediction

# CHANGE ONLY THESE TWO NAMES
predict_winner("Tennessee", "Virginia", year = 2026) #can change year here as well for different testing

# Find exact team names
sort(unique(teams$Team))
