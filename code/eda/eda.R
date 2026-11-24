library(ggplot2)
library(dplyr)

# load the team stats and the tournament games
teams <- read.csv("data/March_Madness_2024_2026_Key_Stats.csv", fileEncoding = "UTF-8-BOM")
games <- read.csv("data/Tournament_Matchups_2024_2026.csv", fileEncoding = "UTF-8-BOM")

# Get feature datatypes
str(teams)
str(games)

# Data Preprocessing 
## Converts "Elite 8" value in games$Round to "Elite Eight" for consistency 
games$Round[games$Round == "Elite 8"] <- "Elite Eight"

## Add new column for whether team made or missed the tournament
teams$Tournament <- ifelse(is.na(teams$Seed), "Missed tournament", "Made tournament")

## Standardize games$Round values
games$Round <- factor(games$Round, levels = c(
  "First Four", "Round of 64", "Round of 32", "Sweet 16",
  "Elite Eight", "Final Four", "Championship"))

## Convert nominal data to factors - used to get counts 
teams$Year <- factor(teams$Year)
teams$Conference <- factor(teams$Conference)
teams$Seed <- factor(teams$Seed)

games$Season <- factor(games$Season)
games$Round <- factor(games$Round)

# Get summary of datasets
summary(teams)
summary(games)

# Box plot showing Strength Index by seed 
ggplot(filter(teams, !is.na(Seed)), aes(factor(Seed), Strength_Index)) +
  geom_boxplot(fill = "#DCE6F2", outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 1, alpha = 0.5) +
  labs(title = "Strength Index by seed, 2024–2026", x = "Seed", y = "Strength Index") +
  theme_minimal()


# Histogram comparing strength index of teams in tourney vs. not in tourney (seed value is NA)
ggplot(teams, aes(Strength_Index, fill = Tournament)) +
  geom_histogram(bins = 40, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("Made tournament" = "#0072B2", "Missed tournament" = "#E69F00")) +
  labs(title = "Strength Index: tournament vs. non-tournament teams", x = "Strength Index", y = "Teams", fill = NULL) +
  theme_minimal()