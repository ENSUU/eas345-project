
library(dplyr)
library(ggplot2)

# load the team stats and the tournament games
teams <- read.csv("March_Madness_2024_2026_Key_Stats.csv", fileEncoding = "UTF-8-BOM")
games <- read.csv("Tournament_Matchups_2024_2026.csv", fileEncoding = "UTF-8-BOM")

# strength index is SOS + PPG_Diff, same as the other scripts
teams <- teams %>%
  mutate(Strength_Index = SOS + PPG_Diff)

# same five stats as the logistic regression script
stats <- c("Strength_Index", "EFG_O", "EFG_D", "TOR", "ORB")
x <- paste0("d_", stats)

# puts both teams' stats next to each other for every game in the years we pick
# the d_ columns are Team1 minus Team2
get_games <- function(years) {

  a <- teams %>% select(Year, Team, Seed, all_of(stats))
  names(a) <- c("Season", "Team1", "Seed1", paste0("a_", stats))

  b <- teams %>% select(Year, Team, Seed, all_of(stats))
  names(b) <- c("Season", "Team2", "Seed2", paste0("b_", stats))

  g <- games %>%
    filter(Season %in% years) %>%
    left_join(a, by = c("Season", "Team1")) %>%
    left_join(b, by = c("Season", "Team2"))

  for (s in stats) {
    g[[paste0("d_", s)]] <- g[[paste0("a_", s)]] - g[[paste0("b_", s)]]
  }

  # 1 if Team1 won, 0 if Team2 won
  g$win <- as.integer(g$Winner == g$Team1)

  if (anyNA(g[, x])) {
    stop("Some team stats didn't match, check the team names.")
  }
  g
}

# trains the logistic regression on whatever seasons we give it
# only give it seasons BEFORE the one we're testing, otherwise it's cheating
train_model <- function(train_years) {

  training <- get_games(train_years)

  # flip every game too so the order of the teams doesn't matter
  flipped <- training
  flipped[, x] <- -flipped[, x]
  flipped$win <- 1 - flipped$win
  training <- bind_rows(training, flipped)

  glm(reformulate(x, response = "win"), data = training, family = binomial())
}

# go through each season and count how many games each method gets right
results <- data.frame()

for (yr in 2024:2026) {

  test <- get_games(yr)

  # method 1: just pick whoever has the higher strength index
  si_right <- sum(as.integer(test$d_Strength_Index > 0) == test$win)
  results <- rbind(results, data.frame(
    Season = yr, Model = "Strength Index rule",
    Correct = si_right, Total = nrow(test)))

  # method 2: logistic regression trained on the earlier seasons
  # 2024 gets skipped since there's nothing before it to train on
  if (yr > 2024) {
    model <- train_model(2024:(yr - 1))
    p <- predict(model, newdata = test, type = "response")
    lr_right <- sum(as.integer(p >= 0.5) == test$win)
    results <- rbind(results, data.frame(
      Season = yr, Model = "Logistic regression",
      Correct = lr_right, Total = nrow(test)))
  }

  # baseline: always pick the better seed
  # games between the same seed (like the First Four) get skipped
  diff_seed <- test[test$Seed1 != test$Seed2, ]
  seed_right <- sum(as.integer(diff_seed$Seed1 < diff_seed$Seed2) == diff_seed$win)
  results <- rbind(results, data.frame(
    Season = yr, Model = "Higher seed (baseline)",
    Correct = seed_right, Total = nrow(diff_seed)))
}

results$Accuracy <- round(100 * results$Correct / results$Total, 1)
print(results)

# chart: green = predicted right, red = predicted wrong
# leaving the seed baseline out of the chart since it has a different number of games
chart_data <- results %>% filter(Model != "Higher seed (baseline)")

plot_data <- rbind(
  data.frame(Season = chart_data$Season, Model = chart_data$Model,
             Result = "Correct", Games = chart_data$Correct),
  data.frame(Season = chart_data$Season, Model = chart_data$Model,
             Result = "Incorrect", Games = chart_data$Total - chart_data$Correct)
)
plot_data$Result <- factor(plot_data$Result, levels = c("Incorrect", "Correct"))

# simple rule on the left, logistic regression on the right
model_order <- c("Strength Index rule", "Logistic regression")
plot_data$Model <- factor(plot_data$Model, levels = model_order)
chart_data$Model <- factor(chart_data$Model, levels = model_order)

# text that goes above each bar
chart_data$Label <- sprintf("%d of %d\n(%.1f%%)",
                            chart_data$Correct, chart_data$Total, chart_data$Accuracy)

ggplot(plot_data, aes(x = factor(Season), y = Games, fill = Result)) +
  geom_col(width = 0.6) +
  geom_text(data = chart_data,
            aes(x = factor(Season), y = Total, label = Label),
            inherit.aes = FALSE, vjust = -0.3, size = 3.4, lineheight = 0.9) +
  scale_fill_manual(values = c(Correct = "#2E8B57", Incorrect = "#C0392B"),
                    breaks = c("Correct", "Incorrect")) +
  scale_y_continuous(limits = c(0, 85)) +
  facet_grid(~ Model, scales = "free_x", space = "free_x") +
  labs(title = "Tournament games predicted correctly, by season",
       x = "Tournament season", y = "Number of games", fill = NULL) +
  theme_minimal(base_size = 12)

ggsave("model_accuracy_by_season.png", width = 9, height = 5, dpi = 300, bg = "white")
