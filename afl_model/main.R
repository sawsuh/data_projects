### main
library(tidyverse)
library(rstan)
library(parallel)

year_filter <- F
year_to_use <- 2012
model_saved <- F
model_path <- "model_v2.RData"

source("code/load_data.R")
games <- get_file("data/games.csv", "game", filter_years=year_filter, years=year_to_use)
players <- get_file("data/players.csv", "player")
stats <- get_file("data/stats.csv", "row", filter_years=year_filter, years=year_to_use)
df <- get_main_df(stats, games)

player_stats_cols <-c(
  "Disposals", "Kicks", "Marks", "Handballs", "Goals", 
  "Behinds", "Hit.Outs", "Tackles", "Rebounds", "Inside.50s", "Clearances", 
  "Clangers", "Frees", "Frees.Against", "Brownlow.Votes", "Contested.Possessions", 
  "Uncontested.Possessions", "Contested.Marks", "Marks.Inside.50", 
  "One.Percenters", "Bounces", "Goal.Assists"
)
player_mean_data <- get_player_data(df, player_stats_cols, players)

source("code/feature_gen.R")
home <- get_presence_matrix(df, games, players)
away <- get_presence_matrix(df, games, players, is_away=1)
player_matrix <- get_player_matrix(player_mean_data, player_stats_cols)
stan_input <- get_stan_input(player_matrix, home, away, games)

source("code/train_model.R")
if (model_saved) {
  load(model_path)
} else {
  model_fit <- train_model("code/model.stan", stan_input)
  save(model_fit, file=model_path)
}
