library(tidyverse)
library(rstan)
library(parallel)
#games <- read.csv("afl_model/games.csv")
#games$game_idx <- seq_len(nrow(games))
#players <- read.csv("afl_model/players.csv")
#players$player_idx <- seq_len(nrow(players))
#stats <- read.csv("afl_model/stats.csv")
#stats$row_idx <- seq_len(nrow(stats))
#
#length(which(duplicated(stats[, c("gameId", "playerId")])))
#
#df<- merge(stats, games[, c("homeTeam", "awayTeam", "homeTeamScore", "awayTeamScore", "gameId")], by="gameId") %>%
#  mutate(
#    team_score = ifelse(homeTeam == team, homeTeamScore, awayTeamScore),
#    opp_score = ifelse(homeTeam != team, homeTeamScore, awayTeamScore),
#    home = as.numeric(homeTeam == team)
#  ) %>%
#  select(-c(homeTeam, awayTeam, homeTeamScore, awayTeamScore)) %>%
#  arrange(row_idx)
#
#player_stats_cols <-c(
#  "Disposals", "Kicks", "Marks", "Handballs", "Goals", 
#  "Behinds", "Hit.Outs", "Tackles", "Rebounds", "Inside.50s", "Clearances", 
#  "Clangers", "Frees", "Frees.Against", "Brownlow.Votes", "Contested.Possessions", 
#  "Uncontested.Possessions", "Contested.Marks", "Marks.Inside.50", 
#  "One.Percenters", "Bounces", "Goal.Assists"
#)
#
#player_mean_data <- df %>%
#  group_by(playerId) %>%
#  summarise(across(player_stats_cols, mean)) %>%
#  merge(players[, c('playerId', 'player_idx')]) %>%
#  arrange(player_idx)

row_membership <- (df %>%
  merge(players[c('playerId', 'player_idx')], by="playerId") %>%
  arrange(row_idx))$player_idx

home_df <- df %>%
  merge(games[, c('game_idx', 'gameId')], by='gameId') %>%
  merge(players[, c('playerId', 'player_idx')], by='playerId') %>%
  select(c(game_idx, player_idx, home)) %>%
  arrange(player_idx) %>%
  pivot_wider(names_from = player_idx, values_from = home) %>%
  arrange(game_idx)  %>%
  select(-game_idx) %>%
  replace(is.na(.), 0) %>%
  as.matrix 

away_df <- df %>%
  merge(games[, c('game_idx', 'gameId')], by='gameId') %>%
  merge(player_mean_data[, c('playerId', 'player_idx')], by='playerId') %>%
  select(c(game_idx, player_idx, home)) %>%
  mutate(home = 1-home) %>%
  arrange(player_idx) %>%
  pivot_wider(names_from = player_idx, values_from = home) %>%
  arrange(game_idx)  %>%
  select(-game_idx) %>%
  replace(is.na(.), 0) %>%
  as.matrix 

model_fit <- stan(
  file = "afl_model/model.stan",
  cores = parallel::detectCores(),
  data = list(
    K = length(player_stats_cols),
    N_data = nrow(df),
    N_players = nrow(player_mean_data),
    N_games = n_distinct(df$gameId),
    player = row_membership,
    player_data = player_mean_data[, player_stats_cols] %>%
      as.matrix %>%
      scale,
    home = home_df,
    away = away_df,
    deficit = games$homeTeamScore - games$awayTeamScore
  )
)
save(model_fit, file='model_v1.RData')

## sum X over team
#x_cols <- c()
#
#team_X <- df %>%
#  merge(player_mean_data, on='playerId') %>%
#  group_by(team, gameId) %>%
#  summarise(across(x_cols, mean))
#
#team_ELO <-
#
#game_ELOs <- games %>%
#  mutate(score_deficit = homeTeamScore - awayTeamScore)
#  select(gameId, homeTeam, awayTeam, score_deficit) %>%
#  merge(team_ELO, by.x="homeTeam", by.y=team) %>%
#  rename(home_elo = elo) %>%
#  merge(team_ELO, by.x="awayTeam", by.y=team) %>%
#  rename(away_elo = elo)
#unique(players$position)
#positions <- c("Midfield", "Forward", "Defender", "Ruck")