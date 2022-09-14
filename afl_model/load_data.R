### Data building functions

get_file <- function(filepath, name) {
  dat <- read.csv(filepath)
  dat[, paste0(name,'_idx')] <- seq_len(nrow(dat))
  return(dat)
}

get_main_df <- function(stats_df, games_df) {
  df<- merge(stats_df, games_df[, c("homeTeam", "awayTeam", "homeTeamScore", "awayTeamScore", "gameId")], by="gameId") %>%
    mutate(
      team_score = ifelse(homeTeam == team, homeTeamScore, awayTeamScore),
      opp_score = ifelse(homeTeam != team, homeTeamScore, awayTeamScore),
      home = as.numeric(homeTeam == team)
    ) %>%
    select(-c(homeTeam, awayTeam, homeTeamScore, awayTeamScore)) %>%
    arrange(row_idx)
  return(df)
}

get_player_data <- function(main_df, player_cols, players_df) {
  player_mean_data <- main_df %>%
    group_by(playerId) %>%
    summarise(across(player_cols, mean)) %>%
    merge(players_df[, c('playerId', 'player_idx')]) %>%
    arrange(player_idx)
}

get_player_data <- function(player_mean_df, cols) {
  player_df[, cols] %>%
    as.matrix %>%
    scale
}

get_stan_input <- function(main_df, player_mean_df, player_mean_matrix, row_membership_df, home_df, away_df, game_df) {
  list(
    K = length(player_stats_cols),
    N_data = nrow(df),
    N_players = nrow(player_mean_data),
    N_games = n_distinct(df$gameId),
    player = row_membership,
    player_data = player_mean_matrix,
    home = home_df,
    away = away_df,
    deficit = game_df$homeTeamScore - game_df$awayTeamScore
  )
}