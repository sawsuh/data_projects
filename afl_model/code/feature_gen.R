### create features for stan

get_row_membership <- function(main_df, players_df) {
  main_df %>%
    merge(players_df[c('playerId', 'player_idx')], by="playerId") %>%
    arrange(row_idx)
}

get_presence_matrix <- function(main_df, games_df, players_df, is_away=F) {
  main_df %>%
    merge(games_df[, c('game_idx', 'gameId')], by='gameId') %>%
    merge(players_df[, c('playerId', 'player_idx')], by='playerId') %>%
    select(c(game_idx, player_idx, home)) %>%
    #mutate(home = ifelse(is_away, 1-home, home)) %>%
    arrange(player_idx) %>%
    pivot_wider(names_from = player_idx, values_from = home) %>%
    arrange(game_idx)  %>%
    select(-game_idx) %>%
    replace(is.na(.), 0) %>%
    as.matrix 
}

get_player_matrix <- function(player_mean_df, cols) {
  player_mean_df[, cols] %>%
    as.matrix %>%
    scale
}

get_stan_input <- function(main_df, player_mean_matrix, row_membership_df, home_df, away_df, game_df) {
  list(
    K = ncol(player_mean_matrix),
    N_data = nrow(main_df),
    N_players = nrow(player_mean_matrix),
    N_games = n_distinct(main_df$gameId),
    player = row_membership_df,
    player_data = player_mean_matrix,
    home = home_df,
    away = away_df,
    deficit = game_df$homeTeamScore - game_df$awayTeamScore
  )
}