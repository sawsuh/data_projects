### create features for stan


get_presence_matrix <- function(main_df, games_df, players_df, is_away=F, game_idxcol="game_idx", 
                                game_idcol="gameId", player_idxcol="player_idx", player_idcol="playerId", home_col="home") {
  main_df %>%
    merge(games_df[, c(game_idxcol, game_idcol)], by=game_idcol) %>%
    merge(players_df[, c(player_idcol, player_idxcol)], by=player_idcol) %>%
    select(all_of(c(game_idxcol, player_idxcol, home_col))) %>%
    when(
      is_away ~ mutate({.}, "{home_col}" := 1-.data[[home_col]]),
      ~ .
    ) %>%
    arrange(.data[[player_idxcol]]) %>%
    pivot_wider(names_from = all_of(player_idxcol), values_from = all_of(home_col)) %>%
    arrange(.data[[game_idxcol]]) %>%
    select(-all_of(game_idxcol)) %>%
    replace(is.na(.), 0) %>%
    as.matrix
}

get_player_matrix <- function(player_mean_df, cols) {
  player_mean_df[, cols] %>%
    as.matrix %>%
    scale
}

get_stan_input <- function(player_mean_matrix, home_df, away_df, game_df, row_membership_col='player_idx',
                           homescore="homeTeamScore", awayscore="awayTeamScore") {
  list(
    K = ncol(player_mean_matrix),
    N_players = nrow(player_mean_matrix),
    N_games = nrow(game_df),
    player_data = player_mean_matrix,
    home = home_df,
    away = away_df,
    deficit = game_df[[homescore]] - game_df[[awayscore]]
  )
}