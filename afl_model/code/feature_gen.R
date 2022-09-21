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

# for each game
# get cumavg of player stats up to that game
get_game_avgs <- function(main, players_df, games_df, stats_cols, player_idxcol='player_idx', 
                                 homecol='home', game_idxcol='game_idx', date_idxcol='date_idx', row_idxcol='row_idx')
{
  get_game_rolling_avg_helper <- function(homedf)
  {
    main %>%
      merge(players_df[, c('playerId', player_idxcol)]) %>%
      merge(games_df[, c('gameId', game_idxcol, date_idxcol)]) %>%
      # get rolling avgs for each player by game
      arrange(.data[[player_idxcol]]) %>%
      group_by(.data[[player_idxcol]]) %>%
      arrange(.data[[date_idxcol]], .by_group = T) %>%
      mutate(across(all_of(stats_cols), .fns = function(x)cumsum(x)/seq_along(x))) %>%
      ungroup %>%
      # filter for home/away
      dplyr::filter(.data[[homecol]]==as.numeric(homedf)) %>%
      # get avgs over team per game
      group_by(.data[[game_idxcol]]) %>%
      summarise(across(all_of(stats_cols), mean)) %>%
      arrange(.data[[game_idxcol]]) %>%
      select(all_of(stats_cols))
  }
  map(
    c(T,F),
    ~ get_game_rolling_avg_helper(.)
  )
}
scale_stats <- function(avgs, N) {
  ov_stats <- map(
    c(mean, sd),
    ~ t(replicate(N,apply(do.call(rbind, avgs), 2, .)))
  )
  map(
    avgs,
    ~ (.-ov_stats[[1]])/ov_stats[[2]]
  )
}

get_player_matrix <- function(player_mean_df, cols) {
  player_mean_df[, cols] %>%
    as.matrix %>%
    scale
}

get_stan_input <- function(player_mean_matrix, presence_dfs, game_df, row_membership_col='player_idx',
                           homescore="homeTeamScore", awayscore="awayTeamScore") {
  list(
    K = ncol(player_mean_matrix),
    N_players = nrow(player_mean_matrix),
    N_games = nrow(game_df),
    player_data = player_mean_matrix,
    home = presence_dfs[[1]],
    away = presence_dfs[[2]],
    deficit = game_df[[homescore]] - game_df[[awayscore]]
  )
}