### create features for stan

get_row_membership <- function(main_df, players_df) {
  main_df %>%
    merge(players_df[c('playerId', 'player_idx')], by="playerId") %>%
    arrange(row_idx)
}

get_presence_matrix <- function(main_df, games_df, players_df, is_home=0) {
  main_df %>%
    merge(games_df[, c('game_idx', 'gameId')], by='gameId') %>%
    merge(players_df[, c('playerId', 'player_idx')], by='playerId') %>%
    select(c(game_idx, player_idx, home)) %>%
    arrange(player_idx) %>%
    pivot_wider(names_from = player_idx, values_from = home) %>%
    arrange(game_idx)  %>%
    select(-game_idx) %>%
    replace(is.na(.), 0) %>%
    as.matrix 
}