### Data building functions

get_file <- function(filepath, name) {
  dat <- read.csv(filepath)
  dat[, paste0(name,'_idx')] <- seq_len(nrow(dat))
  return(dat)
}

get_main_df <- function(stats_df, games_df) {
  merge(stats_df, games_df[, c("homeTeam", "awayTeam", "homeTeamScore", "awayTeamScore", "gameId")], by="gameId") %>%
    mutate(
      team_score = ifelse(homeTeam == team, homeTeamScore, awayTeamScore),
      opp_score = ifelse(homeTeam != team, homeTeamScore, awayTeamScore),
      home = as.numeric(homeTeam == team)
    ) %>%
    select(-c(homeTeam, awayTeam, homeTeamScore, awayTeamScore)) %>%
    arrange(row_idx)
}

get_player_data <- function(main_df, player_cols, players_df) {
  main_df %>%
    group_by(playerId) %>%
    summarise(across(all_of(player_cols), mean)) %>%
    merge(players_df[, c('playerId', 'player_idx')]) %>%
    arrange(player_idx)
}