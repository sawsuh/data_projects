### Data building functions

get_file <- function(filepath, name) {
  dat <- read.csv(filepath)
  dat[, paste0(name,'_idx')] <- seq_len(nrow(dat))
  return(dat)
}

get_main_df <- function(stats_df, games_df, homecol="homeTeam", awaycol="awayTeam", homescore="homeTeamScore", awayscore="awayTeamScore", 
                        game_idcol="gameId", teamcol="team", row_idxcol="row_idx") {
  dat <- merge(stats_df, games_df[, c(homecol, awaycol, homescore, awayscore, game_idcol)], by=game_idcol)# %>%
  dat[['team_score']] <- ifelse(dat[[homecol]] == dat[[teamcol]], dat[[homescore]], dat[[awayscore]])
  dat[['opp_score']] <- ifelse(dat[[homecol]] != dat[[teamcol]], dat[[homescore]], dat[[awayscore]])
  dat[['home']] <- as.numeric(dat[[homecol]] == dat[[teamcol]])
  return(dat[order(dat[[row_idxcol]]), ])
}

get_player_data <- function(main_df, player_cols, players_df, player_idcol="playerId", player_idxcol="player_idx") {
  dat <- main_df %>%
    group_by(.data[[player_idcol]]) %>%
    summarise(across(all_of(player_cols), mean)) %>%
    merge(players_df[, c(player_idcol, player_idxcol)])
  return(dat[order(dat[[player_idxcol]]), ])
}