### Data building functions

get_file <- function(filepath, name, filter_years=F, years=2012:2021, yearcol='year', get_date=F, datecol='date') {
  sort_year_helper <- function(dat) {
    dat %>%
      mutate('{datecol}' := parse_date(.data[[datecol]], "%d-%b-%Y")) %>%
      arrange(.data[[paste0(datecol)]]) %>%
      mutate('{datecol}_idx' := row_number()) %>%
      arrange(.data[[paste0(name,'_idx')]])
  }
  read.csv(filepath) %>%
    when(
      filter_years ~ .[.[[yearcol]] %in% years, ],
      ~ .
    ) %>%
    mutate("{name}_idx" := row_number())  %>%
    when(
      get_date ~ sort_year_helper(.),
      ~ .
    ) 
}

get_main_df <- function(stats_df, games_df, homecol="homeTeam", awaycol="awayTeam", homescore="homeTeamScore", awayscore="awayTeamScore", 
                        game_idcol="gameId", teamcol="team", row_idxcol="row_idx") {
  merge(stats_df, games_df[, c(homecol, awaycol, homescore, awayscore, game_idcol)], by=game_idcol) %>%
    mutate(
      'team_score' := ifelse(.data[[homecol]] == .data[[teamcol]], .data[[homescore]], .data[[awayscore]]),
      'opp_score' := ifelse(.data[[homecol]] != .data[[teamcol]], .data[[homescore]], .data[[awayscore]]),
      'home' := as.numeric(.data[[homecol]] == .data[[teamcol]])
    ) %>%
    arrange(.data[[row_idxcol]])
}

get_player_data <- function(main_df, player_cols, players_df, player_idcol="playerId", player_idxcol="player_idx") {
  main_df %>%
    group_by(.data[[player_idcol]]) %>%
    summarise(across(all_of(player_cols), mean)) %>%
    merge(players_df[, c(player_idcol, player_idxcol)]) %>%
    arrange(.data[[player_idxcol]])
}