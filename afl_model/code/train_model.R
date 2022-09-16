#### Train model and analyse results

train_model <- function(filepath, input) {
  stan(
    file = filepath,
    cores = parallel::detectCores(),
    data = input
  )
}
index_startswith <- function(v, s) {
  v[startsWith(names(v), paste0(s,'['))]
}
get_stan_res <- function(mf, player_cols) {
    fitsum <- summary(mf)$summary[, 'mean']
    beta_a <- index_startswith(fitsum, 'beta_a')
    beta_b <- index_startswith(fitsum, 'beta_b')
    names(beta_a) <- player_cols
    names(beta_b) <- player_cols
    list(
        alpha = fitsum['alpha'],
        beta_home = index_startswith(fitsum, 'beta_home'),
        beta_away = index_startswith(fitsum, 'beta_away'),
        sigma_player = index_startswith(fitsum, 'sigma_player'),
        sigma_deficit = fitsum['sigma_deficit'],
        beta_intercept = index_startswith(fitsum, 'beta_intercept'),
        beta_a = beta_a,
        beta_b = beta_b,
        player_value_a = index_startswith(fitsum, 'player_value_a'),
        player_value_b = index_startswith(fitsum, 'player_value_b'),
        player_value_raw_a = index_startswith(fitsum, 'player_value_raw_a'),
        player_value_raw_b = index_startswith(fitsum, 'player_value_raw_b'),
    )
}
predict_games_stan <- function(stan_res, home, away, player_mat) {
    random_deficits <- stan_res$alpha + stan_res$beta_home*(home%*%stan_res$player_value) - stan_res$beta_away*(away%*%stan_res$player_value)
    random_deficits <- stan_res$alpha + 
      stan_res$beta_home_a*(home%*%stan_res$player_value_a) +
      stan_res$beta_home_b*(home%*%stan_res$player_value_b) - 
      stan_res$beta_away_a*(away%*%stan_res$player_value_a) -
      stan_res$beta_away_b*(away%*%stan_res$player_value_b)
    player_scores_a <- stan_res$beta_intercept[1] + player_mat %*% stan_res$beta_a
    player_scores_b <- stan_res$beta_intercept[2] + player_mat %*% stan_res$beta_b
    scores_home_a <- apply(home, 1, function(x) sum(x*player_scores_a)/sum(x))
    scores_away_a <- apply(away, 1, function(x) sum(x*player_scores_a)/sum(x))
    scores_home_b <- apply(home, 1, function(x) sum(x*player_scores_b)/sum(x))
    scores_away_b <- apply(away, 1, function(x) sum(x*player_scores_b)/sum(x))
    expected_deficits <- stan_res$alpha + stan_res$beta_home[1]*scores_home_a + stan_res$beta_home[2]*scores_home_b - 
      stan_res$beta_away[1]*scores_away_a - stan_res$beta_away[2]*scores_away_b
    list(
         deficits=random_deficits,
         deficits_expc=expected_deficits,
         wins=random_deficits>0
     )
}
topn_players_stan <- function(stan_res, main_df, players_df, n=5, idcol="playerId", yearcol="year") {
  players_with_n_seasons <- main_df %>%
    group_by(.data[[idcol]]) %>%
    summarise(n_seasons = n_distinct(.data[[yearcol]])) %>%
    dplyr::filter(n_seasons >= n)
  players_df[players_df[[idcol]] %in% players_with_n_seasons[[idcol]], ]
}