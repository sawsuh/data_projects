#### Train model and analyse results

train_model <- function(filepath, input) {
  stan(
    file = filepath,
    cores = parallel::detectCores(),
    data = input
  )
}
get_stan_res <- function(mf, player_cols) {
    fitsum <- summary(mf)$summary[, 'mean']
    beta <- fitsum[startsWith(names(fitsum), 'beta[')]
    names(beta) <- player_cols
    list(
        alpha = fitsum['alpha'],
        beta_home = fitsum['beta_home'],
        beta_away = fitsum['beta_away'],
        sigma_deficit = fitsum['sigma_deficit'],
        beta_intercept = fitsum['beta_intercept'],
        beta = beta,
        player_value = fitsum[startsWith(names(fitsum), 'player_value[')],
        player_value_raw = fitsum[startsWith(names(fitsum), 'player_value_raw[')]
    )
}
predict_games_stan <- function(stan_res, home, away, player_mat) {
    random_deficits <- stan_res$alpha + stan_res$beta_home*(home%*%stan_res$player_value) - stan_res$beta_away*(away%*%stan_res$player_value)
    player_scores <- stan_res$beta_intercept + player_mat %*% stan_res$beta
    scores_home <- apply(home, 1, function(x) sum(x*player_scores)/sum(x))
    scores_away <- apply(away, 1, function(x) sum(x*player_scores)/sum(x))
    expected_deficits <- stan_res$alpha + stan_res$beta_home*scores_home  - stan_res$beta_away*scores_away
    list(
         deficits=random_deficits,
         deficits_expc=expected_deficits,
         wins=random_deficits>0
     )
}
