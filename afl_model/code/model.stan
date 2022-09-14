data {
    int<lower=0> K; // num of predictors
    int<lower=0> N_players; // num of players
    int<lower=0> N_games; // num of games
    matrix[N_players, K] player_data; // mean in each stat for each player
    vector[N_players] home[N_games]; // 1 = player is home team for that game
    vector[N_players] away[N_games]; // 1 = player is home team for that game
    vector[N_games] deficit; // home-away for that game (this is what we predict)
}
parameters {
    real alpha;
    real<lower=0> beta_home;
    real<lower=0> beta_away;
    real<lower=0> sigma_deficit;
    real<lower=0> sigma_player; // sd of X
    real beta_intercept; // predict X for each player
    vector[K] beta; // predict X for each player
    vector[N_players] player_value_raw; // this is X
}
transformed parameters {
//    vector[N_players] player_value = append_row(player_value_raw, -sum(player_value_raw))/sd(append_row(player_value_raw, -sum(player_value_raw)));
//
    vector[N_players] player_value =(player_value_raw-mean(player_value_raw))/sd(player_value_raw);
}
model {
    beta ~ double_exponential(0, 3);
    beta_intercept ~ double_exponential(0,3);
    sigma_player ~ normal(0, 3);
    sigma_deficit ~ normal(0, 50);

    player_value_raw ~ logistic(beta_intercept + player_data*beta, sigma_player);

    for (n in 1:N_games) {
        target += normal_lpdf(deficit[n] | alpha + beta_home*(dot_product(home[n],player_value)/sum(home[n])) - beta_away*(dot_product(away[n],player_value)/sum(away[n])), sigma_deficit);
    }

}
