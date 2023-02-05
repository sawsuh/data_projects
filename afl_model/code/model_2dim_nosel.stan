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
    real<lower=0> beta_home[2];
    real<lower=0> beta_away[2];
    real<lower=0> sigma_deficit;
    real<lower=0> sigma_player[2]; // sd of X
    real beta_intercept[2]; // predict X for each player
    vector[K] beta_a; // predict X for each player
    vector[K] beta_b; // predict X for each player
    vector[N_players] player_value_raw_a; // this is X
    vector[N_players] player_value_raw_b; // this is X
}
transformed parameters {
//    vector[N_players] player_value = append_row(player_value_raw, -sum(player_value_raw))/sd(append_row(player_value_raw, -sum(player_value_raw)));
//
    vector[N_players] player_value_a =(player_value_raw_a-mean(player_value_raw_a))/sd(player_value_raw_a);
    vector[N_players] player_value_b =(player_value_raw_b-mean(player_value_raw_b))/sd(player_value_raw_b);
}
model {
    beta_a ~ double_exponential(0, 3);
    beta_b ~ double_exponential(0, 3);
    beta_intercept[1] ~ double_exponential(0,3);
    beta_intercept[2] ~ double_exponential(0,3);
    sigma_player[1] ~ normal(0, 0.5);
    sigma_player[2] ~ normal(0, 0.5);
    sigma_deficit ~ normal(0, 50);

    player_value_raw_a ~ logistic(beta_intercept[1] + player_data*beta_a, sigma_player[1]);
    player_value_raw_b ~ logistic(beta_intercept[2] + player_data*beta_b, sigma_player[2]);

    for (n in 1:N_games) {
        target += normal_lpdf(deficit[n] | alpha + beta_home[1]*(dot_product(home[n],player_value_a)/sum(home[n])) + beta_home[2]*(dot_product(home[n],player_value_b)/sum(home[n])) - beta_away[1]*(dot_product(away[n],player_value_a)/sum(away[n])) - beta_away[2]*(dot_product(away[n],player_value_b)/sum(away[n])), sigma_deficit);
    }

}
