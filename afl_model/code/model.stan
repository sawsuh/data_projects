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
    vector[N_players] player_value_atk; // this is X
    vector[N_players] player_value_def; // this is X
}
model {
    beta_player[1] ~ double_exponential(0, 3);
    beta_player[2] ~ double_exponential(0, 3);
    beta_intercept[1] ~ double_exponential(0,3);
    beta_intercept[2] ~ double_exponential(0,3);
    sigma_deficit ~ normal(0, 50);

    player_value_atk~ logistic(beta_intercept[1] + player_data*beta_player[1], 1);
    player_value_def ~ logistic(beta_intercept[2] + player_data*beta_player[2], 1);
    sigma_residual ~ logistic(0, 10)
    player_residual ~ logistic(0, sigma_residual)

    for (n in 1:N_games) {
        target += normal_lpdf(deficit[n] | alpha + beta_home[1]*(dot_product(home[n],player_value_a)/sum(home[n])) + beta_home[2]*(dot_product(home[n],player_value_b)/sum(home[n])) - beta_away[1]*(dot_product(away[n],player_value_a)/sum(away[n])) - beta_away[2]*(dot_product(away[n],player_value_b)/sum(away[n])), sigma_deficit);
        target += normal_lpdf(deficit[n] | alpha + 
            beta_game[1]*(dot_product(beta_player[1], home_stats[n]-away_stats[n]) + dot_product(player_residual[1], home[n] - away[n]))/sum(home[n])+
            beta_game[2]*(dot_product(beta_player[2], home_stats[n]-away_stats[n]) + dot_product(player_residual[2], home[n] - away[n]))/sum(home[n])
          sigma_deficit);
    }

}
