data {
    int<lower=0> K; // num of predictors
    int<lower=0> N_data; // num of rows
    int<lower=0> N_players; // num of players
    int<lower=0> N_games; // num of games
    int<lower=1, upper=N_players> player[N_data]; // which player each row belongs to
    vector[K] player_data[N_players];
    vector[N_players] home[N_games]; // 1 = player is home team for that game
    vector[N_players] away[N_games]; // 1 = player is home team for that game
    vector[N_games] deficit;
}
parameters {
    real beta_intercept; // predict X for each player
    vector[K] beta; // predict X for each player
    real<lower=0> sigma_player; // sd of X
    real<lower=0> sigma_deficit;
    real alpha;
    real<lower=0> beta_home;
    real<lower=0> beta_away;
    vector[N_players-1] player_value_raw; // this is X, constrain to sum to 0
}
transformed parameters {
    vector[N_players] player_value = append_row(player_value_raw, -sum(player_value_raw))/sd(append_row(player_value_raw, -sum(player_value_raw)));
}
model {
    beta ~ double_exponential(0, 3);
    beta_intercept ~ normal(0,3);
    sigma_player ~ gamma(1,1);
    sigma_deficit ~ gamma(1,1);
    for (n in 1:N_players) {
        target += normal_lpdf(player_value[n] | beta_intercept + dot_product(player_data[n],beta), sigma_player) ;
    }
    for (n in 1:N_games) {
        target += normal_lpdf(deficit[n] | alpha + beta_home*(dot_product(home[n],player_value)/sum(home[n])) - beta_away*(dot_product(away[n],player_value)/sum(away[n])), sigma_deficit);
    }
}
