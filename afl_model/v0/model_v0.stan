data {
    int<lower=0> K; // num of predictors
    int<lower=0> N_data; // num of rows
    int<lower=0> N_players; // num of players
    int<lower=0> N_games; // num of games
    int<lower=1, upper=N_players> player[N_data]; // which player each row belongs to
    //matrix [N_players, K] player_data;
    vector[K] player_data[N_players];
    vector[N_players] home[N_games]; // 1 = player is home team for that game
    vector[N_players] away[N_games]; // 1 = player is home team for that game
    //vector[N_games] away[N_players]; // 1 = player is home team for that game
    vector[N_games] deficit;
}
parameters {
    vector[K] beta; // predict X for each player
    real beta_intercept; // predict X for each player
    vector[N_players-1] player_value_raw; // this is X, constrain to sum to 0
    vector<lower=0>[N_players] player_sigma; // sd of X
    real alpha;
    real beta_home;
    real beta_away;
    real<lower=0> sigma_deficit;
}
transformed parameters {
    vector[N_players] player_value = append_row(player_value_raw, -sum(player_value_raw));
}
model {
    for (n in 1:N_players) {
        target += normal_lpdf(player_value[n] | beta_intercept + dot_product(player_data[n],beta), player_sigma[n]) ;
    }
    for (n in 1:N_games) {
        target += normal_lpdf(deficit[n] | alpha + beta_home*(dot_product(home[n],player_value)/sum(home[n])) + beta_away*(dot_product(away[n],player_value)/sum(away[n])), sigma_deficit);
        //target += normal_lpdf(deficit[n] | alpha + beta_home + beta_away*(dot_product(away[n],player_value)), sigma_deficit);
    }
}
