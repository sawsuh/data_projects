Results Analysis
================

First we will load the variables.

``` r
vars_1dim <- c( 'alpha', 'beta_home', 'beta_away', 'sigma_deficit', 'beta_intercept')
walk(
  vars_1dim,
  ~  cat(., ' = ', res[[.]], '\n')
)
```

    ## alpha  =  5.954247 
    ## beta_home  =  110.618 114.2652 
    ## beta_away  =  111.0613 114.5515 
    ## sigma_deficit  =  32.03463 
    ## beta_intercept  =  -0.5882494 -1.276378

We can see the most important statistics in order:

``` r
res$beta_a[order(res$beta_a, decreasing=T)]
```

    ##              Inside.50s            Goal.Assists          One.Percenters 
    ##             3.006903761             2.144383198             1.707916113 
    ##   Contested.Possessions               Disposals                   Kicks 
    ##             1.606688595             1.325864174             0.935369553 
    ##         Marks.Inside.50                Rebounds                   Marks 
    ##             0.879801849             0.791106430             0.770143251 
    ##                 Bounces                   Frees                Hit.Outs 
    ##             0.764960129             0.655258399             0.647453958 
    ##               Handballs          Brownlow.Votes         Contested.Marks 
    ##             0.633321421             0.039878503             0.002085108 
    ##                 Tackles           Frees.Against                   Goals 
    ##            -0.314194785            -0.471742775            -0.751673947 
    ##              Clearances                 Behinds                Clangers 
    ##            -0.798307646            -1.320211672            -1.897423160 
    ## Uncontested.Possessions 
    ##            -2.246564666

``` r
res$beta_a[order(abs(res$beta_a), decreasing=T)]
```

    ##              Inside.50s Uncontested.Possessions            Goal.Assists 
    ##             3.006903761            -2.246564666             2.144383198 
    ##                Clangers          One.Percenters   Contested.Possessions 
    ##            -1.897423160             1.707916113             1.606688595 
    ##               Disposals                 Behinds                   Kicks 
    ##             1.325864174            -1.320211672             0.935369553 
    ##         Marks.Inside.50              Clearances                Rebounds 
    ##             0.879801849            -0.798307646             0.791106430 
    ##                   Marks                 Bounces                   Goals 
    ##             0.770143251             0.764960129            -0.751673947 
    ##                   Frees                Hit.Outs               Handballs 
    ##             0.655258399             0.647453958             0.633321421 
    ##           Frees.Against                 Tackles          Brownlow.Votes 
    ##            -0.471742775            -0.314194785             0.039878503 
    ##         Contested.Marks 
    ##             0.002085108

``` r
res$beta_b[order(res$beta_b, decreasing=T)]
```

    ##         Marks.Inside.50           Frees.Against               Disposals 
    ##             1.511186459             1.436342446             1.292199271 
    ##                   Goals                Hit.Outs                 Tackles 
    ##             1.265920501             1.264595674             1.232050251 
    ##            Goal.Assists   Contested.Possessions                Rebounds 
    ##             1.048765219             1.022729233             0.872295237 
    ##                   Marks               Handballs                   Kicks 
    ##             0.809442316             0.738515726             0.704811859 
    ##          One.Percenters         Contested.Marks                 Bounces 
    ##             0.438369278             0.374799740             0.290819139 
    ##          Brownlow.Votes              Inside.50s                Clangers 
    ##             0.117953679            -0.001720181            -0.327844050 
    ##                   Frees                 Behinds Uncontested.Possessions 
    ##            -0.884631830            -1.210922528            -2.562448379 
    ##              Clearances 
    ##            -3.874055845

``` r
res$beta_b[order(abs(res$beta_b), decreasing=T)]
```

    ##              Clearances Uncontested.Possessions         Marks.Inside.50 
    ##            -3.874055845            -2.562448379             1.511186459 
    ##           Frees.Against               Disposals                   Goals 
    ##             1.436342446             1.292199271             1.265920501 
    ##                Hit.Outs                 Tackles                 Behinds 
    ##             1.264595674             1.232050251            -1.210922528 
    ##            Goal.Assists   Contested.Possessions                   Frees 
    ##             1.048765219             1.022729233            -0.884631830 
    ##                Rebounds                   Marks               Handballs 
    ##             0.872295237             0.809442316             0.738515726 
    ##                   Kicks          One.Percenters         Contested.Marks 
    ##             0.704811859             0.438369278             0.374799740 
    ##                Clangers                 Bounces          Brownlow.Votes 
    ##            -0.327844050             0.290819139             0.117953679 
    ##              Inside.50s 
    ##            -0.001720181

Let us see who the best players are (with at least 100 games in the
data).

``` r
player_mean_data$score_a <- res$player_value_raw_a
player_mean_data$score_b <- res$player_value_raw_b
std_score_a <- scale(player_mean_data$score_a)
std_score_b <- scale(player_mean_data$score_b)
player_mean_data$score <- mean(c(res$beta_home[1], res$beta_away[1]))*std_score_a + mean(c(res$beta_home[2], res$beta_away[2]))*std_score_b
players <- topn_players_stan(res, df, players, n=100) %>%
  merge(player_mean_data[,c('playerId', 'score_a', 'score_b', 'score')])
scorecols <-  c('score', 'score_a', 'score_b')
walk(
  scorecols,
  function(sc) {
    cat('\n ', rep('=', 30), '\n score: ', sc, '\n')
    players %>%
      arrange(desc(.data[[sc]])) %>%
      head(20) %>%
      select(all_of(c('displayName', 'position', sc))) %>%
      print
  }
)
```

    ## 
    ##   = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    ##  score:  score 
    ##          displayName position    score
    ## 1       Hawkins, Tom  Forward 536.6941
    ## 2     Riewoldt, Nick  Forward 515.6991
    ## 3      Waite, Jarrad  Forward 510.2666
    ## 4        Shaw, Heath Defender 500.3607
    ## 5     Riewoldt, Jack  Forward 499.2352
    ## 6    Franklin, Lance  Forward 478.5018
    ## 7       Rioli, Cyril  Forward 478.4924
    ## 8       Petrie, Drew  Forward 473.0275
    ## 9         Lynch, Tom  Forward 456.0143
    ## 10     Cloke, Travis  Forward 439.2326
    ## 11    Walker, Taylor  Forward 422.1852
    ## 12     Darling, Jack  Forward 418.0281
    ## 13     Kennedy, Josh  Forward 398.0374
    ## 14       Rance, Alex Defender 393.9597
    ## 15    Mumford, Shane     Ruck 389.6425
    ## 16 Sandilands, Aaron     Ruck 380.2290
    ## 17     McDonald, Tom  Forward 379.6792
    ## 18      Membrey, Tim  Forward 372.2973
    ## 19     Naitanui, Nic     Ruck 372.2811
    ## 20     Taylor, Harry Defender 369.2222
    ## 
    ##   = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    ##  score:  score_a 
    ##             displayName          position  score_a
    ## 1  Dangerfield, Patrick Midfield, Forward 18.55173
    ## 2     Pendlebury, Scott          Midfield 16.14207
    ## 3          Sloane, Rory          Midfield 14.22573
    ## 4   Bontempelli, Marcus          Midfield 13.72121
    ## 5        Deledio, Brett Midfield, Forward 13.50789
    ## 6          Macrae, Jack          Midfield 13.30884
    ## 7         Selwood, Joel          Midfield 12.95963
    ## 8           Shuey, Luke          Midfield 12.86600
    ## 9         Mitchell, Sam          Midfield 12.68390
    ## 10        Naitanui, Nic              Ruck 12.35347
    ## 11         Rioli, Cyril           Forward 12.13726
    ## 12         Ablett, Gary           Forward 12.11234
    ## 13        Harvey, Brent           Forward 11.94429
    ## 14         Jack, Kieren Midfield, Forward 11.44656
    ## 15        Griffen, Ryan Midfield, Forward 11.37902
    ## 16            Fyfe, Nat Midfield, Forward 11.02236
    ## 17           Saad, Adam          Defender 10.80280
    ## 18  Petracca, Christian           Forward 10.42399
    ## 19        Simpson, Kade          Defender 10.42340
    ## 20      Dal Santo, Nick          Midfield 10.35257
    ## 
    ##   = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    ##  score:  score_b 
    ##         displayName position   score_b
    ## 1      Hawkins, Tom  Forward 16.026463
    ## 2   Franklin, Lance  Forward 14.558043
    ## 3    Riewoldt, Jack  Forward 14.237206
    ## 4     Kennedy, Josh  Forward 14.213682
    ## 5     Waite, Jarrad  Forward 14.054518
    ## 6        Lynch, Tom  Forward 12.656406
    ## 7    Walker, Taylor  Forward 11.234499
    ## 8     Cloke, Travis  Forward 11.099014
    ## 9    Riewoldt, Nick  Forward 11.080444
    ## 10    Darling, Jack  Forward 10.970250
    ## 11   Mumford, Shane     Ruck 10.484360
    ## 12     Petrie, Drew  Forward 10.387684
    ## 13      Shaw, Heath Defender  9.773229
    ## 14   Dixon, Charlie  Forward  9.445971
    ## 15     Membrey, Tim  Forward  8.996096
    ## 16  Cameron, Jeremy  Forward  8.831939
    ## 17 Roughead, Jarryd  Forward  8.593775
    ## 18       Brown, Ben  Forward  8.590433
    ## 19      Bruce, Josh  Forward  8.235071
    ## 20    Jenkins, Josh  Forward  8.012107

We can also show the top 5 players by position:

``` r
walk(
  scorecols,
  function(scorecol) {
    cat('\n ', rep('=', 30), '\n score: ', scorecol, '\n')
    players %>%
      group_by(position) %>%
      slice_max(order_by=.data[[scorecol]], n=5) %>%
      group_walk(~ {
        cat(paste0('\n', 'score: ', scorecol, ', position: ', .y, '\n'))
        print(.x[, c('displayName', scorecol)])
        cat('\n')
      })
  }
)
```

    ## 
    ##   = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    ##  score:  score 
    ## 
    ## score: score, position: Defender
    ## # A tibble: 5 × 2
    ##   displayName      score[,1]
    ##   <chr>                <dbl>
    ## 1 Shaw, Heath           500.
    ## 2 Rance, Alex           394.
    ## 3 Taylor, Harry         369.
    ## 4 McGovern, Jeremy      356.
    ## 5 Saad, Adam            327.
    ## 
    ## 
    ## score: score, position: Defender, Forward
    ## # A tibble: 5 × 2
    ##   displayName       score[,1]
    ##   <chr>                 <dbl>
    ## 1 Hooker, Cale           288.
    ## 2 Duryea, Taylor         170.
    ## 3 Casboult, Levi         168.
    ## 4 Johannisen, Jason      154.
    ## 5 Burgoyne, Shaun        141.
    ## 
    ## 
    ## score: score, position: Defender, Midfield
    ## # A tibble: 5 × 2
    ##   displayName      score[,1]
    ##   <chr>                <dbl>
    ## 1 Bartel, Jimmy         298.
    ## 2 Mayne, Chris          228.
    ## 3 Goddard, Brendon      188.
    ## 4 Lewis, Jordan         183.
    ## 5 Blicavs, Mark         157.
    ## 
    ## 
    ## score: score, position: Defender, Ruck
    ## # A tibble: 1 × 2
    ##   displayName score[,1]
    ##   <chr>           <dbl>
    ## 1 McEvoy, Ben      271.
    ## 
    ## 
    ## score: score, position: Forward
    ## # A tibble: 5 × 2
    ##   displayName     score[,1]
    ##   <chr>               <dbl>
    ## 1 Hawkins, Tom         537.
    ## 2 Riewoldt, Nick       516.
    ## 3 Waite, Jarrad        510.
    ## 4 Riewoldt, Jack       499.
    ## 5 Franklin, Lance      479.
    ## 
    ## 
    ## score: score, position: Forward, Ruck
    ## # A tibble: 3 × 2
    ##   displayName      score[,1]
    ##   <chr>                <dbl>
    ## 1 Ryder, Paddy          337.
    ## 2 Lobb, Rory            319.
    ## 3 Ceglar, Jonathon      174.
    ## 
    ## 
    ## score: score, position: Midfield
    ## # A tibble: 5 × 2
    ##   displayName         score[,1]
    ##   <chr>                   <dbl>
    ## 1 Sloane, Rory             301.
    ## 2 Bontempelli, Marcus      297.
    ## 3 Pendlebury, Scott        240.
    ## 4 Zorko, Dayne             220.
    ## 5 Duncan, Mitch            219.
    ## 
    ## 
    ## score: score, position: Midfield, Forward
    ## # A tibble: 5 × 2
    ##   displayName          score[,1]
    ##   <chr>                    <dbl>
    ## 1 Deledio, Brett            358.
    ## 2 Westhoff, Justin          356.
    ## 3 Dangerfield, Patrick      329.
    ## 4 Elliott, Jamie            264.
    ## 5 Fyfe, Nat                 259.
    ## 
    ## 
    ## score: score, position: Ruck
    ## # A tibble: 5 × 2
    ##   displayName       score[,1]
    ##   <chr>                 <dbl>
    ## 1 Mumford, Shane         390.
    ## 2 Sandilands, Aaron      380.
    ## 3 Naitanui, Nic          372.
    ## 4 Gawn, Max              366.
    ## 5 Goldstein, Todd        265.
    ## 
    ## 
    ##   = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    ##  score:  score_a 
    ## 
    ## score: score_a, position: Defender
    ## # A tibble: 5 × 2
    ##   displayName     score_a
    ##   <chr>             <dbl>
    ## 1 Saad, Adam        10.8 
    ## 2 Simpson, Kade     10.4 
    ## 3 Montagna, Leigh   10.0 
    ## 4 Boyd, Matthew      9.37
    ## 5 Murphy, Robert     9.37
    ## 
    ## 
    ## score: score_a, position: Defender, Forward
    ## # A tibble: 5 × 2
    ##   displayName       score_a
    ##   <chr>               <dbl>
    ## 1 Johannisen, Jason    6.84
    ## 2 Burgoyne, Shaun      6.16
    ## 3 Duryea, Taylor       4.71
    ## 4 Hall, Aaron          2.61
    ## 5 Hooker, Cale         2.14
    ## 
    ## 
    ## score: score_a, position: Defender, Midfield
    ## # A tibble: 5 × 2
    ##   displayName      score_a
    ##   <chr>              <dbl>
    ## 1 Lewis, Jordan      10.1 
    ## 2 Heppell, Dyson      9.64
    ## 3 Bartel, Jimmy       8.74
    ## 4 Goddard, Brendon    8.02
    ## 5 Sinclair, Jack      6.49
    ## 
    ## 
    ## score: score_a, position: Defender, Ruck
    ## # A tibble: 1 × 2
    ##   displayName score_a
    ##   <chr>         <dbl>
    ## 1 McEvoy, Ben    5.04
    ## 
    ## 
    ## score: score_a, position: Forward
    ## # A tibble: 5 × 2
    ##   displayName         score_a
    ##   <chr>                 <dbl>
    ## 1 Rioli, Cyril          12.1 
    ## 2 Ablett, Gary          12.1 
    ## 3 Harvey, Brent         11.9 
    ## 4 Petracca, Christian   10.4 
    ## 5 Gray, Robbie           9.66
    ## 
    ## 
    ## score: score_a, position: Forward, Ruck
    ## # A tibble: 3 × 2
    ##   displayName      score_a
    ##   <chr>              <dbl>
    ## 1 Ryder, Paddy        7.11
    ## 2 Lobb, Rory          4.04
    ## 3 Ceglar, Jonathon    2.29
    ## 
    ## 
    ## score: score_a, position: Midfield
    ## # A tibble: 5 × 2
    ##   displayName         score_a
    ##   <chr>                 <dbl>
    ## 1 Pendlebury, Scott      16.1
    ## 2 Sloane, Rory           14.2
    ## 3 Bontempelli, Marcus    13.7
    ## 4 Macrae, Jack           13.3
    ## 5 Selwood, Joel          13.0
    ## 
    ## 
    ## score: score_a, position: Midfield, Forward
    ## # A tibble: 5 × 2
    ##   displayName          score_a
    ##   <chr>                  <dbl>
    ## 1 Dangerfield, Patrick    18.6
    ## 2 Deledio, Brett          13.5
    ## 3 Jack, Kieren            11.4
    ## 4 Griffen, Ryan           11.4
    ## 5 Fyfe, Nat               11.0
    ## 
    ## 
    ## score: score_a, position: Ruck
    ## # A tibble: 5 × 2
    ##   displayName      score_a
    ##   <chr>              <dbl>
    ## 1 Naitanui, Nic      12.4 
    ## 2 Kreuzer, Matthew    7.61
    ## 3 Gawn, Max           7.52
    ## 4 Martin, Stefan      7.40
    ## 5 Goldstein, Todd     6.79
    ## 
    ## 
    ##   = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    ##  score:  score_b 
    ## 
    ## score: score_b, position: Defender
    ## # A tibble: 5 × 2
    ##   displayName       score_b
    ##   <chr>               <dbl>
    ## 1 Shaw, Heath          9.77
    ## 2 Rance, Alex          6.10
    ## 3 Taylor, Harry        6.03
    ## 4 Thompson, Scott      5.72
    ## 5 Henderson, Lachie    5.63
    ## 
    ## 
    ## score: score_b, position: Defender, Forward
    ## # A tibble: 5 × 2
    ##   displayName    score_b
    ##   <chr>            <dbl>
    ## 1 Hooker, Cale    7.31  
    ## 2 Casboult, Levi  7.03  
    ## 3 Duryea, Taylor  0.961 
    ## 4 Impey, Jarman  -0.0726
    ## 5 Turner, Kayne  -0.245 
    ## 
    ## 
    ## score: score_b, position: Defender, Midfield
    ## # A tibble: 5 × 2
    ##   displayName   score_b
    ##   <chr>           <dbl>
    ## 1 Mayne, Chris    4.34 
    ## 2 Bartel, Jimmy   2.63 
    ## 3 Blicavs, Mark   1.80 
    ## 4 Houston, Dan    0.542
    ## 5 Mills, Callum  -0.421
    ## 
    ## 
    ## score: score_b, position: Defender, Ruck
    ## # A tibble: 1 × 2
    ##   displayName score_b
    ##   <chr>         <dbl>
    ## 1 McEvoy, Ben    4.46
    ## 
    ## 
    ## score: score_b, position: Forward
    ## # A tibble: 5 × 2
    ##   displayName     score_b
    ##   <chr>             <dbl>
    ## 1 Hawkins, Tom       16.0
    ## 2 Franklin, Lance    14.6
    ## 3 Riewoldt, Jack     14.2
    ## 4 Kennedy, Josh      14.2
    ## 5 Waite, Jarrad      14.1
    ## 
    ## 
    ## score: score_b, position: Forward, Ruck
    ## # A tibble: 3 × 2
    ##   displayName      score_b
    ##   <chr>              <dbl>
    ## 1 Lobb, Rory          7.01
    ## 2 Ryder, Paddy        5.29
    ## 3 Ceglar, Jonathon    2.95
    ## 
    ## 
    ## score: score_b, position: Midfield
    ## # A tibble: 5 × 2
    ##   displayName      score_b
    ##   <chr>              <dbl>
    ## 1 de Boer, Matt      1.81 
    ## 2 McIntosh, Kamdyn   0.468
    ## 3 Menegola, Sam      0.348
    ## 4 Ellis, Brandon     0.330
    ## 5 Duncan, Mitch      0.265
    ## 
    ## 
    ## score: score_b, position: Midfield, Forward
    ## # A tibble: 5 × 2
    ##   displayName      score_b
    ##   <chr>              <dbl>
    ## 1 Elliott, Jamie      5.80
    ## 2 Westhoff, Justin    5.62
    ## 3 Stringer, Jake      2.88
    ## 4 Walters, Michael    1.42
    ## 5 Barlow, Michael     1.19
    ## 
    ## 
    ## score: score_b, position: Ruck
    ## # A tibble: 5 × 2
    ##   displayName       score_b
    ##   <chr>               <dbl>
    ## 1 Mumford, Shane      10.5 
    ## 2 Sandilands, Aaron    7.40
    ## 3 Gawn, Max            6.06
    ## 4 Bellchambers, Tom    4.25
    ## 5 Sinclair, Callum     4.13

We can now try and apply this to the games in our dataset.

``` r
walk2(
  c('deficits', 'deficits_expc'),
  c("Residual Density Plot (Expected ELO)","Residual Density Plot (Observed ELO)"),
  ~ {
    (ggplot(data.frame(Residual=stan_input$deficit-pred[[.x]]), aes(x=Residual)) +
      geom_histogram(aes(y=stat(density)), colour=1, fill="white", bins=10) +
      geom_density() +
      ggtitle(.y)) %>%
      print
  }
)
```

![](analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->![](analysis_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

We can now try and apply this to compute win prediction accuracy.

``` r
game_wins <- as.numeric(stan_input$deficit>0)
print( sum(game_wins==pred$wins)/length(game_wins) )
```

    ## [1] 0.7257905

We can show the scores by origin:

``` r
walk(
  scorecols,
  function(sc) {
    cat('\n score: ', sc, '\n')
    players %>%
      group_by(origin) %>%
      summarise (avg_score = mean(.data[[sc]]), n_players=n()) %>%
      arrange(desc(avg_score)) %>%
      dplyr::filter(n_players >= 5) %>%
      head(10) %>%
      print
  }
)
```

    ## 
    ##  score:  score 
    ## # A tibble: 10 × 3
    ##    origin             avg_score n_players
    ##    <chr>                  <dbl>     <int>
    ##  1 Swan Districts          212.         8
    ##  2 Gippsland Power         211.        11
    ##  3 West Perth              188.         5
    ##  4 Eastern Ranges          184.        12
    ##  5 East Fremantle          178.        13
    ##  6 Murray Bushrangers      166.        21
    ##  7 Northern Knights        157.        15
    ##  8 Claremont               152.         7
    ##  9 Werribee Tigers         145.         5
    ## 10 Bendigo Pioneers        132.         7
    ## 
    ##  score:  score_a 
    ## # A tibble: 10 × 3
    ##    origin              avg_score n_players
    ##    <chr>                   <dbl>     <int>
    ##  1 Eastern Ranges           5.93        12
    ##  2 Gippsland Power          5.87        11
    ##  3 Bendigo Pioneers         5.74         7
    ##  4 Swan Districts           5.33         8
    ##  5 Northern Knights         5.15        15
    ##  6 Oakleigh Chargers        4.96        23
    ##  7 Claremont                4.90         7
    ##  8 West Perth               4.75         5
    ##  9 East Fremantle           4.73        13
    ## 10 Sandringham Dragons      4.67        16
    ## 
    ##  score:  score_b 
    ## # A tibble: 10 × 3
    ##    origin             avg_score n_players
    ##    <chr>                  <dbl>     <int>
    ##  1 Swan Districts         2.02          8
    ##  2 Werribee Tigers        1.83          5
    ##  3 Gippsland Power        1.59         11
    ##  4 West Perth             1.58          5
    ##  5 East Fremantle         1.24         13
    ##  6 Murray Bushrangers     1.14         21
    ##  7 Eastern Ranges         0.527        12
    ##  8 Claremont              0.158         7
    ##  9 Northern Knights       0.151        15
    ## 10 Port Adelaide         -0.186         7
