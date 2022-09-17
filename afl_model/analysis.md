Results Analysis
================

First we will load the variables.

``` r
cat(res$alpha)
```

    ## 5.954247

``` r
cat(res$beta_home)
```

    ## 110.618 114.2652

``` r
cat(res$beta_away)
```

    ## 111.0613 114.5515

``` r
cat(res$sigma_deficit)
```

    ## 32.03463

``` r
cat(res$beta_intercept)
```

    ## -0.5882494 -1.276378

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

Let us see who the best players are (with at least 5 seasons in the
data).

``` r
player_mean_data$score_a <- res$player_value_raw_a
player_mean_data$score_b <- res$player_value_raw_b
players <- topn_players_stan(res, df, players, n=5) %>%
  merge(player_mean_data[,c('playerId', 'score_a', 'score_b')])
players %>%
  arrange(desc(score_a)) %>%
  head(20)
```

    ##      playerId          displayName height weight         dob          position
    ## 1  2008681760 Dangerfield, Patrick    189     92  5-Apr-1990 Midfield, Forward
    ## 2  2006801835    Pendlebury, Scott    191     90  7-Jan-1988          Midfield
    ## 3  2009835778         Sloane, Rory    182     83 17-Mar-1990          Midfield
    ## 4  2014665253  Bontempelli, Marcus    193     93 24-Nov-1995          Midfield
    ## 5  2005682511       Deledio, Brett    188     88 18-Apr-1987 Midfield, Forward
    ## 6  2003879516        Wells, Daniel    181     79  3-Feb-1985           Forward
    ## 7  2013773561         Macrae, Jack    191     85  3-Aug-1994          Midfield
    ## 8  2003833172           Swan, Dane    185     93 25-Feb-1984          Midfield
    ## 9  2007835833        Selwood, Joel    183     84 26-May-1988          Midfield
    ## 10 2010833473          Shuey, Luke    183     90  2-Jun-1990          Midfield
    ## 11 2002775293        Mitchell, Sam    180     84 12-Oct-1982          Midfield
    ## 12 2009787123        Naitanui, Nic    201    110  4-May-1990              Ruck
    ## 13 2003874470         Watson, Jobe    191     93  8-Feb-1985          Midfield
    ## 14 2008822999         Rioli, Cyril    177     80 14-Jul-1989           Forward
    ## 15 2002652211         Ablett, Gary    182     87 14-May-1984           Forward
    ## 16 1996722607        Harvey, Brent    177     75 14-May-1978           Forward
    ## 17 2007746814         Jack, Kieren    178     81 28-Jun-1987 Midfield, Forward
    ## 18 2005717264        Griffen, Ryan    192     88 27-Jul-1986 Midfield, Forward
    ## 19 2010704250            Fyfe, Nat    192     96 18-Sep-1991 Midfield, Forward
    ## 20 2015839510           Saad, Adam    178     78 23-Jul-1994          Defender
    ##                 origin player_idx  score_a     score_b
    ## 1      Geelong Falcons        270 18.55173 -3.76322603
    ## 2      Gippsland Power        136 16.14207 -5.23345198
    ## 3       Eastern Ranges         32 14.22573 -1.47931225
    ## 4     Northern Knights        634 13.72121 -1.23081442
    ## 5   Murray Bushrangers        824 13.50789  1.17175726
    ## 6         Peel Thunder        799 13.37041 -4.40384833
    ## 7    Oakleigh Chargers        655 13.30884 -5.58585562
    ## 8       Calder Cannons       1064 13.05707 -4.78931394
    ## 9              Bendigo        291 12.95963 -7.80468262
    ## 10   Oakleigh Chargers        626 12.86600 -7.51094359
    ## 11      Eastern Ranges       1034 12.68390 -7.09410084
    ## 12      Swan Districts        617 12.35347  2.59031171
    ## 13 Sandringham Dragons        982 12.14299 -5.90355988
    ## 14            St Marys        917 12.13726  6.69186897
    ## 15     Geelong Falcons        715 12.11234 -2.92658529
    ## 16    Northern Knights       1109 11.94429 -0.05559874
    ## 17        Nsw-act Rams        857 11.44656 -1.36084543
    ## 18      South Adelaide        912 11.37902 -4.12567277
    ## 19           Claremont        200 11.02236 -0.57266797
    ## 20              Coburg        101 10.80280  2.11870428

``` r
players %>%
  arrange(desc(score_b)) %>%
  head(20)
```

    ##      playerId      displayName height weight         dob      position
    ## 1  2007724084     Hawkins, Tom    198    103 21-Jul-1988       Forward
    ## 2  2005708162  Franklin, Lance    199    105 30-Jan-1987       Forward
    ## 3  2007822538   Riewoldt, Jack    193     92 31-Oct-1988       Forward
    ## 4  2006757023    Kennedy, Josh    196    102 25-Aug-1987       Forward
    ## 5  2003874001    Waite, Jarrad    194     96  4-Feb-1983       Forward
    ## 6  2003832712      Schulz, Jay    193     94 18-Apr-1985       Forward
    ## 7  2011769877       Lynch, Tom    199     99 31-Oct-1992       Forward
    ## 8  2008845101    Tippett, Kurt    202    105  8-May-1987 Forward, Ruck
    ## 9  2009877055   Walker, Taylor    193    100 25-Apr-1990       Forward
    ## 10 2005673207    Cloke, Travis    196    105  5-Mar-1987       Forward
    ## 11 2001823842   Riewoldt, Nick    193     92 17-Oct-1982       Forward
    ## 12 2011688430    Darling, Jack    191     95 13-Jun-1992       Forward
    ## 13 2008773839   Mumford, Shane    197    105  5-Jul-1986          Ruck
    ## 14 2001801982     Petrie, Drew    197    101 15-Oct-1982       Forward
    ## 15 2005836583      Shaw, Heath    183     86 27-Nov-1985      Defender
    ## 16 2011685707   Dixon, Charlie    200    105 23-Sep-1990       Forward
    ## 17 2000807726 Pavlich, Matthew    192     99 31-Dec-1981       Forward
    ## 18 2015723350     Hogan, Jesse    195    100 12-Feb-1995       Forward
    ## 19 2014777376     Membrey, Tim    190     89 26-May-1994       Forward
    ## 20 2017779237     McKay, Harry    204     99 24-Dec-1997       Forward
    ##                    origin player_idx    score_a   score_b
    ## 1       Melbourne Grammar        276  2.7807339 16.026463
    ## 2                   Perth        570  1.8840270 14.558043
    ## 3         Tassie Mariners        513  3.3037133 14.237206
    ## 4          East Fremantle        614 -1.5536996 14.213682
    ## 5      Murray Bushrangers        927  4.0746917 14.054518
    ## 6  Woodville-West Torrens       1118  0.9887431 13.068928
    ## 7     Dandenong Stingrays        501  3.2766510 12.656406
    ## 8               Southport       1028  1.9333059 11.313684
    ## 9       Broken Hill North         36  3.4961067 11.234499
    ## 10         Eastern Ranges       1041  4.4961409 11.099014
    ## 11              Southport       1024  8.2138481 11.080444
    ## 12             West Perth        604  3.6397652 10.970250
    ## 13                Geelong        321  2.9020427 10.484360
    ## 14  North Ballarat Rebels       1037  7.0557405 10.387684
    ## 15       Northern Knights        722  9.1769450  9.773229
    ## 16                Redland        455  2.3626756  9.445971
    ## 17 Woodville-West Torrens       1082  5.7139467  9.204666
    ## 18              Claremont        314  3.8824587  9.196571
    ## 19        Gippsland Power        551  4.0042094  8.996096
    ## 20        Gippsland Power         91 -4.5422487  8.893544

We can also show the top 5 players by position:

``` r
players %>%
  group_by(position) %>%
  slice_max(order_by=score_a, n=5) %>%
  select(c(displayName, position, score_a)) %>%
  print(n=50)
```

    ## # A tibble: 41 × 3
    ## # Groups:   position [9]
    ##    displayName          position           score_a
    ##    <chr>                <chr>                <dbl>
    ##  1 Saad, Adam           Defender             10.8 
    ##  2 Simpson, Kade        Defender             10.4 
    ##  3 Montagna, Leigh      Defender             10.0 
    ##  4 Boyd, Matthew        Defender              9.37
    ##  5 Murphy, Robert       Defender              9.37
    ##  6 Adcock, Jed          Defender, Forward     8.72
    ##  7 Johannisen, Jason    Defender, Forward     6.84
    ##  8 Walker, Andrew       Defender, Forward     6.60
    ##  9 Burgoyne, Shaun      Defender, Forward     6.16
    ## 10 Duryea, Taylor       Defender, Forward     4.71
    ## 11 Lewis, Jordan        Defender, Midfield   10.1 
    ## 12 Heppell, Dyson       Defender, Midfield    9.64
    ## 13 Bartel, Jimmy        Defender, Midfield    8.74
    ## 14 Goddard, Brendon     Defender, Midfield    8.02
    ## 15 Sinclair, Jack       Defender, Midfield    6.49
    ## 16 McEvoy, Ben          Defender, Ruck        5.04
    ## 17 Wells, Daniel        Forward              13.4 
    ## 18 Rioli, Cyril         Forward              12.1 
    ## 19 Ablett, Gary         Forward              12.1 
    ## 20 Harvey, Brent        Forward              11.9 
    ## 21 Petracca, Christian  Forward              10.4 
    ## 22 Ryder, Paddy         Forward, Ruck         7.11
    ## 23 Marshall, Rowan      Forward, Ruck         6.90
    ## 24 Pedersen, Cameron    Forward, Ruck         4.83
    ## 25 Lobb, Rory           Forward, Ruck         4.04
    ## 26 Wright, Peter        Forward, Ruck         2.52
    ## 27 Pendlebury, Scott    Midfield             16.1 
    ## 28 Sloane, Rory         Midfield             14.2 
    ## 29 Bontempelli, Marcus  Midfield             13.7 
    ## 30 Macrae, Jack         Midfield             13.3 
    ## 31 Swan, Dane           Midfield             13.1 
    ## 32 Dangerfield, Patrick Midfield, Forward    18.6 
    ## 33 Deledio, Brett       Midfield, Forward    13.5 
    ## 34 Jack, Kieren         Midfield, Forward    11.4 
    ## 35 Griffen, Ryan        Midfield, Forward    11.4 
    ## 36 Fyfe, Nat            Midfield, Forward    11.0 
    ## 37 Naitanui, Nic        Ruck                 12.4 
    ## 38 Kreuzer, Matthew     Ruck                  7.61
    ## 39 Gawn, Max            Ruck                  7.52
    ## 40 Martin, Stefan       Ruck                  7.40
    ## 41 Maric, Ivan          Ruck                  6.94

``` r
players %>%
  group_by(position) %>%
  slice_max(order_by=score_b, n=5) %>%
  select(c(displayName, position, score_b)) %>%
  print(n=50)
```

    ## # A tibble: 41 × 3
    ## # Groups:   position [9]
    ##    displayName       position           score_b
    ##    <chr>             <chr>                <dbl>
    ##  1 Shaw, Heath       Defender             9.77 
    ##  2 Merrett, Daniel   Defender             6.28 
    ##  3 Rance, Alex       Defender             6.10 
    ##  4 Taylor, Harry     Defender             6.03 
    ##  5 Thompson, Scott   Defender             5.72 
    ##  6 Hooker, Cale      Defender, Forward    7.31 
    ##  7 Casboult, Levi    Defender, Forward    7.03 
    ##  8 Stewart, James    Defender, Forward    3.66 
    ##  9 Walker, Andrew    Defender, Forward    1.75 
    ## 10 Fantasia, Orazio  Defender, Forward    1.68 
    ## 11 Mayne, Chris      Defender, Midfield   4.34 
    ## 12 Bartel, Jimmy     Defender, Midfield   2.63 
    ## 13 Blicavs, Mark     Defender, Midfield   1.80 
    ## 14 Houston, Dan      Defender, Midfield   0.542
    ## 15 Mills, Callum     Defender, Midfield  -0.421
    ## 16 McEvoy, Ben       Defender, Ruck       4.46 
    ## 17 Hawkins, Tom      Forward             16.0  
    ## 18 Franklin, Lance   Forward             14.6  
    ## 19 Riewoldt, Jack    Forward             14.2  
    ## 20 Kennedy, Josh     Forward             14.2  
    ## 21 Waite, Jarrad     Forward             14.1  
    ## 22 Tippett, Kurt     Forward, Ruck       11.3  
    ## 23 Lobb, Rory        Forward, Ruck        7.01 
    ## 24 Pedersen, Cameron Forward, Ruck        5.77 
    ## 25 McKernan, Shaun   Forward, Ruck        5.33 
    ## 26 Ryder, Paddy      Forward, Ruck        5.29 
    ## 27 de Boer, Matt     Midfield             1.81 
    ## 28 McIntosh, Kamdyn  Midfield             0.468
    ## 29 Mzungu, Tendai    Midfield             0.417
    ## 30 Menegola, Sam     Midfield             0.348
    ## 31 Ellis, Brandon    Midfield             0.330
    ## 32 Elliott, Jamie    Midfield, Forward    5.80 
    ## 33 Westhoff, Justin  Midfield, Forward    5.62 
    ## 34 Stringer, Jake    Midfield, Forward    2.88 
    ## 35 Walters, Michael  Midfield, Forward    1.42 
    ## 36 Barlow, Michael   Midfield, Forward    1.19 
    ## 37 Mumford, Shane    Ruck                10.5  
    ## 38 Sandilands, Aaron Ruck                 7.40 
    ## 39 Gawn, Max         Ruck                 6.06 
    ## 40 Nankervis, Toby   Ruck                 5.08 
    ## 41 Bellchambers, Tom Ruck                 4.25

We can now try and apply this to the games in our dataset.

``` r
ggplot(data.frame(Residual=stan_input$deficit-pred$deficits), aes(x=Residual)) +
  geom_histogram(aes(y=stat(density)), colour=1, fill="white", bins=10) +
  geom_density() +
  ggtitle("Residual Density Plot (Observed ELO)")
```

![](analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggplot(data.frame(Residual=stan_input$deficit-pred$deficits_expc), aes(x=Residual)) +
  geom_histogram(aes(y=stat(density)), colour=1, fill="white", bins=10) +
  geom_density() +
  ggtitle("Residual Density Plot (Expected ELO)")
```

![](analysis_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

We can now try and apply this to compute win prediction accuracy.

``` r
game_wins <- as.numeric(stan_input$deficit>0)
print( sum(game_wins==pred$wins)/length(game_wins) )
```

    ## [1] 0

We can show the scores by origin:

``` r
players %>%
  group_by(origin) %>%
  summarise (avg_score_a = mean(score_a), n_players=n()) %>%
  arrange(desc(avg_score_a)) %>%
  dplyr::filter(n_players >= 5) %>%
  head(10) %>%
  print
```

    ## # A tibble: 10 × 3
    ##    origin              avg_score_a n_players
    ##    <chr>                     <dbl>     <int>
    ##  1 Bendigo Pioneers           5.01         9
    ##  2 Subiaco                    4.49         5
    ##  3 Peel Thunder               4.36         7
    ##  4 Swan Districts             3.57        13
    ##  5 Northern Knights           3.37        25
    ##  6 Southport                  3.36         8
    ##  7 East Fremantle             3.19        20
    ##  8 Claremont                  3.11        16
    ##  9 Sandringham Dragons        2.89        29
    ## 10 Sturt                      2.84        10

``` r
players %>%
  group_by(origin) %>%
  summarise (avg_score_b = mean(score_b), n_players=n()) %>%
  arrange(desc(avg_score_b)) %>%
  dplyr::filter(n_players >= 5) %>%
  head(10) %>%
  print
```

    ## # A tibble: 10 × 3
    ##    origin           avg_score_b n_players
    ##    <chr>                  <dbl>     <int>
    ##  1 Southport              3.33          8
    ##  2 Frankston              2.55          5
    ##  3 Werribee Tigers        1.83          5
    ##  4 Claremont              0.995        16
    ##  5 Central District       0.971         9
    ##  6 East Fremantle         0.966        20
    ##  7 Perth                  0.904         8
    ##  8 South Fremantle        0.811         7
    ##  9 West Perth             0.763         8
    ## 10 Gippsland Power        0.750        25
