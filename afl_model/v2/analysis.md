Results Analysis
================

First we will load the variables.

``` r
cat(res$alpha)
```

    ## 6.119124

``` r
cat(res$beta_home)
```

    ## 103.7534

``` r
cat(res$beta_away)
```

    ## 104.3989

``` r
cat(res$sigma_deficit)
```

    ## 32.01356

``` r
cat(res$beta_intercept)
```

    ## 0.08860122

We can see the most important statistics in order:

``` r
res$beta[order(res$beta, decreasing=T)]
```

    ##            Goal.Assists               Disposals   Contested.Possessions 
    ##               2.5982440               2.2307919               2.0528167 
    ##              Inside.50s         Marks.Inside.50                 Tackles 
    ##               1.8370303               1.6586528               1.5511784 
    ##                Rebounds          One.Percenters               Handballs 
    ##               1.2785337               1.2226626               1.1571124 
    ##                   Kicks                   Marks                Hit.Outs 
    ##               1.0865258               0.9136021               0.4473210 
    ##           Frees.Against                   Goals          Brownlow.Votes 
    ##               0.4117603               0.3497074               0.2005220 
    ##         Contested.Marks                 Bounces                   Frees 
    ##               0.1908005               0.1880500              -0.2344651 
    ##                Clangers                 Behinds              Clearances 
    ##              -1.5381203              -2.1669243              -3.6530080 
    ## Uncontested.Possessions 
    ##              -3.7388994

``` r
res$beta[order(abs(res$beta), decreasing=T)]
```

    ## Uncontested.Possessions              Clearances            Goal.Assists 
    ##              -3.7388994              -3.6530080               2.5982440 
    ##               Disposals                 Behinds   Contested.Possessions 
    ##               2.2307919              -2.1669243               2.0528167 
    ##              Inside.50s         Marks.Inside.50                 Tackles 
    ##               1.8370303               1.6586528               1.5511784 
    ##                Clangers                Rebounds          One.Percenters 
    ##              -1.5381203               1.2785337               1.2226626 
    ##               Handballs                   Kicks                   Marks 
    ##               1.1571124               1.0865258               0.9136021 
    ##                Hit.Outs           Frees.Against                   Goals 
    ##               0.4473210               0.4117603               0.3497074 
    ##                   Frees          Brownlow.Votes         Contested.Marks 
    ##              -0.2344651               0.2005220               0.1908005 
    ##                 Bounces 
    ##               0.1880500

Let us see who the best players are (with at least 5 seasons in the
data).

``` r
player_mean_data$score <- res$player_value_raw
players <- topn_players_stan(res, df, players, n=5) %>%
  merge(player_mean_data[,c('playerId', 'score')])
players %>%
  arrange(desc(score)) %>%
  head(20)
```

    ##      playerId          displayName height weight         dob          position
    ## 1  2008822999         Rioli, Cyril    177     80 14-Jul-1989           Forward
    ## 2  2003874001        Waite, Jarrad    194     96  4-Feb-1983           Forward
    ## 3  2009835778         Sloane, Rory    182     83 17-Mar-1990          Midfield
    ## 4  2001823842       Riewoldt, Nick    193     92 17-Oct-1982           Forward
    ## 5  2007724084         Hawkins, Tom    198    103 21-Jul-1988           Forward
    ## 6  2006801835    Pendlebury, Scott    191     90  7-Jan-1988          Midfield
    ## 7  2007822538       Riewoldt, Jack    193     92 31-Oct-1988           Forward
    ## 8  2010664604      Barlow, Michael    189     91 18-Dec-1987 Midfield, Forward
    ## 9  2001801982         Petrie, Drew    197    101 15-Oct-1982           Forward
    ## 10 2005836583          Shaw, Heath    183     86 27-Nov-1985          Defender
    ## 11 2014665253  Bontempelli, Marcus    193     93 24-Nov-1995          Midfield
    ## 12 2007746814         Jack, Kieren    178     81 28-Jun-1987 Midfield, Forward
    ## 13 2011663064         Breust, Luke    184     84 11-Nov-1990           Forward
    ## 14 2008681760 Dangerfield, Patrick    189     92  5-Apr-1990 Midfield, Forward
    ## 15 2005682511       Deledio, Brett    188     88 18-Apr-1987 Midfield, Forward
    ## 16 2016799932      Oliver, Clayton    187     85 22-Jul-1997          Midfield
    ## 17 2011688430        Darling, Jack    191     95 13-Jun-1992           Forward
    ## 18 2008849301        Taylor, Harry    195     94 12-Jun-1986          Defender
    ## 19 2011769877           Lynch, Tom    199     99 31-Oct-1992           Forward
    ## 20 2009822285          Rance, Alex    194     96  9-Oct-1989          Defender
    ##                   origin player_idx    score
    ## 1               St Marys        917 15.59011
    ## 2     Murray Bushrangers        927 13.78532
    ## 3         Eastern Ranges         32 13.38700
    ## 4              Southport       1024 13.37156
    ## 5      Melbourne Grammar        276 13.34782
    ## 6        Gippsland Power        136 12.69264
    ## 7        Tassie Mariners        513 12.40327
    ## 8        Werribee Tigers        899 12.37092
    ## 9  North Ballarat Rebels       1037 12.30793
    ## 10      Northern Knights        722 12.13765
    ## 11      Northern Knights        634 11.71066
    ## 12          Nsw-act Rams        857 11.62337
    ## 13                Temora        337 11.58418
    ## 14       Geelong Falcons        270 11.52895
    ## 15    Murray Bushrangers        824 11.42535
    ## 16    Murray Bushrangers        395 11.30026
    ## 17            West Perth        604 11.26090
    ## 18        East Fremantle        719 11.22326
    ## 19   Dandenong Stingrays        501 10.93252
    ## 20        Swan Districts        846 10.87023

We can also show the top 5 players by position:

``` r
players %>%
  group_by(position) %>%
  slice_max(order_by=score, n=5) %>%
  select(c(displayName, position, score)) %>%
  print(n=50)
```

    ## # A tibble: 41 × 3
    ## # Groups:   position [9]
    ##    displayName          position           score
    ##    <chr>                <chr>              <dbl>
    ##  1 Shaw, Heath          Defender           12.1 
    ##  2 Taylor, Harry        Defender           11.2 
    ##  3 Rance, Alex          Defender           10.9 
    ##  4 Simpson, Kade        Defender            9.58
    ##  5 Lever, Jake          Defender            9.47
    ##  6 Adcock, Jed          Defender, Forward   9.82
    ##  7 Hooker, Cale         Defender, Forward   7.55
    ##  8 Burgoyne, Shaun      Defender, Forward   6.06
    ##  9 Duryea, Taylor       Defender, Forward   6.05
    ## 10 Walker, Andrew       Defender, Forward   5.47
    ## 11 Bartel, Jimmy        Defender, Midfield 10.8 
    ## 12 Mayne, Chris         Defender, Midfield  9.09
    ## 13 Lewis, Jordan        Defender, Midfield  8.04
    ## 14 Goddard, Brendon     Defender, Midfield  6.76
    ## 15 Heppell, Dyson       Defender, Midfield  6.33
    ## 16 McEvoy, Ben          Defender, Ruck      5.10
    ## 17 Rioli, Cyril         Forward            15.6 
    ## 18 Waite, Jarrad        Forward            13.8 
    ## 19 Riewoldt, Nick       Forward            13.4 
    ## 20 Hawkins, Tom         Forward            13.3 
    ## 21 Riewoldt, Jack       Forward            12.4 
    ## 22 Tippett, Kurt        Forward, Ruck       8.25
    ## 23 Pedersen, Cameron    Forward, Ruck       7.97
    ## 24 Lobb, Rory           Forward, Ruck       7.84
    ## 25 Marshall, Rowan      Forward, Ruck       5.62
    ## 26 Ryder, Paddy         Forward, Ruck       5.49
    ## 27 Sloane, Rory         Midfield           13.4 
    ## 28 Pendlebury, Scott    Midfield           12.7 
    ## 29 Bontempelli, Marcus  Midfield           11.7 
    ## 30 Oliver, Clayton      Midfield           11.3 
    ## 31 Steele, Jack         Midfield            9.93
    ## 32 Barlow, Michael      Midfield, Forward  12.4 
    ## 33 Jack, Kieren         Midfield, Forward  11.6 
    ## 34 Dangerfield, Patrick Midfield, Forward  11.5 
    ## 35 Deledio, Brett       Midfield, Forward  11.4 
    ## 36 Westhoff, Justin     Midfield, Forward  10.3 
    ## 37 Mumford, Shane       Ruck                9.42
    ## 38 Naitanui, Nic        Ruck                8.93
    ## 39 Sandilands, Aaron    Ruck                6.11
    ## 40 Kreuzer, Matthew     Ruck                5.57
    ## 41 Gawn, Max            Ruck                4.80

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

    ## [1] 0.7238142

We can show the scores by origin:

``` r
players %>%
  group_by(origin) %>%
  summarise (avg_score = mean(score), n_players=n()) %>%
  arrange(desc(avg_score)) %>%
  dplyr::filter(n_players >= 5) %>%
  head(10) %>%
  print
```

    ## # A tibble: 10 × 3
    ##    origin           avg_score n_players
    ##    <chr>                <dbl>     <int>
    ##  1 Southport             5.44         8
    ##  2 Werribee Tigers       5.04         5
    ##  3 Bendigo Pioneers      4.75         9
    ##  4 Claremont             4.65        16
    ##  5 Peel Thunder          4.13         7
    ##  6 Swan Districts        4.05        13
    ##  7 Frankston             3.94         5
    ##  8 East Fremantle        3.80        20
    ##  9 Perth                 3.80         8
    ## 10 Subiaco               3.44         5
