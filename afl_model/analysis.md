Results Analysis
================

First we will load the variables.

``` r
cat(res$alpha)
```

    ## 12.21783

``` r
cat(res$beta_home)
```

    ## 107.3925

``` r
cat(res$beta_away)
```

    ## 132.1639

``` r
cat(res$sigma_deficit)
```

    ## 32.71063

``` r
cat(res$beta_intercept)
```

    ## 0.0003837441

We can see the most important statistics in order:

``` r
res$beta[order(res$beta, decreasing=T)]
```

    ## Uncontested.Possessions          Brownlow.Votes   Contested.Possessions 
    ##              0.47471299              0.37053928              0.36974425 
    ##           Frees.Against              Inside.50s                 Tackles 
    ##              0.36771737              0.24129088              0.14852372 
    ##          One.Percenters                   Marks                 Bounces 
    ##              0.12374787              0.07315163              0.06940545 
    ##                Hit.Outs                   Goals            Goal.Assists 
    ##              0.06748653              0.05229705              0.05074194 
    ##               Disposals                Rebounds                 Behinds 
    ##              0.02265914             -0.01160775             -0.03223412 
    ##         Contested.Marks         Marks.Inside.50                   Frees 
    ##             -0.03848329             -0.08878032             -0.12494997 
    ##              Clearances                   Kicks               Handballs 
    ##             -0.16825430             -0.28216669             -0.38699743 
    ##                Clangers 
    ##             -0.59904225

``` r
res$beta[order(abs(res$beta), decreasing=T)]
```

    ##                Clangers Uncontested.Possessions               Handballs 
    ##             -0.59904225              0.47471299             -0.38699743 
    ##          Brownlow.Votes   Contested.Possessions           Frees.Against 
    ##              0.37053928              0.36974425              0.36771737 
    ##                   Kicks              Inside.50s              Clearances 
    ##             -0.28216669              0.24129088             -0.16825430 
    ##                 Tackles                   Frees          One.Percenters 
    ##              0.14852372             -0.12494997              0.12374787 
    ##         Marks.Inside.50                   Marks                 Bounces 
    ##             -0.08878032              0.07315163              0.06940545 
    ##                Hit.Outs                   Goals            Goal.Assists 
    ##              0.06748653              0.05229705              0.05074194 
    ##         Contested.Marks                 Behinds               Disposals 
    ##             -0.03848329             -0.03223412              0.02265914 
    ##                Rebounds 
    ##             -0.01160775

Let us see who the best players are.

``` r
player_mean_data$score <- res$player_value
players <- merge(players, player_mean_data[,c('playerId', 'score')])
players %>%
  arrange(desc(score)) %>%
  head(20)
```

    ##      playerId          displayName height weight         dob          position
    ## 1  2002775293        Mitchell, Sam    180     84 12-Oct-1982          Midfield
    ## 2  2003874470         Watson, Jobe    191     93  8-Feb-1985          Midfield
    ## 3  2010704250            Fyfe, Nat    192     96 18-Sep-1991 Midfield, Forward
    ## 4  2008672876       Cotchin, Trent    185     86  7-Apr-1990          Midfield
    ## 5  2008681760 Dangerfield, Patrick    189     92  5-Apr-1990 Midfield, Forward
    ## 6  2005682511       Deledio, Brett    188     88 18-Apr-1987 Midfield, Forward
    ## 7  2009664745         Beams, Dayne    187     88 12-Feb-1990          Midfield
    ## 8  2001849962      Thompson, Scott    184     86 14-Mar-1983          Midfield
    ## 9  2007835833        Selwood, Joel    183     84 26-May-1988          Midfield
    ## 10 2007714710         Grigg, Shaun    190     85 19-Apr-1988          Midfield
    ## 11 2010833473          Shuey, Luke    183     90  2-Jun-1990          Midfield
    ## 12 2004837240       Stanton, Brent    183     82  1-May-1986          Midfield
    ## 13 2002749381          Judd, Chris    189     88  8-Sep-1983          Midfield
    ## 14 2009835778         Sloane, Rory    182     83 17-Mar-1990          Midfield
    ## 15 2008822999         Rioli, Cyril    177     80 14-Jul-1989           Forward
    ## 16 2003879516        Wells, Daniel    181     79  3-Feb-1985           Forward
    ## 17 2000677111        Chapman, Paul    179     87  5-Nov-1981           Forward
    ## 18 2002652211         Ablett, Gary    182     87 14-May-1984           Forward
    ## 19 2005706701        Foley, Nathan    178     79  8-Sep-1985 Midfield, Forward
    ## 20 2003836344        Simpson, Kade    183     75  5-May-1984          Defender
    ##                 origin player_idx    score
    ## 1       Eastern Ranges       1034 3.489601
    ## 2  Sandringham Dragons        982 3.334242
    ## 3            Claremont        200 2.630491
    ## 4     Northern Knights        492 2.338829
    ## 5      Geelong Falcons        270 2.299998
    ## 6   Murray Bushrangers        824 2.245719
    ## 7            Southport        797 2.120523
    ## 8        Port Adelaide        956 2.113110
    ## 9              Bendigo        291 1.927683
    ## 10      North Ballarat        932 1.767512
    ## 11   Oakleigh Chargers        626 1.752617
    ## 12    Northern Knights        981 1.660170
    ## 13 Sandringham Dragons       1165 1.601500
    ## 14      Eastern Ranges         32 1.591756
    ## 15            St Marys        917 1.508731
    ## 16        Peel Thunder        799 1.487306
    ## 17      Calder Cannons       1176 1.455255
    ## 18     Geelong Falcons        715 1.439585
    ## 19     Geelong Falcons       1292 1.380993
    ## 20      Eastern Ranges        695 1.354567

We can also show the top 5 players by position:

``` r
players %>%
  group_by(position) %>%
  slice_max(order_by=score, n=5) %>%
  select(c(displayName, position, score)) %>%
  print(n=50)
```

    ## # A tibble: 42 × 3
    ## # Groups:   position [10]
    ##    displayName            position             score
    ##    <chr>                  <chr>                <dbl>
    ##  1 Simpson, Kade          Defender            1.35  
    ##  2 Allen, Jackson         Defender            1.27  
    ##  3 Waters, Beau           Defender            1.26  
    ##  4 Malceski, Nick         Defender            1.04  
    ##  5 Jetta, Lewis           Defender            0.995 
    ##  6 Burgoyne, Shaun        Defender, Forward   0.794 
    ##  7 Hall, Aaron            Defender, Forward   0.775 
    ##  8 Merrett, Jackson       Defender, Forward   0.750 
    ##  9 Roberts-Thomson, Lewis Defender, Forward   0.269 
    ## 10 Adcock, Jed            Defender, Forward   0.195 
    ## 11 Kelly, James           Defender, Midfield  1.03  
    ## 12 Mackay, David          Defender, Midfield  0.763 
    ## 13 Pearce, Clancee        Defender, Midfield  0.696 
    ## 14 Bartel, Jimmy          Defender, Midfield  0.554 
    ## 15 Young, Clinton         Defender, Midfield  0.484 
    ## 16 McEvoy, Ben            Defender, Ruck     -0.0259
    ## 17 Rioli, Cyril           Forward             1.51  
    ## 18 Wells, Daniel          Forward             1.49  
    ## 19 Chapman, Paul          Forward             1.46  
    ## 20 Ablett, Gary           Forward             1.44  
    ## 21 Johnston, Lewis        Forward             1.30  
    ## 22 Ryder, Paddy           Forward, Ruck       0.536 
    ## 23 Clarke, Zac            Forward, Ruck       0.318 
    ## 24 Hale, David            Forward, Ruck       0.0759
    ## 25 West, Trent            Forward, Ruck      -0.0374
    ## 26 Tippett, Kurt          Forward, Ruck      -0.0525
    ## 27 Mitchell, Sam          Midfield            3.49  
    ## 28 Watson, Jobe           Midfield            3.33  
    ## 29 Cotchin, Trent         Midfield            2.34  
    ## 30 Beams, Dayne           Midfield            2.12  
    ## 31 Thompson, Scott        Midfield            2.11  
    ## 32 Fyfe, Nat              Midfield, Forward   2.63  
    ## 33 Dangerfield, Patrick   Midfield, Forward   2.30  
    ## 34 Deledio, Brett         Midfield, Forward   2.25  
    ## 35 Foley, Nathan          Midfield, Forward   1.38  
    ## 36 Griffen, Ryan          Midfield, Forward   1.30  
    ## 37 Bruce, Cameron         Midfield, Ruck     -0.914 
    ## 38 Naitanui, Nic          Ruck                0.842 
    ## 39 Kreuzer, Matthew       Ruck                0.720 
    ## 40 Jolly, Darren          Ruck                0.716 
    ## 41 Bailey, Max            Ruck                0.659 
    ## 42 Jacobs, Sam            Ruck                0.604

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

    ## [1] 0.7777778

We can show the scores by origin:

``` r
players %>%
  group_by(origin) %>%
  summarise (avg_score = mean(score), n_players=n()) %>%
  arrange(desc(avg_score)) %>%
  head(10) %>%
  print
```

    ## # A tibble: 10 × 3
    ##    origin                avg_score n_players
    ##    <chr>                     <dbl>     <int>
    ##  1 NSW-ACT Under 18s         0.930         1
    ##  2 Nsw-act Rams              0.817         1
    ##  3 Subiaco Football Club     0.812         1
    ##  4 Diamond Creek             0.705         1
    ##  5 Devonport                 0.675         1
    ##  6 Zillmere                  0.598         2
    ##  7 Osborne                   0.590         1
    ##  8 Melbourne Grammar         0.581         1
    ##  9 Wagga Tigers              0.576         1
    ## 10 North Hobart              0.559         1
