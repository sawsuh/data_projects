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
players %>%
  arrange(desc(score)) %>%
  head(20)
```

    ##      playerId       displayName height weight         dob position
    ## 1  2007724084      Hawkins, Tom    198    103 21-Jul-1988  Forward
    ## 2  2001823842    Riewoldt, Nick    193     92 17-Oct-1982  Forward
    ## 3  2003874001     Waite, Jarrad    194     96  4-Feb-1983  Forward
    ## 4  2005836583       Shaw, Heath    183     86 27-Nov-1985 Defender
    ## 5  2007822538    Riewoldt, Jack    193     92 31-Oct-1988  Forward
    ## 6  2005708162   Franklin, Lance    199    105 30-Jan-1987  Forward
    ## 7  2008822999      Rioli, Cyril    177     80 14-Jul-1989  Forward
    ## 8  2001801982      Petrie, Drew    197    101 15-Oct-1982  Forward
    ## 9  2011769877        Lynch, Tom    199     99 31-Oct-1992  Forward
    ## 10 2005673207     Cloke, Travis    196    105  5-Mar-1987  Forward
    ## 11 2009877055    Walker, Taylor    193    100 25-Apr-1990  Forward
    ## 12 2011688430     Darling, Jack    191     95 13-Jun-1992  Forward
    ## 13 2006757023     Kennedy, Josh    196    102 25-Aug-1987  Forward
    ## 14 2009822285       Rance, Alex    194     96  9-Oct-1989 Defender
    ## 15 2008773839    Mumford, Shane    197    105  5-Jul-1986     Ruck
    ## 16 2003837329 Sandilands, Aaron    211    118  6-Dec-1982     Ruck
    ## 17 2011779716     McDonald, Tom    194     99 18-Sep-1992  Forward
    ## 18 2014777376      Membrey, Tim    190     89 26-May-1994  Forward
    ## 19 2009787123     Naitanui, Nic    201    110  4-May-1990     Ruck
    ## 20 2008849301     Taylor, Harry    195     94 12-Jun-1986 Defender
    ##                   origin player_idx   score_a   score_b    score
    ## 1      Melbourne Grammar        276  2.780734 16.026463 536.6941
    ## 2              Southport       1024  8.213848 11.080444 515.6991
    ## 3     Murray Bushrangers        927  4.074692 14.054518 510.2666
    ## 4       Northern Knights        722  9.176945  9.773229 500.3607
    ## 5        Tassie Mariners        513  3.303713 14.237206 499.2352
    ## 6                  Perth        570  1.884027 14.558043 478.5018
    ## 7               St Marys        917 12.137264  6.691869 478.4924
    ## 8  North Ballarat Rebels       1037  7.055740 10.387684 473.0275
    ## 9    Dandenong Stingrays        501  3.276651 12.656406 456.0143
    ## 10        Eastern Ranges       1041  4.496141 11.099014 439.2326
    ## 11     Broken Hill North         36  3.496107 11.234499 422.1852
    ## 12            West Perth        604  3.639765 10.970250 418.0281
    ## 13        East Fremantle        614 -1.553700 14.213682 398.0374
    ## 14        Swan Districts        846  8.827932  6.098257 393.9597
    ## 15               Geelong        321  2.902043 10.484360 389.6425
    ## 16        East Fremantle        810  6.473508  7.395669 380.2290
    ## 17 North Ballarat Rebels        392  6.802022  7.123276 379.6792
    ## 18       Gippsland Power        551  4.004209  8.996096 372.2973
    ## 19        Swan Districts        617 12.353471  2.590312 372.2811
    ## 20        East Fremantle        719  7.717513  6.033466 369.2222

``` r
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
    ## 6  2013773561         Macrae, Jack    191     85  3-Aug-1994          Midfield
    ## 7  2007835833        Selwood, Joel    183     84 26-May-1988          Midfield
    ## 8  2010833473          Shuey, Luke    183     90  2-Jun-1990          Midfield
    ## 9  2002775293        Mitchell, Sam    180     84 12-Oct-1982          Midfield
    ## 10 2009787123        Naitanui, Nic    201    110  4-May-1990              Ruck
    ## 11 2008822999         Rioli, Cyril    177     80 14-Jul-1989           Forward
    ## 12 2002652211         Ablett, Gary    182     87 14-May-1984           Forward
    ## 13 1996722607        Harvey, Brent    177     75 14-May-1978           Forward
    ## 14 2007746814         Jack, Kieren    178     81 28-Jun-1987 Midfield, Forward
    ## 15 2005717264        Griffen, Ryan    192     88 27-Jul-1986 Midfield, Forward
    ## 16 2010704250            Fyfe, Nat    192     96 18-Sep-1991 Midfield, Forward
    ## 17 2015839510           Saad, Adam    178     78 23-Jul-1994          Defender
    ## 18 2016803810  Petracca, Christian    186     96  4-Jan-1996           Forward
    ## 19 2003836344        Simpson, Kade    183     75  5-May-1984          Defender
    ## 20 2002688908      Dal Santo, Nick    185     85 22-Feb-1984          Midfield
    ##                origin player_idx  score_a     score_b     score
    ## 1     Geelong Falcons        270 18.55173 -3.76322603 329.14253
    ## 2     Gippsland Power        136 16.14207 -5.23345198 239.57893
    ## 3      Eastern Ranges         32 14.22573 -1.47931225 301.21673
    ## 4    Northern Knights        634 13.72121 -1.23081442 297.47789
    ## 5  Murray Bushrangers        824 13.50789  1.17175726 357.89895
    ## 6   Oakleigh Chargers        655 13.30884 -5.58585562 171.41230
    ## 7             Bendigo        291 12.95963 -7.80468262 104.30389
    ## 8   Oakleigh Chargers        626 12.86600 -7.51094359 110.29242
    ## 9      Eastern Ranges       1034 12.68390 -7.09410084 117.77164
    ## 10     Swan Districts        617 12.35347  2.59031171 372.28108
    ## 11           St Marys        917 12.13726  6.69186897 478.49242
    ## 12    Geelong Falcons        715 12.11234 -2.92658529 218.40609
    ## 13   Northern Knights       1109 11.94429 -0.05559874 292.40537
    ## 14       Nsw-act Rams        857 11.44656 -1.36084543 246.87666
    ## 15     South Adelaide        912 11.37902 -4.12567277 170.86481
    ## 16          Claremont        200 11.02236 -0.57266797 259.36466
    ## 17             Coburg        101 10.80280  2.11870428 327.45038
    ## 18     Eastern Ranges        396 10.42399  0.90961003 286.97848
    ## 19     Eastern Ranges        695 10.42340  2.34611557 325.73271
    ## 20   Bendigo Pioneers       1107 10.35257 -6.15733667  94.78642

``` r
players %>%
  arrange(desc(score_b)) %>%
  head(20)
```

    ##      playerId      displayName height weight         dob position
    ## 1  2007724084     Hawkins, Tom    198    103 21-Jul-1988  Forward
    ## 2  2005708162  Franklin, Lance    199    105 30-Jan-1987  Forward
    ## 3  2007822538   Riewoldt, Jack    193     92 31-Oct-1988  Forward
    ## 4  2006757023    Kennedy, Josh    196    102 25-Aug-1987  Forward
    ## 5  2003874001    Waite, Jarrad    194     96  4-Feb-1983  Forward
    ## 6  2011769877       Lynch, Tom    199     99 31-Oct-1992  Forward
    ## 7  2009877055   Walker, Taylor    193    100 25-Apr-1990  Forward
    ## 8  2005673207    Cloke, Travis    196    105  5-Mar-1987  Forward
    ## 9  2001823842   Riewoldt, Nick    193     92 17-Oct-1982  Forward
    ## 10 2011688430    Darling, Jack    191     95 13-Jun-1992  Forward
    ## 11 2008773839   Mumford, Shane    197    105  5-Jul-1986     Ruck
    ## 12 2001801982     Petrie, Drew    197    101 15-Oct-1982  Forward
    ## 13 2005836583      Shaw, Heath    183     86 27-Nov-1985 Defender
    ## 14 2011685707   Dixon, Charlie    200    105 23-Sep-1990  Forward
    ## 15 2014777376     Membrey, Tim    190     89 26-May-1994  Forward
    ## 16 2012671758  Cameron, Jeremy    196     96  1-Apr-1993  Forward
    ## 17 2005825900 Roughead, Jarryd    193     98 23-Jan-1987  Forward
    ## 18 2014665536       Brown, Ben    200    101 20-Nov-1992  Forward
    ## 19 2012666113      Bruce, Josh    197    101  8-Jun-1992  Forward
    ## 20 2012743763    Jenkins, Josh    200    108  8-Feb-1989  Forward
    ##                   origin player_idx   score_a   score_b    score
    ## 1      Melbourne Grammar        276  2.780734 16.026463 536.6941
    ## 2                  Perth        570  1.884027 14.558043 478.5018
    ## 3        Tassie Mariners        513  3.303713 14.237206 499.2352
    ## 4         East Fremantle        614 -1.553700 14.213682 398.0374
    ## 5     Murray Bushrangers        927  4.074692 14.054518 510.2666
    ## 6    Dandenong Stingrays        501  3.276651 12.656406 456.0143
    ## 7      Broken Hill North         36  3.496107 11.234499 422.1852
    ## 8         Eastern Ranges       1041  4.496141 11.099014 439.2326
    ## 9              Southport       1024  8.213848 11.080444 515.6991
    ## 10            West Perth        604  3.639765 10.970250 418.0281
    ## 11               Geelong        321  2.902043 10.484360 389.6425
    ## 12 North Ballarat Rebels       1037  7.055740 10.387684 473.0275
    ## 13      Northern Knights        722  9.176945  9.773229 500.3607
    ## 14               Redland        455  2.362676  9.445971 350.4533
    ## 15       Gippsland Power        551  4.004209  8.996096 372.2973
    ## 16              Dartmore        265 -2.609735  8.831939 230.9388
    ## 17       Gippsland Power        827  4.241191  8.593775 366.3462
    ## 18       Werribee Tigers        377  2.094958  8.590433 321.8226
    ## 19              Canberra        635 -4.343741  8.235071 178.9322
    ## 20             Frankston        281  2.873480  8.012107 322.3332

We can also show the top 5 players by position:

``` r
players %>%
  group_by(position) %>%
  slice_max(order_by=score, n=5) %>%
  select(c(displayName, position, score)) %>%
  print(n=50)
```

    ## # A tibble: 39 × 3
    ## # Groups:   position [9]
    ##    displayName          position           score[,1]
    ##    <chr>                <chr>                  <dbl>
    ##  1 Shaw, Heath          Defender                500.
    ##  2 Rance, Alex          Defender                394.
    ##  3 Taylor, Harry        Defender                369.
    ##  4 McGovern, Jeremy     Defender                356.
    ##  5 Saad, Adam           Defender                327.
    ##  6 Hooker, Cale         Defender, Forward       288.
    ##  7 Duryea, Taylor       Defender, Forward       170.
    ##  8 Casboult, Levi       Defender, Forward       168.
    ##  9 Johannisen, Jason    Defender, Forward       154.
    ## 10 Burgoyne, Shaun      Defender, Forward       141.
    ## 11 Bartel, Jimmy        Defender, Midfield      298.
    ## 12 Mayne, Chris         Defender, Midfield      228.
    ## 13 Goddard, Brendon     Defender, Midfield      188.
    ## 14 Lewis, Jordan        Defender, Midfield      183.
    ## 15 Blicavs, Mark        Defender, Midfield      157.
    ## 16 McEvoy, Ben          Defender, Ruck          271.
    ## 17 Hawkins, Tom         Forward                 537.
    ## 18 Riewoldt, Nick       Forward                 516.
    ## 19 Waite, Jarrad        Forward                 510.
    ## 20 Riewoldt, Jack       Forward                 499.
    ## 21 Franklin, Lance      Forward                 479.
    ## 22 Ryder, Paddy         Forward, Ruck           337.
    ## 23 Lobb, Rory           Forward, Ruck           319.
    ## 24 Ceglar, Jonathon     Forward, Ruck           174.
    ## 25 Sloane, Rory         Midfield                301.
    ## 26 Bontempelli, Marcus  Midfield                297.
    ## 27 Pendlebury, Scott    Midfield                240.
    ## 28 Zorko, Dayne         Midfield                220.
    ## 29 Duncan, Mitch        Midfield                219.
    ## 30 Deledio, Brett       Midfield, Forward       358.
    ## 31 Westhoff, Justin     Midfield, Forward       356.
    ## 32 Dangerfield, Patrick Midfield, Forward       329.
    ## 33 Elliott, Jamie       Midfield, Forward       264.
    ## 34 Fyfe, Nat            Midfield, Forward       259.
    ## 35 Mumford, Shane       Ruck                    390.
    ## 36 Sandilands, Aaron    Ruck                    380.
    ## 37 Naitanui, Nic        Ruck                    372.
    ## 38 Gawn, Max            Ruck                    366.
    ## 39 Goldstein, Todd      Ruck                    265.

``` r
players %>%
  group_by(position) %>%
  slice_max(order_by=score_a, n=5) %>%
  select(c(displayName, position, score_a)) %>%
  print(n=50)
```

    ## # A tibble: 39 × 3
    ## # Groups:   position [9]
    ##    displayName          position           score_a
    ##    <chr>                <chr>                <dbl>
    ##  1 Saad, Adam           Defender             10.8 
    ##  2 Simpson, Kade        Defender             10.4 
    ##  3 Montagna, Leigh      Defender             10.0 
    ##  4 Boyd, Matthew        Defender              9.37
    ##  5 Murphy, Robert       Defender              9.37
    ##  6 Johannisen, Jason    Defender, Forward     6.84
    ##  7 Burgoyne, Shaun      Defender, Forward     6.16
    ##  8 Duryea, Taylor       Defender, Forward     4.71
    ##  9 Hall, Aaron          Defender, Forward     2.61
    ## 10 Hooker, Cale         Defender, Forward     2.14
    ## 11 Lewis, Jordan        Defender, Midfield   10.1 
    ## 12 Heppell, Dyson       Defender, Midfield    9.64
    ## 13 Bartel, Jimmy        Defender, Midfield    8.74
    ## 14 Goddard, Brendon     Defender, Midfield    8.02
    ## 15 Sinclair, Jack       Defender, Midfield    6.49
    ## 16 McEvoy, Ben          Defender, Ruck        5.04
    ## 17 Rioli, Cyril         Forward              12.1 
    ## 18 Ablett, Gary         Forward              12.1 
    ## 19 Harvey, Brent        Forward              11.9 
    ## 20 Petracca, Christian  Forward              10.4 
    ## 21 Gray, Robbie         Forward               9.66
    ## 22 Ryder, Paddy         Forward, Ruck         7.11
    ## 23 Lobb, Rory           Forward, Ruck         4.04
    ## 24 Ceglar, Jonathon     Forward, Ruck         2.29
    ## 25 Pendlebury, Scott    Midfield             16.1 
    ## 26 Sloane, Rory         Midfield             14.2 
    ## 27 Bontempelli, Marcus  Midfield             13.7 
    ## 28 Macrae, Jack         Midfield             13.3 
    ## 29 Selwood, Joel        Midfield             13.0 
    ## 30 Dangerfield, Patrick Midfield, Forward    18.6 
    ## 31 Deledio, Brett       Midfield, Forward    13.5 
    ## 32 Jack, Kieren         Midfield, Forward    11.4 
    ## 33 Griffen, Ryan        Midfield, Forward    11.4 
    ## 34 Fyfe, Nat            Midfield, Forward    11.0 
    ## 35 Naitanui, Nic        Ruck                 12.4 
    ## 36 Kreuzer, Matthew     Ruck                  7.61
    ## 37 Gawn, Max            Ruck                  7.52
    ## 38 Martin, Stefan       Ruck                  7.40
    ## 39 Goldstein, Todd      Ruck                  6.79

``` r
players %>%
  group_by(position) %>%
  slice_max(order_by=score_b, n=5) %>%
  select(c(displayName, position, score_b)) %>%
  print(n=50)
```

    ## # A tibble: 39 × 3
    ## # Groups:   position [9]
    ##    displayName       position           score_b
    ##    <chr>             <chr>                <dbl>
    ##  1 Shaw, Heath       Defender            9.77  
    ##  2 Rance, Alex       Defender            6.10  
    ##  3 Taylor, Harry     Defender            6.03  
    ##  4 Thompson, Scott   Defender            5.72  
    ##  5 Henderson, Lachie Defender            5.63  
    ##  6 Hooker, Cale      Defender, Forward   7.31  
    ##  7 Casboult, Levi    Defender, Forward   7.03  
    ##  8 Duryea, Taylor    Defender, Forward   0.961 
    ##  9 Impey, Jarman     Defender, Forward  -0.0726
    ## 10 Turner, Kayne     Defender, Forward  -0.245 
    ## 11 Mayne, Chris      Defender, Midfield  4.34  
    ## 12 Bartel, Jimmy     Defender, Midfield  2.63  
    ## 13 Blicavs, Mark     Defender, Midfield  1.80  
    ## 14 Houston, Dan      Defender, Midfield  0.542 
    ## 15 Mills, Callum     Defender, Midfield -0.421 
    ## 16 McEvoy, Ben       Defender, Ruck      4.46  
    ## 17 Hawkins, Tom      Forward            16.0   
    ## 18 Franklin, Lance   Forward            14.6   
    ## 19 Riewoldt, Jack    Forward            14.2   
    ## 20 Kennedy, Josh     Forward            14.2   
    ## 21 Waite, Jarrad     Forward            14.1   
    ## 22 Lobb, Rory        Forward, Ruck       7.01  
    ## 23 Ryder, Paddy      Forward, Ruck       5.29  
    ## 24 Ceglar, Jonathon  Forward, Ruck       2.95  
    ## 25 de Boer, Matt     Midfield            1.81  
    ## 26 McIntosh, Kamdyn  Midfield            0.468 
    ## 27 Menegola, Sam     Midfield            0.348 
    ## 28 Ellis, Brandon    Midfield            0.330 
    ## 29 Duncan, Mitch     Midfield            0.265 
    ## 30 Elliott, Jamie    Midfield, Forward   5.80  
    ## 31 Westhoff, Justin  Midfield, Forward   5.62  
    ## 32 Stringer, Jake    Midfield, Forward   2.88  
    ## 33 Walters, Michael  Midfield, Forward   1.42  
    ## 34 Barlow, Michael   Midfield, Forward   1.19  
    ## 35 Mumford, Shane    Ruck               10.5   
    ## 36 Sandilands, Aaron Ruck                7.40  
    ## 37 Gawn, Max         Ruck                6.06  
    ## 38 Bellchambers, Tom Ruck                4.25  
    ## 39 Sinclair, Callum  Ruck                4.13

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

    ## [1] 0.7257905

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
    ##  1 Eastern Ranges             5.93        12
    ##  2 Gippsland Power            5.87        11
    ##  3 Bendigo Pioneers           5.74         7
    ##  4 Swan Districts             5.33         8
    ##  5 Northern Knights           5.15        15
    ##  6 Oakleigh Chargers          4.96        23
    ##  7 Claremont                  4.90         7
    ##  8 West Perth                 4.75         5
    ##  9 East Fremantle             4.73        13
    ## 10 Sandringham Dragons        4.67        16

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
    ##    origin             avg_score_b n_players
    ##    <chr>                    <dbl>     <int>
    ##  1 Swan Districts           2.02          8
    ##  2 Werribee Tigers          1.83          5
    ##  3 Gippsland Power          1.59         11
    ##  4 West Perth               1.58          5
    ##  5 East Fremantle           1.24         13
    ##  6 Murray Bushrangers       1.14         21
    ##  7 Eastern Ranges           0.527        12
    ##  8 Claremont                0.158         7
    ##  9 Northern Knights         0.151        15
    ## 10 Port Adelaide           -0.186         7
