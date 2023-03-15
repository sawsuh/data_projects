library(gtree)
game = org.game = new_game(
  gameId = "KuhnPoker",
  params = list(numPlayers=2),
  options = make_game_options(verbose=FALSE),
  stages = list(
    stage("dealCards",
          nature = list(
            # Player 1 gets a random card 1, 2, or 3
            natureMove("card1", 2:14),
            # Draw from remaining cards for player 2
            natureMove("card2", ~setdiff(2:14, card1))
          )
    ),
    stage("pl1CheckBet",
          player=1,
          observe = "card1",
          actions = list(
            action("cb1",c("check","bet"))
          )
    ),
    stage("pl2CheckBet",
          player=2,
          condition = ~ cb1 == "check",
          observe = c("card2","cb1"),
          actions = list(
            action("cb2",c("check","bet"))
          )
    ),
    stage("pl2FoldCall",
          player=2,
          condition = ~ cb1 == "bet",
          observe = c("card2","cb1"),
          actions = list(
            action("fc2",c("fold","call"))
          )
    ),
    stage("pl1FoldCall",
          player=1,
          condition = ~ is_true(cb1 == "check" & cb2=="bet"),
          observe = "cb2",
          actions = list(
            action("fc1",c("fold","call"))
          )
    ),
    stage("PayoffStage",
          player=1:2,
          compute=list(
            # Which player folds?
            folder ~ case_distinction(
              is_true(fc1 == "fold"),1,
              is_true(fc2 == "fold"),2,
              0 # 0 means no player folds
            ),
            
            # Which player wins?
            winner ~ case_distinction(
              folder == 1,2,
              folder == 2,1,
              folder == 0, (card2 > card1) +1
            ),
            
            # How much gave each player to the pot?
            gave1 ~ 1 + 2*is_true((cb1 == "bet") | (fc1 == "call")),
            gave2 ~ 1 + 2*is_true((cb2 == "bet") | (fc2 == "call")),
            pot ~ gave1 + gave2,
            
            # Final payoffs
            payoff_1 ~ (winner == 1)*pot - gave1,
            payoff_2 ~ (winner == 2)*pot - gave2
          )
    )
  )
) 