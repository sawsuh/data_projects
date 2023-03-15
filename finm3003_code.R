crr_american <- function(S, payoff_func, n, r, t, u, d, p, node){
  if (n == 0) {
    print(sprintf('value at node %s is %.3f', node, payoff_func(S)))
    return(payoff_func(S))
  } else {
    v_exercise_now <- payoff_func(S)
    v_up <- crr_american(S*u, payoff_func, n-1, r,t,  u, d, p, paste0(node, 'u'))
    v_down <- crr_american(S*d, payoff_func, n-1, r,t, u, d, p, paste0(node, 'd'))
    v_disc <- exp(-r*t)*(v_up*p + v_down*(1-p))
    if (v_exercise_now > v_disc) {
      print(sprintf('Exercise early at node %s for %.3f (as opposed to holding for %.3f)', node, v_exercise_now, v_disc))
      return(v_exercise_now)
    } else {
      print(sprintf('Hold at node %s for %.3f', node, v_disc))
      return(v_disc)
    }
    print(sprintf('value at node %s is %.3f', node, max(v_exercise_now,exp(-r*t)*(v_up*p + v_down*(1-p)))))
    return(max(v_exercise_now, exp(-r*t)*(v_up*p + v_down*(1-p))))
  }
}

crr_american_nice <- function(S, payoff_func, n, r, sigma, t) {
  u <- exp(sigma*sqrt(t))
  d <- 1/u
  p <- (exp(r*t) - d)/(u-d)
  print(sprintf('u=%.3f, d=%.3f, p=%.3f', u, d, p))
  crr_american( S, payoff_func, n, r, t, u, d, p,'')
}

crr_european <- function(S, payoff_func, n, r, t, u, d, p, node){
  if (n == 0) {
    print(sprintf('value at node %s is %.3f', node, payoff_func(S)))
    return(payoff_func(S))
  } else {
    v_up <- crr_european(S*u, payoff_func, n-1,r, t, u, d, p, paste0(node, 'u'))
    v_down <- crr_european(S*d, payoff_func, n-1,r, t, u, d, p, paste0(node, 'd'))
    print(sprintf('value at node %s is %.3f', node, exp(-r*t)*(v_up*p + v_down*(1-p))))
    return(exp(-r*t)*(v_up*p + v_down*(1-p)))
  }
}

crr_euro_nice <- function(S, payoff_func, n, r, sigma, t) {
  u <- exp(sigma*sqrt(t))
  d <- 1/u
  p <- (exp(r*t) - d)/(u-d)
  print(sprintf('u=%.3f, d=%.3f, p=%.3f', u, d, p))
  crr_european( S, payoff_func, n, r, t, u, d, p ,'')
}

repl_pf_euro <- function(S, payoff_func, n, r, t, u, d, node) {
  if (n == 0) {
    return(payoff_func(S))
  } else {
    v_u <- repl_pf_euro(S*u, payoff_func, n-1, r, t, u, d, paste0(node, 'u'))
    v_d <- repl_pf_euro(S*d, payoff_func, n-1, r, t, u, d, paste0(node, 'd'))
    stock_amt <- (v_u - v_d)/(S*(u-d))
    cash_amt <- exp(-r*t)*(u*v_d - d*v_u)/(u-d)
    print(sprintf('At node %s we have %.3f cash and %.3f in stock (worth %.3f)', node, cash_amt, stock_amt, stock_amt*S+cash_amt))
    return(stock_amt*S + cash_amt)
  }
}
u <-exp(0.3*sqrt(1/2))
d <- 1/u
p <- (exp(0.08*(1/2))-d)/(u-d)
repl_pf_euro(41, \(x) max(x-40,0),2, 0.08, 1/2, u, d, '' )