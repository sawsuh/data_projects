library(memoise)
crr <- function(S, payoff_func, n, r, t, sigma=NULL, u=NULL, d=NULL, p=NULL, is_american=F) { local({
  if (is.null(u)) {
    u <- exp(sigma*sqrt(t))
    d <- 1/u
  }
  if (is.null(p)) {
    p <- (exp(r*t)-d)/(u-d)
  }
  print(sprintf('u=%.3f d=%.3f r=%.3f p=%.3f', u, d, r, p))
  func_inner <- function(S, n, node){
      if (n==0){
        v <- payoff_func(S)
        print(sprintf('Value at terminal node %s (S=%.3f) is %.3f', node, S[1], v))
      } else {
        v_u <- func_inner(c(S[1]*u, S), n-1, paste0(node, 'u'))
        v_d <- func_inner(c(S[1]*d, S), n-1, paste0(node, 'd'))
        v <- exp(-r*t)*(v_u*p + v_d*(1-p))
        if (is_american) {
          v_exercise_now <- payoff_func(S)
          if (v_exercise_now > v) {
            print(sprintf('Exercise early at node %s (S=%.3f) for %.3f (as opposed to holding for %.3f)', node, S[1], v_exercise_now, v))
          }
          v <- max(v, v_exercise_now)
        }
        print(sprintf('Value at intermediate node %s (S=%.3f) is %.3f', node, S[1], v))
      }
    v
  }
  mf <- memoise(func_inner)
  out <- mf(S, n, '')
  forget(mf)
  return(out)
})}
repl_pf_euro <- function(S, payoff_func, n, r, t, sigma=NULL, u=NULL, d=NULL, is_american=F) { local({
  if (is.null(u)) {
    u <- exp(sigma*sqrt(t))
    d <- 1/u
  }
  print(sprintf('u=%.3f d=%.3f r=%.3f', u, d, r))
  func_inner <- function(S, n, node) {
    if (n == 0) {
      v <- payoff_func(S)
      print(sprintf('Value at terminal node %s (S=%.3f) is %.3f', node, S[1], v))
      return(v)
    } else {
      v_u <- func_inner(c(S[1]*u, S), n-1, paste0(node, 'u'))
      v_d <- func_inner(c(S[1]*d, S), n-1, paste0(node, 'd'))
      stock_amt <- (v_u - v_d)/(S[1]*(u-d))
      cash_amt <- exp(-r*t)*(u*v_d - d*v_u)/(u-d)
      v_hold <- stock_amt*S[1] + cash_amt
      if (is_american){
        v <- payoff_func(S)
        if (v > v_hold) {
          print(sprintf('Exercise early at node %s (S=%.3f) for %.3f instead of holding %.3f', node, S[1], v, v_hold))
          v_hold <- v
        }
      }
      print(sprintf('At node %s (S=%.3f) we hold %.3f cash and %.3f stock (worth %.3f)', node, S[1], cash_amt, stock_amt, v_hold))
      return(v_hold)
    }
  }
  mf <- memoise(func_inner)
  out <- mf(S, n, '')
  forget(mf)
  return(out)
})}
print(crr(60, \(S) max(S[1]-70,0), 1, 0.05, 0.5, u=65/60, d=55/60))
print(crr(60, \(S) max(S[1]-50,0), 1, 0.05, 0.5, u=65/60, d=55/60))
print(crr(40, \(S) max(45-S[1],0), 2, 0.12, 0.25, u=1.1, d=0.9))
print(crr(40, \(S) max(45-S[1],0), 2, 0.12, 0.25, u=1.1, d=0.9, is_american = T))
print(crr(90, \(S) (S[1]-100)**2, 3, 0.1, 1/3, sigma=0.3))
print(crr(100, \(S) max(S[1]-95,0), 3, 0.08, 1/3, sigma=0.3, is_american=T))
print(crr(100, \(S) max(95-S[1],0), 3, 0.08, 1/3, sigma=0.3, is_american=F))
print(crr(100, \(S) max(95-S[1],0), 3, 0.08, 1/3, sigma=0.3, is_american=T))
print(crr(41, \(S) max(S[1]-40,0), 2, 0.08, 1/2, sigma=0.3))
print(crr(82, \(S) max(S[1]-80,0), 2, 0.05, 1, sigma=0.3))
print(crr(79, \(S) max(80-S[1],0), 2, 0.05, 1, sigma=0.3, is_american=T))
print(repl_pf_euro(79, \(S) max(80-S[1],0), 2, 0.05, 1, sigma=0.3, is_american=T))
