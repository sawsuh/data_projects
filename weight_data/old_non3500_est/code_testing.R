library(dplyr)
library(mice)
library(imputeTS)
library(glmnet)
library(smooth)
options(scipen=999)

#### weight change = future moving average - past moving average / divisor reflecting day gap
#### fits weight change = x0 + x1 * weight + x2 * calories
#### independent variables averaged over current day window

##### modeling using narrow data (only weight + cals)
#for (i in seq(-1, 700, 100)){

model <- function(curr_width, weight_width, cut_off, up_to, desired_daily_change, w) {
  curr_size = 2*curr_width + 1
  max_gap_c = curr_width
  weight_size = 2*weight_width + 1
  
  df_clean <- read.csv('~/weight_data/data.csv')
  ## reset index and set subset of rows
  for (id in seq(min(df_clean$idx, na.rm=TRUE), max(df_clean$idx, na.rm=TRUE)))
  {
    if (!(id %in% df_clean$idx))
    {
      df_clean[nrow(df_clean) + 1, "idx"] <- list(idx=id) 
    }
  }
  df_clean$idx = df_clean$idx - min(df_clean$idx, na.rm=TRUE)
  df_clean <- df_clean[order(df_clean$idx), ]
  df_clean=df_clean[(df_clean$idx > cut_off) & (df_clean$idx < up_to),]
  
  ## take averages
  
  # compute weight change
  #df_clean$weight_dependent = na_ma(df_clean$weight, weight_width, maxgap=max_gap_w)
  
  # rolling mean imputation
  cols_to_change = c('cals','weight')
  df_clean[,cols_to_change] <- lapply(df_clean[,cols_to_change], function(col){na_ma(col, curr_width, maxgap=max_gap_c)})
  
  # rolling mean for weight
  df_clean$weight_dependent = stats::filter(df_clean$weight, rep(1/weight_size,weight_size))
  # rolling means for independent vars
  df_clean[,cols_to_change] <- lapply(df_clean[,cols_to_change], function(col) stats::filter(col, rep(1/curr_size,curr_size)))
  
  df_clean$weight_dependent <- unclass(df_clean$weight_dependent)
  
  # compute weight change
  df_clean <- df_clean %>% 
    mutate(
      weight_change = (lead(weight_dependent, weight_width+1) - lag(weight_dependent, weight_width))/weight_size
    )
  
  ## smoothing
  
  ## filter NA's
  df_clean = na.omit(df_clean[c('weight_change',cols_to_change)])
  
  ## fit model
  weight.lm <- lm(weight_change ~ weight + cals, data=df_clean)
  summary(weight.lm)
  
  ## do calculations
  const <- unname(weight.lm$coefficients[1])
  alpha <- unname(weight.lm$coefficients[2])
  beta <- unname(weight.lm$coefficients[3])
  
  tdee <- (-alpha*w - const)/beta
  cals_to_lose <- (desired_daily_change - const - alpha*w)/beta
  
  ## diagnostics
  partial_y = lm(weight_change ~ weight, data=df_clean)$residuals
  partial_x = lm(cals ~ weight, data=df_clean)$residuals
  plot(partial_x, partial_y)
  abline(lm(partial_y ~ partial_x))
  print(summary(weight.lm))
  print(c( cals_to_lose,
           tdee))
  #plot(weight.lm)
  print(predict(weight.lm, newdata = data.frame(weight=76, cals=1900), interval="prediction")*7)
 
  
  ## testing with weight_coefficient=0
  test_lm <- lm(weight_change ~ cals, data=df_clean) 
  plot(df_clean$cals, df_clean$weight_change)
  abline(test_lm)
  print(summary(test_lm))
  print(predict(test_lm, newdata = data.frame(cals=1900), interval="prediction")*7)
  
  ## try lasso regression (pretty bad)
  
  la.eq <- glmnet(
    x = as.matrix(df_clean[,c('weight','cals')]),
    y = df_clean$weight_change,
    family="gaussian",
    lambda=0.001
  )
  print(la.eq)
  print(la.eq$beta)
  
  return(df_clean)
  
}

#for (i in c(-1,99,199,299,399)){
for (i in c(-1)){
  df_cl <- model(
    curr_width=3, 
    weight_width=3,
    cut_off=i, 
    up_to=Inf,
    desired_daily_change = -0.05,
    w = 76 )
}
