library(dplyr)
library(mice)
library(imputeTS)
library(glmnet)

model <- function(curr_width, max_gap_c, weight_width, max_gap_w) {
  
  weight_dist = weight_width*2 + 1
  # compute weight change
  data_full <- read.csv('~/weight_data/data_full.csv')
  data_full$weight_dependent = na_ma(data_full$weight, weight_width, maxgap=max_gap_w)
  data_full <- data_full %>% 
    mutate(
      weight_change = (lead(weight_dependent, weight_width) - lag(weight_dependent, weight_width + 1))/weight_dist
    )
  
  # rolling mean imputation
  cols_to_change = c('cals','weight', 'prot', 'carb', 'fat', 'exer', 'ex_min', 'sleep', 'sick')
  data_full[,cols_to_change] <- lapply(data_full[,cols_to_change], function(col){na_ma(col, curr_width, maxgap=max_gap_c)})
  
  print(data_full)
  data_full = na.omit(data_full[c('weight_change', cols_to_change)])
  
  full.lm <- lm(weight_change ~ weight + cals, data=data_full)
  #full.lm <- lm(weight_change ~ weight + cals + sick + sleep + exer, data=data_full)
  print(summary(full.lm))
  
  partial_y = lm(weight_change ~ weight, data=data_full)$residuals
  partial_x = lm(cals ~ weight, data=data_full)$residuals
  #partial_y = lm(weight_change ~ weight + sick + sleep+exer, data=data_full)$residuals
  #partial_x = lm(cals ~ weight + sick + sleep+exer, data=data_full)$residuals
  plot(partial_x, partial_y)
  abline(lm(partial_y ~ partial_x))
  
  # lasso
  
  la.eq <- glmnet(
    x=as.matrix(data_full[,c('cals','weight')]),
    y=data_full$weight_change,
    lambda = 0.001,
    family="gaussian"
  )
  print(la.eq$beta)
}

model(
  curr_width=2, 
  max_gap_c=2, 
  weight_width=3, 
  max_gap_w=3
)
