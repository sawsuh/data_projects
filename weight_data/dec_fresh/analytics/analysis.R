library(dplyr)
library(imputeTS)
library(ggplot2)
library(tidyr)
source('~/data_projects/weight_data/dec_fresh/preprocessing/functions.R')
source('~/data_projects/weight_data/dec_fresh/model_fitting/functions.R')

#fb <- sapply(11:1, function(x) .8^x)
fb <- rep(1,13)
#fs <- sapply(1:7, function(x) .8^x)
fs <- rep(1,9)
fc <- rep(1,7)
fw <- rep(1,5)
#fc <- sapply(3:1, function(x) .5^x)
# must introduce rolling mean

df_in <- read.csv('~/data_projects/weight_data/data.csv') %>%
  preprocess

model <- fit_model(df_in, fb ,fs ,fc ,fw)
output <- summary(model)
print(output)

intercept <- model$coefficients[1] %>% unname
cals_coef <- model$coefficients[2] %>% unname
intercept_se <- output$coefficients[1,2]
cals_se <- output$coefficients[2,2]

how_many <- function(x, m1, m2) {(x-intercept + m1*intercept_se)/(cals_coef + m2*cals_se)}
print(how_many(-0.05,0,0))

df <- data.frame(cals_ma = seq(1000,3000, by=10))
df <- df %>%
  cbind(
    predict(model, df, interval="predict", level=0.5) %>%
      as.data.frame %>%
      rename(pred_lwr = lwr,
             pred_upr = upr)
  ) %>%
  cbind(
    predict(model, df, interval="confidence", level=0.5) %>%
      as.data.frame %>%
      rename(conf_lwr = lwr,
             conf_upr = upr) %>%
      select(-fit)
  ) %>%
  rename(Calories = cals_ma,
         `Weight change` = fit)

ggplot(df, aes(Calories, `Weight change`)) +
  geom_line() + 
  geom_ribbon(aes(ymin = pred_lwr, ymax = pred_upr), alpha=0.2, fill = "aquamarine3") +
  geom_ribbon(aes(ymin = conf_lwr, ymax = conf_upr), alpha=0.15, fill = "red") +
  ggtitle("Caloric intakes and corresponding induced weight changes")