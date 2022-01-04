library(ggplot2)
library(dplyr)
source('~/data_projects/weight_data/dec_fresh/preprocessing/functions.R')
df_in <- read.csv('~/data_projects/weight_data/data.csv') %>%
  preprocess

#fb <- sapply(11:1, function(x) .8^x)
fb <- rep(1,13)
##fs <- sapply(1:7, function(x) .8^x)
fs <- rep(1,9)
fc <- rep(1,7)
fw <- rep(1,5)
#fc <- sapply(3:1, function(x) .5^x)
# must introduce rolling mean
create_features <- function(df, filter_big, filter_small, filter_cals, filter_weight) {
  
  filter_std <- function(f) {f/sum(f)}
  
  df$weight_ma_big <- stats::filter(df$weight, filter = filter_std(filter_big), sides=1)
  df$weight_ma_small <- stats::filter(df$weight, filter = filter_std(filter_small), sides=1)
  df$cals_ma <- stats::filter(df$cals, filter = filter_std(filter_cals), sides = 2)
  df$weight_cur <- stats::filter(df$weight, filter = filter_std(filter_weight), sides = 2)
  
#  df$cals_exp <- exp(df$cals_ma/1000)
  
  l_big <- length(filter_big)
  l_small <- length(filter_small)
  
  window_gap <- (l_big+l_small)/2
  #window_gap <- 1
  
  df$weight_change <- (lead(df$weight_ma_small, n=l_small) - df$weight_ma_big)/window_gap
  
  return(df)
}

output <- create_features(df_in, fb, fs, fc, fw)

ggplot(output, aes(idx, cals_ma)) + geom_point()
ggplot(output, aes(idx, weight_change)) + geom_point()
ggplot(output, aes(idx, weight_cur)) + geom_point()

fit_model <- function(df, fb, fs, fc ,fw) {
  df_features <- create_features(df, fb ,fs ,fc ,fw)
  #model <- lm(weight_change ~ cals_ma + weight_cur, data=df_features)
  model <- lm(weight_change ~ cals_ma, data=df_features)
  return(model)
}

output$weight_delta <- lead(output$weight)-output$weight
ggplot(output, aes(cals_ma, weight_change)) + geom_point() + geom_smooth(method = "lm")
ggplot(output, aes(cals, weight_delta)) + geom_point() + geom_smooth(method = "lm")

output <- fit_model(df_in, fb ,fs ,fc ,fw)
summary(output)