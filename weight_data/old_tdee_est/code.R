library(dplyr)
library(mice)
library(imputeTS)
library(glmnet)

curr_width=2
max_gap_c=2
weight_width=3
max_gap_w=3
weight_dist = weight_width*2 + 1

weight_loss_to_cals_multiplier = 7700

data <- read.csv('~/weight_data/data.csv')
df_clean = data

## reset index
for (id in seq(min(df_clean$idx, na.rm=TRUE), max(df_clean$idx, na.rm=TRUE)))
{
  if (!(id %in% df_clean$idx))
  {
    df_clean[nrow(df_clean) + 1, "idx"] <- list(idx=id) 
  }
}
df_clean$idx = df_clean$idx - min(df_clean$idx, na.rm=TRUE)
df_clean <- df_clean[order(df_clean$idx), ]

#df_clean=df_clean[(df_clean$idx > cut_off) & (df_clean$idx < up_to),]

## take averages

# compute weight change
df_clean$weight_dependent = na_ma(df_clean$weight, weight_width, maxgap=max_gap_w)
df_clean <- df_clean %>% 
  mutate(
    weight_change = (lead(weight_dependent, weight_width) - lag(weight_dependent, weight_width + 1))/weight_dist
  )

# rolling mean imputation
cols_to_change = c('cals','weight')
df_clean[,cols_to_change] <- lapply(df_clean[,cols_to_change], function(col){na_ma(col, curr_width, maxgap=max_gap_c)})

## filter NA's
df_clean = na.omit(df_clean[c('weight_change',cols_to_change)])

df_clean$cals_delta = df_clean$weight_change*7700
df_clean$cals_tdee = df_clean$cals- df_clean$cals_delta
mean(df_clean$cals_tdee)
df_clean$cals_tdee
hist(df_clean$cals_tdee, breaks=40)
summary(df_clean$cals_tdee)
