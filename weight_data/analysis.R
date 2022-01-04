library(dplyr)
library(mice)
library(imputeTS)
library(glmnet)

##### modeling using narrow data (only weight + cals)
#for (i in seq(-1, 700, 100)){
for (i in c(-1)){
df_cl <- model(
      curr_width=2, 
      max_gap_c=2, 
      weight_width=3, 
      max_gap_w=3, 
      cut_off=i, 
      up_to=Inf,
      desired_daily_change = -0.05,
      w = 76 )
}
model <- function(curr_width, max_gap_c, weight_width, max_gap_w, cut_off, up_to, desired_daily_change, w) {
weight_dist = weight_width*2 + 1

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

df_clean=df_clean[(df_clean$idx > cut_off) & (df_clean$idx < up_to),]

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
plot(weight.lm)

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

##### modeling now using wide data


data_full <- read.csv('~/weight_data/data_full.csv')
model(
      curr_width=2, 
      max_gap_c=2, 
      weight_width=3, 
      max_gap_w=3
)
model <- function(curr_width, max_gap_c, weight_width, max_gap_w) {

weight_dist = weight_width*2 + 1
# compute weight change
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




########
# TESTING FITTING TDEE ONLY
########




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
