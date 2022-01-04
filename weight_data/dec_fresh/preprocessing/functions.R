library(dplyr)
library(tidyr)
library(imputeTS)
library(ggplot2)

df_in <- read.csv('~/data_projects/weight_data/data.csv')

# multiple imputation using Amelia
impute_amelia <- function(df){
  return(amelia(df, m = 5, ts = 'idx', cs = 'cals', intercs = F, polytime = 3))
}

# regular time series imputation using move average window
impute <- function (df, w_n, w_g, c_n, c_g) {
  w_v <- (w_n:1)/sum(w_n:1)
  c_v <- (c_n:1)/sum(c_n:1)
  weight_filtered <- na_ma(df$weight, k = w_n, maxgap = w_g)
  cal_filtered <- na_ma(df$cals, k = c_n, maxgap = c_g)
  return(data.frame(list(idx = df$idx, weight = weight_filtered, cals = cal_filtered)))
}

# apply imputation
preprocess <- function(df){
  imputed_vals <- df %>%
    filter(!((idx>890)&(cals>2150))) %>%
    impute(., 7, 9, 11, 11)
    #return(.)
  
  print(imputed_vals)
  
  all_vals <- df %>%
    merge(imputed_vals, by='idx', all.x=T)
  
  all_vals$weight <- ifelse(is.na(all_vals$weight.x), all_vals$weight.y, all_vals$weight.x)
  all_vals$cals <- ifelse(is.na(all_vals$cals.x), all_vals$cals.y, all_vals$cals.x)
  
  #weight_ma <- all_vals$weight
  #return(all_vals[,c('idx','weight','cals', 'weight.x', 'weight.y')])
  all_vals$cals.y <- ifelse(is.na(all_vals$cals.x), all_vals$cals.y, NA)
  all_vals$weight.y <- ifelse(is.na(all_vals$weight.x), all_vals$weight.y, NA)
  return(all_vals)
}

output <- preprocess(df_in)


# diagnostics/testing

plotf <- function(colname) {
  xcol <- paste0(colname, '.x')
  ycol <- paste0(colname, '.y')
  new_points <- output[, c('idx', xcol, ycol)] %>% 
    gather("xory", "value", !!sym(xcol), !!sym(ycol)) %>%
    rename(`Real/imputed` = xory) %>%
    mutate(`Real/imputed` = case_when(
      `Real/imputed` == xcol ~ "Real",
      `Real/imputed` == ycol ~ "Imputed"
    ))
  print(colnames(new_points))
  ggplot(new_points, aes(idx, value, col=`Real/imputed`)) + 
    geom_point() +
    ggtitle(paste("Time series of", colname)) + 
    xlab("") + 
    ylab("") + 
    scale_fill_discrete(name = "", labels = c("Recorded", "Imputed")) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank())
}
plotf("weight")
plotf("cals")