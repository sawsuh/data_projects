x=c()
for (i in seq(20000)){
  coef_intercept <- rnorm(1, mean=-0.26651055, sd=0.01540417)
  coef_cals <- rnorm(1, mean=0.00011315, sd=0.00000643)
  x <- c(x, (-0.05-coef_intercept)/coef_cals)
}
summary(x)
hist(x,breaks=40)
plot(ecdf(x), xlim=c(1800,2000))

x <- sort(x)
bot <- x[floor(length(x)*0.35)]
top <- x[ceiling(length(x)*0.65)]
print(paste("30% CI is: (", bot, ", ", top, ")"))