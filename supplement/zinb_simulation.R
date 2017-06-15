# simulate error rates for ZINB modeled as ANOVA
library(pscl)

# generate data ----
# An alternative parametrization (often used in ecology) is by 
# the mean mu, and size, the dispersion parameter, 
# where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization.
celln = 100

x1 = rbinom(n = celln, size = 1, prob = exp(1.71)/(1+exp(1.71)))
x2 = rnbinom(n = celln, size = exp(1.34), mu = 1.7)
x = ifelse(x1 == 0, x2, 0)

barplot(table(x1))
barplot(table(x2))

barplot(table(x))

sum(x == 0)/length(x) # 87% -- too high. Am I parameterizing something wrong
sum(dat$Calls == 0, na.rm=T)/sum(!is.na(dat$Calls)) # 61%
