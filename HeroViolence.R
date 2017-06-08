library(haven)
source("~/GitHub/joe-package/joe-package.R")
dat = read.delim("HeroViolence_Gibson.txt", stringsAsFactors = F)

dim(dat)
sapply(dat, FUN=class)

# fix names to be consistent with old datafile
dat = dat %>% 
  rename(Intercepted = intercepted,
         Location = Location_filter,
         Guilty = Gulty)

# Clean up labels for factors
dat$Game = factor(dat$Game, levels=c(2,1,3)
                  , labels=c("Antisocial", "Control", "Prosocial"))
dat$GameOrdered = ordered(dat$Game)
dat$Request = factor(dat$Request, levels=c(1,2), labels=c("Help Red Cross", "Save Lives"))
# Remove all not intercepted by confederate
dat = filter(dat, Intercepted == 1)
# Make binary response variable
dat$DV = dat$Calls; dat$DV[dat$DV >= 1] = 1

# Zero-inflated poisson ----
library(pscl)
zipmod = zeroinfl(Calls ~ Game * Request, data = dat)
zinbmod = zeroinfl(Calls ~ Game * Request, data = dat, dist = "negbin")
zinbmod2 = zeroinfl(Calls ~ GameOrdered * Request, data = dat, dist = "negbin")
summary(zipmod)
summary(zinbmod)
summary(zinbmod2)

# Bad models w/ violated assumptions of ANOVA ----
badmodel1 = aov(Calls ~ Game * Request, data = dat) # bad b/c calls not normally distributed
badmodel2 = lm(Calls ~ Game * Request, data = dat[dat$Calls > 0,]) # same as above + ignores base-rate
summary(badmodel1)
summary(badmodel2)
tapply(dat$Calls, dat$Game, FUN = mean)


# Lesser tests ensue ----
# Chi-square tests ----
chisq.test(dat$DV, y=dat$Game)
chisq.test(dat$DV, y=dat$Request)
chisq.test(dat$DV, y=interaction(dat$Game, dat$Request))

# Logistic multiple regression ----
model = glm(DV ~ Game*Request, family="binomial", data=dat)
summary(model)
ORs = data.frame("OR"=exp(summary(model)$coefficients[,1]),
                 "LL"=exp(summary(model)$coefficients[,1]-1.98*summary(model)$coefficients[,2]),
                 "UL"=exp(summary(model)$coefficients[,1]+1.98*summary(model)$coefficients[,2])
)
d = OR.to.d(b=summary(model)$coefficients[,1])
# Doesn't look like it. Let's have the means.
means = tapply(dat$DV, INDEX=list(dat$Game, dat$Request), FUN=mean, na.rm=T)
barplot(means, beside=T, legend.text=c("Antisocial", "Control", "Prosocial")
        , ylab="Volunteering (proportion)"
        , args.legend=list(x=3.2, y=.55))
# Ordinal factor of game?
modelOrdinal = glm(DV ~ GameOrdered*Request, family="binomial", data=dat)
summary(modelOrdinal)
ORs = data.frame("OR"=exp(summary(modelOrdinal)$coefficients[,1]),
                 "LL"=exp(summary(modelOrdinal)$coefficients[,1]-1.98*summary(modelOrdinal)$coefficients[,2]),
                 "UL"=exp(summary(modelOrdinal)$coefficients[,1]+1.98*summary(modelOrdinal)$coefficients[,2])
)
d = OR.to.d(b=summary(modelOrdinal)$coefficients[,1])

# Extreme groups contrast, prosocial vs. antisocial ----
modelContrast = dat %>% 
  filter(Game %in% c("Prosocial", "Antisocial")) %>% 
  glm(DV ~ Game + Request, family="binomial", data = .)
summary(modelContrast)
ORs = data.frame("OR"=exp(summary(modelContrast)$coefficients[,1]),
                 "LL"=exp(summary(modelContrast)$coefficients[,1]-1.98*summary(modelContrast)$coefficients[,2]),
                 "UL"=exp(summary(modelContrast)$coefficients[,1]+1.98*summary(modelContrast)$coefficients[,2])
)
d = OR.to.d(b=summary(modelContrast)$coefficients[,1])

# Calls per volunteer, poisson ----
agreed = dat[dat$DV==1,]
hist(agreed$Calls)
modelCalls = glm(Calls ~ Game*Request, family="poisson",data=agreed)
summary(modelCalls) 
table(agreed$Game, agreed$Request)
means2 = tapply(agreed$Calls, INDEX=list(agreed$Game, agreed$Request), FUN=mean, na.rm=T)
means2
barplot(means2, beside=T, legend.text=c("Antisocial", "Control", "Prosocial")
       , ylab="Volunteering (#calls)"
        , args.legend=list(y=10.5)
       )

# Bayes?
require(BayesFactor)
tab = table(dat$DV, dat$Game, dat$Request)
bf = contingencyTableBF(tab, sampleType="indepMulti", fixedMargin = "cols")
bf
tab2 = table(dat$DV, dat$Game)
bf2 = contingencyTableBF(tab2, sampleType="indepMulti", fixedMargin="cols", priorConcentration=10)
bf2

# Drop the interactions
model2 = glm(DV ~ Game + Request, data=dat, family="binomial")
summary(model2)
d2 = OR.to.d(b=summary(model2)$coefficients[,1])

model2Trim = glm(DV ~ Game + Request, data=trim, family="binomial")
summary(model2Trim)
d2Trim = OR.to.d(b=summary(model2Trim)$coefficients[,1])

d2Trim - d2
# Future directions:
# Convert OR to r for Bayesian model comparison with Dienes calculator
# http://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
# http://www.campbellcollaboration.org/escalc/html/EffectSizeCalculator-R6.php

ggplot(dat, aes(x=Calls, fill = Game)) +
  geom_bar(position = "dodge") + 
  guides(fill = FALSE) +
  scale_x_continuous("DV") +
  ggtitle("Don't use ANOVA on this.")


