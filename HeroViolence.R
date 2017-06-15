# Primary analysis script for Hero-Violence experiment
library(broom)
library(pscl)
library(car)
library(plyr)
library(BayesFactor)
library(tidyverse)

# read in data
dat <- read.delim("HeroViolence_Gibson.txt", stringsAsFactors = F)

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
# Make "Control" the reference level
dat$Game = relevel(dat$Game, ref = "Control")
dat$GameOrdered = ordered(dat$Game)
# Make numeric scores for complex contrast
dat$GameNum = ifelse(dat$Game == "Antisocial", -1,
                     ifelse(dat$Game == "Prosocial", 1, 0))
dat$Request = factor(dat$Request, levels=c(1,2), labels=c("Help Red Cross", "Save Lives"))
# Remove all not intercepted by confederate
dat = filter(dat, Intercepted == 1)
# Make binary response variable
dat$DV = dat$Calls; dat$DV[dat$DV >= 1] = 1

# Export to JASP
# Export data in long-table form, 1 obs per row
dat %>% 
  select(DV, Game, Request) %>% 
  filter(complete.cases(.)) %>% 
  write.table("for_jasp.csv", row.names = F)
# Export data as a long-format 3x2 table of counts
# Not sure if this is wise
conting = dat %>% 
  select(DV, Game, Request) %>% 
  filter(complete.cases(.)) %>% 
  group_by(Game, Request) %>% 
  summarize(DV = sum(DV, na.rm=T)) 
# This doesn't work and I'm not sure what it was
#test2 = bcct(DV~(Game + Request), data = conting, n.sample = 190)
write.table(conting, "for_jasp_conting.csv", row.names = F)

# Zero-inflated poisson ----
# TODO: Consider recoding so that Request is coded contr.sum
zipmod = zeroinfl(Calls ~ Game * Request, data = dat)
zinbmod = zeroinfl(Calls ~ Game * Request, data = dat, dist = "negbin")

# Zero-inflated Poisson results
Anova(zipmod, type = 3)
summary(zipmod) 
# Prosocial-count > control count, p = .01, prob antisocial too
# No differences in zero-inflation parameter, p > .17
# No effects or interactions with Request

# Zero-inflated negative binomial results
Anova(zinbmod, type = 3)
summary(zinbmod)
# Very, very strong overdispersion parameter
# No longer any sig effects on count parameter
# No effects on zero-inflation parameter

# Ordered models?
zipmod.ordered <- zeroinfl(Calls ~ GameNum * Request, data = dat)
Anova(zipmod.ordered, type = 3)
summary(zipmod.ordered) # sig effect of game on count value, p = .014

zinbmod.ordered <- zeroinfl(Calls ~ GameNum * Request, data = dat, dist = "negbin")
Anova(zinbmod.ordered, type = 3)
summary(zinbmod.ordered) # very sig overdispersion parameter & not much else

# Bad models w/ violated assumptions of ANOVA ----
# TODO: Specify dummy / contrast codes
badmodel1 <- lm(Calls ~ Game * Request, 
                contrasts = list(Request = contr.sum),
                data = dat) # bad b/c calls not normally distributed
badmodel2 <- lm(Calls ~ Game * Request, 
                contrasts = list(Request = contr.sum),
                data = dat[dat$Calls > 0,]) # same as above + ignores base-rate

# results of badmodel 1: number of calls
Anova(badmodel1, type = 2) # an effect of game?
Anova(badmodel1, type = 3) # depends on Type II vs III sum of squares
summary(badmodel1)

# results of badmodel 2: number of calls among those volunteering at all
Anova(badmodel2, type = 2)
Anova(badmodel2, type = 3)
summary(badmodel2)
tapply(dat$Calls, dat$Game, FUN = mean)


# Lesser tests ensue ----
# Chi-square tests ----
chisq.test(dat$DV, y=dat$Game)
chisq.test(dat$DV, y=dat$Request)
chisq.test(dat$DV, y=interaction(dat$Game, dat$Request))

# Logistic multiple regression ----
# model = glm(DV ~ Game*Request, family="binomial", data=dat)
# summary(model)
# ORs = data.frame("OR"=exp(summary(model)$coefficients[,1]),
#                  "LL"=exp(summary(model)$coefficients[,1]-1.98*summary(model)$coefficients[,2]),
#                  "UL"=exp(summary(model)$coefficients[,1]+1.98*summary(model)$coefficients[,2])
# )
# d = OR.to.d(b=summary(model)$coefficients[,1])
# # Doesn't look like it. Let's have the means.
# means = tapply(dat$DV, INDEX=list(dat$Game, dat$Request), FUN=mean, na.rm=T)
# barplot(means, beside=T, legend.text=c("Antisocial", "Control", "Prosocial")
#         , ylab="Volunteering (proportion)"
#         , args.legend=list(x=3.2, y=.55))
# # Ordinal factor of game?
# modelOrdinal = glm(DV ~ GameOrdered*Request, family="binomial", data=dat)
# summary(modelOrdinal)
# ORs = data.frame("OR"=exp(summary(modelOrdinal)$coefficients[,1]),
#                  "LL"=exp(summary(modelOrdinal)$coefficients[,1]-1.98*summary(modelOrdinal)$coefficients[,2]),
#                  "UL"=exp(summary(modelOrdinal)$coefficients[,1]+1.98*summary(modelOrdinal)$coefficients[,2])
# )
# d = OR.to.d(b=summary(modelOrdinal)$coefficients[,1])
# 
# Extreme groups contrast, prosocial vs. antisocial ----
# modelContrast = dat %>% 
#   filter(Game %in% c("Prosocial", "Antisocial")) %>% 
#   glm(DV ~ Game + Request, family="binomial", data = .)
# summary(modelContrast)
# ORs = data.frame("OR"=exp(summary(modelContrast)$coefficients[,1]),
#                  "LL"=exp(summary(modelContrast)$coefficients[,1]-1.98*summary(modelContrast)$coefficients[,2]),
#                  "UL"=exp(summary(modelContrast)$coefficients[,1]+1.98*summary(modelContrast)$coefficients[,2])
# )
# d = OR.to.d(b=summary(modelContrast)$coefficients[,1])

# Calls per volunteer, poisson ----
agreed = dat[dat$DV==1,]
# hist(agreed$Calls)
modelCalls = glm(Calls ~ Game*Request, family="poisson",data=agreed)
summary(modelCalls) 
table(agreed$Game, agreed$Request)
means2 = tapply(agreed$Calls, INDEX=list(agreed$Game, agreed$Request), FUN=mean, na.rm=T)
means2
# barplot(means2, beside=T, legend.text=c("Antisocial", "Control", "Prosocial")
#        , ylab="Volunteering (#calls)"
#         , args.legend=list(y=10.5)
#        )

# Bayes ----
# I don't know a lot about contingencyTableBF and this may be a mistake
tab = table(dat$DV, dat$Game, dat$Request) # table of yes/no volunteering
bf = contingencyTableBF(tab, sampleType="indepMulti", fixedMargin = "cols")
bf

tab2 = table(dat$DV, dat$Game)
bf2 = contingencyTableBF(tab2, sampleType="indepMulti", fixedMargin="cols")
bf2 # 2:1 against?

# antisocial vs. control
tab3 = tab2[,1:2]
bf3 = contingencyTableBF(tab3, sampleType="indepMulti", fixedMargin="cols")
bf3

# antisocial vs prosocial
tab4 = tab2[,2:3]
bf4 = contingencyTableBF(tab4, sampleType="indepMulti", fixedMargin="cols")
bf4 # 2.3 : 1 for?

# Comparison w/ literature ----
# Turn odds ratio for yes/no helping into cohen's d for comparison with literature?
# Seems like a rough idea...
# model2 = glm(DV ~ Game + Request, data=dat, family="binomial")
# summary(model2)
# d2 = OR.to.d(b=summary(model2)$coefficients[,1])
# 
# model2Trim = glm(DV ~ Game + Request, data=trim, family="binomial")
# summary(model2Trim)
# d2Trim = OR.to.d(b=summary(model2Trim)$coefficients[,1])
# 
# d2Trim - d2
# Future directions:
# Convert OR to r for Bayesian model comparison with Dienes calculator
# http://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
# http://www.campbellcollaboration.org/escalc/html/EffectSizeCalculator-R6.php

# Non-parametric Kruskal-Wallis test ----
# NOTE: this can only handle one-way ANOVA; does not generalize to two-way.
kruskal.test(formula = Calls ~ Game, data = dat)
# pros vs ctrl, p = .262
kruskal.test(formula = Calls ~ Game, data = dat, 
             subset = Game %in% c("Prosocial", "Control"))
# pros vs anti, p = .026
kruskal.test(formula = Calls ~ Game, data = dat, 
             subset = Game %in% c("Prosocial", "Antisocial"))
# ctrl vs anti, p = .224
kruskal.test(formula = Calls ~ Game, data = dat, 
             subset = Game %in% c("Control", "Antisocial"))
