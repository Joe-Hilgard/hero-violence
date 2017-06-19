# Primary analysis script for Hero-Violence experiment
library(broom)
library(pscl)
library(car)
library(plyr)
library(BayesFactor)
library(tidyverse)
library(forcats)

# OR.to.d for comparison with literature
# Hasselblad & Hedges (1995) technique
OR.to.d = function(OR=NULL, b=NULL) {
  if (!is.null(OR)) b1 = log(OR)
  if (!is.null(b) & !is.null(OR)) if (b1 != b) print("Nonmatching OR and b! One or the other, please.")
  if (is.null(b)) b = b1
  return(b * sqrt(3)/pi)
}

# read in data
dat <- read.delim("HeroViolence_Master.txt", stringsAsFactors = F)

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
# Remove all data flagged as bad by CMU collaborators
dat <- filter(dat, badgroup != 1)

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
# contrasts
filter(dat, Game %in% c("Antisocial", "Prosocial")) %>% 
  with(., chisq.test(DV, Game))
filter(dat, Game %in% c("Antisocial", "Control")) %>% 
  with(., chisq.test(DV, Game))
filter(dat, Game %in% c("Control", "Prosocial")) %>% 
  with(., chisq.test(DV, Game))

# Logistic multiple regression ----
logisticmodel = glm(DV ~ Game*Request, family="binomial", data=dat)
Anova(logisticmodel, type = 3)
summary(logisticmodel)
ORs = data.frame("OR"=exp(summary(logisticmodel)$coefficients[,1]),
                 "LL"=exp(summary(logisticmodel)$coefficients[,1]-1.98*summary(logisticmodel)$coefficients[,2]),
                 "UL"=exp(summary(logisticmodel)$coefficients[,1]+1.98*summary(logisticmodel)$coefficients[,2])
)
d = mutate_all(ORs, funs(OR.to.d)) %>% 
  mutate(SE = (UL-OR)/1.98)

# Doesn't look like it. Let's have the means.
means = tapply(dat$DV, INDEX=list(dat$Game, dat$Request), FUN=mean, na.rm=T)
barplot(means, beside=T, legend.text=c("Antisocial", "Control", "Prosocial")
        , ylab="Volunteering (proportion)"
        , args.legend=list(x=3.2, y=.55))

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
kruskal.test(formula = Calls ~ Request, data = dat)
# by subgroups?
kruskal.test(formula = Calls ~ Game, data = dat, subset = Request == "Help Red Cross")
kruskal.test(formula = Calls ~ Game, data = dat, subset = Request == "Save Lives")

# pros vs ctrl, p = .262
kruskal.test(formula = Calls ~ Game, data = dat, 
             subset = Game %in% c("Prosocial", "Control"))
# pros vs anti, p = .026
kruskal.test(formula = Calls ~ Game, data = dat, 
             subset = Game %in% c("Prosocial", "Antisocial"))
# ctrl vs anti, p = .224
kruskal.test(formula = Calls ~ Game, data = dat, 
             subset = Game %in% c("Control", "Antisocial"))


# Plots
# ZINB model of counts
lsmeans(zinbmod, c("Game", "Request")) %>% 
  summary() %>% 
  mutate(Game = fct_relevel(Game, c("Antisocial", "Control", "Prosocial"))) %>% 
  ggplot(aes(x = Game, y = lsmean)) +
  geom_point() +
  geom_jitter(aes(y = Calls), data = dat, 
              alpha = .2, width = .25, height = .5) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_wrap(~Request) +
  scale_y_continuous("Predicted mean calls")

# probability model of volunteering at all
lsmeans(logisticmodel, c("Game", "Request")) %>% 
  summary() %>% 
  mutate(Game = fct_relevel(Game, c("Antisocial", "Control", "Prosocial"))) %>% 
  ggplot(aes(x = Game, y = lsmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_wrap(~Request) +
  scale_y_continuous("Probability of volunteering at all (log-odds)")

lsmeans(logisticmodel, c("Game", "Request")) %>% 
  summary() %>% 
  mutate(Game = fct_relevel(Game, c("Antisocial", "Control", "Prosocial"))) %>% 
  mutate_at(.cols = vars(lsmean, asymp.LCL, asymp.UCL), .funs = funs(exp(.)/(1+exp(.)))) %>% 
  ggplot(aes(x = Game, y = lsmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_wrap(~Request) +
  scale_y_continuous("Probability of volunteering at all (%)", limits = c(0, 1))

# what if we bin it to 0, 1-5, 5-10, 10+?
dat.mutant <- mutate(dat, Calls = ifelse(Calls == 0, 0, 
                                         ifelse(Calls > 0 & Calls <= 5, 1,
                                                ifelse(Calls <= 10, 2, 3))))
qplot(dat.mutant$Calls, geom = "histogram", binwidth = 1)
mutantmod <- lm(Calls ~ Game * Request, data = dat.mutant)
Anova(mutantmod, type = 3)
summary(mutantmod) # nah, and this is goofy anyway
