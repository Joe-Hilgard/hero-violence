# bootstrapping demo

library(car)
library(pscl)
library(ggplot2)
library(dplyr)

# data ingest ----
dat = read.delim("HeroViolence_Master.txt", stringsAsFactors = F)

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
# Remove those guys from the bad semester
dat = filter(dat, badgroup != 1)
# Make binary response variable
dat$DV = dat$Calls; dat$DV[dat$DV >= 1] = 1
# trim down to just complete cases
dat = dat %>% 
  select(Game, Request, Calls) %>% 
  filter(complete.cases(.))

# Examine Type I error rate via bootstrapping ----
simLength = 1e3

pvals = NULL # make holster

for (i in 1:simLength) {
  y = sample(dat$Calls, size = length(dat$Calls), replace = F) # bootstrap outcome
  x = sample(dat$Game, size = length(dat$Game), replace = F) # bootstrap predictor
  # fit models
  bootmod.bad = lm(y ~ x)
  bootmod.good = zeroinfl(y ~ x, dist = "negbin")
  # retrieve p-values
  pvals.out = data.frame("bad.xControl" = summary(bootmod.bad)$coefficients[2,4],
                         "bad.xProsocial" = summary(bootmod.bad)$coefficients[3,4],
                         "bad.xOmnibus" = tidy(Anova(bootmod.bad))[1,5],
                         "good.xControl.count" = summary(bootmod.good)$coefficients$count[2,4],
                         "good.xProsocial.count" = summary(bootmod.good)$coefficients$count[2,4],
                         "good.xControl.zero" = summary(bootmod.good)$coefficients$zero[2,4],
                         "good.xProsocial.zero" = summary(bootmod.good)$coefficients$zero[2,4],
                         "good.xOmnibus" = Anova(bootmod.good)[[3]]
                         )
  # export p-values
  pvals = rbind(pvals, pvals.out)

  # progress report
  if (i %% 50 == 0) {
    paste0("Iteration ", i, "!") %>% 
      print
  }
}

#inspect
hist(pvals$bad.xControl)
hist(pvals$bad.xProsocial)
hist(pvals$bad.xOmnibus)
hist(pvals$good.xControl.zero)
hist(pvals$good.xProsocial.zero)
hist(pvals$good.xControl.count)
hist(pvals$good.xProsocial.count)
hist(pvals$good.xOmnibus)

# compare Type I error rates
sigrate <- function(x) sum(x < .05)/length(x)

results = sapply(pvals, sigrate)
results

