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
# Make "Request" contr.sum coding
dat$Request <- as.factor(dat$Request)
contrasts(dat$Request) <- contr.sum
# Make numeric scores for complex contrast
dat$GameNum = ifelse(dat$Game == "Antisocial", -1,
                     ifelse(dat$Game == "Prosocial", 1, 0))
# dat$Request = factor(dat$Request, levels=c(1,2), labels=c("Help Red Cross", "Save Lives"))
# Restrict dataset to only those intercepted by confederate
dat <- filter(dat, Intercepted == 1)

# Make binary response variable
dat$DV <-  dat$Calls; dat$DV[dat$DV >= 1] = 1

# Make 2 (primary data collection, secondary data collection) x 
#   2 (exclusions, no exclusions) datasets for modeling ambiguity

# Collaborators say the badgroup had hardly anyone volunteer, therefore null
# said the RAs weren't working hard enough
with(dat, table(badgroup, Calls == 0)) %>% 
  prop.table(margin = 1)
# 40% volunteered in "good", 14% in "bad", 37% overall
badgrouptest <- with(dat, table(badgroup, Calls == 0)) %>% 
  chisq.test() 
# I guess they are significantly less likely (p = .045) to volunteer


dat.1 <- dat # both collections, no exclusions
dat.2 <- filter(dat, collection == 1) #first collection, no exclusion
dat.3 <- filter(dat, collection == 1, badgroup == 0) #first collection, exclusion
dat.4 <- filter(dat, badgroup == 0) # both collections, exclusions

dat.list <- list(dat.1, dat.2, dat.3, dat.4)

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
#zipmod = zeroinfl(Calls ~ Game * Request, data = dat)

# t1 LL = -353.1 on 7 DF (Game)
# t2 LL = -354.5 on 5 DF (Request)
# t3 LL = -351.3 on 9 DF (Game + Request)
# Anova(t3) gives Game chisq = 3.02, approx t2 - t3
#   gives Request chisq = .03, too small to be t1 - t3

# Conduct omnibus
zinbmod <- lapply(dat.list, 
                  FUN = function(x) {
                    zeroinfl(Calls ~ Game * Request,
                             data = x,
                             dist = "negbin")
                    }) 
# Conduct in MASS:glm.nb for comparison
# Nevermind, bad idea -- doesn't handle zero-inflation
# zinbmod.glm <- lapply(dat.list,
#                       FUN = function(x) {
#                         glm.nb(Calls ~ Game * Request,
#                                data = x)
#                       })

# Set names for convenience
# names(zinbmod) <- c()

# Fetch all count parameters - z-values
count.z <- lapply(zinbmod, summary) %>% 
  lapply(FUN = function(x) x$coefficients[[1]]) %>% 
  lapply(FUN = tidy) %>% 
  bind_rows(.id = "dataset")
colnames(count.z) <- c("dataset", "term", "Estimate",
                  "Std.Err", "z.value", "p.value")
# Fetch all count parameters - F-values
zinbmod.result <- lapply(zinbmod, Anova, type = 3) %>% 
  lapply(tidy) %>% 
  bind_rows(.id = "dataset")

# fetch omnibus test for Game parameter across datasets
filter(zinbmod.result, term == "Game")
# restrict e.g. to antisocial-game vs control contrast
filter(count.z, term == "GameAntisocial")

# Fetch all zero parameters - z-values
zero.z <- lapply(zinbmod, summary) %>% 
  lapply(FUN = function(x) x$coefficients[[2]]) %>% 
  lapply(FUN = tidy) %>% 
  bind_rows(.id = "dataset")
colnames(count.z) <- c("dataset", "term", "Estimate",
                       "Std.Err", "z.value", "p.value")

# antisocial vs prosocial contrast
zinbmod.c <- lapply(dat.list, 
                           FUN = function(x) {
                             zeroinfl(Calls ~ Game * Request,
                                      data = x,
                                      dist = "negbin",
                                      subset = Game %in% c("Prosocial", "Antisocial"))
                           })

# Fetch all count parameters - z-values
zinb.count.c.result <- lapply(zinbmod.c, summary) %>% 
  lapply(FUN = function(x) x$coefficients[[1]]) %>% 
  lapply(FUN = tidy) %>% 
  bind_rows(.id = "dataset")
colnames(zinb.count.c.result) <- c("dataset", "term", "Estimate",
                                   "Std.Err", "z.value", "p.value")
# Fetch all zero parameters - z-values
zinb.zero.c.result <- lapply(zinbmod.c, summary) %>% 
  lapply(FUN = function(x) x$coefficients[[2]]) %>% 
  lapply(FUN = tidy) %>% 
  bind_rows(.id = "dataset")
colnames(zinb.zero.c.result) <- c("dataset", "term", "Estimate",
                       "Std.Err", "z.value", "p.value")

# ANOVA models ----
# omnibus
aovmod <- lapply(dat.list, 
                 FUN = function(x) {
                   lm(Calls ~ Game * Request,
                      data = x)
                 })
# pairwise contrast
aovmod.c <- lapply(dat.list, 
                          FUN = function(x) {
                            lm(Calls ~ Game * Request,
                               data = x,
                               subset = Game %in% c("Prosocial", "Antisocial"))
                          })

# Another idea we'd considered was fitting ANOVA to subset Calls > 0

# Gather omnibus results
anova.result <- lapply(aovmod, Anova, type = 3) %>% 
  lapply(tidy) %>% 
  bind_rows(.id = "dataset")
# Gather t values
anova.t <- lapply(aovmod, tidy) %>% 
  bind_rows(.id = "dataset")
# Gather pairwise contrasts
anova.c.result <- lapply(aovmod.c, tidy) %>% 
  bind_rows(.id = "dataset")

# Chi-square tests ----
# all omnibus tests by Game
chisq <- lapply(dat.list,
                FUN = function(x) {
                  with(x, chisq.test(DV, y = Game))
                })
# gather results
chisq.result <- lapply(chisq, tidy) %>% 
  bind_rows(.id = "dataset")
# kludge parameter -> df and label term as game
chisq.result <- rename(chisq.result, df = parameter) %>% 
  mutate(term = "Game")

# Run the contrasts
chisq.c <- lapply(dat.list,
                FUN = function(x) {
                  filter(x, Game %in% c("Antisocial", "Prosocial")) %>% 
                    with(., chisq.test(DV, y = Game))
                })
# gather results
chisq.c.result <- lapply(chisq.c, tidy) %>% 
  bind_rows(.id = "dataset")
# kludge parameter -> df and label term as game
chisq.c.result <- rename(chisq.c.result, df = parameter) %>% 
  mutate(term = "Game")

# chisq.test(dat$DV, y=dat$Request)
# chisq.test(dat$DV, y=interaction(dat$Game, dat$Request))

# Logistic multiple regression ----
# fit omnibus
logist <- lapply(dat.list, 
                FUN = function(x) {
                  glm(DV ~ Game * Request, 
                      data = x,
                      family = "binomial")
                })
# fit contrast
logist.c <- lapply(dat.list, 
                   FUN = function(x) {
                     glm(DV ~ Game * Request, 
                         data = x,
                         family = "binomial",
                         subset = Game %in% c("Antisocial", "Prosocial"))
                   })

# gather omnibus tests
logist.result <- lapply(logist, Anova, type = 3) %>% 
  lapply(tidy) %>% 
  bind_rows(.id = "dataset")
# gather contrast tests
logist.c.result <- lapply(logist.c, tidy) %>% 
  bind_rows(.id = "dataset")

# ORs = data.frame("OR"=exp(summary(logisticmodel)$coefficients[,1]),
#                  "LL"=exp(summary(logisticmodel)$coefficients[,1]-1.98*summary(logisticmodel)$coefficients[,2]),
#                  "UL"=exp(summary(logisticmodel)$coefficients[,1]+1.98*summary(logisticmodel)$coefficients[,2])
# )
# d = mutate_all(ORs, funs(OR.to.d)) %>% 
#   mutate(SE = (UL-OR)/1.98)

# Doesn't look like it. Let's have the means.
# means = tapply(dat$DV, INDEX=list(dat$Game, dat$Request), FUN=mean, na.rm=T)
# barplot(means, beside=T, legend.text=c("Antisocial", "Control", "Prosocial")
#         , ylab="Volunteering (proportion)"
#         , args.legend=list(x=3.2, y=.55))

# Non-parametric Kruskal-Wallis test ----
# NOTE: this can only handle one-way ANOVA; does not generalize to two-way.
kruskal <- lapply(dat.list, 
                  FUN = function(x) {
                    kruskal.test(Calls ~ Game, data = x)
                  })
# contrast
kruskal.c <- lapply(dat.list, 
                    FUN = function(x) {
                      kruskal.test(Calls ~ Game, data = x,
                                   subset = Game %in% c("Prosocial", "Antisocial"))
                    })
# Fetch kruskal results
kruskal.result <- lapply(kruskal, tidy) %>% 
  bind_rows(.id = "dataset")
# kludge kruskal names
kruskal.result <- rename(kruskal.result, df = parameter) %>% 
  mutate(term = "Game")
# Fetch kruskal contrast results
kruskal.c.result <- lapply(kruskal.c, tidy) %>% 
  bind_rows(.id = "dataset")
# kludge kruskal names
kruskal.c.result <- rename(kruskal.c.result, df = parameter) %>% 
  mutate(term = "Game")

# Synthesize all results and p-values
# gather all omnibus
omnibus.results <- bind_rows("ANOVA" = anova.result, 
                             "ChiSq" = chisq.result,
                             "KW-median" = kruskal.result, 
                             "logistic" = logist.result,
                             "ZINB" = zinbmod.result,
                             .id = "Model")
# gather all pairwise contrast
contrast.results <- bind_rows("ANOVA" = anova.c.result, 
                             "ChiSq" = chisq.c.result,
                             "KW-median" = kruskal.c.result, 
                             "logistic" = logist.c.result,
                             "ZINB-zero" = zinb.count.c.result,
                             "ZINB-count" = zinb.zero.c.result,
                             .id = "Model")

# Export for use in the .RMD file
dir.create("./R-objects")
save.image("./R-objects/analysis.RData")

# Read 'em and weep
# Omnibus tests
filter(omnibus.results, term %in% c("Game", "GameProsocial")) %>% 
  ggplot(aes(x = p.value)) +
  geom_rug(aes(col = (p.value < .05))) +
  geom_density() +
  scale_x_continuous(limits = c(0, 1), 
                     breaks = c(0, .05, .10, .25, .5, .75, 1)) +
  geom_vline(aes(xintercept = mean(p.value)), lty = 2) +
  geom_vline(aes(xintercept = median(p.value)))
# contrast tests
filter(contrast.results, term %in% c("Game", "GameProsocial")) %>% 
  ggplot(aes(x = p.value)) +
  geom_rug(aes(col = (p.value < .05))) +
  geom_density() +
  scale_x_continuous(limits = c(0, 1), 
                     breaks = c(0, .05, .10, .25, .5, .75, 1)) +
  geom_vline(aes(xintercept = mean(p.value)), lty = 2) +
  geom_vline(aes(xintercept = median(p.value)))
# Boxplot style
filter(omnibus.results, term %in% c("Game", "GameProsocial")) %>% 
  ggplot(aes(x = 1, y = p.value)) +
  geom_boxplot(width = .5) +
  geom_jitter(aes(col = (p.value < .05)), 
              height = 0, width = .2) +
  scale_x_continuous(limits = c(0, 1), 
                     breaks = c(0, .05, .10, .25, .5, .75, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_vline(aes(xintercept = mean(p.value)), lty = 2) +
  geom_vline(aes(xintercept = median(p.value)))
filter(contrast.results, term %in% c("Game", "GameProsocial")) %>% 
  ggplot(aes(x = 1, y = p.value)) +
  geom_boxplot(width = .5) +
  geom_jitter(aes(col = (p.value < .05)), 
              height = 0, width = .2) +
  scale_x_continuous(limits = c(0.5, 1.5)) +
  scale_y_continuous(limits = c(0, 1))

# May restrict to just dataset == 4 since that's what the collabs want
# Omnibus tests
filter(omnibus.results, 
       term %in% c("Game", "GameProsocial"),
       dataset == 4) %>% 
  select(Model:p.value)
# contrast tests
filter(contrast.results, 
       term %in% c("Game", "GameProsocial"),
       dataset == 4) %>% 
  select(Model:p.value)


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

# Here there be dragons ----
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

# ANOVABF
dat.1 %>% 
  filter(!is.na(Calls)) %>% 
  as.data.frame() %>% 
  anovaBF(Calls ~ Game * Request, 
          rscaleEffects = .4,
          data = .)

dat.1 %>% 
  filter(!is.na(Calls),
         Game %in% c("Antisocial", "Prosocial")) %>% 
  as.data.frame() %>% 
  anovaBF(Calls ~ Game * Request, 
          rscaleEffects = .4,
          data = .)

dat.4 %>% 
  filter(!is.na(Calls)) %>% 
  as.data.frame() %>% 
  anovaBF(Calls ~ Game * Request, 
          rscaleEffects = .4,
          data = .)

dat.4 %>% 
  filter(!is.na(Calls),
         Game %in% c("Antisocial", "Prosocial")) %>% 
  as.data.frame() %>% 
  anovaBF(Calls ~ Game * Request, 
          rscaleEffects = .4,
          data = .)
