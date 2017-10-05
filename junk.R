
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
  ggplot(aes(x = .5, y = p.value)) +
  geom_boxplot(width = .5) +
  geom_jitter(aes(col = (p.value < .05)), 
              height = 0, width = .2) +
  coord_flip() +
  scale_x_continuous(limits = c(0, 1), 
                     breaks = c(0, .05, .10, .25, .5, .75, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(aes(yintercept = mean(p.value)), lty = 2) +
  geom_hline(aes(yintercept = median(p.value)))
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
# agreed = dat[dat$DV==1,]
# # hist(agreed$Calls)
# modelCalls = glm(Calls ~ Game*Request, family="poisson",data=agreed)
# summary(modelCalls) 
# table(agreed$Game, agreed$Request)
# means2 = tapply(agreed$Calls, INDEX=list(agreed$Game, agreed$Request), FUN=mean, na.rm=T)
# means2
# barplot(means2, beside=T, legend.text=c("Antisocial", "Control", "Prosocial")
#        , ylab="Volunteering (#calls)"
#         , args.legend=list(y=10.5)
#        )

# Bayes ----
# I don't know a lot about contingencyTableBF and this may be a mistake
# tab = table(dat$DV, dat$Game, dat$Request) # table of yes/no volunteering
# bf = contingencyTableBF(tab, sampleType="indepMulti", fixedMargin = "cols")
# bf
# 
# tab2 = table(dat$DV, dat$Game)
# bf2 = contingencyTableBF(tab2, sampleType="indepMulti", fixedMargin="cols")
# bf2 # 2:1 against?
# 
# # antisocial vs. control
# tab3 = tab2[,1:2]
# bf3 = contingencyTableBF(tab3, sampleType="indepMulti", fixedMargin="cols")
# bf3
# 
# # antisocial vs prosocial
# tab4 = tab2[,2:3]
# bf4 = contingencyTableBF(tab4, sampleType="indepMulti", fixedMargin="cols")
# bf4 # 2.3 : 1 for?

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
