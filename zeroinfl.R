# A junkfile where I tinkered with how zeroinfl works.

#zipmod = zeroinfl(Calls ~ Game * Request, data = dat)

m1 <- zeroinfl(Calls ~ Game * Request, data = dat.4, dist = "negbin")
m2 <- zeroinfl(Calls ~ Game + Request, data = dat.4, dist = "negbin")
m3 <- zeroinfl(Calls ~ Request, data = dat.4, dist = "negbin")
m4 <- zeroinfl(Calls ~ Game, data = dat.4, dist = "negbin")

waldtest(m1, m2)
waldtest(m2, m3)

lrtest(m1, m2)
lrtest(m2, m3)

# t1 LL = -353.1 on 7 DF (Game)
# t2 LL = -354.5 on 5 DF (Request)
# t3 LL = -351.3 on 9 DF (Game + Request)
# Anova(t3) gives Game chisq = 3.02, approx t2 - t3
#   gives Request chisq = .03, too small to be t1 - t3


# ------

mfull <- zeroinfl(Calls ~ Game * Request, 
                  data = dat.4, 
                  dist = "negbin")
m.dropgame <- zeroinfl(Calls ~ Request + Game:Request, data = dat.4, dist = "negbin")

summary(mfull)
summary(m.dropgame)
lrtest(mfull, m.dropgame) # identical models. Is type III not feasible?

m.additive <- zeroinfl(Calls ~ Game + Request,
                       data = dat.4,
                       dist = 'negbin')
m.request <- zeroinfl(Calls ~ Request,
                      data = dat.4,
                      dist = 'negbin')
m.game <- zeroinfl(Calls ~ Game, 
                   data = dat.4,
                   dist = 'negbin')
m.empty <- zeroinfl(Calls ~ 1,
                    data = dat.4,
                    dist = 'negbin')
summary(m.additive)
summary(m.request)
lrtest(m.additive, m.request)
lrtest(m.game, m.empty)

drop1(m.additive, test = "Chisq")
drop1(mfull, test = "Chisq")
