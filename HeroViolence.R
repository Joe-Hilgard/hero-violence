setwd("~/GitHub/Hero_Violence")
dat = read.delim("HeroViolence.txt")
dim(dat)
sapply(dat, FUN=class)

# First things first: everyone needs a unique ID.
dat$ID = paste(dat$Location, dat$Subject, sep=".")
# Clean up labels for factors
dat$Game = factor(dat$Game, levels=c(1,2,3)
                  , labels=c("Control", "Antisocial", "Prosocial"))
dat$Request = factor(dat$Request, levels=c(1,2), labels=c("Help Red Cross", "Save Lives"))
# Remove all not intercepted by confederate
dat = dat[dat$Intercepted == 1]
# Make binary response variable
dat$DV = dat$Calls; dat$DV[dat$DV >= 1] = 1

# Let's make some models or chi-square tests I guess
chisq.test(dat$DV, y=interaction(dat$Game, dat$Request))
model = glm(DV ~ Game*Request, family="binomial", data=dat)
summary(model)
# Doesn't look like it. Let's have the means.
means = tapply(dat$DV, INDEX=list(dat$Game, dat$Request), FUN=mean, na.rm=T)
barplot(means, beside=T, legend.text=c("Control", "Antisocial", "Hero")
        , ylab="Volunteering (proportion)"
        , args.legend=list(x=3.2, y=.55))

# Calls per volunteer?
agreed = dat[dat$DV==1,]
hist(agreed$Calls)
modelCalls = aov(Calls ~ Game*Request, data=agreed)
summary(modelCalls) #again nothing

# Ian's suggestion re: bad subjects 
trim = dat[as.numeric(row.names(dat)) < 187,]
modelTrim = glm(DV ~ Game*Request, family="binomial", data=trim)
summary(modelTrim)
# Doesn't look like it. Let's have the means.
meansTrim = tapply(trim$DV, INDEX=list(trim$Game, trim$Request), FUN=mean, na.rm=T)
barplot(meansTrim, beside=T)

# Just how much usable data is there?
sum(complete.cases(data.frame(dat$DV, dat$Game, dat$Request)))
sum(complete.cases(data.frame(trim$DV, trim$Game, trim$Request)))
