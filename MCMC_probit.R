# MCMC chain for performing probit ANOVA
library(msm)
library(mvtnorm)

dat = read.delim("HeroViolence.txt")
dat = dat[!(is.na(dat$Game) | is.na(dat$Request) | is.na(dat$Calls)),]
N = nrow(dat)
# Generate some data
# design matrix
X = matrix(c("x0" = rep(1, nrow(dat))
             , "violent-hero"       = as.numeric(dat$Game == 1)
             , "violent-antisocial" = as.numeric(dat$Game == 2)
             , "saveLives-request"  = as.numeric(dat$Request == 2)
             ),
           nrow=nrow(dat),
           byrow=F)
X = cbind(X, X[,2]*X[,4]) # violent-hero / save-lives interaction
X = cbind(X, X[,3]*X[,4]) # violent-antihero / save-lives interaction
# could use model.matrix if I were working with real data?
y = as.numeric(dat$Calls > 0)

# establish priors on beta params
k = ncol(X) #nparams
priorMeans = rep(0, k)
scale = 5
B = solve(diag(scale, k, k))
# temp = seq(-4, 4, .1)
# plot(temp, dnorm(temp, mu.0, s2.0), type='l') # looks like a reasonable prior

# prepare chain holsters
M = 1e4
# make beta a matrix: rows are params, columns are iterations
b = matrix(0, nrow=k, ncol=M)
z = matrix(0, nrow=N, ncol=M)
# selector functions for truncated normal sampling of z
lowerLimits = c(-Inf, 0)
upperLimits = c(0, Inf)

for (i in 2:M) {
  V = solve(1/1*crossprod(X) + B)
  C = 1/1*crossprod(X, z[,i-1]) + crossprod(B,priorMeans)
  b[,i] = rmvnorm(1, crossprod(V,C), V)
  
  # sample z | nu y
  z[,i] = rtnorm(N, X %*% b[,i], 1, lower=lowerLimits[y+1], upper=upperLimits[y+1])

  if (i %% 1000 == 0) print(paste("Iteration", i))
}

for (i in 1:k) plot(b[i,], main=paste("beta", i), type='l')

apply(b, 1, mean)


hist(b[1,], xlim=c(-2,2))
hist(b[2,], xlim=c(-2,2))
# hist(z[y==0 & x ==0,], xlim=c(-4,4))
# hist(z[y==1 & x ==0,], xlim=c(-4,4))
# hist(z[y==0 & x ==1,], xlim=c(-4,4))
# hist(z[y==1 & x ==1,], xlim=c(-4,4))

