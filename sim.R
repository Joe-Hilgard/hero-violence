
m <- 3e4

# Sample until p < .05
strat1 <- NULL

for (i in 1:m){
  x <- rnorm(100)
  p <- t.test(x, alternative = "greater")$p.value
  # If p isn't significant, collect more data
  if (p > .05) {
    cat("hack!")
    x <- c(x, rnorm(100))
    p <- t.test(x, alternative = "greater")$p.value
  }
  strat1 <- c(strat1, p)
}

# Abandon if p > .5
strat2 <- NULL

for (i in 1:m) {
  x <- rnorm(100)
  p <- t.test(x, alternative = "greater")$p.value
  # if results are going the right direction, finish data collection
  if (p < .5) {
    print("onward!")
    x <- c(x, rnorm(100))
    p <- t.test(x, alternative = "greater")$p.value
  }
  else print("ditch it!")
  strat2 <- c(strat2, p)
}

mean(strat1 < .05)
mean(strat2 < .05)

# Abandon if not at least marginally significant
strat3 <- NULL

for (i in 1:m) {
  x <- rnorm(100)
  p <- t.test(x, alternative = "greater")$p.value
  # if results are going the right direction, finish data collection
  if (p < .1) {
    print("onward!")
    x <- c(x, rnorm(100))
    p <- t.test(x, alternative = "greater")$p.value
  }
  else {print("ditch it!")}
  strat3 <- c(strat3, p)
}

mean(strat1 < .05)
mean(strat2 < .05)
mean(strat3 < .05)
