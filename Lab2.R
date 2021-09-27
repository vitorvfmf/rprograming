rm(list=ls()) 
getwd()
library(plotrix)

Nsim <- 1000             # the number of simulations
N <- 1000               # sample size
x_bar <- rep(NA, Nsim)     # default values for sample mean
sigma2_x <- rep(NA, Nsim)  # default values for sample variance

for (i in 1:Nsim){                   # for loop: repeat 4.2 Nsim times
  x.data <- runif(N, min=0, max=2)   # generate a sample with size N
  x_bar[i] <- mean(x.data)              # record the sample mean for sample i
  sigma2_x[i] <- var(x.data)            # record the sample variance for sample i
  mean(x.data)
  sd(x.data)
  }   




mean(x_bar)
sd(x_bar)
var(x_bar)

std.error(x_bar)


hist(x_bar, breaks=24, freq = FALSE)

