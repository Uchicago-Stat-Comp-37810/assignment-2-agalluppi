setwd("~/GitHub/assignment-2-agalluppi")
source("Metropolis-Hasting functions.R")
trueA <- 5
trueB <- 0
trueSd <- 10
sampleSize <- 31

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
# create dependent values according to ax + b + N(0,sd)
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)

plot(x,y, main="Test Data")

# Example: plot the likelihood profile of the slope a
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues)
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")

startvalue = c(4,0,10)
chain = run_metropolis_MCMC(startvalue, 20000)

burnIn = 10000
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))


par(mfrow = c(2,3))
summary(chain, burnIn, 30, "a")
summary(chain, burnIn, 30, "b")
summary(chain, burnIn, 30, "sd")

compare_outcomes <- function(iterations) {
  a_data = array(dim = c(2,10))
  rownames(a_data) <- paste(c('mean', "stdev"))
  for (i in 1:10) {
    startvalue = c(runif(1, 0, 10), runif(1, -1,1),runif(1,0,20))
    chain = run_metropolis_MCMC(startvalue, iterations)
    a_data[1,i] <- mean(chain[1,])
    a_data[2,i] <- sd(chain[1,])
  }
  return(print(a_data))
}
 
