# Sets slope of linear relationship between x and y (y = Ax+B)
trueA <- 5
# Sets y-intercept of linear relationship between x and y
trueB <- 0
# Sets standard deviation of normal distribution used to generate random noise in y values
trueSd <- 10
# Sets number of data points
sampleSize <- 31

# Generates a vector of x values from [-N/2, N/2] where N is the sample size.
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
# Generates a vector of y values that deviate randomly from the above linear relationship between x and y by adding a random number from a normal distribution with std dev = trueSd
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)

# Plots the generated x and y vectors against each other in a plot where the title is "Test Data"
plot(x,y, main="Test Data")

# Defines a function likelihood(param) which outputs the probability the given parameter configuration would result in the random y vector above
likelihood <- function(param){
  # Defines the first element of the param vector as "a" (i.e. slope)
  a = param[1]
  # Defines the second element of the param vector as "b" (i.e. intercept)
  b = param[2]
  # Defines the third element of the param vector as "sd" (i.e. standard deviation)
  sd = param[3]
  
  # Define a vector "pred" as the exact y values for each x in the sample given the inital conditions in param
  pred = a*x + b
  # Generates a vector in which each element is the log(probability) that the deviation from "pred" in the corresponding element of the randomly varied y vector would occur
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
  # Sums the values of all the elements in the likelihoods vector to determine the probability of generating this permutation of the y vector
  # Note that summing makes sense because the values in singlelikelihoods are logarithms, so summing these values is analagous to multiplying the probabilities
  sumll = sum(singlelikelihoods)
  # Returns sumll as the output of the likelihood function
  return(sumll)
}

# Leverages the above likelihood function to find the most likely value of slope for the data vectors
# Defines a slopevalues() function which returns the result of a likelihood function with the argument as the slope "a" and trueB/trueSd being constant
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}
# Applies the slopevalues function for each "a" from 3 to 7 in intervals of 0.5 and stores each result as an element in the vector "slopelikelihoods"
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues )
# Plots slopelikelihoods vs. argument "a" as a smooth line connecting the values.  The x-axis label is "values of slope parameter a" and the y-axis label is "Log likelihood"
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")

# A function that defines a prior distribution for each parameter in the likelihood vector.  The information from these distributions will be added to the likelihood data to generate a posterior distribution for analysis with the M-H function.
prior <- function(param){
  # Defines the first element of the param vector as "a" (i.e. slope)
  a = param[1]
  # Defines the second element of the param vector as "b" (i.e. intercept)
  b = param[2]
  # Defines the third element of the param vector as "sd" (i.e. standard deviation)
  sd = param[3]
  # Defines a distribution "aprior" as a uniform distribution of "a" bounded from 0 to 10 on a log scale
  aprior = dunif(a, min=0, max=10, log = T)
  # Defines a distribution "bprior" as a uniform distribution of "b" with a standard deviation of 5 on a log scale
  bprior = dnorm(b, sd = 5, log = T)
  # Defines a distribution "sdprior" as a uniform distribution of "sd" bounded from 0 to 30 on a log scale
  sdprior = dunif(sd, min=0, max=30, log = T)
  # Returns the sum of the three distributions (the distributions are summed because they are on a log scale.)
  return(aprior+bprior+sdprior)
}

# A function for adding the prior and likelihood distributions together to make the posterior distribution.
posterior <- function(param){
  # Returns the sum of the two distributions to form the posterior distribution (again, adding because of the log scale)
  return (likelihood(param) + prior(param))
}

proposalfunction <- function(param){
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}

run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,3))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}

startvalue = c(4,0,10)
chain = run_metropolis_MCMC(startvalue, 10000)

burnIn = 5000
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))

### Summary: #######################

par(mfrow = c(2,3))
hist(chain[-(1:burnIn),1],nclass=30, , main="Posterior of a", xlab="True value = red line" )
abline(v = mean(chain[-(1:burnIn),1]))
abline(v = trueA, col="red" )
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),2]))
abline(v = trueB, col="red" )
hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),3]) )
abline(v = trueSd, col="red" )
plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a", )
abline(h = trueA, col="red" )
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b", )
abline(h = trueB, col="red" )
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd", )
abline(h = trueSd, col="red" )

# for comparison:
summary(lm(y~x))