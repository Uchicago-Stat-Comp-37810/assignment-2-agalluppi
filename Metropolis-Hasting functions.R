# Outputs the probability the given parameter configuration would result in the random y vector
likelihood <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  
  pred = a*x + b
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)   
}

# Returns the result of a likelihood function with the argument as the slope "a" and trueB/trueSd being constant
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}

# Defines a prior distribution for each parameter in the likelihood vector.  The information from these distributions will be added to the likelihood data to generate a posterior distribution for analysis with the M-H function.
prior <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  aprior = dunif(a, min=0, max=10, log = T)
  bprior = dnorm(b, sd = 5, log = T)
  sdprior = dunif(sd, min=0, max=30, log = T)
  return(aprior+bprior+sdprior)
}
# A function for adding the prior and likelihood distributions together to make the posterior distribution.
posterior <- function(param){
  return (likelihood(param) + prior(param))
}

# Returns a random number from a normal distribution with mean = parameter and given sd for each element of the param vector
proposalfunction <- function(param){
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}

# M-H function
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

# Summary function that takes the Markov chain from above, burnIn, nclass, and a parameter (a, b, or sd) as arguments and outputs a plot of the values in the chain and a histogram of the probability distribution of the values
summary <- function(chain, burnIn, nclass, parameter) {
  if (parameter == "a") {
    hist(chain[-(1:burnIn),1],nclass=nclass, main="Posterior of a", xlab="True value = red line" )
    abline(v = mean(chain[-(1:burnIn),1]))
    abline(v = trueA, col="red" )
    plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a", )
    abline(h = trueA, col="red" )
  } 
  if (parameter == "b") {
    hist(chain[-(1:burnIn),2],nclass=nclass, main="Posterior of b", xlab="True value = red line")
    abline(v = mean(chain[-(1:burnIn),2]))
    abline(v = trueB, col="red" )
    plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b", )
    abline(h = trueB, col="red" )
  }
  if (parameter == "sd") {
    hist(chain[-(1:burnIn),3],nclass=nclass, main="Posterior of sd", xlab="True value = red line")
    abline(v = mean(chain[-(1:burnIn),3]) )
    abline(v = trueSd, col="red" )
    plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd", )
    abline(h = trueSd, col="red" )
  }
}

# Function for comparing ten loops of the run_metropolis_MCMC function (this code compares the values output for "a" in each run)
compare_outcomes <- function(iterations) {
  # Create an empty array for the mean and stdev of the Markov chain values for a for each run of the algorithm
  a_data = array(dim = c(2,10))
  rownames(a_data) <- paste(c('mean a', "stdev a"))
  for (i in 1:10) {
    # Select a random start value for each parameter
    startvalue = c(runif(1, 0, 10), runif(1, -1,1),runif(1,0,20))
    chain = run_metropolis_MCMC(startvalue, iterations)
    # Store the mean of the values in the chain in the ith element of the mean row of the a_data array
    a_data[1,i] <- mean(chain[1,])
    # Store the stdev of the values in the chain in the ith element of the stdev row of the a_data array
    a_data[2,i] <- sd(chain[1,])
  }
  return(print(a_data))
}
