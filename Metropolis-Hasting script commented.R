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

# Defines the function proposalfunction(param), which returns a random number from a normal distribution with mean = parameter and given sd for each element of the param vector
proposalfunction <- function(param){
  # The return of proposalfunction() described above.  This is used as the next set of proposed parameter values in the MCMC function later.
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}

# Defines the M-H function whose arguments are starting inputs from the startvalue vector for each parameter and the number of iterations of the function to run
run_metropolis_MCMC <- function(startvalue, iterations){
  # Defines an empty "chain" matrix with (iterations + 1) columns and three rows (one for each parameter) 
  chain = array(dim = c(iterations+1,3))
  # Puts the elements in startvalue as the first column of the chain matrix
  chain[1,] = startvalue
  # Loops the following for the number of iterations
  for (i in 1:iterations){
    # Defines a proposal vector using the proposalfunction() function above--generates three random numbers from normal distributions around the ith elements in chain
    proposal = proposalfunction(chain[i,])
    # Defines a vector "probab" which uses the posterior function to compute the ratio of probabilites of the proposed parameter value and the ith element in chain
    # The exponential is to return the probabilities as raw values rather than the log values from before
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    # If probab is greater than a random number between 0 and 1... (this encompases if the probability of the proposed value is greater than the probability of the ith element in chain)
    if (runif(1) < probab){
    # ...the proposed value becomes the next entry in that row of the chain matrix
      chain[i+1,] = proposal
    # Otherwise...
    }else{
    # ...the next entry in the row of the chain matrix is the same as the previous one.
      chain[i+1,] = chain[i,]
    }
  }
  # Return chain after all iterations--this is the Markov chain that shows values in the probability distribution of each parameter
  return(chain)
}
# Defines the starting values for each parameter for the function above
startvalue = c(4,0,10)
# Runs the M-H function with startvalue as the initial conditions and with 10,000 iterations
chain = run_metropolis_MCMC(startvalue, 10000)
# Sets variable burnIn to 5000--future lines of code exclude elements of chain from 1:burnIn because early iterations of the M-H function are more biased than later ones
burnIn = 5000

# Defines an acceptance vector as the frequency of accepted proposals for each parameter.  This leverages the fact that a duplicated value will appear in the chain if a proposal is not accepted.
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))

# Calls a graphical interface with a 2x3 array of graphs
par(mfrow = c(2,3))
# Creates a histogram of the entries in the first row of the chain matrix (i.e. the accepted prosposed values of a, noting that the first values up to burnIn are exluded).  30 bins, title is "Posterior of a", x-axis label is "True value = red line"
hist(chain[-(1:burnIn),1],nclass=30, main="Posterior of a", xlab="True value = red line" )
# Adds a vertical line to the above histogram at the value of the mean for all the relevant entries in the class matrix.
abline(v = mean(chain[-(1:burnIn),1]))
# Adds a red vertical line to the above histogram at the true value of a as input at the beginning of the code
abline(v = trueA, col="red" )
# Creates a histogram of the entries in the second row of the chain matrix (i.e. the accepted prosposed values of b, noting that the first values up to burnIn are exluded).  30 bins, title is "Posterior of b", x-axis label is "True value = red line"
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")
# Adds a vertical line to the above histogram at the value of the mean for all the relevant entries in the class matrix.
abline(v = mean(chain[-(1:burnIn),2]))
# Adds a red vertical line to the above histogram at the true value of a as input at the beginning of the code
abline(v = trueB, col="red" )
# Creates a histogram of the entries in the third row of the chain matrix (i.e. the accepted prosposed values of sd, noting that the first values up to burnIn are exluded).  30 bins, title is "Posterior of sd", x-axis label is "True value = red line"
hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")
# Adds a vertical line to the above histogram at the value of the mean for all the relevant entries in the class matrix.
abline(v = mean(chain[-(1:burnIn),3]))
# Adds a red vertical line to the above histogram at the true value of a as input at the beginning of the code
abline(v = trueSd, col="red" )
# Creates a plot of the entries in the first row of the chain matrix (i.e. the accepted prosposed values of a, noting that the first values up to burnIn are exluded), each point connected by a line.  Title is "Chain values of a," x-axis label is "True value = red line"
plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a", )
# Adds a red horizontal line to the above plot at the true value of a as input at the beginning of the code
abline(h = trueA, col="red" )
# Creates a plot of the entries in the second row of the chain matrix (i.e. the accepted prosposed values of b, noting that the first values up to burnIn are exluded), each point connected by a line.  Title is "Chain values of b," x-axis label is "True value = red line"
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b", )
# Adds a red horizontal line to the above plot at the true value of b as input at the beginning of the code
abline(h = trueB, col="red" )
# Creates a plot of the entries in the third row of the chain matrix (i.e. the accepted prosposed values of sd, noting that the first values up to burnIn are exluded), each point connected by a line.  Title is "Chain values of sd," x-axis label is "True value = red line"
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd", )
# Adds a red horizontal line to the above plot at the true value of sd as input at the beginning of the code
abline(h = trueSd, col="red" )

# As a comparison, this function outputs a summary of a linear fit model between elements in the y and x vectors to see how closely they are correlated
summary(lm(y~x))
