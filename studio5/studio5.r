####################################################
# studioBayes.r

# K-Sided Dice Example from Lecture 11

# Random process: choose among K-sided Dies at random and roll the die
# Conduct Bayesian analysis inferring the chosen die type using outcomes of sample rolls


#--------------------------------
# Updating for one roll
dice=c(4,6,8,12,20)
prior=c(.2,.2,.2,.2,.2)

# Build likelihood table 5 dice x 20 possible outcomes
v = rep(0,100)
likelihoodTable = matrix(v,nrow=5, ncol=20)
# Make the probabilities (rows) for each die separately. 
# We could have done this in a loop
likelihoodTable[1,1:4] = 1/4    #4 sided die; outcomes 5-20 are all 0
likelihoodTable[2,1:6] = 1/6    #6 sided die; outcomes 7-20 are all 0
likelihoodTable[3,1:8] = 1/8    #8 sided die; outcomes 9-20 are all 0
likelihoodTable[4,1:12] = 1/12  #12 sided die; outcomes 13-20 are all 0
likelihoodTable[5,1:20] = 1/20  #20 sided die; 
print(likelihoodTable, digits=3)

title = "Prior probabilities"
barplot(prior, col='blue', width=rep(.1,5),xlim=c(0,5),space=3, 
        names=dice, main=title)

# Choose die according to prior distribution
set.seed(1)
randomDie=sample(dice,1,prob=prior)

# roll the die once
x = sample(1:randomDie, size=1, replace=TRUE)
print(x)

# Choose Likelihood column:
likelihood = likelihoodTable[,x]
print(likelihood)

unnormalizedPosterior = prior*likelihood
posterior = unnormalizedPosterior/sum(unnormalizedPosterior)

title = paste("Posterior probabilities after one roll: roll =", x)
barplot(posterior, col='orange', width=rep(.1,5),xlim=c(0,5),space=3, 
        names=dice, main=title)

#-------------------------------------
# Storing the entire table in a data frame

# Put the Bayes table in a data frame for easy printing
# Data frames have named columns, the syntax is name = values.
# So dice = dice creates a column named 'dice' with values from the dice vector
# Likewise someothername = dice creates a column called 'someothername' with
# values from the dice vector
# 
bayesTable=data.frame(
    dice=dice, 
    prior=prior, 
    likelihood=likelihood, 
    posterior.prenormalize=unnormalizedPosterior, 
    posterior.normalized=posterior
)
title = paste("Bayes table after one roll: roll =", x)
print(title)
print(bayesTable)

#--------------------------------
# Roll die repeatedly
# Just to keep this part self-contained we repeat the setup above
nrolls=20  #roll the die nrolls times
dice=c(4,6,8,12,20)
prior=c(.2,.2,.2,.2,.2)
# Build likelihood table 5 dice x 20 possible outcomes
v = rep(0,100)
likelihoodTable = matrix(v,nrow=5, ncol=20)
# Make the probabilities (rows) for each die separately. 
# We could have done this in a loop
likelihoodTable[1,1:4] = 1/4    #4 sided die
likelihoodTable[2,1:6] = 1/6    #6 sided die
likelihoodTable[3,1:8] = 1/8    #8 sided die
likelihoodTable[4,1:12] = 1/12  #12 sided die
likelihoodTable[5,1:20] = 1/20  #20 sided die

# Choose die according to prior distribution
set.seed(1)
randomDie=sample(dice,1,prob=prior)
  
# Get all the data at once
dataRolls = sample(1:randomDie, size=nrolls, replace=TRUE)

# It's good practice to just plot the data to see if there are obvious problems
plot(dataRolls, xlab="Roll Index", main="Sample of iid Rolls")

# Plot the prior
title = "Prior probabilities"
barplot(prior, col='blue', width=rep(.1,5),xlim=c(0,5),space=3, 
        names=dice, main=title)

# Create the data frame to hold  our computations
bayesTable=data.frame(
    dice=dice, 
    prior=NA, 
    likelihood=NA, 
    posterior.prenormalize=NA, 
    posterior.normalized=NA)

# Initialize matrix whose jth column will store 
# the posterior distribution after updating the jth roll
# We will use this to make a nifty stacked bar plot at the end
posteriorMat = matrix(NA, nrow=length(dice), ncol=nrolls )

#set the first prior
prior.jroll = prior

# Go throught the updata process for each roll 
for (jroll in 1:nrolls){
  x.jroll =dataRolls[jroll]
  
  likelihood.jroll = likelihoodTable[,x.jroll]
  unnormalizedPosterior.jroll = prior.jroll * likelihood.jroll
  posterior.jroll = unnormalizedPosterior.jroll/sum(unnormalizedPosterior.jroll)
  
  # store the posterior
  posteriorMat[,jroll] = posterior.jroll
  
  # plot the posterior
  title = paste("Posterior probabilities after roll ", jroll, ": roll =", x.jroll)
  barplot(posterior.jroll, col='orange', width=rep(.1,5),xlim=c(0,5),space=3, 
          names=dice, main=title)
  
  # put the table in our data frame for easy printing
  bayesTable[,"prior"] = prior.jroll
  bayesTable[,"likelihood"] = likelihood.jroll
  bayesTable[,"posterior.prenormalize"] = unnormalizedPosterior.jroll
  bayesTable["posterior.normalized"] = posterior.jroll

  title = paste("Bayes table after one roll", jroll, ": roll =", x.jroll)
  print(title)
  print(bayesTable)
  
  # SET THE PRIOR for the next roll
  prior.jroll = posterior.jroll
  
}
  
# stacked barplot of the prior/posterior distributions
# as a function of the number of rolls
# (cbind --column bind is a easy way to add columns to a matrix)
allProbs = cbind(prior,posteriorMat)
barplot(allProbs,legend.text=paste("Sides",dice,sep=""),
        names.arg=c("0(Prior)",c(1:nrolls)), col=rainbow(length(dice)))
title(xlab="Number of Rolls")
title(main="Stacked Barplot of Posterior (Prior) Probabilities")
# sub-bar heights equal posterior probabilities
####################################
