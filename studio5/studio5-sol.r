####################################################
# studioBayes-solutions.r

# Solutions to R questions from Studio 5.
#
# The solutions require small modifications of the code from studioBayes.r
# What we did was copy that code and put it into a function so we 
# don't need a separate copy for each question. The arguments of the function
# are all the things we might want to change. If you look at the function
# code you should understand the syntax.

#----------------------------
# Turn the studioBayes.r code into a function
studioBayesFunction = function(dice, prior, likelihoodTable, dataRolls, 
                               showIntermediateResults = TRUE, 
                               isCensoredData = FALSE) {
  # This code does Bayesian updating for the probability a certain die was 
  # chosen from the list of dice.
  # dice = list of dice (hypotheses)
  # prior = prior probabilities on the hypotheses
  # likelihoodTable -- each row is the probability table for a hypothesis
  # dataRolls -- random data for rolling a randomly chosen die
  # showIntermediateResults is a boolean to print tables and draw barplots of 
  #   intermediate posteriors, it defaults to TRUE. Setting it to FALSE can 
  #   significantly speed up the code
  # isCensoredData is a boolean used for problem 3. It says the data has been   
  # 'censored'
  # RED_FLAG: we don't check compatibility between the arguments
  # 1. length(dice) == length(prior) == number of rows of likelihoodTable
  # 2. Allowable data = 1 to number of columns of  likelihoodTable
  
  nrolls = length(dataRolls) 
  
  # It's good practice to just plot the data
  plot(dataRolls, xlab="Roll Index", main="Sample of iid Rolls")
  
  if (showIntermediateResults)
  {
    # Plot the prior
    title = "Prior probabilities"
    barplot(prior, col='blue', width=rep(.1,5),xlim=c(0,5),space=3, 
            names=dice, main=title)
  }
  
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
  posteriorMat<-matrix(NA, nrow=length(dice), ncol=nrolls )
  
  #set the first prior
  prior.jroll = prior
  
  # Go throught the updata process for each roll 
  for (jroll in 1:nrolls) {
    x.jroll =dataRolls[jroll]
    
    likelihoodColumn = x.jroll #normally the likelihood column number is the roll
    if (isCensoredData)
    {
      # censored data = 0 corresponds to column 1 in the likelihood table
      # censored data = 1 corresponds to column 2 in the likelihood table
      likelihoodColumn = x.jroll + 1
    }
    likelihood.jroll = likelihoodTable[,likelihoodColumn]
    unnormalizedPosterior.jroll = prior.jroll * likelihood.jroll
    posterior.jroll = unnormalizedPosterior.jroll/sum(unnormalizedPosterior.jroll)
    
    # store the posterior
    posteriorMat[,jroll] = posterior.jroll
    
    if (showIntermediateResults)
    {
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
    }
    # SET THE PRIOR for the next roll
    prior.jroll = posterior.jroll
    
  }
  
  # stacked barplot of the prior/posterior distributions
  # as a function of the number of rolls
  # (cbind --column bind is a easy way to add columns to a matrix)
  allProbs = cbind(prior,posteriorMat)
  barplot(allProbs,legend.text=paste("Sides",dice,sep=""),
          names.arg=c("0(Prior)",c(1:nrolls)), col=rainbow(length(dice)),
          border=NA)
  title(xlab="Number of Rolls")
  title(main="Stacked Barplot of Posterior (Prior) Probabilities")
  # sub-bar heights equal posterior probabilities
} 

#---------------------------- 
# So we can reuse them we define our standard dice and likelihoodTable here
dice=c(4,6,8,12,20)
# Build likelihood table 5 dice x 20 possible outcomes
v = rep(0,100)
standardLikelihoodTable = matrix(v,nrow=5, ncol=20)
# Make the probabilities (rows) for each die separately. 
# We could have done this in a loop
standardLikelihoodTable[1,1:4] = 1/4    #4 sided die
standardLikelihoodTable[2,1:6] = 1/6    #6 sided die
standardLikelihoodTable[3,1:8] = 1/8    #8 sided die
standardLikelihoodTable[4,1:12] = 1/12  #12 sided die
standardLikelihoodTable[5,1:20] = 1/20  #20 sided die
print(standardLikelihoodTable, digits=3)

#--------------------------- 
# Problem 2a: go through the original example from studioBayes.r
nrolls=20  #roll the die nrolls times
prior=c(.2,.2,.2,.2,.2)

# Choose die according to prior distribution, generate data and call studioBayesFunction
set.seed(1)
randomDie=sample(dice,1,prob=prior)
dataRolls = sample(1:randomDie, size=nrolls, replace=TRUE)
studioBayesFunction(dice, prior, standardLikelihoodTable, dataRolls)

# Problem 2b: Use the same data as above and redo using a new prior
prior=c(.05,.05,.05,.05,.80)  ## NEW PRIOR
studioBayesFunction(dice, prior, standardLikelihoodTable, dataRolls)

# Problem 2d: Redo the simulation generating data from a 20-sided die
nrolls=20  #roll the die nrolls times
prior=c(.2,.2,.2,.2,.2)
set.seed(1)
randomDie = 20  # Set the die to the 20 sided die
dataRolls = sample(1:randomDie, size=nrolls, replace=TRUE)
studioBayesFunction(dice, prior, standardLikelihoodTable, dataRolls)

#---------------------------
# 3. Censored data
dice=c(4,6,8,12,20)

# Build the likelihood table
# We define our censored likelihoodTable here
# Build likelihood table 5 dice x 2 possible outcomes
# column 1 is for censored data = 0 (not a 1), column 2 is for censored data = 1
# To select a column we will have to add 1 to the data value
#  e.g if the data is 0 we want to use column 1.
v = rep(0,10)
censoredLikelihoodTable = matrix(v,nrow=5, ncol=2)
censoredLikelihoodTable[1,] = c(3/4, 1/4)  #4 sided die
censoredLikelihoodTable[2,] = c(5/6, 1/6)  #6 sided die
censoredLikelihoodTable[3,] = c(7/8, 1/8)  #8 sided die
censoredLikelihoodTable[4,] = c(11/12, 1/12)  #12 sided die
censoredLikelihoodTable[5,] = c(19/20, 1/20)  #20 sided die
print(censoredLikelihoodTable, digits=3)

nrolls=20  #roll the die nrolls times
prior=c(.2,.2,.2,.2,.2)

# Choose die according to prior distribution
set.seed(1)
randomDie=sample(dice,1,prob=prior)

# Generate data
dataRolls = sample(1:randomDie, size=nrolls, replace=TRUE)

# Censor data
censoredDataRolls = (dataRolls == 1)  #Booleans are 0 or 1
studioBayesFunction(dice, prior, censoredLikelihoodTable, censoredDataRolls,
                    isCensoredData=TRUE)

# Redo with nrolls = 200
nrolls=200
prior=c(.2,.2,.2,.2,.2)
set.seed(1)
randomDie=sample(dice,1,prob=prior)
dataRolls = sample(1:randomDie, size=nrolls, replace=TRUE)
censoredDataRolls = (dataRolls == 1)  #Booleans are 0 or 1
data = censoredDataRolls
# We set showIntermediateResults to FALSE to speed up the code
studioBayesFunction(dice, prior, censoredLikelihoodTable, censoredDataRolls, 
                    showIntermediateResults=FALSE, isCensoredData=TRUE)

# Redo with nrolls = 2000
nrolls=2000
prior=c(.2,.2,.2,.2,.2)
set.seed(1)
randomDie=sample(dice,1,prob=prior)
dataRolls = sample(1:randomDie, size=nrolls, replace=TRUE)
censoredDataRolls = (dataRolls == 1)  #Booleans are 0 or 1
# We set showIntermediateResults to FALSE to speed up the code
studioBayesFunction(dice, prior, censoredLikelihoodTable, censoredDataRolls,
                    showIntermediateResults=FALSE, isCensoredData=TRUE)
