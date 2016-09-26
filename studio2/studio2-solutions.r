#---------------------------------
# Solutions to R problems from 18.05 Studio 2
#---------------------------------

# CLASS EXERCISE 1----
# 1. Run a simulat ion with 1000 trials to estimate 
#       P(Y=6) and P(Y<=6) for a Y \sim binomial(8,.6)

ntosses = 8
ntrials = 5000
phead = .6
trials = rbinom(ntrials, ntosses, phead)
mean(trials == 6) #estimate of P(Y = 6)   --pmf
mean(trials<=6)   #estimate of P(Y <= 6)  --cdf

#---------------------------------------
# CLASS EXERCISE 2 ----
# 2. Use R and the formula for binomial probabilities to compute P(Y=6) exactly

ntosses = 8
phead = .6
choose(ntosses,6)*phead^6*(1-phead)^(ntosses-6)
# This gives 0.2090189

# ALTERNATIVE ONE LINE SOLUTIONS -----
# dbinom() and pbinom()

# Naturally R has a function to do the computation from the previous exercise
# Give the command ?dbinom for R help

# dbinom(values, ntosses, phead ) is the probability mass function (pmf)
# pbinom(values, ntosses, phead ) is the cumulative distribution function (cdf)

# Compute  P(Y=6) for Y ~ Binomial(8,.6), compare with previous answer
dbinom(6, 8, .6)
# Answer = 0.2090189 --same as in exercise 2

# Compute P(Y = 8) for Y ~ Binomial(10,.9)
dbinom(8, 10, .9)
# 0.1937102

# values can be a vector of values, then dbinom returns a vector of probabilities
# This returns 9 probabilities (for k = 0,1,2,...,8)
dbinom(0:8, 8, .6)
# [1] 0.00065536 0.00786432 0.04128768 0.12386304 0.23224320 0.27869184
# [7] 0.20901888 0.08957952 0.01679616

# pbinom(values, ntosses, phead)
#Compute P(Y <= 6) for Y ~ Binomial(8,.6). 
pbinom(6,8,.6)  #cumulative probability
# 0.8936243  

# You can get a vector of probabilities. Note that for the cdf the probabilities
# increase to 1
pbinom(0:8, 8, .6)
# [1] 0.00065536 0.00851968 0.04980736 0.17367040 0.40591360
# [6] 0.68460544 0.89362432 0.98320384 1.00000000


#---------------------------------------
# CLASS EXERCISE 3  ---- 
# A friend has a coin with probability .6 of heads.
# She proposes the following gambling game.
#  You will toss it 10 times and count the number of heads. 
#  The amount you win or lose on k heads is given by k^2 - 7k
# (a) Plot the payoff function.
# (b) Make an exact computation using R to decide if this is a good bet.
# (c) Run a simulation and see that it approximates your computation in part (b)

# (a) Plot the payoff as a function of k
outcomes = 0:10
payoff = outcomes^2 - 7*outcomes
plot(outcomes, payoff, pch=19) # pch=19 tells plot to use solid circles

#----------------
# (b) Compute expected value E(payoff) where payoff = random variable of payoffs
phead = .6
ntosses = 10
outcomes = 0:ntosses
payoff = outcomes^2 - 7*outcomes

#  We can compute the entire vector of probabilities using dbinom
countProbabilities = dbinom(outcomes, ntosses, phead)
countProbabilities  # This is just to take a look at the probabilities

# This is the weighted sum: of probabilities times payoff
expectedValue = sum(countProbabilities*payoff)  
expectedValue    

# This should give expectedValue = -3.6. The bet is not a good one

#----------------
# (c) Simulation --estimate the expected value 
phead = .6
ntosses = 10
ntrials = 1000

# We use rbinom to generate a vector of ntrials binomial outcomes
trials = rbinom(ntrials, ntosses, phead)

# trials is a vector of counts. We can apply the payoff formula to the entire vector
payoffs = trials^2 - 7*trials
mean(payoffs)

#Run the simulation several times to see how well it estimates the expected value -3.6

#----------------


