#---------------------------------
# Sample code from 18.05 Studio 2
#---------------------------------

# For this studio we will think of a permutation as n people getting out of their seats, running around and then sitting down in a random seat.

# Full permutation of 1:n
n = 5
sample(1:n,n)  # No replacement

# Finding the number of people who sit back in their original seat
n = 5
x = 1:n
y = sample(x,n)
y == x  #This is a list of TRUE, FALSE, each TRUE is someone in their orig. seat
sum(y==x) #The number of TRUE, i.e. the number in their original seat

#---------------------------------------
# rep() and for() loops
ntrials = 10
squares = rep(0,ntrials) #make a list of 0's  ntrials long
squares
for (j in 1:ntrials)     #'loop' through the sequence 1:ntrials
{
    squares[j] = j^2     #store j^2 in squares[j]
}
squares

# Use a 'for loop' to estimate the average number of people who sit in
# their original seat
n = 9
x = 1:n
ntrials = 10000
trials = rep(0,ntrials)
for (j in 1:ntrials)
{
    y = sample(x,n)
    trials[j] = sum(y == x)
}
mean(trials)

#A permutation is a derangement if no one sits in their original seat.
#So 4123 is a derangement but 1342 is not because the 1 is in position 1

# Use a 'for loop' to estimate the probability of a derangement
# their original seat
n = 9
x = 1:n
ntrials = 10000
trials = rep(0,ntrials)
for (j in 1:ntrials)
{
    y = sample(x,n)
    s = sum(y == x)    #s = number of people in their original seat
    trials[j] = (s == 0) #1 if a derangement, 0 if not
}
mean(trials)  #mean(trials) = fraction that are 1's

#---------------------------------------
# set.seed()
# Set the seed for the random number generator so results are reproducible

# This code will always generate the same random numbers
# Run these two lines several times to see they always give the same sequence
# of 10 numbers. 
set.seed(1)
sample(1:5, 6, replace=TRUE) 

# Now see what happens when you don't use set.seed(1) each time.

#---------------------------------------
# rbinom(ntrials, ntosses, phead)

# R makes it easy to generate random binomial values using the function rbinom()

# Generate 10 values from a Binomial(8,.7) random variable
rbinom(10, 8, .7)

# Generate 10 values from a Binomial(8,.2) random variable
rbinom(10, 8, .2)

# Note that when p = .7 the values generated tend to be bigger than when p = .2

# A Bernoulli(p) random variable is just a Binomial with ntosses = 1.
# Generate 10 values from a Bernoulli(.5) random variable
rbinom(10, 1, .5)

#---------------------------------------
# table() and plot()

# Generate 500 Binomial(8,.7) random values.
# Then make a plot of the frequency of each value
phead = .7
ntosses = 8
ntrials = 500
trials = rbinom(ntrials, ntosses, phead)

# table(trials) computes a frequency table:
# Comment: a common practice in R is to use dots in names of variables
trials.frequency = table(trials)   
print(trials.frequency)  #This writes to the screen
## trials.frequency
##  1   2   3   4   5   6   7   8 
##  2   6  26  68 112 158  94  34

# Plot the table --plot() is smart enough just plot the table
plot(trials.frequency)     #

#---------------------------------------

# CLASS EXERCISES
# 1. Run a simulat ion with 1000 trials to estimate 
#       P(Y=6) and P(Y<=6) for a Y \sim binomial(8,.6)
# 2. Use R and the formula for binomial probabilities to compute P(Y=6) exactly
# 3. A friend has a coin with probability .6 of heads.
# She proposes the following gambling game.
#  You will toss it 10 times and count the number of heads. 
#  The amount you win or lose on k heads is given by k^2 - 7k
# (a) Plot the payoff function.
# (b) Make an exact computation using R to decide if this is a good bet.
# (c) Run a simulation and see that it approximates your computation in part (b)

#SOLUTIONS ARE IN ZIP FILE

#---------------------------------------
# dbinom() and pbinom()

# Naturally R has a function to do the computation from the previous exercise
# dbinom(values, ntosses, phead ) is the probability mass function (pmf)
# pbinom(values, ntosses, phead ) is the cumulative distribution function (cdf)

# dbinom(values, ntosses, phead)
# Compute  P(Y=6) for Y ~ Binomial(8,.6), compare with answer in exercise 2.
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


