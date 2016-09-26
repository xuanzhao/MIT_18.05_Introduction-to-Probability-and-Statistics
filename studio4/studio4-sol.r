#studio4-sol.r
# Contains solutions to studio 4 problems

#Problem 1 ----
# Let jill.winnings be Jill's winnings and jack.winnings be Jack's winnings.

# 1(a) E(X) = sum x_i*p(x_i) where x_i are the possible values of X.
# On one bet the probability of winning $1 is 18/38 and the probability of 
# losing is $1
oneBet.values = c(1,-1)
oneBet.pmf = c(18/38,20/38)
oneBet.expected = sum(oneBet.values*oneBet.pmf)

jill.expected = 15*oneBet.expected
jack.expected = 10*oneBet.expected
print(jill.expected)
print(jack.expected)

#1(b) variance(X) = E((X-E(X))^2)
oneBet.variance = sum( (oneBet.values - oneBet.expected)^2 * oneBet.pmf )
jill.variance = 15*oneBet.variance
jack.variance = 10*oneBet.variance
print(oneBet.variance)
print(jill.variance)
print(jack.variance)

#1(c) By linearity Cov(jill.winnings, jack.winnings) = 10*Var(oneBet)
jilljack.covariance = 10*oneBet.variance
jilljack.correlation = jilljack.covariance/sqrt(jill.variance*jack.variance)
print(jilljack.covariance)
print(jilljack.correlation)

#1d
jillTotal.expected = 100*jill.expected
jackTotal.expected = 100*jack.expected
jillTotal.variance = 100*jill.variance
jackTotal.variance = 100*jack.variance
jilljackTotal.covariance = 100*jilljack.covariance
jilljackTotal.correlation = jilljack.correlation
print(jillTotal.expected)
print(jackTotal.expected)
print(jillTotal.variance)
print(jillTotal.variance)
print(jilljackTotal.covariance)
print(jilljackTotal.correlation)

#1e see studio4-solutions.pdf

#Problem 2 ----
#2a
ntrials = 5000
probWin = 18/38
jack.nplays = 1000
# Over 100 days jack makes 100*10 = 1000 bets. The number of winning bets follows a 
# binomial(1000, 18/38) distribution
# So we start the simulation of 5000 trials by 5000 draws from a binomial(1000,18/38) distribution
jack.wins = rbinom(ntrials, jack.nplays, probWin)

#To compute Jack's total wins we have to subtract losses from wins
jack.losses = jack.nplays - jack.wins
jack.totalWinnings = jack.wins-jack.losses  #list with 5000 entries

hist(jack.totalWinnings, freq=FALSE,col='orange', breaks=20)

#2b See studio4-solutions.pdf

#2c
mu = 100*jack.expected
sigma = sqrt(100*jack.variance)
print(mu)
print(sigma)
# Most of the probability is within 3.5*sigma of mu
x = seq(mu-3.5*sigma, mu+3.5*sigma, 0.1)
y = dnorm(x,mu,sigma)
lines(x,y, col='blue', lwd=2)

#2d
interval95.low = mu - 2*sigma
interval95.high = mu + 2*sigma
interval95 = c(interval95.low, interval95.high)
print(interval95)

#2e We use boolean logic 
# first an array of 0's and 1's indicating if winnings in a trial is >interval95.low
trialsAboveLow = jack.totalWinnings > interval95.low
# then an array of 0's and 1's for trials < interval95.high
trialsBelowHigh = jack.totalWinnings < interval95.high
# By multiplying these together the terms that are 1 indicate trials that are in inteval95
trialsInInterval95 = trialsAboveLow*trialsBelowHigh
# Since trialsInInterval95 consists of 0 and 1's its mean is the fraction in the interval
fractionInInterval95 = mean(trialsInInterval95)
print(fractionInInterval95)

#Problem 3 ----

#3a We use nTrials and probWin from 2a
# We need to be careful here. Each day Jill plays with Jack for all the rounds
# Jack plays. Therefore Jill's total winnings are Jack's + the winnings from the extra 500 rounds she play every 100 days
# 
jill.additionalPlays = 500
jill.additionalWins = rbinom(ntrials, jill.additionalPlays, probWin)

#To compute Jack's total wins we have to subtract losses from wins
jill.additionalLosses = jill.additionalPlays - jill.additionalWins
jill.additionalTotalWinnings = jill.additionalWins-jill.additionalLosses
jill.totalWinnings = jack.totalWinnings + jill.additionalTotalWinnings

plot(jack.totalWinnings, jill.totalWinnings, pch=19, cex=.5,
     col=rgb(1,.643,.224,alpha=.2))

#3b
simulatedCorrelation = cor(jill.totalWinnings, jack.totalWinnings)
print(simulatedCorrelation)
