#Studio 3 answers to coding problems

# 1. Generate a frequency histogram of 1000 samples from an exp(1) random variable.
lambda = 1
data = rexp(1000,lambda)
binwidth = .5
bins = seq(min(data), max(data)+binwidth, binwidth)
hist(data, breaks=bins, col='yellow', freq=TRUE)

#2. Generate a density histogram for the average of 2 independent exp(1) random variable.
lambda = 1
data1 = rexp(1000,lambda)
data2 = rexp(1000,lambda)
aveData = (data1+data2)/2
binwidth = .4
bins = seq(min(aveData), max(aveData)+binwidth, binwidth)
hist(aveData, breaks=bins, col='yellow', freq=TRUE)


#3. Using \texttt{rexp(), matrix() and colMeans()} generate a density histogram for 
# the average of 50 independent exp(1) random variables.
lambda = 1
nexponentials = 50
ntrials = 10000
x = rexp(ntrials*nexponentials,lambda)
data = matrix(x, nrow=nexponentials, ncol=ntrials)
aveData = colMeans(data)
binwidth = .05
bins = seq(min(aveData), max(aveData)+binwidth, binwidth)
hist(aveData, breaks=bins, col='yellow', freq=FALSE)

#4. Superimpose a graph of the pdf of  Norm(1, 1/50) on your plot in problem 3.
# Remember our notation differs from R. We write Norm(mu, sigma^2) and R uses 
# rnorm(x, mu, sigma)
x = seq(-2,4,.01)
lines(x,dnorm(x,1,1/sqrt(50)), col='red',lwd=3)
