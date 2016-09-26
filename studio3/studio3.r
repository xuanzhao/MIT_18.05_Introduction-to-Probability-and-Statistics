# rnorm, dnorm, pnorm, qnorm

#rnorm generates random values
# rnorm(n, mu, sigma) gives us n random values from Norm(mu, sigma^2)
rnorm(5,0,1)
# Test the .68, .95 rules
x = rnorm(10000,0,1)
mean(abs(x) < 1)
mean(abs(x) < 2)

#dnorm is the pdf it gives the values of the density at x
x = 0
dnorm(x,0,1)
x = c(0,.2,.4,.6,.8,1.0)
dnorm(x,0,1)
dnorm(x,0,2)
#Plot the standard normal density
z = seq(-6,6,.01)  #sequence from -6 to 6 in steps of .1
plot(z,dnorm(z,0,1), type='l', col='blue', lwd=3)
#add some other densities
lines(z,dnorm(z,1,1), col='red', lwd=3)
lines(z,dnorm(z,0,2), col='green', lwd=3)

#pnorm is the cdf
x = 0
pnorm(x,0,1)
x = c(0,.2,.4,.6,.8,1.0)
pnorm(x,0,1)
pnorm(x,0,2)
#Plot the standard normal cdf
z = seq(-6,6,.01)  #sequence from -6 to 6 in steps of .1
plot(z,pnorm(z,0,1), type='l', col='blue', lwd=3)
#add some other cdf's
lines(z,pnorm(z,1,1), col='red', lwd=3)
lines(z,pnorm(z,0,2), col='green', lwd=3)

#qnorm is the inverse of the cdf it takes a probability and produces a value
#a = qnorm(p, 0,1)  <==> P(Z<a) = p <===> p = pnorm(a,0,1)
qnorm(.5,0,1)
p = pnorm(.351,0,1)
p
a = qnorm(p,0,1)
a

#There is also runif(x,a,b) and rexp(x, lambda)
#plot the pdf of exponential(.5)
x = seq(0,10,.01)
plot(x,dexp(x,.5), type='l', col='blue', lwd=3)
abline(v=0)
abline(h=0)

#Histograms
data = rexp(100,.5)
hist(data)

#controlling the number of bins
#breaks = a single number gives the number of bins 
#(actually this just 'suggests' the number of bins. I'm not sure how R chooses the exact number)
data = rexp(100,.5)
hist(data, breaks=12)
hist(data, breaks=100)
hist(data, breaks=2)

#breaks = a vector gives the edges of each bin
data = rexp(100,.5)
binwidth = .4
bins = seq(min(data), max(data)+binwidth, binwidth)
hist(data, breaks=bins, col='purple')

#By default you get a frequency histogram. 
#To get a density you set freq=FALSE
data = rexp(100,.5)
binwidth = .4
bins = seq(min(data), max(data)+binwidth, binwidth)
hist(data, breaks=bins, col='purple', freq=FALSE)

#With enough data the densitiy histogram should look like the pdf
lambda = .5
data = rexp(10000,lambda)
binwidth = .3
bins = seq(min(data), max(data)+binwidth, binwidth)
hist(data, breaks=bins, col='green', freq=FALSE)
x = seq(0,max(data)+1,.01)
lines(x,dexp(x,lambda), col='purple', lwd=4)

