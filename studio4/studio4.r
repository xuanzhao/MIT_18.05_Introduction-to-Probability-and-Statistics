# studio4.r --reminders on plotting and random simulation

# NOTE: in R studio clearing all the plots will reset the graphics parameters back to their default values.

data1 = rnorm(100,2,5) # generate 100 values from N(mu, sigma^2) where mu=2, sigma=5
data2 = rbinom(100,12,0.7) #generate 100 values from binomial(12,0.7)

#drawing a density histogram
hist(data1, freq=FALSE, breaks=12, col='orange')

#add a plot of the pdf of Norm(2,5) to the historgram
x = seq(-15.5, 19.5,.05)  #range of 3.5 sigma around mean
y = dnorm(x,2,5)          #y = f(x) where f = pdf of Norm(2,5)
lines(x,y, col='blue', lwd=2) #add the curve to the current plot

