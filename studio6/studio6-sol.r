# studi06-sol.r -- Bayesian updating from studio6dataframe

# load  studio6dataframe ----
source('studio6-loaddata.r')
#This defines: 
# 1. studio6dataframe --the data frame for this studio
# 2. fiveMinuteTimesInHours --a list of all the start times for the 5 minute slots
# getWaitTimesInSeconds() -- a function that returns as list of all the 
# wait times in seconds for one five minute slot on one day. See studio6-loaddata.r 
# for more details.

# Update one day/5 minute time slot ----

# Once we discretize theta the updating code is just like for discrete updating
dt = 20140304 # date = March 4
t = fiveMinuteTimesInHours[8] #Choose an arbitrary time slot
currentData = getWaitTimesInSeconds(dt, t) # get the data for this day/time slot

#Discretize theta -- start range at dtheta instead of 0 because lambda = 1/theta
dtheta = 0.02 
thetaRange = seq(dtheta, 8, dtheta) 

#Uniform prior (function of theta), 
unnormalizedUniformPrior = rep(1,length(thetaRange))
# To normalize we want sum(prior*dtheta) = 1
uniformPrior = unnormalizedUniformPrior/(dtheta*sum(unnormalizedUniformPrior))

originalPrior = uniformPrior

# Initialize matrix whose jth column will store 
# the posterior distribution after the jth update
# We will use this to make plots at the end
ndata = length(currentData)
posteriorMat = matrix(NA, nrow=length(thetaRange), ncol=ndata )

#Update by looping over the data
prior = originalPrior
for (j in 1:ndata)
{
  waitTime = currentData[j]
  likelihood = exp(-waitTime/thetaRange)/thetaRange
  unnormalizedPosterior = likelihood*prior
  normalizedPosterior = unnormalizedPosterior/(dtheta*sum(unnormalizedPosterior))
  posteriorMat[,j] = normalizedPosterior
  
  prior = normalizedPosterior
}

# Find the MAP estimate (maximum a posteriori estimate of theta)
# R has a great function which.max that will give you the index of the maximum in a list
k = which.max(normalizedPosterior)
MAPEstimate = thetaRange[k]
print(MAPEstimate)

#Plot all the posteriors on one graph
thetamin = 0
thetamax = max(thetaRange)
ymin = 0
ymax = max(posteriorMat,originalPrior)
# Create an empty plot with the correct ranges
s = paste('March 4, 2014 at ', t, " hours" )
plottitle = paste(c("Plot of all posteriors (and prior)", s))
plot(c(thetamin,thetamax),c(ymin,ymax), type='n', xlab="theta", ylab="pdf",
      main = plottitle)
lines(thetaRange, originalPrior, col='red')
for (j in 1:ndata)
{
  #R lets you cycle through its list of colors by using col=j
  lines(thetaRange, posteriorMat[,j], col=j)
}

# Now we repeat the above for each time slot. ----
# We only save the final posterior for each time slot

# Once we discretize theta the updating code is just like discrete updating
dt = 20140304 # date = March 4
timeSlots = fiveMinuteTimesInHours # for covenience use an easier name
nslots = length(timeSlots)

#Discretize theta -- start range at dtheta instead of 0 because lambda = 1/theta
dtheta = 0.02 
thetaRange = seq(dtheta, 8, dtheta) 

#Uniform prior (function of theta), 
unnormalizedUniformPrior = rep(1,length(thetaRange))
# To normalize we want sum(prior*dtheta) = 1
uniformPrior = unnormalizedUniformPrior/(dtheta*sum(unnormalizedUniformPrior))

#Quadratic prior (function of theta), 
unnormalizedQuadraticPrior = (4-thetaRange)^2
# To normalize we want sum(prior*dtheta) = 1
quadraticPrior = unnormalizedQuadraticPrior/(dtheta*sum(unnormalizedQuadraticPrior))

# Set the commenting to choose the prior you want
originalPrior = uniformPrior
#originalPrior = quadraticPrior

# Initialize matrix whose jth column will store 
# the final posterior distribution for the jth time slot
# We will use this for analysis and plotting at the end
posteriorMat = matrix(NA, nrow=length(thetaRange), ncol=nslots )

#Loop over each time slot
for (j in 1:nslots)
{
  currentData = getWaitTimesInSeconds(dt, timeSlots[j]) 
  ndata = length(currentData)
  prior = originalPrior
  
  #Update by looping over the data
  
  for (k in 1:ndata)
  {
    waitTime = currentData[k]
    likelihood = exp(-waitTime/thetaRange)/thetaRange
    unnormalizedPosterior = likelihood*prior
    normalizedPosterior = unnormalizedPosterior/(dtheta*sum(unnormalizedPosterior))
    
    prior = normalizedPosterior
  }
  #Only save the final posterior
  posteriorMat[,j] = normalizedPosterior
}

# Analysis and plotting

# Find the MAP estimate (maximum a posteriori estimate of theta) for each time slot
# Plot MAP as function of time
MAPEstimates = rep(0,nslots)
for (j in 1:nslots)
{
  k = which.max(posteriorMat[,j])
  MAPEstimates[j] = thetaRange[k]
}
plot(timeSlots, MAPEstimates, type='l',col='blue', lwd=2, xlab="time (hours from midnight)",ylab="MAP",
     main="March 4, 2014: MAP Estimates for theta")

# You haven't learned to fit curves to noisy data yet, so we'll just do the magic right hear and you can see the results in the orange curve on the plot
fit = lm(MAPEstimates ~ poly(timeSlots,2))
lines(timeSlots, predict(fit,data.frame(x=timeSlots)),col='orange', lwd=2)

