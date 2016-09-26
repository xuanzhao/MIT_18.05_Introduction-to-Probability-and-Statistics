# studi06.r -- load the data and show some analysis

# load the data and test things out ----
source('studio6-loaddata.r')
#This defines: 
# 1. studio6dataframe --the data frame for this studio
# 2. fiveMinuteTimesInHours --a list of all the start times for the 5 minute slots
# getWaitTimesInSeconds() -- a function that returns as list of all the 
# wait times in seconds for one five minute slot on one day. See studio6-loaddata.r 
# for more details.
  
teststudio6 = function()
{
  print(head(fiveMinuteTimesInHours))
  print(head(studio6dataframe))
  x = getWaitTimesInSeconds(20140303, fiveMinuteTimesInHours[6])
  print(head(x))
  
  opar = par('mfrow'=c(2,1))
  plot(x, main='Times between trades',ylab="Time in seconds")
  hist(x,breaks=0:12, main="Histogram of time between trades", xlab="Time in seconds")
  par(opar)
}
teststudio6()

# Explore the hypothesis the waiting times in the 5 minute slots are exponential ----
# by plotting histograms
print(length(fiveMinuteTimesInHours))

dt = 20140303

t = fiveMinuteTimesInHours[1]
x = getWaitTimesInSeconds(dt, t)
opar = par('mfrow'=c(2,1))
plottitle = paste('Times between trades:',dt, ", t =", t, sep=' ')
plot(x, main=plottitle, xlab="Time in seconds")
breaks = 0:(floor(max(x)+1))
histtitle = paste("Histogram for",dt, ': t =', t, sep=' ')
hist(x,breaks=breaks, main=plottitle, xlab="Time in seconds")
par(opar)

t = fiveMinuteTimesInHours[2]
x = getWaitTimesInSeconds(dt, t)
opar = par('mfrow'=c(2,1))
plottitle = paste('Times between trades:',dt, ", t =", t, sep=' ')
plot(x, main=plottitle, xlab="Time in seconds")
breaks = 0:(floor(max(x)+1))
histtitle = paste("Histogram for",dt, ': t =', t, sep=' ')
hist(x,breaks=breaks, main=plottitle, xlab="Time in seconds")
par(opar)

#Do a bunch at once
for (n in c(10,25,35,50,60,70,77,78))
{
  t = fiveMinuteTimesInHours[n]
  x = getWaitTimesInSeconds(dt, t)
  opar = par('mfrow'=c(2,1))
  plottitle = paste('Times between trades:',dt, ", t =", t, sep=' ')
  plot(x, main=plottitle, xlab="Time in seconds")
  breaks = 0:(floor(max(x)+1))
  histtitle = paste("Histogram for",dt, ': t =', t, sep=' ')
  hist(x,breaks=breaks, main=plottitle, xlab="Time in seconds")
  par(opar)
}
