# studio12.r

# Bootstrap problem --------------------------
# ------------------------------------- #
# Sample code for finding a bootstrap confidence interval for the mean 
# We use simulated data

# Simulate some data: we arbitrarily get it from an exponential(2) distribution.
# with sample size 97
data = rexp(97, 2)

# Plot the data to see that it appears reasonable
plot(data, col="orange", type='l', lwd=2, ylab="data")
title("Plot of the data")

#find the sample mean 
sampleMean = mean(data)

# Generate a lot of bootstrap mean differences and use them to compute delta*.
nboot = 5000
ndata = length(data)

deltaStar = rep(0,nboot)
for (j in 1:nboot)
{
  dataStar = sample(data, ndata, replace=TRUE)
  deltaStar[j] = mean(dataStar) - sampleMean
}
# Pick out the 0.025 and 0.975 quantiles of deltaStar
dstar_025 = quantile(deltaStar, 0.025)  
dstar_975 = quantile(deltaStar, 0.975)

# Since dstar_975 must be > dstar_025, the lower limit of the confidence interval
# must be sampleMean - dstar_975
confint = sampleMean - c(dstar_975, dstar_025)

cat("Sample mean difference = ", sampleMean, "\n")
s = sprintf("95%% bootstrap confidence interval = [%.3f, %.3f]\n", confint[1],confint[2])
cat(s)

# ------------------------------------- #
# Sample code for reading in the salary data
salarydata = read.csv("salaries.csv")
head(salarydata)

salaries1 = salarydata[,"Salaries.1"]
salaries2 = salarydata[,"Salaries.2"]

plot(salaries1, col="orange", type='l', lwd=2, ylab="Salaries")
lines(salaries2, col='blue', lwd=2)
title("Plots of salaries1 and salaries2")

# ------------------------------------- #
# Financial data problem: linear regression ---
# Read financial data
s12data.daily = read.csv('studio12financialDaily.csv')
print(head(s12data.daily))
print(tail(s12data.daily))
typeof(s12data.daily)
print(dim(s12data.daily))

# Extract columns
SP500.daily = s12data.daily[,"SP500.daily"]
GE.daily = s12data.daily[,"GE.daily"]
DCOILWTICO.daily = s12data.daily[,"DCOILWTICO.daily"]

# Set colors
GE.col = 'cyan'
SP500.col = 'blue'
scatter.col = rgb(1,.643,.224,alpha=.5)

# Linear regression of GE.daily vs, SP500.daily
plot(DCOILWTICO.daily, GE.daily, pch=19, cex=.5, col=scatter.col)
abline(h=0,v=0)

# This fits the model DCOILWTICO.daily = a + b1*SP500.daily
lmfit = lm(DCOILWTICO.daily ~ SP500.daily)
abline(lmfit,col='blue', lwd=2)
print(summary(lmfit))
# Using numbers in lmfit we can write the title --this could be automated
title(c("Linear fit of DCOILWTICO.daily vs SP500,daily", "slope = 0.33, intercept = 0"))

# ------------------------------------- #
# Financial data problem: multiple linear regression ---
# Try to make sense of the output of the following multiple linear regression
# Linear regression of GE.daily vs, (SP500.daily, DCOILWTICO.daily)

# This fits the model GE.daily = a + b1*SP500.daily + b2*DCOILWTICO.daily
lmfit = lm(GE.daily ~ SP500.daily + DCOILWTICO.daily)
print(summary(lmfit))
