# studio12.r

# 2. Bootstrap problem ------------------------------
# Find a 95% bootstrap confidence interval for the 
# difference of the means for the two columns of salaries.csv
# Read the data
salarydata = read.csv("salaries.csv")
head(salarydata)

salaries1 = salarydata[,"Salaries.1"]
salaries2 = salarydata[,"Salaries.2"]

# Plot the data to see that it appears reasonable
plot(salaries1, col="orange", type='l', lwd=2, ylab="Salaries")
lines(salaries2, col='blue', lwd=2)
title("Plots of salaries1 and salaries2")

#find the sample mean difference
sampleMeanDiff = mean(salaries1) - mean(salaries2)

# Generate a lot of bootstrap mean differences and use them to compute delta*.
nboot = 5000
nsalaries1 = length(salaries1)
nsalaries2 = length(salaries2)

deltaStar = rep(0,nboot)
for (j in 1:nboot)
{
  salaries1Star = sample(salaries1, nsalaries1, replace=TRUE)
  salaries2Star = sample(salaries2, nsalaries2, replace=TRUE)  
  deltaStar[j] = (mean(salaries1Star) - mean(salaries2Star)) - sampleMeanDiff
}
# Pick out the 0.025 and 0.975 right critical valus of deltaStar
dstar_025 = quantile(deltaStar, 0.975)
dstar_975 = quantile(deltaStar, 0.025)

confint = sampleMeanDiff - c(dstar_025, dstar_975)

cat("Sample mean difference = ", sampleMeanDiff, "\n")
cat("95% bootstrap confidence interval = [", confint[1], ", ", confint[2], "]\n", sep="")


# Financial data problem: linear regression ----------------------------

# Read financial data
s12data.daily = read.csv('studio12financialDaily.csv')
print(head(s12data.daily))
print(tail(s12data.daily))
print(dim(s12data.daily))

# Extract columns
SP500.daily = s12data.daily[,"SP500.daily"]
GE.daily = s12data.daily[,"GE.daily"]
DCOILWTICO.daily = s12data.daily[,"DCOILWTICO.daily"]

#color
scatter.col = rgb(1,.643,.224,alpha=.5)

# Linear regression of GE.daily vs, SP500.daily
# This fits the model GE.daily = a + b1*SP500.daily
plot(SP500.daily, GE.daily, pch=19, cex=.5, col=scatter.col)
abline(h=0,v=0)
lmfit = lm(GE.daily ~ SP500.daily)
abline(lmfit,col='blue', lwd=2)
print(summary(lmfit))
# Using numbers in lmfit we can write the title --this could be automated
title(c("Linear fit of GE.daily vs SP500,daily", "slope = 1.2, intercept = 0"))


     