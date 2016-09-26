# Studio 11 solution code

# ----------------------- #
# Compute the 90% confidence interval for sigma^2 for simulated N(2, 3^2) data ----
mu = 2
sigma = 3
n = 17
alpha = 0.10

x = rnorm(n,mu,sigma)
s2 = var(x)
c.left = qchisq(alpha/2, n-1)    #left critical point
c.right = qchisq(1-alpha/2, n-1) #right critical point
confint = (n-1)*s2/c(c.right, c.left )
print(confint)

# ----------------------- #
# Stock market volatility ----
# Read the data
studio11SP500data = read.csv("studio11SP500data.csv")
head(studio11SP500data)
tail(studio11SP500data)
dim(studio11SP500data)

# ----- Extract the PctReturn for Mondays (day=1) and Fridays (day=5) ------------#
b = studio11SP500data[,"dayofweek"]==1
pctReturn.monday = studio11SP500data[b,"PctReturn"]

b = studio11SP500data[,"dayofweek"]==5
pctReturn.friday = studio11SP500data[b,"PctReturn"]

# ------ Use var.test to compare volatility
alpha = .05
res = var.test(pctReturn.monday, pctReturn.friday, alternative="two.sided")
print(res)
# This gives the 95% confidence interval for the ratio of the variances is
# [.27, 18.5]. So, it would appear that Mondays are more volatile than Fridays,
# i.e. we reject the null hypothesis that the ratios are the same.
#

# ------- Compute the F stat and confidence interval directly.
df1 = length(pctReturn.monday) - 1
df2 = length(pctReturn.friday) -1

F_stat = var(pctReturn.monday)/var(pctReturn.friday)
# Compute critical points
c_alpha2 = qf(1-alpha/2, df1, df2)
c_1minusalpha2 = qf(alpha/2, df1, df2)
confInt = F_stat/c(c_alpha2, c_1minusalpha2)
print(F_stat)
print(confInt)
# We get the same answers as with var.test


