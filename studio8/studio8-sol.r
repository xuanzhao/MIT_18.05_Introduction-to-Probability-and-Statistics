#Studio 8 solutions to R questions

# ------------------------- #
# Problem 2: Two sample t-tests: class heights ----
# Heights above 5 feet. ---- Take data from left and right sides of the room
# ******* Since this was written ahead of class, this is not the same as the data from class.
cat("\n-------- Problem 2: Two-sample t-tests: class heights -----------------\n")
leftSideOfRoom = c(10,12,13,12,9,8,16,12,22,13,8)
rightSideOfRoom = c(6,7,6,4,5,0,11,5,9,5,5,3,5,5,3,3,7,8,4,7,3,3)

mean.L = mean(leftSideOfRoom)
var.L = var(leftSideOfRoom)
n.L = length(leftSideOfRoom )

mean.R = mean(rightSideOfRoom)
var.R = var(rightSideOfRoom)
n.R = length(rightSideOfRoom)

#Print the stats
cat("left side: mean = ",mean.L, ", var = ", var.L, ", n = ", n.L, "\n", sep="")
cat("right side: mean = ",mean.R, ", var = ", var.R, ", n = ", n.R, "\n", sep="")

# Run two-sample t-tests
alpha = 0.05

# 2(a) By hand
cat("------------------\n")
cat("2(a) Compute by hand-coding\n")
cat("\nEqual variances: two-sample t-test\n")
# We use convenience variables to write s, so it looks nicer
s1 = sqrt( ((n.L-1)*var.L + (n.R-1)*var.R)/(n.L+n.R-2) )
s2 = sqrt(1/n.L + 1/n.R)
s = s1*s2
t = (mean.L - mean.R)/s
df = n.L + n.R - 2
p = 2*pt(-abs(t),df)
cat("t = ", t, ", df = ", df, ", p = ", p, "\n", sep="")
{ #Need a brace so the R interpreter get confused by the else statement
  if (p < alpha) { print("*** Rejected the null hypothesis ***") }
  else { print("*** Failed to reject the null hypothesis ***") }
}

cat("\nUnequal variances: Wech's two-sample t-test\n")
s = sqrt(var.L/n.L + var.R/n.R)
t = (mean.L - mean.R)/s
df = (var.L/n.L + var.R/n.R)^2 / ( (var.L/n.L)^2/(n.L-1) + (var.R/n.R)^2/(n.R-1) )
p = 2*pt(-abs(t),df)
cat("t = ", t, ", df = ", df, ", p = ", p, "\n", sep="")
{ #Need a brace so the R interpreter get confused by the else statement
  if (p < alpha) { print("*** Rejected the null hypothesis ***") }
  else { print("*** Failed to reject the null hypothesis ***") }
}

# 2(b) Using R function t.test
cat("------------------\n")
cat("2(b) Compute using t.test()\n")
# Equal variances
res = t.test(leftSideOfRoom, rightSideOfRoom, alternative="two.sided", mu=0,
             paired=FALSE, var.equal=TRUE, conf.level = 1-alpha)
print(res)

# Unequal variances
res = t.test(leftSideOfRoom, rightSideOfRoom, alternative="two.sided", mu=0,
             paired=FALSE, var.equal=FALSE, conf.level = 1-alpha)
print(res)

# ------------------------- 
# Problem 3: Binomial rejection regions and off-by-1 errors ----
cat("\n----- Problem 3: Binomial rejection regions and off-by-1 errors -------\n")

# Choosing two sided rejection regions for 
# binomial hypotheses

# Idea: if we want a significance level of alpha we take each side of the rejection region to be the tail on that side with probabilitiy as close as possible to alpha/2 without going over

cat("------------------\n")
cat("Warmup\n")
# Example n=10, theta = .3, alpha = .1 
n = 10
theta = .4
alpha = .1
cat("n = ", n, ", theta = ", theta, ", alpha = ", alpha, "\n", sep="" )

# First we do it the long way with probability tables. 
# This is only practical if n is small
x = 0:n
prob.table = dbinom(x,n, theta)
print(round(prob.table, digits=3))
# Each tail should have probability less than 0.05
# By inspection we see that the left tail contains only the value 0 and the right tail contains 6-10.
criticalPoint.left = 1
criticalPoint.right = 8
rejection.left = 0:criticalPoint.left
rejection.right = criticalPoint.right:n
cat("\n rejection.left:", rejection.left, "\n")
cat("rejection.right:", rejection.right, "\n\n")

# We can check this using dbinom
cat("alpha/2 = ", alpha/2, "\n")
cat("Total prob. in rejection.left = ", sum(dbinom(rejection.left, n, theta)), "\n")
cat("Total prob. in rejection.right = ", sum(dbinom(rejection.right, n,theta)), "\n")

# check if we widen the tail the prob. is > alpha/2
wide.left = 0:(criticalPoint.left+1)
wide.right = (criticalPoint.right-1):n
cat("Total prob. in wide.left = ", sum(dbinom(wide.left, n, theta)), "\n")
cat("Total prob. in wide.right = ", sum(dbinom(wide.right, n, theta)), "\n\n")

# PROBLEM 3
cat("------------------\n")
cat("Problem 3\n")
# Let H0 be that the data follows a binomial(50, 0.5) distribution.
# Use qbinom() to find a two-sided rejection region at significance level 0.1
# Choose the region so the both sides are as close to 0.05 as possbile

# Since this is discrete we have to be careful to avoid being 'off by 1'
# qbinom gives the value with a little more probability than you
# ask for and we want a little less.
# Example the quantile for 0.05 includes slightly more than 0.05 prob.
n = 50
theta = 0.5
alpha = 0.1
cat("n = ", n, ", theta = ", theta, ", alpha = ", alpha, "\n", sep="" )
q_alphaOver2 = qbinom(alpha/2, n, theta)
x = pbinom(q_alphaOver2, n, theta)
cat("q_alphaOver2 =",q_alphaOver2, ", (prob <= q_alphaOver2) =", x, "(Note this is > alpha/2)")

# So qbinom give the value just 'outside' the desired rejection region
# RED_FLAG: this will fail if qbinom returns 0 or n. It could easily be handled with a little bit of logic in the code.
criticalPoint.left = qbinom(alpha/2,n,theta) - 1
criticalPoint.right = qbinom(1-alpha/2, n, theta) + 1
rejection.left = 0:criticalPoint.left
rejection.right = criticalPoint.right:n
cat("\n rejection.left:", rejection.left, "\n")
cat("rejection.right:", rejection.right, "\n\n")

# Use dbinom to check the tail probabilities are less then alpha/2
cat("alpha/2 = ", alpha/2, "\n")
cat("Total prob. in rejection.left = ", sum(dbinom(rejection.left, n, theta)), "\n")
cat("Total prob. in rejection.right = ", sum(dbinom(rejection.right, n,theta)), "\n")

# check if we widen the tail the prob. is > alpha/2
wide.left = 0:(criticalPoint.left+1)
wide.right = (criticalPoint.right-1):n
cat("Total prob. in wide.left = ", sum(dbinom(wide.left, n, theta)), "\n")
cat("Total prob. in wide.right = ", sum(dbinom(wide.right, n, theta)), "\n\n")

# **** NOTE *** We could widen one of the sides and keep the total significance < alpha

# ------------------------- 
# Problem 4: t-tests: left-handedness ----

cat("\n----- Problem 4: One-sample NHST: Lefties -------\n")

# These numbers are made up. We'll collect the data in class.
n = 40
nLefties = 9

# Use the same code as in problem 3 to find the rejection region
theta = 0.11
alpha = 0.05

cat("n = ", n, ", nLefties = ", nLefties, ", theta = ", 
    theta, ", alpha = ", alpha, "\n", sep="")

cat("\n-----------------\n")
cat("4(a) Exact binomial test\n")

#RED_FLAG --see the red_flag in problem 3. Because theta is small this is a real
#danger here. For alpha = 0.05 we're okay since 0 is in the left rejection region.
criticalPoint.left = qbinom(alpha/2,n,theta) - 1
criticalPoint.right = qbinom(1-alpha/2, n, theta) + 1
cat("Reject H_0 if nLefties is: less than or equal to",
    criticalPoint.left, "or greater than or equal to", criticalPoint.right, "\n")
rejRegion = c(0:criticalPoint.left, criticalPoint.right:n)
exactAlpha = sum(dbinom(rejRegion, n, theta))
cat("Exact significance =", exactAlpha, "\n")
{
  if (is.element(nLefties, rejRegion)) { cat("Rejected H_0\n")}
  else { cat("Failed to reject H_0\n")}
}

# Do a one-sample z-test
cat("\n-----------------\n")
cat("4(a) z-test\n")
mu = n*theta  #mean for binomial(n, theta)
sigma = sqrt(n*theta*(1-theta))  #standard dev form binomial(n,theta)
z = (nLefties - mu)/sigma
p = 2*pnorm(-abs(z), 0, 1)  #two-sided p-value
cat("mu = ", mu, ", sigma = ", sigma, ", z = ", z, ", p =", p, "\n", sep="")
{
  if (p < alpha) { cat("Rejected H_0\n")}
  else { cat("Failed to reject H_0\n")}
}

#4(b) Check normality assumption graphically
cat("\n-----------------\n")
cat("4(b) Checking binomial(40, 0.11) is approximately normal\n")
cat("See plot\n")
plot(0:n, dbinom(0:n,n,theta), pch=19, cex=.5)
x = seq(mu-4*sigma, mu+4*sigma, .01)
lines(x, dnorm(x,mu,sigma), col='orange', lwd=1.5)
title(c("Compare binomial(40, 0.11) with normal", "of same mean and variance"))

#4(c) One-sided tests
cat("\n-----------------\n")
cat("4(c) one-sided (on the right) exact binomial test\n")
criticalPoint.right = qbinom(1-alpha, n, theta) + 1
cat("Reject H_0 if nLefties is greater than or equal to", criticalPoint.right, "\n")
rejRegion = c(criticalPoint.right:n)
exactAlpha = sum(dbinom(rejRegion, n, theta))
cat("Exact significance =", exactAlpha, "\n")
{
  if (is.element(nLefties, rejRegion)) { cat("Rejected H_0\n")}
  else { cat("Failed to reject H_0\n")}
}

cat("\n-----------------\n")
cat("4(c) One-sided z-test\n")
mu = n*theta  #mean for binomial(n, theta)
sigma = sqrt(n*theta*(1-theta))  #standard dev form binomial(n,theta)
z = (nLefties - mu)/sigma
p = 1 - pnorm(z, 0, 1)  #Right-sided p-value
cat("mu = ", mu, ", sigma = ", sigma, ", z = ", z, ", p =", p, "\n", sep="")
{
  if (p < alpha) { cat("Rejected H_0\n")}
  else { cat("Failed to reject H_0\n")}
}
