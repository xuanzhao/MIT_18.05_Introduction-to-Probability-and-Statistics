#---------------------------------------- #
# ---- Coin tossing problem ----
# 
# Coin problem: 9 coins in 10 tosees; H0 = coin is fair; alpha = 0.05
# part (a)
p = 2*sum(dbinom(c(9,10),10,.5))
cat("p =", p, "\n")

# ------#
# part (b)
coinDistribution =c(5,5,200,5,5)
coinProbs = c(.1,.3,.5,.7,.9)
prior = coinDistribution/sum(coinDistribution)
lik = dbinom(9, 10, coinProbs)
unnormPost = prior*lik
posterior = unnormPost/sum(unnormPost)
print(round(posterior,4))

# Now waste some time formatting the output as a Bayesian update table
tbl = cbind(hypothesis=coinProbs, prior=round(prior,3), likelihood=round(lik,3), 
            unnormPost=round(unnormPost,3), posterior=round(posterior,3))
print(tbl)

# part (c)
coinDistribution =c(5,5,20,5,5)  # *** This is the only change from part (b)
coinProbs = c(.1,.3,.5,.7,.9)
prior = coinDistribution/sum(coinDistribution)
lik = dbinom(9, 10, coinProbs)
unnormPost = prior*lik
posterior = unnormPost/sum(unnormPost)
print(round(posterior,4))

tbl = cbind(hypothesis=coinProbs, prior=round(prior,3), likelihood=round(lik,3), 
            unnormPost=round(unnormPost,3), posterior=round(posterior,3))
print(tbl)

# --------------------------------------- #
# Hiring salary problem
# Load the data
hiringData = read.table('studio9Data.tbl')
print(hiringData)

# Part (a): Compare distributions of group1 and group2 --- #
# Pull out a subtable consisting of group1 and group2 data
hiringData.subtable = hiringData[,c("group1","group2")]
print(hiringData.subtable)

# --------- chi-square tst by hand ------------- #
gp1.obs = hiringData[,"group1"]
gp2.obs = hiringData[,"group2"]
ngp1 = sum(gp1.obs)  #number of observations in group1
ngp2 = sum(gp2.obs)  #number of observations in group2
ntotal = ngp1 + ngp2  #total number of observations

levelProb = (gp1.obs+gp2.obs)/ntotal  #*1. levelProb explanation below
gp1.exp = ngp1*levelProb  #expected
gp2.exp = ngp2*levelProb  #expected

# Pearson's X^2 statistic.            #*2. X^2 explanation below
X2 = sum((gp1.exp-gp1.obs)^2/gp1.exp) + sum((gp2.exp-gp2.obs)^2/gp2.exp)      
df = length(gp1.obs) - 1              #*3. Degrees of freedom explanation below
p = 1 - pchisq(X2,df)
cat("X2 = ", X2, ", df = ", df, ", p = ", p, "\n")

# --- Explanations ---- #
# *1. levelProb are the estimated probabilities (or proportions) 
#     in each level assuming H0. For example, for level1 the estimated 
#     probability = totalInLevel1/totalObservations
# #2. Pearson's X^2 stat: sum((expected-observed)^2/expected). In
#     this case the observed are gp1 and gp2. We use X^2 instead of G
#     to match what chisq.test uses.
# *3. Degrees of freedom: 
#     The table of expected values is a length(div1) x 2, the columns 
#     have to add up to the number of observations in each group and
#     the rows have to add up to the number in each level. 

# --------- chi-square test using chisq.test ----------- #
print(chisq.test(hiringData.subtable))

# -------------- #
# Part (b): Compare group1 to the economy wide proportions ---- #
# Pull out the company wide proportions
gp1.obs = hiringData[,"group1"]
economyProp = hiringData[,"economyProportions"]
print(economyProp)

# ------- chi-square test by hand ----------- #
ngp1 = sum(gp1.obs)  #number of observations in group1
gp1.exp = ngp1*economyProp

X2 = sum((gp1.exp-gp1.obs)^2/gp1.exp)
df = length(gp1.obs) - 1
p = 1 - pchisq(X2,df)
cat("X2 = ", X2, ", df = ", df, ", p = ", p, "\n")

# ------- chi-square test using chisq.test ----------- #
print(chisq.test(gp1.obs, p=economyProp))
