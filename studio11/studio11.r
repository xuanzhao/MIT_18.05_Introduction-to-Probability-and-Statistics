# Studio 11 script

# Chi square plots ------------------------------
# We did this in an earlier studio. This is just a quick reminder
# To size our plots we note that the mean of chisq(df=n) = n and the variance is 2n
df = c(2,3,4,5,8,10) # set the degrees of freedom to draw
maxx = 20
x = seq(0,maxx,.01)
maxy = max(dchisq(x,2))
plot(c(0,maxx), c(0,maxy), type='n', xlab="x", ylab="chi-sqare")
x = seq(0,maxx,.01)
for (j in 1:length(df))
{
  d = df[j]
  lines(x,dchisq(x,d), col=j, lwd=2)
}

# Load stock market volatility data --------------------------

# Read the data
#   The data is a 3602x5 matrix of daily values from 1/4/2000 - 4/30/2014
studio11SP500data = read.csv("studio11SP500data.csv")
head(studio11SP500data)
tail(studio11SP500data)
dim(studio11SP500data)

# ----- Extract the PctReturn for Mondays (day=1) and Fridays (day=5) ------------#
b = studio11SP500data[,"dayofweek"]==1
pctReturn.monday = studio11SP500data[b,"PctReturn"]

b = studio11SP500data[,"dayofweek"]==5
pctReturn.friday = studio11SP500data[b,"PctReturn"]

# ---------- Explore the data ----------------- #
# The "ndays" column is the number of days in the close-to-close "PctReturn"
# Print out the cross-tabulation of dayofweek by ndays 
table(studio11SP500data[,"dayofweek"], studio11SP500data[,"ndays"])

plot(studio11SP500data[,"SP500"],type="l",ylab="S&P500 Index", xlab="Day Index",
     main="S&P 500 Index from 1/4/2000 - 4/30/2014")
plot(studio11SP500data[,"PctReturn"],type="l",ylab="Percent return", xlab="Day Index",
     main="Daily Percent Returns")

plot(pctReturn.monday,type="l",ylab="Percent return", xlab="", col='orange',
     main=c("S&P 500 Perecentage Returns: Mondays", "1/4/2000 - 4/30/2014"))
plot(pctReturn.friday,type="l",ylab="Percent return", xlab="", col='blue',
     main=c("S&P 500 Perecentage Returns: Fridays", "1/4/2000 - 4/30/2014"))


