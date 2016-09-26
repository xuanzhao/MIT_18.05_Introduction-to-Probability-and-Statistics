#---------------------------------
# Sample code from 18.05 Studio 1
# You can find more at the R tutorials posted in week 1 on MITx
#---------------------------------

# Here's how you load an r file. The file can be on your computer
# or at a url e.g. 
#source('http://web.mit.edu/jorloff/www/18.05/r-code/colMatches.r')

# You should have downloaded thestudio1.zip and unzipped it into
# your 1805 working directory.
#---------------------------
# choose, factorial sample functions

# To find n choose k use choose(n,k)
choose(4,2)
choose(10,3)
choose(100,42)

#Compute the number of ways to get 1 pair and the probability of one pair
choose(13,1) * choose(4,2)* choose(12,3) *4*4*4
choose(13,1) * choose(4,2)* choose(12,3) *4*4*4 /choose(52,5)

#factorial does what is says
factorial(3)
factorial(4)
factorial(22)

#sample picks random values from a list
sample(1:5, 3)  #pick 3 elements from 1,2,3,4,5 --no repeats allowed
sample(1:5, 3)
sample(1:5, 3)
sample(1:20, 8)
sample(1:5, 4, replace=TRUE) #repeats are now allowd   
sample(1:5, 4, replace=TRUE)
sample(1:5, 4, replace=TRUE)

#very often we want to arrange our data in rows and columns
#The matrix command does this for us.
matrix(1:30,nrow=5) #If you only set nrow, matrix will figure out ncol
# N.B. matrix breaks the list into columns --this is opposite what
# you might expect. As is typical in R there is a parameter in the 
# matrix() function to change this. See if you can figure it out.

# colSums and args functions
x=sample(0:1,60,replace=TRUE)
x
y=matrix(x, nrow=5,ncol=6)
y

#colSums() sums each of the columns of y. 
colSums(y)

#---------------------------
# Simulating rolling a 20-sided die 8 times and checking for matches ----

# soucre() is the way of loading source code in other files
source('colMatches.r')
colMatchesHelp()  #we'll use this syntax to write help functions for 1805 code.

# First we'll do 1 trial of nrolls of an nsides-sided die
# Give values understandable names.
# In this code an initial 'n' means 'number of'
nsides = 20
nrolls = 8
ntrials = 10
sizematch = 2
die = 1:nsides

trial =  sample(die, nrolls, replace=TRUE)
#display the values in trials
trial
# colMatches is a function we wrote for 18.05 that checks each column of the matrix of a match
#look for two rolls the same
colMatches(trial,2)

# Now we'll run ntrials at once
nsides = 20
nrolls = 8
ntrials = 10
sizematch = 2
die = 1:nsides

# ntrials of nrolls each requires nrolls*ntrials random samples
# Generate nrolls*ntrials random samples
y =  sample(die, nrolls*ntrials, replace=TRUE)
# Arrange the samples into one nrolls trial per column of an array
# Note the named arguments are 'nrow' NOT 'nrows' and 'ncol' NOT 'ncols'
trials = matrix(y, nrow=nrolls, ncol=ntrials)
trials
# Use the function colMatches to look for matches in each trial (column)
# colMatches returns a vector (list) of 1's and 0's
w = colMatches(trials,sizematch)
# Use sum to count the number of 1's in w
sum(w)
# Divide by ntrials to get the fraction of trials with a match
sum(w)/ntrials
# A simpler way to get the fraction in one command is with mean()
mean(w)

# Here's the same code without comments so selecting and
# running in RStudio is easier. We also remove  the
# line that displayed trials and up ntrials to 1000
nsides = 20
nrolls = 8
ntrials = 1000
sizematch = 2
die = 1:nsides
y =  sample(die, nrolls*ntrials, replace=TRUE)
trials = matrix(y, nrow=nrolls, ncol=ntrials)
w = colMatches(trials,sizematch)
mean(w)

#------------------------------
#Simple plotting
# 1. seq(start, stop, increment) makes a list of numbers from
#  start to stop in steps of increment
# 2. sin(x) applies sin to every element in x
# 3. plot has lots of ways to control the plot
#   type='l' says to connect the points in (x,y) by lines, lwd is the line width
x = seq(0, 6*pi,.01)
y = sin(x)
plot(x,y,type='l', col='red',lwd=3)

#-------------------------
# Here's code to plot
# the (estimated) probability of a match vs nrolls in a trial
# It uses a 'for loop' to run the simulation for various values of nrolls

# If you don't know about 'for loops' don't worry about it. We'll learn more about them
# soon.

nsides = 20
ntrials = 1000
sizematch = 2
die = 1:nsides
# Run experiments with nrolls from 1 to 60
all_nrolls = 1:60

# length(all_nrolls) gives the length of the vector all_nrolls
N = length(all_nrolls)

# rep(value, n) makes a vector with n copies of value.
# We start by creating the probability vector. We let all values be 0. We'll
# fill in our actual computed values in the loop
prob = rep(0, N)

# This is how you make a for loop
for (j in 1:N)
{
    nrolls = all_nrolls[j]
    y =  sample(die, nrolls*ntrials, replace=TRUE)
    trials = matrix(y, nrow=nrolls, ncol=ntrials)
    w = colMatches(trials,sizematch)
    prob[j] = mean(w)
}
plot(all_nrolls,prob,type='l',col="red", lwd=2)

# We do the same plot for sizematch = 3
sizematch = 3

prob = rep(0, N)
for (j in 1:N)
{
    nrolls = all_nrolls[j]
    y =  sample(die, nrolls*ntrials, replace=TRUE)
    trials = matrix(y, nrow=nrolls, ncol=ntrials)
    w = colMatches(trials,sizematch)
    prob[j] = mean(w)
}
# lines adds the plot of all_nrolls,prob to the existing plot instead of
# making a new plot
lines(all_nrolls,prob, col="green", lwd=2) #New color

# Same thing with sizematch = 4
sizematch = 4

prob = rep(0, N)
for (j in 1:N)
{
    nrolls = all_nrolls[j]
    y =  sample(die, nrolls*ntrials, replace=TRUE)
    trials = matrix(y, nrow=nrolls, ncol=ntrials)
    w = colMatches(trials,sizematch)
    prob[j] = mean(w)
}
lines(all_nrolls,prob, col="blue", lwd=2) #New color
