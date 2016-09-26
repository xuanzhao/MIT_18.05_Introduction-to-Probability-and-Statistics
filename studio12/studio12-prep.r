#---------------------------------------#
#Studio12-prep.r
#  This is the code that was used to prep the data sets and create some images for
#  studio 12. It will not be used during the studio.
#---------------------------------------#

#-----------------------------------#
# SP500 financial data.
s12data.orig = read.csv('studio12financialOriginal.csv')
print(head(s12data.orig))
print(tail(s12data.orig))
print(dim(s12data.orig))

# Extract columns from s12data
s12dates = as.Date(s12data.orig[,"date"])
GE = s12data.orig[,"GE"]                   # GE stock price
SP500 = s12data.orig[,"SP500"]             # Standard and Poors 500 index
DCOILWTICO = s12data.orig[,"DCOILWTICO"]   # Crude oil price

# Set colors
GE.col = 'cyan'
SP500.col = 'blue'
DCOILWTICO.col = "purple"
scatter.col = rgb(1,.643,.224,alpha=.5)

# Plot the data. We scale the data so it all fits on one plot
day1 = s12dates[1]
dayLast = s12dates[length(s12dates)]
# Scale the values so they can be put on the same plot

# Comment this in if saving the pdf. 
# Instead of plotting to the screen, R will now write to the file
#pdf(file="studio12-1.pdf", width=6,height=4)

plot(c(day1,dayLast),c(0,1800), type='n', xlab="date", ylab="scaled price")
lines(s12dates,SP500, col=SP500.col, type='l')
lines(s12dates,40*GE, col=GE.col)
lines(s12dates,10*DCOILWTICO, col=DCOILWTICO.col)
title("(Scaled) Stock Prices over 14 years")
legend(as.Date("2002-01-01"),1850, c("SP500","40*GE", "10*DCOILWTICO"), 
       col=c(SP500.col, GE.col, DCOILWTICO.col), cex=.8,lwd=2)

# Comment this in if saving the pdf
# This closes the file, so R will now draw to the screen
#dev.off()

# Compute daily rate of return
# Need number of days between entries --usually 1, 3 over weekends, can be as big as 7
daydiff = as.numeric(diff(s12dates))
print(head(daydiff))

GE.daily = diff(log(GE))/daydiff
SP500.daily = diff(log(SP500))/daydiff
DCOILWTICO.daily = diff(log(DCOILWTICO))/daydiff

#Put all the data in one table for saving
s12data.daily = data.frame(date=s12dates[-1], GE.daily=GE.daily,
                            SP500.daily=SP500.daily, 
                            DCOILWTICO.daily=DCOILWTICO.daily)
print(head(s12data.daily))
print(tail(s12data.daily))
print(dim(s12data.daily))

# Save the data
#COMMENT THIS OUT BECAUSE ONLY NEED TO WRITE ONCE
#write.csv(s12data.daily, "studio12financialDaily.csv", row.names=FALSE)
