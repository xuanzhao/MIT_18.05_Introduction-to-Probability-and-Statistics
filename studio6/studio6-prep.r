# This is the code used to convert the original NASDAQ data into studio6dataframe.csv

# tradesdata0 has data for all NASDAQ trades of a stock for 4 days in March 2014
#   Column      | Description
#   -----------------------------------------------------------------------------
#   Date        | Date of trades in YYYYMMDD format
#   timeNumber  | time of trade as fraction of 24-hour period  (0.5 = 12 noon)
#   timeHHMMSS  | time of trade as an integer in HHMMSS format
#   Size        | trade size in shares
#   Price       | trade price (volume-weighted over all trade rectords at same timeNumber)


# We start with some exporatory data analysis
#load the data ---------
tradesdata0 = read.table(file="tradesdata0.csv",sep=",", header=TRUE)
# Take a peek --note time is given as a decimal = fraction of 24 hours and as HHMMSS
print(head(tradesdata0))
print(dim(tradesdata0))

# Plot price verse trade index (roughly time) --export this for use in slides -------
plottitle =  paste(c("Trade Prices", "Dates: 3/3, 3/4, 3/5, 3/6 in 2014"))
plot(tradesdata0[,"Price"], type = 'l', col='blue',
     xlab="Trade Index", ylab="Price", main=plottitle)

# Add vertical lines at beginning of each Date
list.Dates=unique(tradesdata0[,"Date"])
for (dt in list.Dates){
  abline(v=sum(tradesdata0[,"Date"]< dt))
}

# Condense the data into 5 minute chunks ----
# Make a plot and export it for slides
# Pick out the timeNumber column of tradesdata0 --this is a column of times as fractions of 24 hours, e.g. 0.5 = 12 Noon. One entry for each trade recorded
tradesdata0.timeColumn = tradesdata0[,"timeNumber"]

# We want to round each time down (i.e. floor each time) to the nearest 5 minutes
# If t is a time in fractions of 24 hours, e.g. t = 0.5 is 12 noon, then the formula
# floor(t*24*12)/12 gives t to the nearest multiple of 5 minutes less than t.
# REASON: t is in fractions of 24 hours so t*24*60 is t in units of minutes and
# t*24*12 is t in units of 5 minutes. So floor(t*24*12) is a multiple  of 5 minutes.
# Dividing by 12 converts back to units hours from midnight
# To deal with the beginning and end of the day (9:30 am and 4 pm) we add 1/2 second
# to each time --this means 9:30 am stays in the 5 minute interval starting at 9:30
# and the last time in the day 15:59:59 stayes in the 5 minut interval starting at 
# 15:55
tinyt = 1/(24*60*60*2)
x = floor((tradesdata0.timeColumn+tinyt)*24*12)/12
tradeTimes5minute = round(x, digits = 3)

# The table function with one arguments produces a table of counts
# We combine all 4 days to get a table of counts in each 5 minute interval
tradeTimes5minute.cumtable = table(tradeTimes5minute)
head(tradeTimes5minute.cumtable)
plot(tradeTimes5minute.cumtable, xlab="Time in hours", ylab="Number of trades", main="Trade counts in 5 minute periods (all days combined)", cex.main=.9)

# Explore individual days ----
# Make a table: rows = 5 minute periods, columns = dates, entries = # of trades
tradeTimes5minute.daytable = table(tradeTimes5minute, tradesdata0[,"Date"])
print(tradeTimes5minute.daytable)

#make a plot for export
n = nrow(tradeTimes5minute.daytable)
col = c(rep('orange',n), rep('blue',n), rep('green',n), rep('red',n))
barplot(tradeTimes5minute.daytable, beside=TRUE, 
        col=col, cex.main = 1.3,  space=c(0,1), border=NA, cex.names=1,
        names.arg = c('3/3/2014', '3/4/2014', '3/5/2014', '3/6/2014'),
        main="Table of Trade Counts by 5-Minute Periods by Date")

# Create the csv file holding the dataframe for studio6 ----
studio6dataframe = data.frame(Date = tradesdata0[,'Date'], 
                                fiveMinuteSlot = tradeTimes5minute,
                                timeNumber = tradesdata0.timeColumn)

write.csv(studio6dataframe, 'studio6dataframe.csv')

