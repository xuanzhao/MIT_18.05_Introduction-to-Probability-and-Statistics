# studio9.r
# Sample code to help with some of the Studio 9  problems

# -------------------- #
# Hiring salary problem
# Load the data
hiringData = read.table('studio9Data.tbl')
print(hiringData)

# (a) Pull out a subtable consisting of group1 and group2 data
hiringData.subtable = hiringData[,c("group1","group2")]
print(hiringData.subtable)

# (b) Pull out the economy wide probabilities (proportions) for 
#  hiring at the different levels
economyProp = hiringData[,"economyProportions"]
print(economyProp)