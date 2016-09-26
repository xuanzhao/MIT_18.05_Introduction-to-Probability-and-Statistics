# load the studio6 data frame and define some convenience functions and lists

studio6dataframe = read.csv('studio6dataframe.csv')

fiveMinuteTimesInHours = round(seq(570,955,5)/60, digits=3)

getWaitTimesInSeconds = function(dt, fiveMinuteSlot)
{
  #Returns all the wait times in seconds for one five minute slot on one day
  
  #dt is the date: one of 20140303, 20140304, 20140305, 20140306
  #fiveMinuteSlot is the start of a 5 minute time interval, given in hours from
  #midnight with exactly 3 decimal places --take it from fiveMinuteTimeInHours
  
  #Boolean logic: multiplication of 0's and 1's gives 1 only if both factors are 1
  b = (studio6dataframe[,'Date'] == dt)*(studio6dataframe[,'fiveMinuteSlot']==fiveMinuteSlot)
  y = studio6dataframe[b==1,'timeNumber']*24*60*60
  return(diff(y))
}
