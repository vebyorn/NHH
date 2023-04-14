###############################################
## FIE446 Assignment Spring 2023, Group: ??? ##
###############################################

#####################
## Data Processing ##
#####################

# initialise environment
rm(list=ls())

# Packaging in the cleaning into a function for brevity
# df = data frame
assignmentDataCleaner = function(df) {
    dat = df[-1,-1] # removing fluff
    colnames(dat)[1] = "time" # renaming timestep column
    dat$time = as.Date(dat$time, format = "%d/%m/%Y") # formating time
    dat[, 2:ncol(dat)] = lapply(dat[, 2:ncol(dat)], as.numeric) # formating rates
    dat[, 2:ncol(dat)] = dat[, 2:ncol(dat)] / 100 # converting to decimal
    dat = dat[!is.na(dat$time),] # removing NA time
    colnames(dat) = gsub("\\.$", "", colnames(dat)) # removing trailing dots
    return(dat) # returning "cleaned" data (still NAs to keep in mind)
}

# Loading and cleaning data
csv = read.csv("C:/Users/vebky/Desktop/EUR-Market-Data.csv") # reading in csv
dat = assignmentDataCleaner(csv); str(dat) # cleaning the csv

############
## Task 1 ##
############
# Computing realised quarterly spread of the swap 
# for the time period March 11, 2007 and December 11, 2013.

# Function to select rate and time period:
# df = data frame
# column = column name of the desired rate as a string
# startDate = start date as a string
# endDate = end date as a string
ratePicker = function(df, column, startDate, endDate) {
  interestData = data.frame(time = df$time, column = df[[column]]) # creating data frame
  interestData = interestData[interestData$time >= as.Date(startDate),] # selecting time period
  interestData = interestData[interestData$time <= as.Date(endDate),] # selecting time period
  interestData = interestData[!is.na(interestData[2]),] # removing NA rates
  colnames(interestData)[2] = column # renaming column
  return(interestData) # returning data frame
}

# Quarterly Spread:
# df = data frame
# rateColumn = column name of the desired rate as a string
# lower = lower bound of the spread
# upper = upper bound of the spread
# coupon = function to compute coupon
quartSpread = function(df, rateColumn, lower, upper, coupon) {
    df$spread = 0 # initialising spread column
    euriborPrev = c(lag(df[[rateColumn]], 1)) # lagging rate column
    spreadPrev = c(lag(df$spread, 1)) # lagging spread column
    p1 = 2 * pmax(lower - euriborPrev, 0) # computing p1
    p2 = 2 * pmax(euriborPrev - upper, 0) # computing p2
    df$spread = pmax(0, spreadPrev + p1 + p2 - coupon(euriborPrev)) # computing spread
    return(df) # returning the data frame
}

# DigiCoupon:
# df = data frame
digiCoupon = function(prevRate) {
    ifelse(prevRate >= 0.02 & prevRate <= 0.06, 0.005, 0)
}

## Task 1 Results: ##
# Choosing time interval and rate
taskOne = ratePicker(dat, "euribor3md", "2007-03-11", "2013-12-11") # selecting time interval and rate
head(taskOne); tail(taskOne) # checking

# Computing quarterly spread:
taskOne = quartSpread(taskOne, "euribor3md", lower = 0.02, upper = 0.06, digiCoupon) # computing quarterly spread
head(taskOne); tail(taskOne) # checking

# Validating our results:
boundSpreads = taskOne[taskOne$euribor3md >= 0.02 & taskOne$euribor3md  <= 0.06,] # only rates in bounds
outOfBoundSpreads = taskOne[taskOne$euribor3md < 0.02 | taskOne$euribor3md  > 0.06,] # only rates out of bounds
# If our calculations are correct, the following should be true:
all(boundSpreads$spread == 0) # TRUE
all(outOfBoundSpreads$spread > 0) # TRUE

# Do later: finish task by only selecting realised dates and placing them in a DF.
# Then run them through the quartSpread function and compute the spread from payment to payment.
# As this is what I assume what t and t-1 are referring to. I should probably read the article again...
# Realised dates are for variable payments: 11th of March, June, September and December.