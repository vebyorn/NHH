###############################################
## FIE446 Assignment Spring 2023, Group: ??? ##
###############################################

# initialise environment
rm(list=ls())

#####################
## Data Processing ##
#####################
# Packaging in the cleaning into a function for brevity
# df = data frame
assignmentDataCleaner = function(df) {
    dat = df[-1,-1] # removing fluff
    colnames(dat)[1] = "time" # renaming timestep column
    dat$time = as.Date(dat$time, format = "%d/%m/%Y") # formating time
    dat[, 2:ncol(dat)] = lapply(dat[, 2:ncol(dat)], as.numeric) # formating rates
    dat[, 2:ncol(dat)] = dat[, 2:ncol(dat)] / 100 # converting to decimal
    dat = dat[!is.na(dat$time),] # removing rows with NA time
    colnames(dat) = gsub("\\.$", "", colnames(dat)) # removing trailing dots
    dat = dat[order(dat$time),] # sorting by time
    return(dat) # returning "cleaned" data (still NAs to keep in mind)
}

# Loading and cleaning data
csv = read.csv("C:/Users/vebky/FIE446/Assignment/EUR-Market-Data.csv") # reading in csv
dat = assignmentDataCleaner(csv); str(dat) # cleaning the csv

########################
## Task 1: Algorithms ##
########################
# Computing realised quarterly spread of the swap 
# for the time period March 11, 2007 and December 11, 2013.

# Function to select rate and time period:
# df = data frame
# column = column name of the desired rate as a string
# startDate = start date as a string
# endDate = end date as a string
ratePicker = function(df, columns, startDate, endDate) {
    newFrame = df[df$time >= startDate & df$time <= endDate, c("time", columns)] # selecting time period and columns
  return(newFrame) # returning new data frame
}

# Function to select the realised dates (monthly):
# df = data frame with time and rate column
# real = vector of realised dates in mm-dd format, ex: c("03-11", "06-11", "09-11", "12-11")
# startYear = start year as a number, ex: 2007
# endYear = end year as a number, ex: 2013
realiser = function(df, real, startYear, endYear) {
    df$realised = 0 # initialising realised column
    for (i in startYear:endYear) {
        for (j in 1:length(real)) {
            # this is specifically to help with the months where the desired day is on a weekend
            # we chose to roll forward to the next business day, rather than rolling backwards
            df$realised[df$time == as.Date(paste0(i, "-", real[j]), format = "%Y-%m-%d")] = 1
            df$realised[df$time == as.Date(paste0(i, "-", real[j]), format = "%Y-%m-%d") + 1] = 1
            df$realised[df$time == as.Date(paste0(i, "-", real[j]), format = "%Y-%m-%d") + 2] = 1
        }
    }
    df = df[df$realised == 1,] # selecting realised dates
    df$year = as.numeric(format(df$time, "%Y")) # creating year column
    df$month = as.numeric(format(df$time, "%m")) # creating month column
    df = df[!duplicated(df[, c("year", "month")]),] # keeping only the earliest day of each month per year
    df$realised = NULL; df$year = NULL; df$month = NULL # removing unnecessary columns
    return(df) # returning the data frame
}

# Swap Spread:
# df = data frame
# rateColumn = column name of the desired rate as a string
# lower = lower bound of the spread
# upper = upper bound of the spread
# coupon = function to compute coupon
swapSpread = function(df, rateColumn, lower, upper, coupon) {
    df$spread = 0 # initialising spread column
    euriborPrev = c(lag(df[[rateColumn]], 1)) # lagging rate column
    spreadPrev = c(lag(df$spread, 1)) # lagging spread column
    p1 = 2 * pmax(lower - euriborPrev, 0) # computing p1
    p2 = 2 * pmax(euriborPrev - upper, 0) # computing p2
    df$spread = pmax(0, spreadPrev + p1 + p2 - coupon(euriborPrev)) # computing spread
    return(df) # returning the data frame
}

# DigiCoupon:
# prevRate = previous rate
digiCoupon = function(prevRate) {
    ifelse(prevRate >= 0.02 & prevRate <= 0.06, 0.005, 0)
}

#####################
## Task 1: Results ##
#####################
# Choosing time interval and rate
real = c("03-11", "06-11", "09-11", "12-11") # realised dates
taskOne = realiser(ratePicker(dat, "euribor3md", "2007-03-01", "2013-12-31"), real, 2007, 2013) # generating data frame
taskOne # quarterly euribor3md rates for use in task 1

# Computing quarterly spread:
taskOne = swapSpread(taskOne, "euribor3md", lower = 0.02, upper = 0.06, digiCoupon) # computing quarterly spread
taskOne # quarterly spreads for use in task 1

# Validating our results:
boundSpreads = taskOne[taskOne$euribor3md >= 0.02 & taskOne$euribor3md  <= 0.06,] # only rates in bounds
outOfBoundSpreads = taskOne[taskOne$euribor3md < 0.02 | taskOne$euribor3md  > 0.06,] # only rates out of bounds
# If our calculations are correct, the following should be true of their spreads:
all(boundSpreads$spread == 0) # = TRUE
all(outOfBoundSpreads$spread > 0) # = TRUE

########################
## Task 2: Algorithms ##
########################
# Function to turn annualised rates into quarterly rates:
paToQuart = function(x) {
  return((1 + x)^(1/4) - 1)
}

# Annuity payment:
# loan = initial loan amount
# rate = interest rate
# spread = spread
# n = number of periods
annuityPayment = function(loan, rate, spread, n) {
  adjustedRate = rate + spread # adjusting rate for spread
  numer = (loan * adjustedRate * (1 + adjustedRate)^n) # calculating numerator
  denom = ((1 + adjustedRate)^n - 1) # calculating denominator
  return(numer / denom) # returning annuity payment
}

# Prepayment schedule:
# df = data frame
# loan = initial loan amount
# rate = interest rate (must be second column in data frame)
# spread = spread
# n = number of periods
# Desc: This function computes the interest and principal payments for each period
#       and updates the data frame with the results. It also computes the remaining
#       loan amount for each period. The dataframe can in theory be extended until
#       the remaining loan amount is 0, i.e. maturity. However, you'll need to supply
#       a dataframe with rates from the start of the loan until maturity. Consider
#       using yield curves for this.
prepaymentSchedule = function(df, loan, rate, spread, n) {
  df$annuity = 0 # initialising annuity column
  df$interest = 0 # initialising interest column
  df$principal = 0 # initialising principal column

  for (i in 1:nrow(df)) { # iterating through each row
    rate = df[i ,2] # updating rate
    df$annuity[i] = annuityPayment(loan, rate, spread, n) # computing annuity payment
  
    interestPayment = loan * (rate + spread) # computing interest payment
    principalPayment = df$annuity[i] - interestPayment # computing principal payment

    df$interest[i] = interestPayment # updating interest column
    df$principal[i] = principalPayment # updating principal column
    loan = loan - principalPayment # updating loan amount
    df$remaining[i] = loan # updating remaining column
  }
  return(df) # returning the data frame
}

#####################
## Task 2: Results ##
#####################
# We decided to calculate the payments for the 25 year loan with the data we had,
# i.e. until 2013. The remaining amount will still be correct, and if we had the
# rates until maturity, we could simply extend the data frame until the remaining
# amount is 0. Alternatively use discount curve or yield curve.
# Try it out by changing the dates in the realiser and ratePicker functions.

# Choosing time interval and rate:
real = c("03-11", "06-11", "09-11", "12-11") # realised dates
taskTwo = realiser(ratePicker(dat, "euribor3md", "2007-01-01", "2013-12-31"), real, 2007, 2013) # generating data frame
taskTwo # quarterly euribor3md rates for use in task 2

# Computing quarterly annuity payments:
loan = 89000000; spreadQuarterly = paToQuart(0.01); n = 25 * 4 # initalising parameters
taskTwoSchedule = prepaymentSchedule(taskTwo, loan, "euribor3md", spreadQuarterly, n) # computing prepayment schedule
taskTwoSchedule # prepayment schedule for use in task 3

########################
## Task 3: Algorithms ##
########################
# Converting p.a. rates to semi-annual:
# x = p.a. rate
paToSemi = function(x) {
  return((1 + x)^(1/2) - 1)
}

# Function for computing the swap payments:
# quartFrame = quarterly data frame
# semiFrame = semi-annual data frame
# rBST = rate paid by BST
# rMdP = rate paid by MdP
# This function is not very flexible, but it works for the data we have.
swapPayments = function(quartFrame, semiFrame, rBST, rMdP) {
  # Semi-annualisation of per annum fixed rates:
  for (i in 1:nrow(semiFrame)) { # iterating through each row
    semiFrame$BST[i] = paToSemi(rBST) * semiFrame$remaining[i] # BST payments to MdP
    semiFrame$MdPsemi[i] = paToSemi(rMdP) * semiFrame$remaining[i] # MdP payments to BST
  }

  # Quarterly payments:
  for (i in 1:nrow(quartFrame)) { # iterating through each row
    quartFrame$MdPquart[i] = quartFrame$spread[i] * quartFrame$remaining[i] # MdP payments to BST
  }

  taskThree = merge(semiFrame,  quartFrame, by = "time", all = TRUE) # merging data frames
  taskThree[is.na(taskThree)] = 0 # replacing NA with 0

  for (i in 1:nrow(taskThree)) { # iterating through each row
    taskThree$MdP[i] = taskThree$MdPquart[i] + taskThree$MdPsemi[i] # total MdP payments to BST
  }

  taskThree = taskThree[, c("time", "BST", "MdP")] # selecting relevant columns
  return(taskThree) # returning data frame
}

#####################
## Task 3: Results ##
#####################
# Given task 1 asks for the swaps from 2007 to 2013, we decided to compute the
# payments for the same period. I.e. we shorten down our prepayment plan data frame.
# You can extend it though by changing the dates in the realiser and ratePicker functions.
# From 2007 to 2021:
# BST payments to MdP: 4.76% p.a. (semi-annual)
# MdP payments to BST: 1.76% p.a. (semi-annual) + quarterly spread from task 1

# Loading quarterly data:
taskThreeQuart = swapSpread(taskTwoSchedule, "euribor3md", lower = 0.02, upper = 0.06, digiCoupon) # computing quarterly spread

# loading semi-annual data:
real = c("06-11", "12-11") # realised dates
taskThreeSemi = realiser(ratePicker(dat, "euribor6md", "2007-01-01", "2013-12-31"), real, 2006, 2013) # generating data frame
loan = 89000000; spreadSemi = paToSemi(0.01); n = 25 * 2 # initalising parameters
taskThreeSemi = prepaymentSchedule(taskThreeSemi, loan, rate, spreadSemi, n) # computing prepayment schedule

taskThree = swapPayments(taskThreeQuart, taskThreeSemi, rBST = 0.0476, rMdP = 0.0176) # computing swap payments
taskThree # swap payments made by BST and MdP.

