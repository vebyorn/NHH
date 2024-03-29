#########################################################
## FIE446 Assignment Spring 2023, Group: Deep Learners ##
#########################################################
## Our script is structured as follows:                ##
## 1. Alogrithms we made for the task.                 ##
## 2. Data initialisation for the task.                ##
## 2. Results for the task.                            ##     
##                                                     ##
##                                                     ##
## Warning! The algorithms have function               ##
## dependencies that cross tasks. For example          ##
## many of the algorithms are dependant on the rate-   ##
## picker and realiser functions. Etc. Do not clean    ##
## environment during inspection! :-)                  ##
#########################################################

# initialise environment
rm(list=ls())

################################
## Assigment: Data Processing ##
################################
# Packaging the cleaning of the data into a function for brevity
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

# Loading and cleaning data (only change the file path, not anything else).
csv = read.csv("/YourDirectoryHere/") # reading in csv
dat = assignmentDataCleaner(csv); str(dat) # cleaning the csv

########################
## Task 1: Algorithms ##
########################
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
    ifelse(prevRate >= 0.02 & prevRate <= 0.06, 0.005, 0) # computing coupon
}

#####################
## Task 1: Results ##
#####################
# Computing realised quarterly spread of the swap 
# for the time period March 11, 2007 and December 11, 2013.

# Fetching data:
real = c("03-11", "06-11", "09-11", "12-11") # realised dates
taskOne = realiser(ratePicker(dat, "euribor3md", "2007-01-01", "2013-12-31"), real, 2007, 2013) # generating data frame
taskOne # quarterly euribor3md rates for use in task 1

# Computing quarterly spread:
taskOne = swapSpread(taskOne, "euribor3md", lower = 0.02, upper = 0.06, digiCoupon) # computing quarterly spread
taskOne # Result: Quarterly spreads for the time period March 11, 2007 and December 11, 2013.

# Validating our results:
boundSpreads = taskOne[taskOne$euribor3md >= 0.02 & taskOne$euribor3md  <= 0.06,] # only rates in bounds
outOfBoundSpreads = taskOne[taskOne$euribor3md < 0.02 | taskOne$euribor3md  > 0.06,] # only rates out of bounds
# The following should be true of the spreads:
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
# amount is 0.
# Try it out by changing the dates in the realiser and ratePicker functions.

# Fetching data:
real = c("03-11", "06-11", "09-11", "12-11") # realised dates
taskTwo = realiser(ratePicker(dat, "euribor3md", "2007-01-01", "2013-12-31"), real, 2007, 2013) # generating data frame

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
taskThree # Swap payments made by BST and MdP.


########################
## Task 4: Algorithms ##
########################
# Year Fraction Function:
# t1 = start date
# t2 = end date
# dcount = day count convention
# Desc:
# This function computes the year fraction between two dates given a day count convention.
delta = function(t1, t2, dcount) {
  if (dcount == "act360") { # actual/360
    d1 = as.Date(t1)
    d2 = as.Date(t2)
    days = as.numeric(d2 - d1)
    delta = days / 360
  } else if (dcount == "act365") { # actual/365
    d1 = as.Date(t1)
    d2 = as.Date(t2)
    days = as.numeric(d2 - d1)
    delta = days / 365.242
  }
  return(delta)
}

# Convexity Adjustment
# F = forward rate
# T1 = start date
# T2 = end date
# sigma = volatility
# Desc:
# This function computes the convexity adjustment for a forward rate.
con.adj = function(F, T1, T2, sigma){
  f0T1T2 = F - (sigma^2*T1*T2)/2 # forward rate
  return(f0T1T2)
}

# FRA interpolation
# guess_z = guess for discount factor
# f0 = forward rate
# T1 = start date
# T2 = end date
objective_function = function(guess_Z, f0, T1, T2) {
  # Step 1: Extend Z and t
  Z = c(1, guess_Z)    
  t = c(0, T2)
  
  # Step 2: Calculate Z(0, T1) by linear interpolation between log(Z(0, 0)) and log(Z(0, T2))
  log_Z_T1 = approx(t,log(Z), xout=T1, method = "linear")$y
  Z_T1 = exp(log_Z_T1)
  
  # Step 3: Calculate the forward rate f'(0,T1,T2) using the estimated discount factors
  f0.new = (1 / (T2 - T1)) * ((Z_T1/guess_Z)-1)
  
  # Calculate Error
  e = (f0.new-f0)^2
  
  return(e) # return error
}

# IRS Interpolation
# guess_z = guess for discount factor
# Swap_rate = swap rate
# Z0ti = discount factor
# discount_factors = discount factors
objective_function_IRS = function(guess_Z, Swap_rate, Z0ti, discount_factors) {
  # Step 1: Extend Z and t
  Z1 = c(1, discount_factors[1:length(discount_factors)-1], guess_Z)    
  
  # Step 2: Calculate the swap rate using the estimated discount factors
  nominator = 1 - Z1[length(Z1)]/Z0ti
  denominator  = 0.5 * sum(Z1[3:length(Z1)]) / Z0ti
  Swap_rate_estimate = nominator/denominator
  
  
  # Calculate Error
  e = (Swap_rate_estimate-Swap_rate)^2
  return(e)
}  

# Function to extract the values from the dataframe
# df = dataframe
# rates = vector with name of desired rates
# year = numeric year
# month = numeric month
# day = numeric day
valueExtract = function(df, rates, year, month, day) {
  desYear = paste(year, month, day, sep="-") # desired year
  df = realiser(ratePicker(df, rates, desYear, desYear), paste(month, day, sep="-"), year, year) # generating data frame
  df_vec = c() # initializing vector
  for (i in 1:length(rates)) { # looping through rates
    df_vec = c(df_vec, df[[rates[i]]]) # extracting values
  }
  return(df_vec) # return vector
}

# Function to extract the values and maturities from the dataframe
# df = dataframe
# rates = vector with name of desired rates
# year = numeric year
# month = numeric month
# day = numeric day
# DTM = days to maturity
# dcount = day count convention
matValueExtract = function(df, rates, year, month, day, maturity, dcount) {
  val = valueExtract(df, rates, year, month, day) # extracting values
  mat = delta(as.Date(paste(year, month, day, sep="-")), maturity, dcount) # extracting maturities
  mat_vec = c() # initializing vector
  for (i in 1:length(rates)) { # looping through rates
    mat_vec = c(mat_vec, mat[[i]]) # extracting values
  }
  fin = list(val, mat_vec) # creating list
  names(fin) = c("price", "maturity") # naming list
  return(fin) # return list
  }

# Future value
# df = dataframe
# rates = vector with name of desired rates
# year = numeric year
# month = numeric month
# day = numeric day
# maturity = maturity date
# dcount = day count convention
# init = boolean to add 0 to the beginning of the price vector
futureValue = function(df, rates, year, month, day, maturity, dcount, init = FALSE) {
  res = matValueExtract(df, rates, year, month, day, maturity, dcount)
  res[[1]] = (100 - res[[1]] * 100) / 100
   if (init == TRUE) {
    res[[1]] = c(0, res[[1]])
  }
  return(res)
}

##### Bootstrap function #####
# ON = overnight deposits
# MM = money market deposits
# Fut = futures
# IRS = swaps
bootstrap = function(ON, MM, Fut, IRS) {
  # Initialise vectors
  t = c(0)
  Z = c(1)
  FRAs = c()

  # Overnight and Tomorrow Next Deposits
  Z = c(Z, 1 / ((1 + (1/365.242)*ON[1]) * (1 + (3/365.242)*ON[2])))
  t = c(t, 4/365.242)

  # MM Deposits
  Z = c(Z, 1/((1 + (1/365.242) *  MM$price[1]) * (1 + (10/365.242) * MM$price[2]))) # 1 week and 1 month MM discount factors
  t = c(t, MM$maturity[2]) # 1 week and 1 month MM accrual times

  # From Futures to FRAs
  priceFut = Fut$price
  maturityFut = Fut$maturity
  # Map Futures into FRAs and adj. for convexity
  for (i in 2:(length(priceFut))) {
    F = priceFut[i]
    T2 = maturityFut[i]
    T1 = maturityFut[i-1]
    vals = con.adj(F=F, T2=T2, T1=T1, sigma=0.006)
    FRAs = c(FRAs, vals)
  }
  # remove nas from fras
  FRAs = FRAs[!is.na(FRAs)]

  # FRAs 
  for(i in 1:length(FRAs)){
    T1 = maturityFut[i]
    T2 = maturityFut[i + 1]
    rate = FRAs[i]
    values = optimize(objective_function, interval = c(0, 1),f0 = rate,
                       T1 = T1, T2 = T2, maximum = FALSE)$minimum
    Z = c(Z, values*Z[2])
    t = c(t, T2)
  }

  # Swaps
  for (i in 1:length(IRS$maturity)) { 
    # Determine coupon dates
    current_swap_maturity = IRS$maturity[i] + 4/360
    coupon_dates = c(t[2], seq(from = t[2] + 0.5, to = current_swap_maturity-0.1, by = 0.5), current_swap_maturity)

    # Interpolate for coupon dates and drop unavailable dates 
    discount_factors_int = na.omit(approx(x = t, y = Z, xout = coupon_dates)$y)
    
    # Extrapolate for missing coupon dates with lm
    coupon_dates_ext = na.omit(coupon_dates[length(discount_factors_int)+1:length(coupon_dates)])
    model = lm(Z ~ t)
    discount_factors_ext = predict(model, newdata = data.frame(t = coupon_dates_ext))
    
    # Add discount factors together
    discount_factors = c(discount_factors_int, discount_factors_ext)
    
    # Optimize model
    Swap_rate = IRS$price[i]
    Z0ti = Z[2]
    values = optimize(objective_function_IRS, interval = c(0, 1), Swap_rate = Swap_rate,
                       Z0ti = Z0ti, discount_factors = discount_factors,
                       maximum = FALSE)$minimum
    
    Z = c(Z, values)
    t = c(t, current_swap_maturity)
  }
  # return dataframe with Z and t columns
  return(data.frame(Z, t))
}

#############################
## Task 4: Initialize Data ##
#############################
# Bootstrap the discount curve to market data obtained on December 11, 2006.
# We use the same function as we derived from class. We use functions we created
# for this assignment to get the exact values out of the dat dataframe.
# We declare the names of the rates used and their associated maturities.
# The maturities we found ourselves using a calendar and might not be optimal.

# Overnight deposits
onRates = c("eurond", "eurtnd") # overnight deposits
onDep = valueExtract(dat, onRates, 2006, 12, 11) # fetching rates
onDep # printing rates

# Money Market deposits
mmRates = c("euriborswd", "euribor1md") # money market deposits
mmMats = c("2006-12-18", "2007-01-11") # maturities
mmDep = matValueExtract(dat, mmRates, 2006, 12, 11, maturity = mmMats, dcount = "act360") # fetching rates and maturities
mmDep # printing rates

# Futures
futRates = c("FEIcm1", "FEIcm2", "FEIcm3", "FEIcm4", "FEIcm5") # futures
futMats = c("2007-01-15", "2007-06-12", "2007-09-11", "2007-12-11", "2008-03-11", "2008-06-11") # maturities
futDep = futureValue(dat, futRates, 2006, 12, 11, maturity = futMats, dcount = "act360", init = TRUE) # fetching rates and maturities
futDep # printing rates

# Swaps
swapRates = c("eurirs2y", "eurirs3y", "eurirs4y", "eurirs5y", "eurirs7y", "eurirs10y", "eurirs12y", "eurirs15y", "eurirs20y") # swaps
swapMats = c("2008-12-11", "2009-12-13", "2010-12-12", "2011-12-11", "2013-12-11", "2016-12-11", "2018-12-11", "2021-12-12", "2026-12-13") # maturities
swapDep = matValueExtract(dat, swapRates, 2006, 12, 11, maturity = swapMats, dcount = "act360") # fetching rates and maturities
swapDep # printing rates

##############################
## Task 4: Results and Plot ##
##############################
# Computing discount factors:
taskFour = bootstrap(ON = onDep, MM = mmDep, Fut = futDep, IRS = swapDep) # bootstrap function
taskFour # Result: Discount Factors and their respective maturities.

# Plotting discount factors:
plot(x = taskFour$t, y = taskFour$Z, type = "l", xlab = "Time (Years)", ylab = "Discount Factor") # plot discount factors
points(x = taskFour$t, y = taskFour$Z, col = "blue", pch = 21) # add points to plot to show specific Z entries

########################
## Task 5: Algorithms ##
########################

# Function to compute spot rates from swap rates
# swapRate: vector of swap rates
# tenor: vector of swap tenors
spotRate = function(swapRate, tenor) {
  spotRates = numeric(length(tenor))
  for (i in 1:length(tenor)) { # loop over swap rates
    spotRates[i] = (1 + swapRate[i] * tenor[i])^(1/tenor[i]) - 1 # spot rates
  }
  return(spotRates) # return vector of spot rates
}

# Function to bootstrap forward rates
# swap_rates: vector of swap rates
# tenors: vector of swap tenors
# spot_rates: vector of spot rates
bootstrapForwardRates = function(swap_rates, tenors, spot_rates) {
  n = length(swap_rates) # number of swap rates
  forward_rates = numeric(n) # vector of forward rates
  
  for (i in 2:n) { # loop over swap rates
    forward_rates[i] = ((1 + swap_rates[i] * tenors[i]) / (1 + spot_rates[i-1]) - 1) / (tenors[i] - tenors[i-1]) # forward rates
  }
  
  return(forward_rates) # return vector of forward rates
}

# Objective function: sum of squared errors
# lambda: vector of lambda values
# forward_rates: vector of forward rates
# sigma: vector of implied volatilities
# delta: time step
volObjectiveFun <- function(lambda, forward_rates, sigma, delta) {
  n = length(forward_rates) # number of forward rates
  model_volatilities <- numeric(n) # vector of model volatilities
  
  for (i in 1:n) { # loop over forward rates
    model_volatilities[i] <- sqrt(sum(lambda[1:i]^2) * delta) # model volatilities
  }
  
  sum((model_volatilities - sigma)^2) # sum of squared errors
}

# Function to compute model volatilities
# swaps: vector of swap rates
# impliedVols: vector of implied volatilities
# tenors: vector of swap tenors
# delta: time step
# bestGuessLambda: best guess for lambda
modelVolatil = function(swaps,  impliedVols, tenors, delta, bestGuessLambda) {
  spot = spotRate(swaps, tenors) # spot rates
  forward = bootstrapForwardRates(swaps, tenors, spot) # forward rates
  LambdaInit = rep(bestGuessLambda, length(forward)) # initial lambda values
  result = optim(LambdaInit, volObjectiveFun, forward_rates = forward, sigma = impliedVols, delta = delta) # optimization
  optimLambda = result$par # optimal lambda values
  modelVols = sqrt(cumsum(optimLambda^2) * delta) # model volatilities
  RMSE = sqrt(mean((modelVols - impliedVols)^2)) # RMSE
  return(list(Volatilities = modelVols, RMSE = RMSE))
}

#####################
## Task 5: Results ##
#####################
# We take the ICAP implied volatilities from the market data as the implied volatilities.
# Then we use the model to compute the model volatilities, with swaps.

# loading data for model volatility calculation:
delta = 0.5 # time step
real = c("12-11") # date
vols = c("EU2Y3LATM.ICAP", "EU3Y6LATM.ICAP", "EU4Y6LATM.ICAP", "EU5Y6LATM.ICAP", "EU7Y6LATM.ICAP", "EU10Y6LATM.ICAP", "EU12Y6LATM.ICAP", "EU15Y6LATM.ICAP", "EU20Y6LATM.ICAP") # implied volatilities from market data
taskFiveVols = realiser(ratePicker(dat, vols, "2006-01-01", "2006-12-31"), real, 2006, 2006) # generating data frame
taskFiveVols = as.numeric(taskFiveVols[, -1])
taskFiveVols

tenor = c(2, 3, 4, 5, 7, 10, 12, 15, 20) # tenors
swaps = c("eurirs2y", "eurirs3y", "eurirs4y", "eurirs5y", "eurirs7y", "eurirs10y", "eurirs12y", "eurirs15y", "eurirs20y") # swaps
taskFiveSwaps = realiser(ratePicker(dat, swaps, "2006-01-01", "2006-12-31"), real, 2006, 2006) # generating data frame
taskFiveSwaps = as.numeric(taskFiveSwaps[, -1])
taskFiveSwaps

# Computing model volatilities:
stationaryVolatility = modelVolatil(taskFiveSwaps, taskFiveVols, tenor, delta, bestGuessLambda = 0.01); stationaryVolatility

#######################
## Task 6: Algorithm ##
#######################
## Libor Market Model, Single Factor ##
# L = vector of initial forward libor rates
# Lambda = vector of stationary volatilities
# dt = time step
# K = number of time steps
# N = number of simulations
lmm.sf = function(L, Lambda, dt, K, N) {
  ## Initialize output list
  output = data.frame(k = numeric(), m = numeric(), Lambda = numeric(), `µm(Tk)` = numeric(), `W(Tk)` = numeric(), `Lm(Tk)` = numeric())  
  ## Simulate Ns sims
  for (n in 1:N) { # Loop over n simulations
    L.prev = L[-1] # Previous libor rate
    ## Simulate K time steps
    for (k in 1:K) { # Loop for over time steps
      if (k == 1) {
        L.prev = L[-1] # Previous libor rate at time k = 1
      } else {
        L.prev = c(L[k], L.prev[-1]) # Previous libor rate at time k > 1
      }
      ## Simulate W(Tk)
      Wk = rnorm(1)
      ## Simulate m periods at time k
      for (m in k:(length(L) - 1)) { # Loop over m periods at time k
        sum.mu = 0 # initialize first joint of mu
        ## Sum over i = k to m for the first part of mu
        for (i in k:m) {
          sum.mu = sum.mu + dt * L.prev[i - k + 1] * Lambda[m - k + 1] * Lambda[i - k + 1] / (1 + dt * L.prev[i - k + 1])
        }
        ## Calculate mu and Lm(Tk)
        mu = sum.mu - Lambda[m - k + 1]^2 / 2 # calculate mu
        L.new = L.prev[m - k + 1] * exp(mu * dt + sqrt(dt) * Lambda[m - k + 1] * Wk) # calculate new libor rate
        ## Save output in output dataframe
        output = rbind(output, data.frame(k = k, m = m, Lambda = Lambda[m - k + 1], `µm(Tk)` = mu, `W(Tk)` = Wk, `Lm(Tk)` = L.new))
        L[m - k + 1] = L.new
      }
    }
  }
  ## Return Output dataframe
  return(output)
}

#####################
## Task 6: Results ##
#####################
# Using the LMM single-factor model we derived from class, we attempt to simulate the forward rates.
# We use the stationary volatilities from the model volatility calculation as the input for the simulation.
# We use the initial forward rates from the bootstrap as the input for the simulation.
# Function should work, any issues will arise from inputs.

# Inputs
initForwards = bootstrapForwardRates(taskFiveSwaps, tenor, spotRate(taskFiveSwaps, tenor)); initForwards # initial forward rates
initLambda = stationaryVolatility$Volatilities; initLambda # initial lambda values

# Run simulation
results = lmm.sf(L = initForwards, Lambda = initLambda, dt = 0.25, K = 7, N = 1); results # print results

#######################
## Task 7: Algorithm ##
#######################
# Function to price the exotic swap
priceExoticSwap = function(forwardRates, tenor, paymentDates, swapMaturity) {
  cashFlows = numeric(length(paymentDates))
  discountFactors = numeric(length(paymentDates))
  
  # Compute the cash flows and discount factors at each payment date
  for (i in 1:length(paymentDates)) {
    cashFlows[i] = ifelse(paymentDates[i] <= swapMaturity, forwardRates[i], 0)
    discountFactors[i] = (1 + cashFlows[i])^(-tenor[i])
  }
  
  # The price of the swap is the present value of the future cash flows
  swapPrice = sum(cashFlows * discountFactors)
  
  return(swapPrice)
}

# Function to compute the Value-at-Risk (VaR) of the exotic swap
computeVaR = function(returns, confidenceLevel = 0.95) {
  VaR = quantile(returns, probs = 1 - confidenceLevel)
  return(VaR)
}

########################
## Task 7: Results(?) ##
########################
# We are not exactly sure how to do this.
# We settled on a discounted cashflow method, knowing it is not 100% correct.
# The VaR and Spreads algorithms should be right nonetheless.

# Fetching data:
real = c("06-11", "12-11") # realised dates
underlying = realiser(ratePicker(dat, "euribor6md", "2007-01-01", "2020-12-31"), real, 2007, 2020) # generating data frame
underlying # underlying rate for the exotic swap

# Computing semiannual spread from first payment date to last payment date:
taskSevenSpread = swapSpread(underlying, "euribor6md", lower = 0.02, upper = 0.06, digiCoupon) # computing quarterly spread
taskSevenSpread # spreads of the underlying following the contract

# Inputs for the exotic swap pricing
tenor = c(2, 3, 4, 5, 7, 10, 12, 15, 20) # tenors
paymentDates = seq(from = as.Date("2007-01-01"), to = as.Date("2020-12-11"), by = "6 months")
swapMaturity = 14 # years

# Pricing the exotic swap
swapPrice = priceExoticSwap(forwardRates = initForwards, tenor = tenor, paymentDates = paymentDates, swapMaturity = swapMaturity)
print(swapPrice)

# Calculating the returns of the exotic swap
returns = diff(log(initForwards[-1])) # assuming the returns follow a log-normal distribution

# Calculating the VaR
VaR = computeVaR(returns)
print(VaR)

####################
## Final Comments ##
####################
# We found the assignment very challenging.
