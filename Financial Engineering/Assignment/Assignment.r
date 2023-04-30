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
csv = read.csv("C:/Users/vebky/Desktop/EUR-Market-Data.csv") # reading in csv
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
taskOne = realiser(ratePicker(dat, "euribor3md", "2007-01-01", "2013-12-31"), real, 2007, 2013) # generating data frame
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
  if (dcount == "act360") {
    d1 = as.Date(t1)
    d2 = as.Date(t2)
    days = as.numeric(d2 - d1)
    delta = days / 360
  } else if (dcount == "act365") {
    d1 = as.Date(t1)
    d2 = as.Date(t2)
    days = as.numeric(d2 - d1)
    delta = days / 365.242
  } else if (dcount == "30/360") {
    y1 = as.numeric(format(t1, "%Y"))
    y2 = as.numeric(format(t2, "%Y"))
    m1 = as.numeric(format(t1, "%m"))
    m2 = as.numeric(format(t2, "%m"))
    d1 = as.numeric(format(t1, "%d"))
    d2 = as.numeric(format(t2, "%d"))
    if (d1 == 31) {
      d1 = 30
    }
    if (d2 == 31 & d1 >= 30) {
      d2 = 1
      m2 = m2 + 1
    }
    days = 360 * (y2 - y1) + 30 * (m2 - m1 - 1) + max(0, 30 - d1) + min(30, d2 - 1)
    delta = days / 360
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
  f0T1T2 = F - (sigma^2*T1*T2)/2
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
  val = valueExtract(df, rates, year, month, day)
  mat = delta(as.Date(paste(year, month, day, sep="-")), maturity, dcount)
  mat_vec = c() # initializing vector
  for (i in 1:length(rates)) { # looping through rates
    mat_vec = c(mat_vec, mat[[i]]) # extracting values
  }
  fin = list(val, mat_vec)
  # rename list elements
  names(fin) = c("price", "maturity")
  return(fin)
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
# Overnight deposits
onRates = c("eurond", "eurtnd") # overnight deposits
onDep = valueExtract(dat, onRates, 2006, 12, 11) # fetching rates
onDep

# Money Market deposits
mmRates = c("euriborswd", "euribor1md") # money market deposits
mmMats = c("2006-12-18", "2007-01-11") # maturities
mmDep = matValueExtract(dat, mmRates, 2006, 12, 11, maturity = mmMats, dcount = "act360") # fetching rates and maturities
mmDep

# Futures
futRates = c("FEIcm1", "FEIcm2", "FEIcm3", "FEIcm4", "FEIcm5") # futures
futMats = c("2007-01-15", "2007-06-12", "2007-09-11", "2007-12-11", "2008-03-11", "2008-06-11") # maturities
futDep = futureValue(dat, futRates, 2006, 12, 11, maturity = futMats, dcount = "act360", init = TRUE) # fetching rates and maturities
futDep

# Swaps
swapRates = c("eurirs2y", "eurirs3y", "eurirs4y", "eurirs5y", "eurirs7y", "eurirs10y", "eurirs12y", "eurirs15y", "eurirs20y") # swaps
swapMats = c("2008-12-11", "2009-12-13", "2010-12-12", "2011-12-11", "2013-12-11", "2016-12-11", "2018-12-11", "2021-12-12", "2026-12-13") # maturities
swapDep = matValueExtract(dat, swapRates, 2006, 12, 11, maturity = swapMats, dcount = "act360") # fetching rates and maturities
swapDep

##############################
## Task 4: Results and Plot ##
##############################
# Computing discount factors:
taskFour = bootstrap(ON = onDep, MM = mmDep, Fut = futDep, IRS = swapDep)
taskFour
taskFourPlot = data.frame(description = c("ON/TN", "MM", "FEIcm1", "FEIcm2", 
                                          "FEIcm3", "FEIcm4", "FEIcm5","2Y",
                                          "3Y", "4Y", "5Y", "7Y", "10Y", "12Y", 
                                          "15Y", "20Y"),
                 year=taskFour$t,
                 rate=taskFour$Z)

# Ploting the Discount Factors:
plot(x=taskFourPlot$year,
     y=taskFourPlot$rate,
     type="l",
     xlab = "Time (Years)",
     ylab = "Discount Factor")
points(x=taskFourPlot$year,
       y=taskFourPlot$rate,
       col="blue", pch=21)
