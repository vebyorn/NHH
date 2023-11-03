rm(list=ls()) ## Clear the workspace

###############
## Problem 1 ##
###############
## Declaring logic of payout as function, referencing the termsheet.
## po.dia = payoff diagram.
po.dia <- function(Index_reference_level) {
  if (Index_reference_level >= 3924.80) {
    po <- 100 * min(Index_reference_level/3924.80, 124/100)
  } else if (Index_reference_level >= 3139.84) {
    po <- 100
  } else {
    po <- 100 * Index_reference_level/3139.84
  }
  return(po)
}

ind.ref.lvls <- seq(1000, 5000) ## declaring sequence of index reference levels for x-axis
payoffs <- sapply(ind.ref.lvls, po.dia) ## applying po.dia to a sequence of values

## plotting the payoffs
plot(ind.ref.lvls, payoffs,
     type = "l",
     xlab = "Index Reference Level",
     ylab = "Payoff",
     main = "Airbag Certificate with Cap Payoff Diagram")

###############
## Problem 2 ##
###############
S0 <- 4163.45 # EURO STOXX 50 Closing Jan. 31, 2023.
T <- as.numeric(as.Date("2024-04-30") - as.Date("2023-01-31"))/365 ## Maturity
rf <- 1.03413^T-1 ## 1-year euribor rate adjusted for time-horizon
garch.sigma <- 0.2532165 ## Explanation in Problem 3

## Black-Scholes European Call Option Formula
call <- function(S0, sigma, K, rf, T) {
  d1 <- (log(S0/K) + (rf + sigma^2/2) * T)/sigma/sqrt(T) # parameter 1 of black-scholes
  d2 <- d1 - sigma * sqrt(T) # parameter 2 of black-scholes
  C <- S0 * pnorm(d1) - exp(-rf * T) * K * pnorm(d2) # call variation of BS formula
  return(C)
}

## We buy 2 calls and sell 2 calls to replicate the payoff diagram.
p.call1 <- call(S0, sigma = garch.sigma,0 ,rf,T)
call.1.mod.p <- p.call1 * (100/3139.84)

p.call2 <- call(S0, sigma = garch.sigma,3924.8,rf,T)
call.2.mod.p <- p.call2 * 24/(4866.72-3924.8)

p.hedge1 <- call(S0, sigma = garch.sigma,3139.84,rf,T)
hedge.1.mod.p <- p.hedge1 * (100/3139.84)

p.hedge2 <- call(S0, sigma = garch.sigma,4866.752,rf,T)
hedge.2.mod.p <- p.hedge2 * 24/(4866.72-3924.8)

airbag.val <- call.1.mod.p + call.2.mod.p - (hedge.1.mod.p + hedge.2.mod.p)
airbag.val ## Value of the replicated airbag certificate.
## Quite close to actual trading close on 31. Jan. 2023, approx. 102.15, source: xmarkets.db.com.

###############
## Problem 3 ##
###############
## We use the GARCH(1,1) model to estimate the volatility of the underlying.
## We also calculate EWMA, Standard Deviation and Option-Implied Volatility.
## The reason we choose GARCH(1,1) is due to the fact that option-implied
## volatility is not usable. Due to the fact that the option is not traded
## as frequently and thus is not liquid enough to be used as a proxy for
## the underlying volatility.
## As for standard deviation and EWMA, we do not use SD as it is not
## very precise. We consider the time horizon long enough
## to warrant an adjustment for mean reversion in the volatility measure.
## EWMA is not mean-reverting, which is therefore not used.
## GARCH however, is, so we deem this more appropriate.
## Our GARCH calculation resulted in a volatility of: 0.2681181

## Reading in the data.
## We sampled it to daily data, sourced from Yahoo Finance.
stox <- read.csv("STOXX50Edaily.csv") # Source: Yahoo Finance.
stox$Volume <- NULL # We do not need the volume data.
stox <- stox[, c(1, 5)] # We only need the date and closing price.
stox$Date <- as.Date(stox$Date) # We need the date to be a date object.
stox$Close <- as.numeric(stox$Close) # We need the closing price to be numeric.
stox <- na.omit(stox) # We need to omit the NA values.
stox$r <- c(NA, diff(log(stox$Close))) # We need the daily returns.
head(stox)

r <- na.omit(stox$r) # We omit values again, just in case, and save the column as a vector "r".

#############################################
## Standard Volatility Measure:  0.2290586 ##
#############################################
## We scale the returns up to be over 312 trading days, aprox. 1 year and 3 months of trades.
## This is to make the volatility comparable to the option's maturity.
sigma <- sd(stox$r, na.rm = TRUE) * sqrt(250)
sigma # Standard Deviation

###########################
## EWMA Model: 0.1531811 ##
###########################
lambda <- 0.94 # We use the suggested lambda value, for the sake of brevity.
sigma2 <- sum((1 - lambda) * lambda^(0:(length(r) - 1)) * rev(r)^2)
sigma <- sqrt(sigma2)
sigma * sqrt(250) # EWMA volatility

############################
## GARCH Model: 0.2532165 ##
############################
## Declaring the function for GARCH variance.
garch.var <- function(r, omega, alpha, beta) {
  sigma2 <- r[1]^2
  for (i in 2:length(r)) {
    sigma2 <- c(sigma2, omega + alpha * r[i]^2 + beta * sigma2[i - 1])
  }
  return(sigma2)
}

## Declaring function for GARCH log-likelihood.
garch.ll <- function(params, r) {
  omega <- params[1]
  alpha <- params[2]
  beta <- params[3]
  sigma2 <- garch.var(r, omega, alpha, beta)
  r <- r[-1]
  sigma2 <- sigma2[-length(sigma2)]
  ll <- sum(-log(sigma2) - r^2/sigma2)
  return(-ll)
}

## Minimising the log-likelihood function.
result <- nlminb(c(0.001, 0.3, 0.3), garch.ll, lower = 1e-06, upper = 1 - 1e-06, r = r)

## Storing the result parameters,
omega <- result$par[1]
alpha <- result$par[2]
beta <- result$par[3]
gamma <- 1 - alpha - beta
# long-term variance
long.t.var <- omega/gamma

#Trading days-1
m <- 311
garch.sigma2 <- garch.var(r, omega, alpha, beta)
garch.sigma2.t <- garch.sigma2[length(garch.sigma2)]
garch.sigma2.vec <- long.t.var + (alpha + beta)^(0:m) * (garch.sigma2.t -long.t.var)

garch.sigma.vec <- sqrt(garch.sigma2.vec)
plot(garch.sigma.vec * sqrt(312), xlab = "Days", ylab = "GARCH Volatility",
     type = "l")

#Get garchsigma, higher due to mean reversion to "TRUE" long-term volatility
garch.sigma = sqrt(sum(garch.sigma2.vec))
garch.sigma

##################################################################
## Option-Implied Volatility: Requirements not met (-0.0169289) ##
##################################################################
## Call Option
c.market <- 102.11 # Daily Settlement Jan. 31, 2023 for: (Not very liquid) (X-market)
S0 <- 4163.45 # Closing price Jan. 31, 2023.
K <- 3924.80 # Strike price
T <- as.numeric(as.Date("2024-04-30") - as.Date("2023-01-31"))/365 # Maturity
rf <- 1.03413^T-1 # 1-year euroibor rate adjusted for time-horizon

## European Call Option
call <- function (S0, sigma, K, rf, T) {
  d1 <- (log(S0/K) + (rf + sigma^2/2) * T)/sigma/sqrt(T)
  d2 <- d1 - sigma * sqrt(T)
  c <- S0 * pnorm(d1) - K * exp(-rf * T) * pnorm(d2)
  return(c)
}

## Objective function
obj <- function(sigma, c.market, S0, K, rf, T) {
  c.model <- call(S0, sigma = sigma, K, rf, T)
  se <- (c.model - c.market)^2
  return(se)
}

## Minimise the objective function #
minres <-  nlm(obj, p=0.2, c.market=c.market, S0=S0, K=K, T=T, rf=rf)
minres # Implied Volatility: -0.0169289 (Ridiculously low and negative)
## consider the fact it isn't frequently traded
## poor liquidity and low frequency of trading = bad estimate of implied volatility

###############
## Problem 4 ##
###############
## We decided to use 12-month Euribor compounded to 15-months as the risk-free rate.
## The source we used was www.euribor-rates.eu, with the listing for 31.01.2023.
## Which is: 1,03413. We compound it to our time horizon with the following formula:
## 1,03413^(1/12) = answer, then, answer^15 = 1,0428 aproximately. I.e. a rate of: 4,28%.
## Alternatives we considered were ECB and German bond yields.
## We briefly considered German deposit-rates. In task 5 this is not adjusted, since
## this is done in the pay-off function.

###############
## Problem 5 ##
###############
## Monte-Carlo Parameters
S0 <- 4163.45 ## EURO STOXX 50 Closing Jan. 31, 2023.
T <- as.numeric(as.Date("2024-04-30") - as.Date("2023-01-31"))/365 # Maturity
sigma <- garch.sigma ## We use the annual GARCH volatility measure, adjusted for the time horizon.
## We opted not to use VSTOXX index as it was currently in a low volatility period.
K <- 3924.80 ## Strike price
rf <- 0.03413 # 1-year euroibor raten
f <- 1/39.248 # ratio
n <- 100000 # number of simulations
dt <- 1/250 # time step (1/250 = 1 trading day)

## function to simulate the stock price
multi.paths <- function(S0, rf, sigma, dt, T, n) {
  t <- seq(dt, T, by=dt)
  m <- length(t)
  e <- exp((rf - 0.5*sigma^2)*dt + sigma * sqrt(dt) * rnorm(m*n))
  e <- matrix(e, m, n)
  S <- S0 * apply(e, 2, cumprod)
  S <- rbind(S0, S)
  return(S)
}

## storing our simulated paths
S <- multi.paths(S0, rf, sigma, dt, T, n)
S[1:5, 1:5] ## making sure we have the right dimensions

## payoff function
payoff <- function(S) {
  X0 <- ifelse(S >= 3924.80, 100 * pmin(S/3924.80, 124/100),
               ifelse(S >= 3139.84, 100, 100 * S/3139.84))
  X0 <- exp(-rf * T) * X0 ## discounting
  return(X0)
}

X0 <- payoff(S) ## storing payoffs
mean(X0) ## average payoff

X0v <- c(X0) ## vectorising payoffs
hist(X0v, 100, main = "Histogram of Payoffs", xlab = "Payoff", col = "blue") # checking that we didn't screw up
max(X0v) > 124 ## further screw-up check

## Monte Carlo Error
SE <- sd(X0) / sqrt(length(X0)) ## standard error formula
alpha <- 0.99 ## confidence level
z <- qnorm(1 - (1 - alpha) / 2) ## z-score

## ANSWERS PROBLEM 5 ##
c(mean(X0) - z * SE, mean(X0), mean(X0) + z * SE) ## confidence interval
SE ## standard error (n observations)

###############
## Problem 6 ##
###############
#1 One simpliyfing assumption is that the we have assumed the interest rate for 1 year and ~1,25 is constant,
## which implies a constant yield curve. Not realistic, though likely limited impact and modelling error.
## All else equal, the present value is not sufficiently discounted .(W)
#2 We also have a constant volatility, which is likely not the case,
## but perhaps not too problematic for one year.
## Future volatility may be both higher and lower,
## but can have a large impact on true value.
#3 Another assumoption is that future returns are lognormally distributed.
## If this is not the case we may understimate the size of the tails, in either
## direction which may distort expected pay-off.
#4 Finally we also assume a liquid and solvent counterparty, such that there is
## 0 counterparty risk. If on the contrary there is one expected pay off
## and thus price of option would be lower. 