rm(list=ls()) ## Clear the workspace

###############
## Problem 1 ##
###############
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

#################
## GARCH Model ##
#################
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
sqrt(long.t.var * 250) # Long-term volatility, Garch

#Trading days-1
m <- 912
garch.sigma2 <- garch.var(r, omega, alpha, beta)
garch.sigma2.t <- garch.sigma2[length(garch.sigma2)]
garch.sigma2.vec <- long.t.var + (alpha + beta)^(0:m) * (garch.sigma2.t -long.t.var)

garch.sigma.vec <- sqrt(garch.sigma2.vec)
plot(garch.sigma.vec * sqrt(913), xlab = "Days", ylab = "GARCH Volatility",
     type = "l")

#Get garchsigma, higher due to mean reversion to "TRUE" long-term volatility
garch.sigma = sqrt(sum(garch.sigma2.vec))
garch.sigma


###############
## Problem 2 ##
###############
# 3 year and 8 months yield AAA rated bonds from ECB is:
# 2,237%.
# https://www.ecb.europa.eu/stats/financial_markets_and_interest_rates/euro_area_yield_curves/html/index.en.html
# Utilize ECB government bonds due to no EURIBOR rate beyond 1 year rate.
# As mentioned in previous taks, just utilizing a 3 year yield will not be appropriate,
# due to the yield curve not being horizontal. Thus we cannot use Euribor since anything
# Besides a 1 year rate is not listed. Using 1 year would perhaps not cause a huge deviation
# for our purpose, but feel ECB is a better estimate none the less. 

###############
## Problem 3 ##
###############
## Monte-Carlo Parameters
#Fetch garch.sigma from task 1 
## We opted not to use VSTOXX index as it was currently in a low volatility period.
S0 <- 4163.45 ## EURO STOXX 50 Closing Jan. 31, 2023.
T <- as.numeric(as.Date("2026-09-25") - as.Date("2023-01-31"))/365 # Maturity
rf <- 0.02237 # 3 year and 8 months yield AAA rated bonds from ECB.
f <- 1/40.58 # ratio
n <- 10000 # number of simulations
dt <- 1/250 # time step (1/250 = 1 trading day)
garch.sigma <- garch.sigma # garch volatility
sigma <- garch.sigma ## We use the GARCH volatility measure.

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
  X0 <- ifelse(S >= 3449.997, 124, ifelse(S >= 2638.233, 100, 100 * S/4058.82))
  X0 <- exp(-rf * T) * X0 ## discounting
  return(X0)
}

X0 <- payoff(S) ## storing payoffs
mean(X0) ## average payoff

X0v <- c(X0) ## vectorising payoffs
hist(X0v, 100, main = "Histogram of Payoffs", xlab = "Payoff", col = "orange") # checking that we didn't screw up
max(X0v) > 124 ## further screw-up check

## Monte Carlo Error
SE <- sd(X0) / sqrt(length(X0)) ## standard error formula
alpha <- 0.99 ## confidence level
z <- qnorm(1 - (1 - alpha) / 2) ## z-score

## ANSWERS PROBLEM 3 ##
c(mean(X0) - z * SE, mean(X0), mean(X0) + z * SE) ## confidence interval
SE ## standard error (n observations)

####################
### Final comment###
####################
##Observe that true price in market is approximately 99
##This implies a bid/ask spread of 10% compared to our
## simulation. This may occur due to two reasons.
## For one our volatility estimate may be off,
## and thus there is some modelling error involved
## However it is at least equally important, if not more
## that the lack of liquidity and trading in the instrument
## leasds to a liquidity premium, thus the asset ends up
## being over-priced compared to expected pay-off. 