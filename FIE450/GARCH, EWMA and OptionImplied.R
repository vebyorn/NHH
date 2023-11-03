## Problem 3: Volatility Measures
##########
## Data ##
##########
stox <- read.csv("STOXX50Edaily.csv") # Source: Yahoo Finance
stox$Volume <- NULL
stox <- stox[, c(1, 5)]
stox$Date <- as.Date(stox$Date)
stox$Close <- as.numeric(stox$Close)
stox <- na.omit(stox)
stox$r <- c(NA, diff(log(stox$Close)))
head(stox)

r <- na.omit(stox$r)
plot(stox$Date, stox$Close, type = "l", xlab = "", ylab = "STOX50E Close")

#############################################
## Standard Volatility Measures: 0.2290299 ##
#############################################
mu <- mean(stox$r, na.rm = TRUE) * 312
sigma <- sd(stox$r, na.rm = TRUE) * sqrt(312) # STARDARD DEVIATION: 0.2290299

# Plotting the Volatility
n <- length(r)
SE <- sd(r)/sqrt(n) * 312 # STANDARD ERROR: 0.05746618
plot(stox$Date, stox$r, type = "l", xlab = '', ylab = "STOXX50E Vol.")

###########################
## EWMA Model: 0.1485823 ##
###########################
lambda <- 0.94
sigma2 <- sum((1 - lambda) * lambda^(0:(length(r) - 1)) * rev(r)^2)
sigma <- sqrt(sigma2)
sigma * sqrt(250) # EWMA: 0.1485823

############################
## GARCH Model: 0.2399755 ##
############################
garch.var <- function(r, omega, alpha, beta) {
  sigma2 <- r[1]^2
  for (i in 2:length(r)) {
    sigma2 <- c(sigma2, omega + alpha * r[i]^2 + beta * sigma2[i - 1])
  }
  return(sigma2)
}
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

result <- nlminb(c(0.001, 0.3, 0.3), garch.ll, lower = 1e-06, upper = 1 - 1e-06, r = r)
result

omega <- result$par[1]
alpha <- result$par[2]
beta <- result$par[3]
gamma <- 1 - alpha - beta
long.t.var <- omega/gamma
sqrt(long.t.var * 312) # GARCH: 0.2399755

###############################
## Option-Implied Volatility ##
###############################
# Call Option #
c.market <- 102.11 # Daily Settlement Jan. 31, 2023 for: (Not very liquid) (Frankfurt Stock Exchange)
# STOXX50E call with strike price 4175 expiring on April 30, 2024 (EUREX)
S0 <- 4163.45 # Closing price Jan. 31, 2023.
K <- 3924.80 # Strike price
T <- as.numeric(as.Date("2024-04-30") - as.Date("2023-01-31"))/365 # Maturity
rf <- 1.03413^T-1 # 1-year euroibor rate adjusted for time-horizon # 1 year Euribor rate

# European Call Option #
call <- function (S0, sigma, K, rf, T) {
  d1 <- (log(S0/K) + (rf + sigma^2/2) * T)/sigma/sqrt(T)
  d2 <- d1 - sigma * sqrt(T)
  c <- S0 * pnorm(d1) - K * exp(-rf * T) * pnorm(d2)
  return(c)
}

# Objective function #
obj <- function(sigma, c.market, S0, K, rf, T) {
  c.model <- call(S0, sigma = sigma, K, rf, T)
  se <- (c.model - c.market)^2
  return(se)
}

# Minimise the objective function #
minres <-  nlm(obj, p=0.2, c.market=c.market, S0=S0, K=K, T=T, rf=rf)
minres # Implied Volatility: 0.01510761 (Ridiculously low)
# consider the fact it isnt frequently traded
# and the fact that the market price is not very liquid
# poor liquidity and low frequency of trading = bad estimate of implied volatility
## Standard Deviation: 0.2290299
## EWMA: 0.1485823
## GARCH: 0.2399755
## Implied Volatility: 0.01510761 (poor estimate?)
