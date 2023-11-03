#############
## Options ##
#############
S0 <- 1088.71 # OBX index
K <- 1080 # Strike price
T <- as.numeric(as.Date("2023-12-15") - as.Date("2023-01-06"))/365 # Maturity
rf <- 0.038 # Risk-free interest rate

###################
## Black-Scholes ##
###################
call <- function(S0, K, T, rf, sigma) {
  d1 <- (log(S0/K) + (rf + sigma^2/2)*T)/(sigma*sqrt(T)) ## careful to bracket the last division
  d2 <- d1 - sigma*sqrt(T)
  C <- S0*pnorm(d1) - K*exp(-rf*T)*pnorm(d2)
  return(C)
}
C.model <- call(S0, K, T, rf, 0.2)
C.market <- 118.21

## Objective funciton ##
(C.model - C.market)^2

## Optimization ##
obj.fun <- function(sigma, C.market, S0, K, T, rf) {
  C.model <- call(S0, K, T, rf, sigma=sigma)
  eps <- (C.model - C.market)^2
  return(eps)
}

res <- nlm(obj.fun, p=0.01, C.market=C.market, S0=S0, K=K, T=T, rf=rf)
res

### When to use options implied and when to use historical time-series methods
# 1. Option prices means you have to believe in the prices
# 2. Options must be traded, and frequently so
# 3. Options must be liquid
# 4. You would use options whenever possible
# 5. You would use historical time-series methods when options are not available

############################
## Monte-Carlo Simulation ##
############################
# Trivial diceroll example
n <- 1000 # number of simulations
roll <- sample(1:6, n, replace = TRUE) # replace = TRUE means that the same number can be drawn multiple times
roll[1:10] # first 10 rolls
table(roll) # frequency table
mean(roll) # average roll

# Monte-Carlo Parameters
sigma <- 0.215122 # VDAX-NEW volatility
S0 <- 14490.78 # DAX index January 4th 2023
T <- as.numeric(as.Date("2023-03-30") - as.Date("2023-01-04"))/365 # Maturity
b <- K <- 13800 # Strike price and boundary are identical
rf <- 0.0217 # Risk-free interest rate
f <- 1/100 # ratio
n <- 5000 # number of simulations
dt <- 1/250 # time step (1/250 = 1 trading day)

# Monte-Carlo Simulation
# generating a single path
simulate.path <- function(S0, rf, sigma, dt, T) {
  t <- seq(dt, T, by=dt) # time vector
  S <- S0
  for (i in 1:length(t)) {
    Si <- S[length(S)]*exp((rf - 0.5*sigma^2)*dt + sigma*sqrt(dt)*rnorm(1))
    S <- c(S, Si)
}
  return(S)
}

# plotting a single path
S <- simulate.path(S0, rf, sigma, dt, T)
plot(S, type='l')
abline(h=S0, col='red')
abline(h=b, col='blue')

# simulating many paths
simulate.paths <- function(S0, rf, sigma, dt, T, n) {
  S <- c()
  for (i in 1:n) {
    S <- cbind(S, simulate.path(S0, rf, sigma, dt, T))
}
  return(S)
}

S <- simulate.paths(S0, rf, sigma, dt, T, n=5000)
S[1:10, 1:10] # first 10 rows and columns

# using nested loops in R is computationally slow
# smarter solution to simulating many paths exist
# a multiple path example
simulate.paths.fast <- function(S0, rf, sigma, dt, T, n) {
  t <- seq(dt, T, by=dt)
  m <- length(t)
  e <- exp((rf - 0.5*sigma^2)*dt + sigma*sqrt(dt)*rnorm(m*n))
  e <- matrix(e, m, n)
  S <- S0 * apply(e, 2, cumprod)
  S <- rbind(S0, S)
  return(S)
}

S <- simulate.paths.fast(S0, rf, sigma, dt, T, n=5000)
S[1:10, 1:10] # first 10 rows and columns

# check speed improvement
system.time(S1 <- simulate.paths(S0, rf, sigma, dt, T, n=5000)) # 2.29 sec
system.time(S2 <- simulate.paths.fast(S0, rf, sigma, dt, T, n=5000)) # 0.05 sec :D

# set random number generator seed
set.seed(1) # class used seed 1
S <- simulate.paths.fast(S0, rf, sigma, dt, T, n=5000) # simulate the paths
S[1:5, 1:5] # checking the first 5 rows and columns

# payoff function
payoffs <- function(S, K, b, rf, f) {
  I <- apply(S > b, 2, all)
  X <- I * pmax(S[nrow(S), ] - K, 0) * f
  X0 <- exp(-rf * T) * X
  return(X0)
}

X0 <- payoffs(S, K, b, rf, f) # storing payoffs
length(X0) # length of payoff vector
mean(X0) # average payoff

## Monte-Carlo Error ##
# example of variation
for (i in 1:10) {
  S <- simulate.paths.fast(S0, rf, sigma, dt, T, n=5000)
  X0 <- payoffs(S, K, b, rf, f)
  C <- mean(X0)
  cat("i:", i, " ", C, "\n")
}

SE <- sd(X0)/sqrt(length(X0)) # standard error formula
alpha <- 0.99 # confidence level
z <- qnorm(1 - (1 - alpha)/2) # z-score
c(mean(X0) - z*SE, mean(X0), mean(X0) + z*SE) # confidence interval
SE # standard error

### Variance Reduction
## Antithetic Variates
## Simulates 2*n antithetic pairs of price paths.
## S0: today's price
## rf: risk-free interest rate
## sigma: volatility
## dt: time step
## T: time horizon
## n: number of paired paths
## Returns the simulated paths as a matrix
simulate.paths.fast.as <- function(S0, rf, sigma, dt, T, n) {
  t <- seq(dt, T, by=dt) # timeline
  m <- length(t) # number of time steps
  z <- rnorm(m * n) # n random numbers
  z.as <- -z # antithetic n random numbers
  Z <- matrix(c(z, z.as), m, n * 2) # matrix of random numbers
  e <- exp((rf - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
  S <- apply(e, 2, cumprod)
  S <- rbind(S0, S0 * S)
  return(S)
}

n <- 2500 # number of simulations
set.seed(1) # class used seed 1
S.as <- simulate.paths.fast.as(S0, rf, sigma, dt, T, n) # storing antithetic paths
S.as[1:3, 1:3] # checking

# plotting both paths
plot(S.as[, 1], type='l', col='blue', xlab='', ylab='', ylim=range(S.as[, c(1, n + 1)])) # original path
lines(S.as[, n + 1], type='l', col='red') # anti-thetic path

# payoff function
X0.as <- payoffs(S.as, K, b, rf, f) # storing payoffs
length(X0.as) # length of payoff vector
head(X0.as) # first 6 payoffs
mean(X0.as) # estimated payoff
# we can no longer calculate sd as previously
# we instead do the following
X.pairs <- (X0.as[1:n] + X0.as[(n + 1):(2 * n)]) / 2
SE <- sd(X.pairs)/sqrt(n) # significant improvement to SE, from 0.17 to 0.12
# not the most efficient SE redux technique, but requires only 2 lines of code
SE

alpha <- 0.99
z <- -qnorm(1 - (1 - alpha)/2)
c(mean(X0.as) - z*SE, mean(X0.as), mean(X0.as) + z*SE)

cor(X0.as[1:n], X0.as[(n+1):(2*n)]) # correlation between the two sets of payoffs
# it should be negatively correlated, thus we verify the improvement in SE

## result interpretation
## Check table on page 58 in pdf
# if we assume our mean (monte-carlo analysis) is correct the bid/ask spread is given by
abs(7.69/mean(X0.as) - 1) * 2 # multiply by two as it is antithetical, 7.69 is the closign price

n <- 25000 # number of simulations
set.seed(1) # class used seed 1 (we recycle this code)
S.as <- simulate.paths.fast.as(S0, rf, sigma, dt = 1/250/2, T, n) # measurin at twice a trading day
X0.as <- payoffs(S.as, K, b, rf, f) # storing payoffs
mean(X0.as) # estimated payoff

X.pairs <- (X0.as[1:n] + X0.as[(n + 1):(2 * n)])/2
SE <- sd(X.pairs)/sqrt(n)
c(mean(X0.as) - z * SE, mean(X0.as) + z * SE)