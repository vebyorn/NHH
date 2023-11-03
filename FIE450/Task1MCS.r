# Problem 5: Monte Carlo Simulation #
# Monte-Carlo Parameters
sigma <- 0.185683 # VSTOXX: EURO STOXX 50 Volatility Index
S0 <- 4163.45 # EURO STOXX 50 Closing Jan. 31, 2023.
T <- as.numeric(as.Date("2024-04-30") - as.Date("2023-01-31"))/365 # Maturity
K <- 3924.80 # Strike price
b <- 3139.84 # Barrier
cap <- 4866.75 # Cap
rf <- 1.03413^T-1 # 1-year euroibor rate adjusted for time-horizon
f <- 1/39.248 # ratio
n <- 100000 # number of simulations
dt <- 1/250 # time step (1/250 = 1 trading day)

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

# payoff function
payoffs <- function(S, K, b, rf, f) {
I <- apply(S > b, 2, all)
X <- I * pmax((S[nrow(S), ] - K), 0) * f
X0 <- exp(-rf * T) * X
return(X0)
}

# attempt payoff
payoffs2 <- function(S, K, b) {
  # if S > k, then X = 124
  for (i in seq_len(nrow(S))) {
    if (S[i, ] > b) {
      X <- 124
    } else {
    # if b < S < k, then X = 100
    if (S > b) {
        X <- 100
        } else {
          X <- 100*(S/b)
    }
  }
    return(X)
  }

X0 <- payoffs2(S, K, b, rf, f) # storing payoffs
plot(X0, type = "l", col = "blue", lwd = 2, xlab = "Time", ylab = "Payoff")
abline(h = 120, col = "red")
# if the cap of 120 is reached return 120
X0.adj <- ifelse(X0 > 124, 124, X0)
plot(X0.adj, type = "l", col = "blue", lwd = 2, xlab = "Time", ylab = "Payoff")



# Monte Carlo Error
SE <- sd(X0) / sqrt(length(X0)) # standard error formula
alpha <- 0.99 # confidence level
z <- qnorm(1 - (1 - alpha) / 2) # z-score
c(mean(X0) - z * SE, mean(X0), mean(X0) + z * SE) # confidence interval
SE # standard error (n = 5000)

anti.paths <- function(S0, rf, sigma, dt, T, n) {
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

n <- 50000 # number of simulations
S.as <- anti.paths(S0, rf, sigma, dt, T, n) # storing antithetic paths
X0.as <- payoffs(S.as, K, b, rf, f) # storing payoffs
mean(X0.as) # estimated payoff
X.duo <- (X0.as[1:n] + X0.as[(n + 1):(2 * n)]) / 2
sd(X.duo)/sqrt(n) # standard error, antithetic n = 2500

## Problem 5 ##
SE # Standard Error / Monte Carlo Error
sd(X.duo)/sqrt(n) # Antithetic Monte Carlo Error
c(mean(X0) - z * SE, mean(X0) + z * SE) # Confidence Interval
mean(X0) # Monte Carlo Estimate
