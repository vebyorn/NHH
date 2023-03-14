rm(list=ls())

## Exotic Options ###############################################

## Asian Call Option
# K = Strike Price
# S0 = Stock Price today
# sigma = volatility
# r = risk-free rate
# T = time to maturity
# dt = time interval
# n = number of simulations
azn.call = function(K, S0, sigma, r, T, dt, n) {
    t = seq(dt, T, by = dt)
    m = length(t)
    e = exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * rnorm(m * n))
    e = matrix(e, m, n)
    S = S0 * apply(e, 2, cumprod)
    S = rbind(S0, S)
    Si = apply(S, 2, mean)
    Ci = exp(-r * T) * pmax(Si - K, 0)
    Cn = mean(Ci)
    SE = sd(Ci) / sqrt(n)
    return(c(Cn, SE))
}

azn.outputs = azn.call(K = 100, S0 = 100, sigma = 0.2, r = 0.01, T = 2, dt = 1/12, n = 10000)
Cn = azn.outputs[1] # Numerical Call Option Price
SE = azn.outputs[2] # Standard Error
Cn; SE # Results

## Look-back Put Option
# S0 = Stock Price today
# sigma = volatility
# r = risk-free rate
# T = time to maturity
# dt = time interval
# n = number of simulations
lb.put = function(S0, sigma, r, T, dt, n) {
    t = seq(dt, T, by = dt)
    m = T/dt
    e = exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * rnorm(m * n))
    e = matrix(e, m, n)
    S = S0 * apply(e, 2, cumprod)
    S = rbind(S0, S)
    K = S[m + 1, ] # K = S(T)
    Si = apply(S, 2, max) # Si = max(S(t))
    Pi = exp(-r * T) * pmax(Si - K, 0)
    Pn = mean(Pi)
    SE = sd(Pi) / sqrt(n)
    return(c(Pn, SE))
}

lb.outputs = lb.put(S0 = 100, sigma = 0.2, r = 0.01, T = 2, dt = 1/12, n = 10000)
Pn = lb.outputs[1] # Numerical Put Option Price
SE = lb.outputs[2] # Standard Error
Pn; SE # Results

## Deterministic volatility function
# S0 = inital stock price
# C = coefficients
det.sigma = function(S0) {
  s = matrix(nrow = 3, ncol = 1, data = c(0.05, 0.3, 0.5))
  A = matrix(nrow = 3, ncol = 3, data = c(1, 1, 1, 50, 100, 150, 50^2, 100^2, 150^2))
  C = solve(A, s)
  sigma = C[1] + C[2] * S0 + C[3] * S0^2
  return(sigma)
}

## European Call with deterministic volatility
# K = Strike Price
# S0 = Stock Price today
# r = risk-free rate
# T = time to maturity
# dt = time interval
# n = number of simulations
# f = volatility function
deter.call = function(K, S0, r, T, dt, n, f) {
    # Stock price simulation
    m = T/dt
    t = seq(dt, T, by = dt)
    sigma = f(S0)
    e = exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * rnorm(m * n))
    e = matrix(e, m, n)
    S = S0 * apply(e, 2, cumprod)
    S = rbind(S0, S)
    
    # European call option payoff
    Si = S[m + 1, ] # Si = S(T)
    Ci = exp(-r * T) * pmax(Si - K, 0)

    # Results
    Cn = mean(Ci)
    SE = sd(Ci) / sqrt(n)
    return(c(Cn, SE))
}

deter.outputs = deter.call(K = 100, S0 = 100, r = 0.01, T = 2, dt = 0.25, n = 100000, f = det.sigma)
Cn = deter.outputs[1] # Numerical Call Option Price
SE = deter.outputs[2] # Standard Error
Cn; SE # Results
