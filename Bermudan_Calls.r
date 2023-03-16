####################
## 8th March 2023 ##
####################
rm(list=ls())

## Bermudan Option ###############################################
# K = Strike Price
# S = Stock Price
# sigma = volatility
# r = risk-free rate
# T = time to maturity
# dt = time interval
# b = branches
berm.call = function(K, S, sigma, r, T, dt, b, n) {
    # intrinsic value
    ht = function(S_t, K) {
    return(max(S_t - K, 0)) # ht = payoff at expiry
     }
    # option value
    val = function(K, S_t, sigma, r, T, dt, b) {
        if (T == 0) { # If T = 0 return ht
        return(ht(S_t, K)) # ht = payoff at expiry
        }
        V = rep(0, b)
        for (i in 1:b) { # iterated over branches
            W = rnorm(1) # generate Wiener process
            S_t_next = S_t * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * W) # S(t+dt)
            V[i] = val(K, S_t_next, sigma, r, T - dt, dt, b) # recursive call
        }
        c_t = exp(-r * dt) * mean(V) # continuation value
        V_t = max(ht(S_t, K), c_t) # option value
        return(V_t) # return option value
    }
  # Monte Carlo simulations
  vals = rep(0, n) # vector to store values in
  for (i in 1:n) { # iterated over simulations
    vals[i] = val(K, S, sigma, r, T, dt, b)
  }
  # Return the mean and standard error
  BCn = mean(vals) # mean of option values
  SE = sd(vals) / sqrt(n) # standard error
  return(c(BCn, SE))
}

outputs = berm.call(100, 100, 0.20, 0.01, 2, 0.25, 3, 100)
BCn = outputs[1] # Numerical Call Price
SE = outputs[2] # Standard Error
BCn; SE # Result

## Bermudan Option, Low Estimator #################################
# K = Strike Price
# S = Stock Price
# sigma = volatility
# r = risk-free rate
# T = time to maturity
# dt = time interval
# b = branches
berm.l.est = function(S0, K, sigma, r, T, delta_t, b, n) {
  m = T / delta_t # number of time steps
  dt = T / m # time interval
  
  vals = numeric(n) # vector of option values
  
  for (i in 1:n) { # iterated over simulations
    S = matrix(0, nrow = b, ncol = m + 1) # matrix of stock prices
    S[, 1] = S0 # initial stock price
    
    for (t in 1:m) { # iterated over time steps
      W = rnorm(b) # generate Wiener process
      S[, t + 1] = S[, t] * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * W) # S(t+dt)
    }
    
    V = matrix(0, nrow = b, ncol = m + 1) # matrix of option values
    V[, m + 1] = pmax(S[, m + 1] - K, 0) # option value at expiry

    for (t in m:1) { # iterated over time steps
      for (k in 1:b) { # iterated over branches
        c_k = exp(-r * dt) * mean(V[-k, t + 1]) # continuation value
        h = max(S[k, t] - K, 0) # intrinsic value
        V[k, t] = if (c_k <= h) h else exp(-r * dt) * V[k, t + 1] # option value
      }
    }

    vals[i] = mean(V[, 1]) # option value
  }

  LeCn = mean(vals) # mean of option values
  SE = sd(vals)/sqrt(n) # standard error
  return(c(LeCn, SE)) # return option value and standard error
}

outputs = berm.l.est(100, 100, 0.2, 0.01, 2, 0.25, 3, 100)
LeCn = outputs[1] # Numerical Call Price
SE = outputs[2] # Standard Error
LeCn; SE # Result
