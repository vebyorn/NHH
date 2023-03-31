#####################
## 27th March 2023 ##
#####################
## This was a collaborative effort.
## Credits to: https://github.com/maysamx

# initialise workspace
rm(list=ls())

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

# Inputs
L = c(0.01, 0.02, 0.03) # L = vector of initial forward libor rates
Lambda = c(0.20, 0.30) # Lambda = vector of stationary volatilities

# Run simulation
set.seed(1) # Set seed for reproducibility
results = round(lmm.sf(L, Lambda, dt = 1, K = 2, N = 10), digits = 4); results # print results

###############################################################################################
# initialise workspace
rm(list=ls())

## Libor Market Model, Single Factor ##
# L = vector of initial forward libor rates
# Lambda = vector of stationary volatilities
# dt = time step
# K = number of time steps
# N = number of simulations
lmm.mf = function(L, Lambda, dt, K, N) {
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

# Inputs
L = c(0.01, 0.02, 0.03) # L = vector of initial forward libor rates
Lambda =  # Lambda = vector of stationary volatilities

# Run simulation
set.seed(1) # Set seed for reproducibility
results = round(lmm.mf(L, Lambda, dt = 1, K = 2, N = 10), digits = 4); results # print results
