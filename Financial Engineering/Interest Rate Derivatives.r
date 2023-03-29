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
  # Initialize output list
  output = data.frame(k = numeric(), m = numeric(), Lambda = numeric(), `µm(Tk)` = numeric(), `W(Tk)` = numeric(), `Lm(Tk)` = numeric())  

  # Loop for simulations
  for (n in 1:N) {
    L.prev = L[-1]
    
    # Loop for points in time (k marks the point in time we are valuing at)
    for (k in 1:K) {
      # Update L.prev to include the new value of Lm1(Tk) from previous iteration
      if (k == 1) {
        L.prev = L[-1]
      } else {
        L.prev = c(L[k], L.prev[-1])
      }
      # Generate random number for W(Tk)
      Wk = rnorm(1)
      
      # Loop for periods we are simulation at time k
      for (m in k:(length(L) - 1)) {
        sum.mu = 0
        
        # Loop to replicate sum function in formula
        for (i in k:m) {
          sum.mu = sum.mu + dt * L.prev[i - k + 1] * Lambda[m - k + 1] * Lambda[i - k + 1] / (1 + dt * L.prev[i - k + 1])
        }
        
        # Calculate mu and L at time m
        mu = sum.mu - Lambda[m - k + 1]^2 / 2
        L.new = L.prev[m - k + 1] * exp(mu * dt + sqrt(dt) * Lambda[m - k + 1] * Wk)
        
        # Save output in output dataframe
        output = rbind(output, data.frame(k = k, m = m, Lambda = Lambda[m - k + 1], `µm(Tk)` = mu, `W(Tk)` = Wk, `Lm(Tk)` = L.new))
        L[m - k + 1] = L.new
      }
    }
  }
  # Return Output dataframe
  return(output)
}

# Inputs
L <- c(0.01, 0.02, 0.03) # L = vector of initial forward libor rates
Lambda <- c(0.20, 0.30) # Lambda = vector of stationary volatilities

# Run simulation
set.seed(1) # Set seed for reproducibility
results = round(lmm.sf(L, Lambda, dt = 1, K = 2, N = 10), digits = 4 ); results