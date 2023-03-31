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
results = round(lmm.sf(L, Lambda, dt = 1, K = 2, N = 10), digits = 4); results[1:3, ] # print results

###############################################################################################
# initialise workspace
rm(list=ls())

## Multifactor: Lambda Matrix Function ##
# l1 = vector of volatilities for the first factor
# l2 = vector of volatilities for the second factor
mf.lambda = function (l1, l2) {
  Lambda = sqrt(l1^2 + l2^2)
  return(Lambda)
}

# Inputs
L = c(0.01, 0.02, 0.03) # L = vector of initial forward libor rates
l1 = c(0.2, 0); l2 = c(0, 0.3) # l1, l2 = vectors of volatilities
Lambda = mf.lambda(l1, l2) # Lambda = matrix of volatilities
dt = 1 # dt = time step
K = 2 # K = number of time steps
N = 10 # N = number of simulations

## Initialize output list
output = data.frame(k = numeric(), m = numeric(), Lambda = numeric(), `µm(Tk)` = numeric(), `W(Tk)` = numeric(), `Lm(Tk)` = numeric())  

set.seed(1)

  ## Simulate Ns sims
  for (n in 1:N) { # Loop over n simulations
    L.prev = L[-1] # Previous libor rate

    ## Simulate K time steps
    for (k in 1:K) { # Loop for over time steps
      if (k == 1) {
        L.prev = L[-1] # Previous libor rate at time k = 1
      } else {
        L.prev = c(L[k + 1]) # Previous libor rate at time k > 1
      }

      ## Simulate W(Tk) for each factor
      Wk = rnorm(length(Lambda))

      ## Simulate m periods at time k
      for (m in k:(length(L) - 1)) { # Loop over m periods at time k
        sum.mu = 0 # initialising
        sum.lam = 0 # the sum loop
        neg.mu = 0 # variables
        Wkq = 0 # and also W

        ## Sum over i = k to m for the first part of mu
        for (i in k:m) {

          ## Sum over q = 1 to M for the second part of mu
          for (q in 1:(length(L) - 1)) {
            sum.lam = Lambda[m - k + 1] * Lambda[i - k + 1]
          }
          sum.mu = dt * L.prev[i - k + 1] * sum.lam / (1 + dt * L.prev[i - k + 1])
        }

        ## Sum over q = 1 to p = M
        for (q in 1:(length(L) - 1)) {
          neg.mu = Lambda[m - k + 1]^2 / 2
          Wkq = Wk * Lambda[m - k + 1]
        }

        ## Calculate mu and Lm(Tk)
        mu = sum.mu - neg.mu # calculate mu
        L.new = L.prev[m - k + 1] * exp(mu * dt + sqrt(dt) * t(Wkq)) # calculate new libor rate

        ## if k == m, then save L.new[1] in L.final else save L.new[2] in L.final
        if (k == m) {
          L.final = L.new[1]
        } else {
          L.final = L.new[2]
        }


        ## Save output in output dataframe
        output = rbind(output, data.frame(k = k, m = m, Lambda = Lambda[m - k + 1], `µm(Tk)` = mu, `W1(Tk)` = Wk[1], `W2(Tk)` = Wk[2], `Lm(Tk)` = L.final))
      }
    }
  }

  ## Return first 3 rows of output dataframe
  round(output[1:3, ], 4)

# testing
set.seed(1)

