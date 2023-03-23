#####################
## 20th March 2023 ##
#####################
rm(list=ls())

## Tsitsiklis and Van Roy (2001): Bermudan Call ############################
# K = strike price
# S0 = initial stock price
# sigma = volatility
# r = risk-free rate
# T = maturity
# n = number of paths
# dt = time step
# phi = basis function
berm.reg = function(K, S0, sigma, r, T, n, dt, phi) {
    ## Generating stock paths
    t = seq(dt, T, by = dt) # time steps
    m = length(t) # number of time steps
    e = exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * rnorm(m * n)) # random numbers
    e = matrix(e, m, n) # random matrix
    S = S0 * apply(e, 2, cumprod) # applying random numbers
    S = rbind(S0, S) # adding initial value

    ## Regression via matrix multiplication
    h = exp(-r * dt) * pmax(S - K, 0) # payoff matrix
    X = S0 # initial value matrix
    V = h # option value matrix
    for (i in (m - 1):1) { # backwards induction over time
        B = ((t(phi(X))) %*% phi(X))^(-1) %*% t(phi(X)) %*% V[i + 1, ] # regression coefficients
        c = phi(X) %*% B # continuation value
        V[i, ] = exp(-r * dt) * pmax(c, h[i, ]) # option values
    }

    ## Output
    V0 = mean(V[1,]) # today's option price
    SE = sd(V[1,]) / sqrt(length(V)) # standard error

    ## End function
    return(c(V0, SE)) # return as list
}

outputs = berm.reg(100, 100, 0.2, 0.01, 2, 100000, 0.25, function(x) x)
berm.val = outputs[1] # option price
berm.se = outputs[2] # standard error
berm.val; berm.se

## Longstaff and Schwartz (2021): Bermudan Call #############################
# K = strike price
# S0 = initial stock price
# sigma = volatility
# r = risk-free rate
# T = maturity
# n = number of paths
# dt = time step
# phi = basis function
berm.itm.reg = function(K, S0, sigma, r, T, n, dt, phi) {
    ## Generating stock paths
    t = seq(dt, T, by = dt) # time steps
    m = length(t) # number of time steps
    e = exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * rnorm(m * n)) # random numbers
    e = matrix(e, m, n) # random matrix
    S = S0 * apply(e, 2, cumprod) # applying random numbers
    S = rbind(S0, S) # adding initial value

    ## Regression via matrix multiplication
    h = exp(-r * dt) * pmax(S - K, 0) # payoff matrix
    V = h # option value matrix
    k = m - 1 # backwards induction over time
    X = S[1, ] # initial value matrix

    ## Backwards induction over time
    for (i in k:1) { 
        B = ((t(phi(X))) %*% phi(X))^(-1) %*% t(phi(X)) %*% V[i + 1, ] # regression coefficients
        c = phi(X) %*% B # continuation value

        ## Sorting out out-of-the-money paths
        for (j in 1:n) { # looping over paths
            if (h[j] > 0 && h[i, j] >= c[i]) { # double conditional for h
                V[i, j] = h[i, j] # h is greater than 0 and greater than or equal to c
            } else {
                V[i, j] = V[i + 1, j] # h is 0 or less than c
            }
        }
    }

    ## Output
    V0 = mean(V) # today's option price
    SE = sd(V) / sqrt(n) # standard error

    ## End function
    return(c(V0, SE)) # return as list
}

outputs.itm = berm.itm.reg(100, 100, 0.2, 0.01, 2, 100000, 0.25, function(x) x)
berm.itm.V0 = outputs.itm[1] # option price
berm.itm.SE = outputs.itm[2] # standard error
berm.itm.V0; berm.itm.SE
## V0 target = 12.0265
## SE target = 0.0589








