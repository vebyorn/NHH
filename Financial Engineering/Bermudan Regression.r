#####################
## 20th March 2023 ##
#####################
rm(list=ls())

## Tsitsiklis and Van Roy (2001): Bermudan Call ############################
berm.reg = function(K, S0, sigma, r, T, n, dt) {
    ## Generating stock paths
    t = seq(dt, T, by = dt) # time steps
    m = length(t) # number of time steps
    e = exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * rnorm(m * n)) # random numbers
    e = matrix(e, m, n) # random matrix
    S = S0 * apply(e, 2, cumprod) # applying random numbers
    S = rbind(S0, S) # adding initial value

    ## Regression via matrix multiplication
    h = exp(-r * dt) * pmax(S - K, 0) # payoff matrix
    X = S0
    h = exp(-r * dt) * pmax(S - K, 0) # payoff matrix
    V = h # option value matrix
    for (i in (m - 1):1) {
        B = (t(X) %*% X)^(-1) %*% t(X) %*% V[i + 1, ]
        c = X %*% B
        V[i, ] = exp(-r * dt) * pmax(c, h[i, ])
    }

    ## Output
    V0 = mean(V[1,])
    SE = sd(V[1,])/sqrt(length(V))

    # end function
    return(c(V0, SE))
}

outputs = berm.reg(100, 100, 0.2, 0.01, 2, 100000, 0.25)
berm.val = outputs[1] # option price
berm.se = outputs[2] # standard error
berm.val; berm.se