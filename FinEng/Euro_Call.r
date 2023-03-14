
## European Call Option, Closed Form ############################
rm(list=ls())

an.call = function(S0, K, sigma, T, r) {
    d1 = (log(S0 / K) + (r + sigma^2 / 2) * T) / (sigma * sqrt(T))
    d2 = d1 - sigma * sqrt(T)
    Ci = S0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
    Cn = mean(Ci)
    return(c(Cn, d1, d2))
}

outputs = an.call(120, 130, 0.2, 1, 0.05)
Cn = outputs[1] # Closed Form Call Option Price
d1 = outputs[2] # d1
d2 = outputs[3] # d2
Cn; d1; d2 # Results

## European Call Option, Numerical ###############################
rm(list=ls())

num.call = function(K, T, S0, sigma, r, n) {
    W = rnorm(n)
    Si = S0 * exp((r - sigma^2 / 2) * T + sigma * sqrt(T) * W)
    Ci = exp(-r * T) * pmax(Si - K, 0)
    Cn = mean(Ci)
    SE = sd(Ci) / sqrt(n)
    return(c(Cn, SE))
}

outputs = num.call(130, 1, 120, 0.2, 0.05, 10000)

Cn = outputs[1] # Numerical Call Option Price
SE = outputs[2] # Standard Error
Cn; SE # Results

p = 0.99 # Confidence level
z = -qnorm((1-p)/2) # z-score
c(Cn - z * SE, Cn, Cn + z * SE) # Confidence Interval
