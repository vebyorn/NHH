#####################
## 20th March 2023 ##
#####################
rm(list=ls())

# Example 1: V3 on S2 = C2
lm(c(1, 6, 0, 3, 6) ~ c(108, 107, 96, 106, 104))

# Example 2: V2 on S1 = C1
lm(c(8, 7, 1.01, 6, 4) ~ c(105, 98, 95, 108, 105))

## Regression approach to Bermudan options ############################
berm.reg = function(K, S0, sigma, r, T, n, dt) {
    t = seq(dt, T, by = dt) # time steps
    m = length(t) # number of time steps
    e = exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * rnorm(m * n)) # random numbers
    e = matrix(e, m, n) # random matrix
    S = S0 * apply(e, 2, cumprod) # applying random numbers
    S = rbind(S0, S) # adding initial value
    h = pmax(S - K, 0) # payoff today

    ### Fix later

}

outputs = berm.reg(100, 100, 0.2, 0.01, 2, 100000, 0.25)
RegCn = outputs[1] # regression call
RegSE = outputs[2] # regression standard error
RegCn; RegSE


### testing
#params
K = 100

S0 = c(100, 100, 100, 100, 100)
S1 = c(105, 98, 95, 108, 105)
S2 = c(108, 107, 96, 106, 104)
S3 = c(101, 106, 90, 103, 106)
# create a matrix with these vectors
S = as.matrix(cbind(S0, S1, S2, S3))
S # stock price matrix

# apply h to each column
h = pmax(S - K, 0)
h # payoff today

# V is the value of the next column
V = h[, 2:4]
# add new column with 0
V4 = c(0, 0, 0, 0, 0)
V = cbind(V, V4)
# reverse the order of the columns
V = V[, ncol(V):1]
V
# rename the columns
colnames(V) = c("V4", "V3", "V2", "V1")
V

c1 = lm(V[, 1] ~ S[, 4])$coefficients
c2 = lm(V[, 2] ~ S[, 3])$coefficients
c3 = lm(V[, 3] ~ S[, 2])$coefficients

c1p = c1[1] + c1[2] * S[, 4]
c2p = c2[1] + c2[2] * S[, 3]
c3p = c3[1] + c3[2] * S[, 2]

c1p
c2p
c3p

c = cbind(c1p, c2p, c3p)
c

V = apply(c, 1, max, V[, 1])
V0 = mean(V)
V0
V
