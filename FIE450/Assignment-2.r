## Initialise environment ##
rm(list=ls())

###################
## DATA CLEANING ##
###################
## Load and inspect data ##
df = read.csv("Assignment-2-Data.csv", skip = 1, header = TRUE) # skipping USD/EUR indicators
df$Date = as.Date(df$Date, format = "%d/%m/%Y") # convert dates to date format

## Currency conversion & creating excess returns ##
# Declaring named variables for readability
indices = seq(2, 8) # all indices
usd.ind =  c(2, 3, 5, 7, 8) # indices with values in USD

# Convert USD to EUR
df[, usd.ind] = df[, usd.ind] * df$EUR # converting USD indices to EUR
df$EUR = NULL # remove EUR column

# Omit missing values and correct order
df = na.omit(df)
# reverse order of df
df = df[rev(seq(1, nrow(df))), ]

# checking that the data is correct
# plot(df[, 2], type = "l", xlab = "Date", ylab = "Index", main = "Indices")

# simple return function
simple.r = function(p.vec) {
    # arithmetic return
    r.vec = c(NA, (p.vec[-1] - p.vec[-length(p.vec)]) / p.vec[-length(p.vec)])
    return(r.vec)
}

# apply simple return function to all indices
r = df[, seq(1, 8)]
head(r)
r[, indices] = apply(r[, indices], 2, simple.r)
head(r)

# create dataframe with rf for convencience
rf = df[, c(1, 9)] # only dates and rf
rf$X1M.Euribor = rf$X1M.Euribor / 100 # convert to decimal
rf$X1M.Euribor = c(NA, rf$X1M.Euribor[-1]) # shift Euribor by one day to match indices
head(rf)


# merge R and rf on date
R = merge(r, rf, by = "Date")
#UP untill this point i deem everything correct
R[, indices] = R[, indices] - R$X1M.Euribor # subtract X1M.Euribor from all indices

#Sucpicious outputs already here
summary(R)

# saving R and cleaning environment
save(R, file = "ExcessReturnsA2.RData") # save R to file
rm(list=ls()) # clean environment
load("ExcessReturnsA2.RData") # load R

###################
## Problem 1 & 2 ##
###################
# We assume no need to calculate liquidity or the like due to the 
# vectors being that of indices. We also assume that the values 
# given in the data are the values of the indices at the end of the
# trading day.
R$X1M.Euribor = NULL # remove X1M.Euribor
# ^ For some reason doing this before saving R gave us an error.
# We choose to use the SIM model for the following reasons:
# Markowitz is Markowitz, bad inputs equal bad outputs.
# Why bad inputs? As far as we know now, everything is negative.
require(quadprog)
require(Matrix)

not.NA = !is.na(R[R$Date == "2023-02-28", ])
R = R[, not.NA]
R = tail(R, n = 100) # reversing order of R via rev() gave us nonsensical results

Rho = cor(R[, -1], use = "pairwise.complete.obs") # correlation matrix
mu = apply(R[, -1], 2, mean, na.rm = TRUE) * 12 # annualise mean returns
Sigma = cov(R[, -1], use = "pairwise.complete.obs") * 12 # annualised covariance matrix

## Problem 1: Results ##
mu # annualised expected returns
Sigma # annualised covariance matrix

## We plot the efficient frontier further down in the code.

## SIM model ##
# Regressing all other indices on MSCI.World
reg = apply(R[, c(-1, -2)], 2, function(v) {
    res = lm(v ~ R$MSCI.World)
    c(coefficients(res), var(residuals(res)))
})

rownames(reg) <- c("alpha", "beta", "var.eps")
head(reg)

# Regression results
alpha = 0 # no alpha
beta = reg[2, ] # beta
var.eps = reg[3, ] * 12 # annualise variance of residuals
mu.index = mean(R$MSCI.World) * 12 # annualise mean of MSCI.World
var.index = var(R$MSCI.World) * 12 # annualise variance of MSCI.World
mu = beta * mu.index # expected return of all indices
Sigma = var.index * (as.matrix(beta) %*% beta) # covariance matrix of all indices
diag(Sigma) = diag(Sigma) + var.eps # add variance of residuals to diagonal
Sigma

# Optimisation
A = t(rbind(1, mu)) # constraint matrix
mu.star = 0.07 # target return
d = rep(0, length(mu)) # objective function
b0 = c(1, mu.star) # constraint vector
res = solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 2) # solve QP
omega.p = res$solution # optimal weights for portfolio
mu.p = t(omega.p) %*% as.matrix(mu) # portfolio mean
sigma.p = sqrt(t(omega.p) %*% Sigma %*% as.matrix(omega.p)) # portfolio volatility
sr.p = mu.p / sigma.p # portfolio sharpe ratio

## Problem 2: Results ##
omega.p # optimal portfolio weights
mu.p # portfolio expected return
sigma.p # portfolio standard deviation
sr.p # portfolio sharpe ratio

# Problem 1: Plotting the efficient frontier
mu.p.vec = seq(0, 0.20, length = 100)
sigma.p.vec = c()

for (i in 1:length(mu.p.vec)) {
    mu.star = mu.p.vec[i]
    b0 = c(1, mu.star)
    res = solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 2)
    omega = res$solution
    sigma.p.vec = c(sigma.p.vec, sqrt(t(omega) %*% Sigma %*% as.matrix(omega)))
}

## Problem 1: Efficient Frontier Plot ##
plot(sigma.p.vec, mu.p.vec, type = "l", xlim = c(0, max(sigma.p.vec)),
    xlab = "Volatility", ylab = "Expected return")

## Problem 2: Portfolio Plot ##
points(sigma.p, mu.p, pch = 4, lwd = 4, col = "darkred")

###############
## Problem 3 ##
###############
sigma.15.star = 0.15 # target volatility
objective = function(mu.15.star, sigma.15.star) {
  A = t(rbind(1, mu))
  d = rep(0, length(mu))
  b0 = c(1, mu.15.star)
  res = solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 2)
  omega.15 = res$solution
  sigma.15 = sqrt(t(omega.15) %*% Sigma %*% as.matrix(omega.15))
  eps = (sigma.15 - sigma.15.star)^2 
  return (eps)
}

res = nlminb(0.1, objective, sigma.15.star = sigma.15.star, lower = 1e-6, upper = 1-1e-6)
mu.15 = res$par # expected return with target volatility 0.15

A = t(rbind(1, mu))
d = rep(0, length(mu))
b0 = c(1, mu.15)
res = solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 2) # determining remaining parameters

omega.15 = res$solution
sr.15 = as.vector((mu.15)/sigma.15.star)
sigma.15 = sqrt(t(omega.15) %*% Sigma %*% as.matrix(omega.15))

## Problem 3: Results ##
mu.15 # expected return with target volatility 0.15
sigma.15 # volatility with target volatility 0.15
sr.15 # sharpes ratio with target volatility 0.15
omega.15 # optimal weights with target volatility 0.15

## Problem 3: Portfolio Plot ##
points(sigma.15, mu.15, pch = 4, lwd = 4, col = "magenta")

###############
## Problem 4 ##
###############
A = t(array(1, dim = c(1, length(mu)))) # constraint matrix
d = array(0, dim = c(length(mu),1))  # objective function
b0 = 1 # constraint vector

res = solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 1) 
omega.min = res$solution # get weights

mu.min = as.vector(t(omega.min) %*% as.matrix(mu))
sigma.min = as.vector(sqrt(t(omega.min) %*% Sigma %*% as.matrix(omega.min)))
sr.min = mu.min/sigma.min

## Problem 4: Results ##
omega.min # MinVP weights
mu.min # MinVar expected return
sigma.min # MinVar volatility
sr.min # MinVar Sharpe Ratio

## Problem 4: Portfolio Plot ##
points(sigma.min, mu.min, pch = 4, lwd = 4, col = "orange")

###############
## Problem 5 ##
###############
## Capital Allocation Line / Tangency Portfolio ##
A = t(rbind(mu)) # constraint matrix
mu.star = 0.1 # target return
d = rep(0, length(mu)) # objective function
b0 = c(mu.star) # constraint vector
res = solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 1) # solve QP
w = res$solution # optimal weights
omega.sr = w/sum(w) # normalise weights

mu.maxsr = t(omega.sr) %*% as.matrix(mu) # tangency portfolio mean
sigma.maxsr = sqrt(t(omega.sr) %*% Sigma %*% as.matrix(omega.sr)) # tangency portfolio volatility
sr.maxsr = mu.maxsr/sigma.maxsr

## Problem 5 Results
omega.sr # Optimal Weights
mu.maxsr # Expected Return
sigma.maxsr # Standard Deviation
sr.maxsr # Sharpe Ratio

## Problem 5: Portfolio Plot ##
points(sigma.maxsr, mu.maxsr, pch = 3, lwd = 4, col = "blue")

## Problem 5: Capital Allocation Line Plot ##
abline(0, mu.maxsr/sigma.maxsr, lwd = 2, col = "blue", lty = 2)

###############
## Problem 6 ##
###############
A = t(rbind(1, mu, diag(1, length(mu)))) # constraint matrix
mu.nn.star = 0.07 # target return
b0 = c(1, mu.nn.star, rep(0, length(mu))) # constraint vector
res = solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 2)
omega.nn = res$solution
mu.nn = t(omega.nn) %*% as.matrix(mu)
sigma.nn = sqrt(t(omega.nn) %*% Sigma %*% as.matrix(omega.nn))
sr.nn = mu.nn/sigma.nn

## Problem 6: Results ##
mu.nn # Expected Return with no short sales
omega.nn # Optimal Weights with no short sales
sigma.nn # Standard Deviation with no short sales
sr.nn # Sharpe Ratio with no short sales

## Problem 6: Portfolio Plot ##
points(sigma.nn, mu.nn, pch = 3, lwd = 4, col = "darkgreen")

###############
## Problem 7 ##
###############
mu.cap.star = 0.07
A = t(rbind(1, mu, diag(1, length(mu)),-diag(1, length(mu)))) 
b0 = c(1, mu.cap.star, rep(0, length(mu)), rep(-(0.30), length(mu))) 
d = rep(0, length(mu)) 
res = solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 2)
omega.cap = res$solution 
sigma.cap = as.vector(sqrt(t(omega.cap) %*% Sigma %*% as.matrix(omega.cap)))
mu.cap = t(omega.cap) %*% as.matrix(mu)
sr.cap = (mu.cap)/sigma.cap

## Problem 7: Results ##
mu.cap # Expected Return with 0.30 cap
omega.cap # Optimal Weights with 0.30 cap
sigma.cap # Standard Deviation with 0.30 cap
sr.cap # Sharpe Ratio with 0.30 cap

## Problem 7: Portfolio Plot ##
points(sigma.cap, mu.cap, pch = 3, lwd = 4, col = "red")

################## TASK 2 #####################
## Same initialisation as in Task 1 ##
rm(list=ls()) # clean environment
load("ExcessReturnsA2.RData") # load R
R$X1M.Euribor = NULL # remove X1M.Euribor
not.NA = !is.na(R[R$Date == "2023-02-28", ]) 
R = R[, not.NA]
# When we used too much data, we ended up with a 
# excessively long time-horizon proved computationally
# challenging, and created "strange" outputs.
R = tail(R, 100)

###############
## Problem 1 ##
###############
# we use a for-loop to iterate over the rolling periods
# the rebalancing of the portfolio over the target
# 5 years, aka 60 months.
# As previous we assume an alpha of 0, due to 
# efficient market hypothesis.
t = 40 # 100 - t = 60 (our rolling period)
r = c() # empty vector to store returns in returns

# for items in 60 t-time rolling periods clean NAs to be sure
for (i in 1:sum(nrow(R) - t)) {
  R.2 <- R[i:sum(i + t), -1]
  R.2 <- R.2[, sapply(R.2, function(v) all(!is.na(v)))]

# Declaring the regression to be applied to the data
reg <- apply(R.2[-sum(t + 1), ], 2, function(r) { 
  res <- lm(r ~ R$MSCI.World[i:sum(i + t - 1)])
  c(coefficients(res), var(residuals(res)))
    })

    rownames(reg) = c("alpha", "beta", "var.eps")
    alpha = reg[1, ]
    beta = reg[2, ]
    var.eps = reg[3, ]
    mu.index = mean(R$MSCI.World[i:sum(i + t - 1)])
    var.index = var(R$MSCI.World[i:sum(i + t - 1)])
    mu = beta * mu.index
    Sigma = var.index * (as.matrix(beta) %*% beta)
    diag(Sigma) = diag(Sigma) + var.eps

    # Capital Allocation Line / Tangency Portfolio
    A = t(rbind(mu)) # constraint matrix
    mu.star = 0.01 # target return
    d = rep(0, length(mu)) # objective function
    b0 = c(mu.star) # constraint vector
    res = solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 1) # solve QP
    w = res$solution # optimal weights
    omega = w/sum(w) # normalise weights
    returns = as.vector(t(R.2[sum(t + 1), ]) * omega)                                              # of the period with their weights 
  
  # storing the returns in a vector
    r = c(r, sum(returns))
}

# creating summary statistics
mu = mean(r)*12
sd = sd(r)*sqrt(12)
sr = mu/sd

## Problem 1: Results ##
r.cal = r # Returns for each period
mu.cal = mu # annualized mean
sd.cal = sd # annualized standard deviation
sr.cal = sr # annualized Sharpe ratio

# Viewing the results
r.cal; mu.cal; sd.cal; sr.cal

###############
## Problem 2 ##
###############
for (i in 1:sum(nrow(R) - t)) {
  R.2 = R[i:sum(i + t), -1]
  R.2 = R.2[, sapply(R.2, function(v) all(!is.na(v)))]

reg = apply(R.2[-sum(t + 1), ], 2, function(r) { 
  res = lm(r ~ R$MSCI.World[i:sum(i + t - 1)])
  c(coefficients(res), var(residuals(res)))
    })

    rownames(reg) = c("alpha", "beta", "var.eps")
    alpha = reg[1, ]
    beta = reg[2, ]
    var.eps = reg[3, ]
    mu.index = mean(R$MSCI.World[i:sum(i+t-1)])
    var.index = var(R$MSCI.World[i:sum(i+t-1)])
    mu = beta * mu.index
    Sigma = var.index * (as.matrix(beta) %*% beta)
    diag(Sigma) = diag(Sigma) + var.eps

    # Minimum Variance Portfolio
    A = t(array(1, dim = c(1, length(mu))))
    d = array(0, dim = c(length(mu),1))
    b0 = 1
    res = solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 1)
    omega = res$solution
    returns = as.vector(t(R.2[sum(t+1), ])*omega)
  
    r = c(r, sum(returns))
}

mu = mean(r)*12
sd = sd(r)*sqrt(12)
sr = mu/(sd(r)*sqrt(12))

## Problem 2: Results ##
r.mvp = r # Returns for each period
mu.mvp = mu # annualized mean
sd.mvp = sd # annualized standard deviation
sr.mvp = sr # annualized Sharpe ratio

# Viewing the results
r.mvp; mu.mvp; sd.mvp; sr.mvp

###############
## Problem 3 ##
###############
# for items in 60 t-time rolling periods
for (i in 1:sum(nrow(R) - t)) {
  R.2 = R[i:sum(i + t), -1]
  R.2 = R.2[, sapply(R.2, function(v) all(!is.na(v)))]

# regression
reg = apply(R.2[-sum(t + 1), ], 2, function(r) { 
  res = lm(r ~ R$MSCI.World[i:sum(i + t - 1)])
  c(coefficients(res), var(residuals(res)))
    })

    rownames(reg) = c("alpha", "beta", "var.eps")
    alpha = reg[1, ] # Assuming efficient markets, hence alpha equals 0
    beta = reg[2, ]
    var.eps = reg[3, ]
    mu.index = mean(R$MSCI.World[i:sum(i+t-1)])
    var.index = var(R$MSCI.World[i:sum(i+t-1)])
    mu = beta * mu.index
    Sigma = var.index * (as.matrix(beta) %*% beta)
    diag(Sigma) = diag(Sigma) + var.eps

    # Uniformly distributed weights portfolio
    mu.star = 0.01 # target return
    A = t(rbind(1, mu, diag(1, length(mu)))) # constraint matrix
    d = array(0, dim = c(length(mu),1))
    b0 = c(1, mu.star, rep(1/7, length(mu))) # constraint vector
    res = solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 0)
    omega = res$solution
    mu = t(omega) %*% as.matrix(mu)
    sigma = sqrt(t(omega) %*% Sigma %*% as.matrix(omega))
    sr = mu/sigma
    returns = as.vector(t(R.2[sum(t+1), ])*omega)
 
    r = c(r, sum(returns))
}

mu <- mean(r)*12
sd = sd(r)*sqrt(12)
sr <- mu/sd

## Problem 3: Results ##
r.eq = r # Returns for each period
mu.eq = mu # annualized mean
sd.eq = sd # annualized standard deviation
sr.eq = sr # annualized Sharpe ratio

# Viewing the results
r.eq; mu.eq; sd.eq; sr.eq

###############
## Problem 4 ##
###############
sr.mvp # Third Place
sr.cal # First Place
sr.eq # Second Place

# Reasoning: 
# We can observe that the minimum variance portfolio has the lowest Sharpe ratio,
# Second the uniformially distributed portfolio, while the best is the one that
# maximises sharpe ratio. By definition, we should expect the max.S to be the best,
# seing as we expect to get the highest sharpe ratio when we maximize it.
# If one wants a lower risk we can see from the CAL that we can get higher returns
# with the same amount of risk as the MVP, by combining the max.S with risk free asset.
# Ex-post this seems like a the best way to go, however, we should be aware
# that in reality, the best strategy ex-ante will probably not 
# be the best strategy ex-post.