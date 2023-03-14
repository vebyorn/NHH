rm(list=ls())

## Accrual Factor ########################################
delta = function(t1, t2, dt) {
    s1 = 365.242 * t1
    s2 = 365.242 * t2
    n = s2 - s1
    delta = n / dt
    return(delta)
}

delta(0/365.242, 3/365.242, 360) # Accrual Factor

## Convexity Adjustment ########################################
rm(list=ls())

# F = Future Price
# f = Forward Rate Agreeement
# sigma = short rate volatility
# T1 = start date
# T2 = end date
cnvx.adj = function(F, T1, T2, sigma) {
    f = F - (sigma^2 * T1 * T2) / 2
    return (f)
}

cnvx.adj(100, 1, 2, 0.2) # Convexity Adjustment

## Interpolation ###############################################

t = c(0, 2)
Z = c(1, 0.8)
tx = c(0, 0.5, 2)

# Linear Interpolation
# t = vector of time points, starting at 0
# Z = corresponding discount factors
# tx = time point at which to interpolate
lin.interp = function(t, Z, tx, method = "linear") {
    Zx = exp(-approx(t, -log(Z), tx, method = method)$y)
    return(Zx)
}

lin.interp(t, Z, tx)


## Optimisation Example ########################################

# Variables
obj = function(Zt2, f0, T1, T2, t, Z) {
    # extend Z by Z.bg
    Z.vec = c(Z, Zt2)
    # extend t by T2
    t.vec = c(t, T2)
    # interpolate Zt1 at T1
    Zt1 = lin.interp(t.vec, Z.vec, T1)
    # calculate f0 prime
    f0.p = 1 / delta(T1, T2, 365) * ((Zt1 / Zt2) - 1)
    # calulate error term
    err = (f0.p - f0)^2
    return(err)
}

# minimise obj by determining Zt2
Zt2 = optim(0.25, obj, f0 = 0.05, T1 = 0.25, T2 = 0.5, t = 0, Z = 1)$par
Zt2 # Result is close to target
