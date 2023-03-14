
# ON = Overnight Rate
# MM = Money Market Rate
# FRA = Forward Rate Agreement
# IRS = Interest Rate Swap
bootstrapper = function(ON, MM, FRA, IRS, sigma) {
    t = 0
    Z = 1
    # map interest futures onto FRAs by convexity
    FRA = cnvx.adj(FRA, 1, 20, sigma)
    # ON Deposits
    Z = c(Z, 1 / ((1 + delta(, , 360) * ON[1]) * (1 + delta(, .242, 360) * ON[2])))
    # MM Deposits
    for {i in MM} {
        Z = c(Z, Z / ((1 + delta(, , 360) * i) * (1 + delta(, , 360) * i)))
    }

}
# Params
ON = c(4.70, 4.57) # Overnight Rates: 1 week, 1 month
MM = c(4.71, 4.84) # Money Market Deposits: 1 week, 1 month
FRA = c(5.125, 5.376, 5.5260, 5.375, 5.6140) # Forward Rate Agreements: March 23, June 23, September 23, December 23, March 24
IRS = c(4.9330, 4.5381, 4.2790, 4.1202, 3.9480, 3.8400, 3.8140, 3.7910, 3.7388) # Interest Rate Swaps: 2, 3, 4, 5, 7, 10, 12, 15, 20 years
sigma = 0.006 # Volatility

Z = 1
t = 0

# map interest futures onto FRAs by convexity
FRA = cnvx.adj(FRA, 30, 380, sigma)

# ON Deposits
Z = c(Z, 1 - (1 / ((1 + delta(0, 1, 360) * ON[1]) * (1 + delta(1, 30, 360) * ON[2]))))
Z

# MM Deposits
for (i in MM) {
    Z = c(Z, 1 - Z / ((1 + delta(0, 1, 360) * i) * (1 + delta(1, 30, 360) * i)))
}
Z


