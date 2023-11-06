############################
### ECO421 ASSET PRICING ###
############################

### Autumn 2022 ###

## Problem 1 ##
# Covariance Matrix V
V = matrix(c(0.0004, -0.0006, 0, 
            -0.0006, 0.02250, 0.02625, 
            0, 0.02625, 0.12250), 
            nrow = 3, 
            ncol = 3, 
            byrow = TRUE); V
# Inverse V
V_inv = solve(V); V_inv
# Coefficients
A = 2851.676; B = 2843.277; C = 2835.205
# expected return vectors
a = c(11.627051, -8.486537, -2.140514)
b = c(-10.639291, 8.512767, 2.126525)

## (a) Briefly explain how we can understand the abstract of
# Kritzman, Page, and Turkington (2010).

## Asnwer:
# The short of it: Time series estimates of mean returns are typically of low quality.
# Any approach to improve estimates of expected returns should improve the performance of
# Markowitz, by reducing the issue of “garbage in, garbage out”.

## (b) What is the correlation between the returns of asset 3
# and the two other assets, and what is the volatility of the
# minimum variance portfolio?

## Answer:
# Correlation between asset 3 and asset 1
cor_a3_a1 = V[1,3]/sqrt(V[1,1]*V[3,3]); cor_a3_a1
# Correlation between asset 3 and asset 2
cor_a3_a2 = V[2,3]/sqrt(V[2,2]*V[3,3]); cor_a3_a2
# Volatility of the minimum variance portfolio
vol_min_var = sqrt(1/C); vol_min_var

## Your assistant has used time series to estimate the assets'
# annualised expected returns to the following:
mu_1 = 1.00; mu_2 = 1.08; mu_3 = 1.15

## (c) The assistant used monthly observations to estimate the means.
# How will using daily observations help improve the estimates?
## Answer:
# As the quality of estimates are decided by the standard error,
# further increase in the number of observations will not improve
# the quality of estimates. A longer window of observations will
# however improve the quality of estimates somewhat.

## (d) Compute optimal mean-variance portfolio that achieves an
# expected return of 1.10. What is the intuition behind the resulting
# non-zero position in the asset with expected return of 0%?

## Answer:
# Optimal mean-variance portfolio
eR = 1.10
phi = a + b*eR; phi
# The non-zero short position in asset 1 is due to the fact that
# it takes on the role of the riskless asset in the portfolio.

## In house analysis of your private views gives you the following
# gross rate return vector:
m_hat = c(0.95, 0.13, 0.09)

## (e) Assess wether the views are optimistic or pessimistic.
# Create the associated pick matrix.
## Answer:
# Asset 1 view:
view_1 = m_hat[1]; view_1 < mu_1 # Pessimistic
view_1
# Asset 2 view:
view_2 = m_hat[1] + m_hat[2]; view_2 == mu_2 # Neutral
view_2
# Asset 3 view:
view_3 = view_2 + m_hat[3]; view_3 > mu_3 # Optimistic
view_3
# Pick matrix
P = matrix(c(1, 0, 0, 
             -1, 1, 0, 
             0, -1, 1), 
             nrow = 3, 
             ncol = 3, 
             byrow = TRUE); P

### Problem 3 ###
# Consider twp seperate securities markets a and b.
Xa = matrix(c(
            3, 4, 1, 
            4, 3, 6, 
            2, -2, 10), 
            nrow = 3, 
            ncol = 3, 
            byrow = TRUE); Xa

Xb = matrix(c(
            3, 4, 1, 
            4, 3, 6, 
            2, -2, 8), 
            nrow = 3, 
            ncol = 3, 
            byrow = TRUE); Xb

Pb = c(2.4, 0.4, 5.4); Pb

## (a) Check wether the markets are complete.
## Answer:
# Market a
det(Xa) # Incomplete
# Market b
det(Xb) # Complete

## (b) Are the security proces in market b consistent?
## Answer:
Xb_inv = solve(Xb); Xb_inv
t(Xb_inv) %*% Pb # Yes (All elements are positive)

# Assume for now that we have pure security prices
# pi = (0.2, 0.2, 0.5)^T in both markets.

## (c) Expand market a by a fourth security with payoff
# (1, 1, 1)^T. Find prices Pa = (Pa1, Pa2, Pa3, Pa4)^T,
# that prevent arbitrage, and the riskless rate of both markets.
## Answer:
sp = c(0.2, 0.2, 0.5) # state prices
Pa = t(Xa) %*% sp; Pa # prices market a
P4 = as.numeric(sp %*% c(1, 1, 1)); P4 # fourth asset
R0 = 1/P4; R0 # risk-free rate

## (d) Compute the stochastic discount factors and
# equivalent martingale measures for the two markets.
## Answer:
# We need only one stochastic discount factor.
m = sp * 3; m # stochastic discount factor
q = R0 * sp; q # equivalent martingale measure
sum(q) # verify that q is a probability measure

# Consider a pure exchange economy. The consumption
# good is generated in two seperate securities markets
# c and d, characterised by:
Xc = matrix(c(
            1, 2,
            2, 0), 
            nrow = 2, 
            ncol = 2, 
            byrow = TRUE); Xc
Xd = matrix(c(
            1,
            2), 
            nrow = 2, 
            ncol = 1, 
            byrow = TRUE); Xd
# where the payoffs in the two markets both correspond
# to the same states s = 1, 2. There are two groups of
# non-satiated and risk averse individuals, C and D.
# Group C has exclusive access to market c, and group
# D has exclusive access to market d. Each individual
# is endowned with shares theta in each of the securities
# that sum to one.

## (e) Do the two groups stand to gain anything from
# integrating the two markets.
## Answer:
# Group C gains nothing from spanning.
# Group D stands to gain from spanning, as 
# Xc is invertable. 