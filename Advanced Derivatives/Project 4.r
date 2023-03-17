########################################################
## Bermudan Put Option, Longstaff and Schwartz (2001) ##
## LSMC approximation of the continuation value.      ##
########################################################
# initialise environment
rm(list=ls())

## 1. Define the parameters ##
S0 = 100; K = 100; r = 0.05; sigma = 0.25; T = 5; N = 5000

## 2. Black-Scholes-Merton value of a European put option. ##
d1 = (log(S0/K) + (r + sigma^2/2)*T)/(sigma*sqrt(T))
d2 = d1 - sigma*sqrt(T)
BSM = K * exp(-r * T) * pnorm(-d2) - S0 * pnorm(-d1)
BSM # Put price: 10.38401

## 3. Wiener Process. ##
# 3.1. Set the seed.
set.seed(1)

# 3.2. Generate 3 vectors of N standard normal random variables.
X1 = rnorm(N*T)
X2 = rnorm(N*T)
X3 = rnorm(N*T)

## 4. Risk-adjusted stock prices. ##
S1 = S0 * exp((r - sigma^2/2)*T/3 + sigma * sqrt(T/3) * X1)
S2 = S1 * exp((r - sigma^2/2)*T/3 + sigma * sqrt(T/3) * X2)
S3 = S2 * exp((r - sigma^2/2)*T/3 + sigma * sqrt(T/3) * X3)

## 5. Store the possible put payoffs at date T in pT ##
pT = pmax(K - S3, 0)

## 6. Find the Monte Carlo estimateoof the theoeretical value ##
pMC = mean(pT) * exp(-r * T)
pMC # 10.40275

## 7. Standard Error ##
SE = exp(-r*T) * sd(pT)
SE # 15.4428, explain.

## 8. Confidence Interval ##
CI = c(pMC - 1.96 * SE, pMC + 1.96 * SE)
CI # -19.86513, 40.67063

## 9. Indicator function ##
indicator = function(S) {
  if (S >= CI[1] && S <= CI[2]) 1 else 0
} # returns 1 if S is in the CI, 0 otherwise

indicator(pMC) # 1

## 10. Intrinsic Value & In-the-money ##
IV = pmax(K - S2, 0)
ITM = which(IV > 0)

## 11. Regression: 2T/3 ##
pvPayoff = exp(-r * T/3) * pT # present value of the payoffs
y = pvPayoff[ITM] # Payoffs which are in-the-money
x1 = S2[ITM] # ITM S
x2 = S2[ITM]^2 # ITM S squared
LS = lm(y ~ x1 + x2) # linear regression
summary(LS) # Summary Stats of the regression

## 12. Estimate the exercise and continuation value at 2T/3 ##
newdata = data.frame(x1 = S2, x2 = S2^2) # all possible S
CV = predict(LS, newdata) # continuation value
EX = which(IV > CV) # exercise value
pvPayoff[EX] = IV[EX] # exercise value

## 13. Regression: T/3 ##
IV = pmax(K - S1, 0); ITM = which(IV > 0)
y = pvPayoff[ITM]; x1 = S1[ITM]; x2 = S1[ITM]^2
LS = lm(y ~ x1 + x2); summary(LS)

newdata = data.frame(x1 = S1, x2 = S1^2)
CV = predict(LS, newdata)
EX = which(IV > CV)
pvPayoff[EX] = IV[EX]

## 14. Bermudan option value ##
p0 = mean(pvPayoff) * exp(-r * T/3); p0 # 13.27276
