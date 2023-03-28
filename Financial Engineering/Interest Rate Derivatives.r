#####################
## 27th March 2023 ##
#####################
rm(list=ls())

# params
L = c(0.01, 0.02, 0.03)
A = c(0.2, 0.3)
dt = 1
N = 1
K = 2
M = 2

A = matrix(A, nrow = 1, ncol = 2)
L = matrix(L, nrow = 1, ncol = 3)

# Simulate W
set.seed(1)
W = matrix(rnorm(N*M), nrow = N, ncol = M)

i = 1
k = 1
m = 1
Ai = A[i - k + 1]
Ai

Am = A[m - k + 1]
Am

Li = L[k - 1 + 1]
Li

mu = (dt * Li * Ai * Am) / (1 + dt * Li) - Am^2 / 2
mu

Lm = Li * exp(mu * dt + sqrt(dt) * Am * W)
Lm
