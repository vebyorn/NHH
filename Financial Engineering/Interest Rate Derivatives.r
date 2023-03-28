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
W # OK

## T1: k = 1, m = 1 ## Good
k = 1
i = k
m = 1

num = dt * L[i] * A[i - k + 1] * A[m - k + 1]
dem = 1 + dt * L[k - 1 + 1]
mu = num/dem - A[m - k + 1]^2 / 2
mu

Lm = L[m + 1] * exp(mu * dt + sqrt(dt) * A[m - k + 1] * W[k])
Lm

## T1: k = 1, m = 2 ## Good
k = 1
i = k
m = 2

num = dt * L[i] * A[i - k + 1] * A[m - k + 1]
dem = 1 + dt * L[k - 1 + 1]
mu = num/dem - A[m - k + 1]^2 / 2
mu

Lm = L[m + 1] * exp(mu * dt + sqrt(dt) * A[m - k + 1] * W[k])
Lm

## T2: k = 2, m = 2
k = 2
i = k
m = 2

num = dt * L[i] * A[i - k + 1] * A[m - k + 1]
dem = 1 + dt * L[k - 1 + 1]
mu = num/dem - A[m - k + 1]^2 / 2
mu

Lm = L[m + 1] * exp(mu * dt + sqrt(dt) * A[m - k + 1] * W[k])
Lm
