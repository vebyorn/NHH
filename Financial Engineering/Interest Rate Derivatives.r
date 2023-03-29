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

# Vectors
A.vec = c()
mu.vec = c()
Lm.vec = c()
W.vec = c()

## T1: k = 1, m = 1 ## Perfect
k = 1
m = 1
A.vec[1] = A[m - k + 1]; A.vec
W.vec[1] = W[k]; W.vec

mu.sum = c()
for (i in k:m) {
    mu.sum[i] = (dt * L[i + 1] * A[i - k + 1] * A[m - k + 1]) / (1 + dt * L[i + 1])
    }
mu.vec[1] = sum(mu.sum, na.rm = TRUE) - A[m - k + 1]^2 / 2
mu.vec

Lm.vec[1] = L[m + 1] * exp(mu.vec[1] * dt + sqrt(dt) * A[m - k + 1] * W[k])
Lm.vec

## T1: k = 1, m = 2 # PERFECT
k = 1
m = 2
A.vec[2] = A[m - k + 1]; A.vec
W.vec[2] = W[k]; W.vec

mu.sum = c()
for (i in k:m) {
    mu.sum[i] = (dt * L[i + 1] * A[i - k + 1] * A[m - k + 1]) / (1 + dt * L[i + 1])
    }
mu.vec[2] = sum(mu.sum, na.rm = TRUE) - A[m - k + 1]^2 / 2
mu.vec

Lm.vec[2] = L[m + 1] * exp(mu.vec[2] * dt + sqrt(dt) * A[m - k + 1] * W[k])
Lm.vec

## T2: k = 2, m = 2 # PERFECT
k = 2
m = 2
A.vec[3] = A[m - k + 1]
W.vec[3] = W[k]

mu.sum = c()
for (i in k:m) {
    mu.sum[i] = (dt * Lm.vec[2] * A[i - k + 1] * A[m - k + 1]) / (1 + dt * Lm.vec[2])
    }
mu.vec[3] = sum(mu.sum, na.rm = TRUE) - A[m - k + 1]^2 / 2
mu.vec

Lm.vec[3] = Lm.vec[2] * exp(mu.vec[3] * dt + sqrt(dt) * A[m - k + 1] * W[k])
Lm.vec

RES = as.matrix(cbind(A.vec, round(mu.vec, 4), round(W.vec, 4), round(Lm.vec, 4)))
RES
# next: loop it :)