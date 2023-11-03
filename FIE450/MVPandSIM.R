##############################
## MEAN-VARIANCE PORTFOLIOS ##
##############################
setwd("C:/Users/vebky/FIE450")
rm(list=ls()) # cleaning environment

## Loading our processed data ##
# setwd("Your_directory_here")
load("Stocks-Wide.RData")
load("Riskfree-Rate.RData")

df <- merge(rf, stocks.wide, by = "Date")

df <- df[!is.na(df$R.index), ]
dim(df)

## Optimisation Problem ##
rf <- df[, c("Date", "rf")] # bringing back rf after sorting
head(rf)
df$rf <- NULL # deleting old rf in the stocks frame

df[, -1] <- df[, -1] - rf$rf # deducting risk-free from returns
m <- matrix(1:4, 2, 2) # example of matrix
m - c(0.1, 0.2) # example of deduction

df[1:4, 1:5]

R <- df # renaming data frame to Returns
R.index <- R[, c("Date", "R.index")] # extracting index returns
R$R.index <- NULL # deleting it from old returns frame



head(R.index) # we now have 3 frames, for rf, R and R.index

save(R, R.index, file="Excess-Returns.RData") # saving as file
range(R$Date)

not.NA <- !is.na(R[R$Date == "2020-10-31", ])
dim(R)
R <- R[, not.NA]
dim(R)
R <- tail(R, n = 60) # last 60 calender days
dim(R)

liq <- apply(R, 2, function(v) sum(!is.na(v))/length(v))
summary(liq)
R <- R[, liq >= 0.75]
dim(R)

mu <- apply(R[, -1], 2, mean, na.rm=TRUE) * 12
summary(mu)

Rho <- cor(R[, -1], use="pairwise.complete.obs")
Sigma <- cov(R[, -1], use="pairwise.complete.obs") * 12

summary(Rho[lower.tri(Rho)])
# example lower triangular use for symmetric matrices
m = matrix(1:9, 3, 3)
m # matrice
lower.tri(m) # area which is covered by lower.tri
m[lower.tri(m)] # selecting values covered

library(quadprog) # our first package!!! yay!!!
AT <- rbind(1, mu)
AT[, 1:4]
A <- t(AT) 
m <- matrix(1:6, 2, 3) # example matrix
m 
t(m) # example transpose

mu.star <- 0.05
b0 <- c(1, mu.star)
d = rep(0, length(mu))
solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 2)
# matrix D is not positive definite, ECO401cels.... we just keep winning!!!
library(Matrix)
Sigma2 <- nearPD(Sigma)$mat
Sigma[1:4, 1:4]
Sigma2[1:4, 1:4]
Sigma2 <- as.matrix(Sigma2)

res <- solve.QP(Dmat = Sigma2, dvec = d, Amat = A, bvec = b0, meq = 2)
res
omega <- res$solution # extracting optimal weights
summary(omega)

t(omega) %*% as.matrix(mu) # %*% is R's matrix multiplication notation
sqrt(t(omega) %*% Sigma2 %*% as.matrix(omega)) # standard deviation

summary(omega)

A = t(rbind(1, mu, diag(1, length(mu))))
A[1:5, 1:5]
mu.star = 0.05
b0 = c(1, mu.star, rep(0, length(mu)))
res = solve.QP(Dmat = Sigma2, dvec = d, Amat = A, bvec = b0, meq = 2)
omega = res$solution
# check 1
summary(omega)
# check 2
mu.p = t(omega)%*%as.matrix(mu)
# check 3
sigma.p = sqrt(t(omega)%*%Sigma2%*%as.matrix(omega))
# Sharpe
mu.p/sigma.p

R.index = merge(R, R.index, by = "Date")[, c("Date", "R.index")]
dim(h)
head(h)

## SIM ##
dim(R)
dim(R.index)
reg = apply(R[, -1], 2, function(v) {
  res = lm(v ~ R.index$R.index)
  c(coefficients(res), var(residuals(res)))
})
rownames(reg) = c("alpha", "beta", "var.eps")

summary(reg[1,]) # alphas
summary(reg[2,]) # betas

alpha = reg[1, ] * 12
beta = reg[2, ]
var.eps = reg[3, ] * 12
mu.index = mean(R.index$R.index) * 12
var.index = var(R.index$R.index) * 12
mu.index
var.index

mu = beta * mu.index
Sigma = var.index * (as.matrix(beta)%*%beta)
diag(Sigma) = diag(Sigma) + var.eps

A = t(rbind(1, mu))
mu.star = 0.05
d = rep(0, length(mu))
b0 = c(1, mu.star)
res = solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq=2)
omega = res$solution
# check 1
summary(omega)
# check 2
mu.p = t(omega)%*%as.matrix(mu)
# check 3
sigma.p = sqrt(t(omega)%*%Sigma%*%as.matrix(omega))
# Sharpe
mu.p/sigma.p

mu.p.vec = seq(0, 0.17, length=100)
sigma.p.vec = c()
for (i in seq_along(mu.p.vec)) {
  mu.star = mu.p.vec[i]
  b0 = c(1, mu.star)
  res = solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq=2)
  omega = res$solution
  sigma.p.vec[i] = sqrt(t(omega)%*%Sigma%*%as.matrix(omega))
}
plot(sigma.p.vec, mu.p.vec, type="l")
points(sigma.p, mu.p, pch=4, lwd=4, col="red")

## Capital Allocation Line ##
A <- t(rbind(mu))
mu.star <- 0.1
d <- rep(0, length(mu))
b0 <- c(mu.star)
res <- solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq=1)
w <- res$solution
omega <- w/sum(w)
summary(omega)

mu.t <- t(omega) %*% as.matrix(mu)
sigma.t <- sqrt(t(omega) %*% Sigma %*% as.matrix(omega))
mu.t/sigma.t

points(sigma.t, mu.t, pch=3, lwd=4, col="blue")
abline(0, mu.t/sigma.t, lwd=2, col="blue", lty=2)
abline(0, mu.p/sigma.p, lwd=2, col="darkred", lty=2)
points(sqrt(diag(Sigma)), mu, lwd=2, col='gray')

0.05/mu.t # expected return per asset
0.05/mu.t * sigma.t # volatility
# he should invest 50% of the tangency portfolio
# due to linear combination of assets

round(omega * 0.05/c(mu.t) * 5e+08)
