####################
### Loading Data ###
####################
# read csv file
obx <- read.csv("OBX.csv")
# inspect with head()
head(obx)
# inspect with str()
str(obx)
# this is time series data
# currently date is not a date object

# you can select obx entries with indexes


obx[1, 2] # this selects first row, second column
obx[1, "OBX.GR"] # this selects first row, column "OBX.GR"
obx[1, ] # this selects first row, all columns
obx[1:50, ] # this selects first 50 rows, all columns

#######################
### Data Processing ###
#######################
# rename columns
names(obx) <- c("Date", "Close", "Volume")
# display new names
names(obx)
# see summary statistics
summary(obx)
# convert date to date object
obx$Date <- as.Date(obx$Date)
# remove volume column as it is empty
obx$Volume <- NULL
# plot the data
plot(obx$Date, obx$Close, type = "l", xlab = "Date", ylab = "Close", main = "OBX")

###################
### Log Returns ###
###################
# calculate log returns
obx$r <- c(NA, diff(log(obx$Close)))
# NA is added for first row
# Due to the backwards-gazing nature of the calculation
# there are no entries prior to the first date
# this would've caused an error otherwise.
# estimate mean and standard deviation without NAs
# data is daily, consider 250 trading days per year
mu <- mean(obx$r, na.rm = TRUE) * 250
sigma = sd(obx$r, na.rm = TRUE) * sqrt(250)
# calculate sharpe ratio (risk free rate: 0.039)
(mu-0.039)/sigma

r = na.omit(obx$r)
n = length(r)
SE = sd(r)/sqrt(n) * 250

p = 0.99
z = -qnorm((1-p)/2)
c(mu - z*SE, mu + z*SE) # big

# plot returns
plot(obx$Date, obx$r, type = "l", xlab = '', ylab = "OBX")

###################
### EWMA Model  ###
###################
# calculate EWMA
n = length(obx$r)
lambda = 0.94
lambda.vec = lambda^(0:(n-1))
r.vec = r[length(r):1]
sigma2 = (1 - lambda) * sum(lambda.vec * r.vec^2)
sigma = sqrt(sigma2)
sigma * sqrt(250)

# shorter version
sigma2 = sum((1-lambda) * lambda^(0:(n-1)) * rev(r)^2)
sigma = sqrt(sigma2)
sigma * sqrt(250)

# 2 identical loop examples
v = 2:5
for (i in v) {
  cat(i, "\n")
}

for(i in seq_along(v)) {
  cat(v[i], "\n")
}

##  EWMA in loop ##
sigma2 = c()

for (i in 1:length(r)) {
  x = sum((1-lambda) * lambda^(0:(i-1)) * rev(r[1:i])^2)
  sigma2 = c(sigma2, x)
}

length(sigma2)
sigma = sqrt(sigma2)
plot(sigma * sqrt(250), type = "l", xlab = 'Index', ylab = "EWMA")

## Maximum likelihood approach ##
# defining a simple function
div = function(a, b) {
  return(a/b)
}
div(10,2)

#######################
## Functions and ... ##
#######################
plot.normal <- function(x1, x2, ...) {
  x <- seq(x1, x2, length.out = 50)
  y <- dnorm(x, mu, ...)
    plot(x, y, type = "l", xlab = "", ylab = "")
}

plot.normal(-3, 3)

#######################
## Optimisation in R ##
#######################
# trivial example
x <- seq(-1, 5, by= 0.1)
y <- -(x-2)^2
plot(x,y, type = "l", xlab = "", ylab = "")

# numerical approach
# objective function, we * -1 to make the function maximise
f <- function(x) {
  y <- (x-2)^2
  return(y)
}
# minimisation (now a maximisation)
res <- nlm(f, -100)
res

#######################
## EWMA Optimisaiton ##
#######################
# Compute te EWMA variance
ewma.var <- function(r, lambda) {
  sigma2 <- sum((1-lambda) * lambda^(0:(length(r) - 1)) * rev(r)^2)
  return(sigma2)
}
# testing
ewma.var(r, 0.94)
# volatility
sqrt(ewma.var(r, 0.94)) * sqrt(250)

# comutes the historical EWMA variance
hist.ewma.var <- function(r, lambda) {
  sigma2 <- c()
  for (i in 1:length(r)) {
    sigma2 <- c(sigma2, ewma.var(r[1:i], lambda))
  }
  return(sigma2)
}
# testing
a <- hist.ewma.var(r, 0.94)
summary(a)

# log likelyhood function
ewma.ll.fun <- function(lambda, r) {
  sigma2 <- hist.ewma.var(r, lambda)
  sigma2 <- sigma2[-length(sigma2)]
  r <- r[-1]
  log.ll <- sum(-log(sigma2) - r^2/sigma2)
  return(-log.ll)
}
# testing
res <- nlminb(0.5, ewma.ll.fun, lower = 1e-6, upper = 1 - 1e-6, r = r)
res
res$par
res$objective

# old lambda
sqrt(ewma.var(r, lambda = 0.94) * 250)

# best lambda
sqrt(ewma.var(r, lambda = res$par) * 250)

#################
## Garch Model ##
#################
garch.var <- function(r, omega, alpha, beta) {
  sigma2 <- r[1]^2
  for (i in 2:length(r)) {
    sigma2 <- c(sigma2, omega + alpha * r[i]^2 + beta * sigma2[i -1])
  }
return(sigma2)
}
a <- garch.var(r, omega = 0.001, alpha = 0.4, beta = 0.4)
length(a)

garch.ll.fn <- function(par, r) {
  omega <- par[1]
  alpha <- par[2]
  beta <- par[3]
  sigma2 <- garch.var(r, omega, alpha, beta)
  r <- r[-1]
  sigma2 <- sigma2[-length(sigma2)]
  ll <- sum(-log(sigma2) - r^2/sigma2)
  return(-ll)
}

# testing
res <- nlminb(c(0.001, 0.3, 0.3), garch.ll.fn, lower = 1e-6, upper = 1 - 1e-6, r = r)
res

# long term variance
omega <- res$par[1]
alpha <- res$par[2]
beta <- res$par[3]

gamma <- 1 - alpha - beta
gamma
VL <- omega/gamma
sqrt(VL*250)

# 1 year forecast
ewma.sigma2 <- ewma.var(r, lambda = 0.93)
sqrt(ewma.sigma2*250)

m <- 249
garch.sigma2 <- garch.var(r, omega, alpha, beta)
length(garch.sigma2)
garch.sigma2.t <- garch.sigma2[length(garch.sigma2)]
garch.sigma2.vec <- VL + (alpha + beta)^(0:m) * (garch.sigma2.t - VL)

garch.sigma.vec <- sqrt(garch.sigma2.vec)
plot(garch.sigma.vec * sqrt(250), type = "l", xlab = "days", ylab = "GARCH forecast volatilit")
sqrt(sum(garch.sigma2.vec))
