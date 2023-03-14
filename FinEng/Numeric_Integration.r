rm(list=ls())

## Numerical Integration ########################################
integral = function(n, a, b, g) {
    x = runif(n, a, b)
    Y = g(x)
    a = (b - a) * mean(Y)
    return(a)
}

sol = integral(10000, 1, 5, log)
sol # Numerical solution
