rm(list=ls())
library(ggplot2)

# Expected returns and covariances
m = c(1.1, 1.2) # Expected returns of the two securities
V = matrix(c(0.04, 0.0032, 0.0032, 0.16), nrow=2) # Covariance matrix

# Markowitz portfolio
VI = solve(V) # Inverse of V
i = c(rep(1, length(m))) # Vector of ones

# Markowitz substitutions
A = c(t(m) %*% VI %*% m)
B = c(t(m) %*% VI %*% i)
C = c(t(i) %*% VI %*% i)
D = c(A*C - B^2)
a = VI %*% (A*i - B*m) / D  
b = VI %*% (C*m - B*i) / D

# A) The securities are on the portfolio frontier if their E[r]
#    yield frontier portfolios with weights equal to 1 for the
#    security in question, and zero for all the other securites.
#    The expected return of security 1 is m[1], and that of security
#    2 is m[2].
er1 = a+b*m[1]; er1 # security 1
er2 = a+b*m[2]; er2 # security 2
#    Frontier portfolios are corner solutions in this case.

# B) Given a E[R] of 1.25, the portfolio weights result in
#    a long positions of 150% and a short position of 50%.
phi = a+b*1.25; phi 

# C) The portfolio phi_2 - (0,1) is the same as a 100% position
#    in asset 2, whose expected return is 1.2.
#    Equation is x * mu_2 + (1-x)*1.25 - mu_1, solve for X.
(m[1]-1.25) / (m[2]-1.25) # portfolio weights

# D) Plot equation 5 from slides. (Portfolio Frontier)
eq5 = function(x) { 1/C + (x-B/C)^2*C/D } # Equation 5

# Plot
pf = ggplot(data = data.frame(x=0), aes(x=x)) + 
    xlab("Expected return") + ylab("Variance")

# Apply function to plot
pf + stat_function(fun=eq5, n=100) + xlim(0,2.25) + coord_flip()
