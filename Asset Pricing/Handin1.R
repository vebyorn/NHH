# Problem 1
# Find c such that E{(1/(1-b)) * X^(1-b)} = (1/(1-b))c^(1-b)
# Where E{X^(1-b)} = E{e^((1-b)mu + (1-b)*sigma*z)} = e^((1-b)mu + (1-b)^2*sigma^2/2)
# So c = e^(mu + (1-b)*sigma^2/2) 

# CE
c = exp(0.1 + (1-2)*0.4^2/2); c
# Expected outcome
eOutcome = exp(0.1+0.4^2/2); eOutcome

# Problem 2
w = 5000 
unInsured = 0.5*log(w+1000) + 0.5*log(w-1000)
insured = log(w-125)
unInsured - insured # Positive utility, prefers not to buy insurance

w = 4000
unInsured= 0.5*log(w+1000) + 0.5*log(w-1000)
insured = log(w-125)
unInsured - insured # Negative utility, prefers to buy insurance

# Problem 3
w = 100000
eUtility = 0.1*log(w-99999) + 0.1*log(w-50000) + 0.8*log(w)

# Find X such that eUtility = log(w-X)
X = w - exp(eUtility); X
