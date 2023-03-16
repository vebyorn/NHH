Z = 1
t = 0

# Overnight and Tomorrow Next Deposits
ON = 0.047
TN = 0.0457

ON.dep = c(ON, TN)
Z = c(Z, 1/((1 + (1/365.242) *  ON.dep[1]) * (1 + (3/365.242) * ON.dep[2]))) # ON and TN discount factors
t = c(t, 4/365.242) # ON and TN accrual times

# Money Market Deposits
MM.w1 = 0.0471 # 1 week MM deposit
MM.m1 = 0.0484 # 1 month MM deposit
Z = c(Z, 1/((1 + (1/365.242) *  MM.w1) * (1 + (10/365.242) * MM.m1))) # 1 week and 1 month MM discount factors
t = c(t, 11/365.242) # 1 week and 1 month MM accrual times
t

# Interest Rate Futures
IF.Mar23 = (100 - 94.93) / 100
IF.Jun23 = (100 - 94.57) / 100
IF.Sep23 = (100 - 94.485) / 100
IF.Dec23 = (100 - 94.72) / 100
IF.Mar24 = (100 - 95.15) / 100
IFs = c(FRA.Mar23, FRA.Jun23, FRA.Sep23, FRA.Dec23, FRA.Mar24)

# times to maturity for each contract
t = c(t, 32/365.242, 118/365.242, 216/365.242, 306/365.242, 397/365.242)

# turning IFs into futures rates
IFs = (1 - IFs) / t[4:8]
IFs
