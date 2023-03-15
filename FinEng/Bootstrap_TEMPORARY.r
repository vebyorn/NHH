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

# Forward Rate Agreements
FRA.Mar23 = (100 - 94.93) / 100
FRA.Jun23 = (100 - 94.57) / 100
FRA.Sep23 = (100 - 94.485) / 100
FRA.Dec23 = (100 - 94.72) / 100
FRA.Mar24 = (100 - 95.15) / 100
FRAs = c(FRA.Mar23, FRA.Jun23, FRA.Sep23, FRA.Dec23, FRA.Mar24)


# Forward Rate Agreements Discount Factors
t = c(t, 32/365.242, 118/365.242, 273/365.242) # 364/365.242, 455/365.242
t
Z = c(Z, 1/(1 + FRAs[1] * t[3]), 1/(1 + FRAs[2] * t[4]), 1/(1 + FRAs[3] * t[5]), 1/(1 + FRAs[4] * t[6]), 1/(1 + FRAs[5] * t[7]))
Z
32 + 