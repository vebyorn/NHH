



library(tidyverse)

bermudan_call_low_estimator <- function(S0, K, sigma, r, T, delta_t, b, n) {
  m <- T / delta_t
  dt <- T / m
  
  option_values <- numeric(n)
  
  for (i in 1:n) {
    S <- matrix(0, nrow = b, ncol = m + 1)
    S[, 1] <- S0
    
    for (t in 1:m) {
      Z <- rnorm(b, mean = 0, sd = 1)
      S[, t + 1] <- S[, t] * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
    }
    
    V <- matrix(0, nrow = b, ncol = m + 1)
    V[, m + 1] <- pmax(S[, m + 1] - K, 0)
    
    for (t in m:1) {
      for (k in 1:b) {
        c_k <- exp(-r * dt) * mean(V[-k, t + 1])
        h <- max(S[k, t] - K, 0)
        V[k, t] <- if (c_k <= h) h else exp(-r * dt) * V[k, t + 1]
      }
    }
    
    option_values[i] <- mean(V[, 1])
  }
  
  mean_value <- mean(option_values)
  sd_value <- sd(option_values)
  
  return(list(mean = mean_value, sd = sd_value))
}

S0 <- 100
K <- 100
sigma <- 0.20
r <- 0.01
T <- 2
delta_t <- 0.25
b <- 3
n <- 100

results <- tibble()

num_iterations <- 100
for (i in 1:num_iterations) {
  result <- bermudan_call_low_estimator(S0, K, sigma, r, T, delta_t, b, n)
  standard_error <- result$sd / sqrt(n)
  
  temp_results <- tibble(
    iteration = i,
    low_estimator = result$mean,
    standard_error = standard_error
  )
  
  results <- bind_rows(results, temp_results)
}

print(results)


library(rgl)

# Create a 3D scatter plot
plot3d(results$iteration, results$low_estimator, results$standard_error,
       xlab = "Iteration", ylab = "Low Estimator", zlab = "Standard Error",
       type = "s", col = "blue", size = 0.5)

# You can interact with the plot using your mouse to rotate and zoom












###++++++++++++++++++++
bermudan_call_low_estimator <- function(S0, K, sigma, r, T, delta_t, b, n) {
  m <- T / delta_t
  dt <- T / m
  
  option_values <- numeric(n)
  
  for (i in 1:n) {
    S <- matrix(0, nrow = b, ncol = m + 1)
    S[, 1] <- S0
    
    for (t in 1:m) {
      Z <- rnorm(b, mean = 0, sd = 1)
      S[, t + 1] <- S[, t] * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
    }
    
    V <- matrix(0, nrow = b, ncol = m + 1)
    V[, m + 1] <- pmax(S[, m + 1] - K, 0)
    
    for (t in m:1) {
      for (k in 1:b) {
        c_k <- exp(-r * dt) * mean(V[-k, t + 1])
        h <- max(S[k, t] - K, 0)
        V[k, t] <- if (c_k <= h) h else exp(-r * dt) * V[k, t + 1]
      }
    }
    
    option_values[i] <- mean(V[, 1])
  }
  
  mean_value <- mean(option_values)
  sd_value <- sd(option_values)
  
  return(list(mean = mean_value, sd = sd_value))
}

S0 <- 100
K <- 100
sigma <- 0.20
r <- 0.01
T <- 2
delta_t <- 0.25
b <- 3
n <- 100

result <- bermudan_call_low_estimator(S0, K, sigma, r, T, delta_t, b, n)
standard_error <- result$sd / sqrt(n)

cat("The low estimator of the Bermudan call option price is:", result$mean, "\n")
cat("The standard error of the Bermudan call option price low estimator is:", standard_error)

















bermudan_call_low_estimator <- function(S0, K, sigma, r, T, delta_t, b, n) {
  m <- T / delta_t
  dt <- T / m
  
  option_values <- numeric(n)
  
  for (i in 1:n) {
    S <- matrix(0, nrow = b, ncol = m + 1)
    S[, 1] <- S0
    
    for (t in 1:m) {
      Z <- rnorm(b, mean = 0, sd = 1)
      S[, t + 1] <- S[, t] * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
    }
    
    V <- matrix(0, nrow = b, ncol = m + 1)
    V[, m + 1] <- pmax(S[, m + 1] - K, 0)
    
    for (t in m:1) {
      for (k in 1:b) {
        c_k <- exp(-r * dt) * mean(V[-k, t + 1])
        h <- max(S[k, t] - K, 0)
        V[k, t] <- if (c_k <= h) h else exp(-r * dt) * V[k, t + 1]
      }
    }
    
    option_values[i] <- mean(V[, 1])
  }
  
  return(mean(option_values))
}

S0 <- 100
K <- 100
sigma <- 0.20
r <- 0.01
T <- 2
delta_t <- 0.25
b <- 3
n <- 50

low_estimator <- bermudan_call_low_estimator(S0, K, sigma, r, T, delta_t, b, n)
cat("The low estimator of the Bermudan call option price is:", low_estimator)
cat("The standard error of the Bermudan call option price low estimator is:", standard_error)



















##############33+++++++++++++++++++++++++++++++++++++
library(random)

bermudan_call_low_estimator <- function(S0, K, sigma, r, T, delta_t, b, n) {
  m <- T / delta_t
  dt <- T / m
  
  option_values <- numeric(n)
  
  for (i in 1:n) {
    S <- matrix(0, nrow = b, ncol = m + 1)
    S[, 1] <- S0
    
    for (t in 1:m) {
      Z <- rnorm(b, mean = 0, sd = 1)
      S[, t + 1] <- S[, t] * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
    }
    
    V <- matrix(0, nrow = b, ncol = m + 1)
    V[, m + 1] <- pmax(S[, m + 1] - K, 0)
    
    for (t in m:1) {
      for (k in 1:b) {
        c_k <- exp(-r * dt) * mean(V[-k, t + 1])
        h <- max(S[k, t] - K, 0)
        V[k, t] <- if (c_k <= h) h else exp(-r * dt) * V[k, t + 1]
      }
    }
    
    option_values[i] <- mean(V[, 1])
  }
  
  return(mean(option_values))
}

S0 <- 100
K <- 100
sigma <- 0.20
r <- 0.01
T <- 2
delta_t <- 0.25
b <- 3
n <- 100

low_estimator <- bermudan_call_low_estimator(S0, K, sigma, r, T, delta_t, b, n)
cat("The low estimator of the Bermudan call option price is:", low_estimator)


















bermudan_call_low_estimator <- function(S0, K, sigma, r, T, delta, b, n) {
  h <- function(tj, S) pmax(S - K, 0)
  
  low_estimator_sum <- 0
  for (tree in 1:n) {
    S <- matrix(0, nrow = b, ncol = T/delta + 1)
    S[,1] <- S0
    
    for (i in 1:(T/delta)) {
      randoms <- matrix(rnorm(b * (b - 1), 0, sigma * sqrt(delta)), nrow = b - 1)
      for (j in 1:b) {
        next_rows <- ((j + (0:(b - 2))) %% b) + 1
        S[next_rows, i+1] <- S[j,i] * exp((r - 0.5 * sigma^2) * delta + randoms[, j])
      }
    }
    
    V <- matrix(0, nrow = b, ncol = T/delta + 1)
    V[,ncol(V)] <- h(T, S[,ncol(S)])
    
    for (i in ((T/delta - 1):0)) {
      for (k in 1:b) {
        c_k <- exp(-r * delta) * sum(V[-k, i+1]) / (b - 1)
        V[k, i] <- ifelse(c_k <= h(i * delta, S[k, i]), h(i * delta, S[k, i]), exp(-r * delta) * V[k, i+1])
      }
    }
    low_estimator_sum <- low_estimator_sum + mean(V[,1])
  }
  
  low_estimator <- low_estimator_sum / n
  return(low_estimator)
}

S0 <- 100
K <- 100
sigma <- 0.2
r <- 0.01
T <- 2
delta <- 0.25
b <- 3
n <- 2

low_estimator <- bermudan_call_low_estimator(S0, K, sigma, r, T, delta, b, n)
print(low_estimator)











































library(Matrix)

bermudan_call_low_estimator <- function(S0, K, sigma, r, T, delta, b, n) {
  h <- function(tj, S) pmax(S - K, 0)
  
  low_estimator_sum <- 0
  for (tree in 1:n) {
    S <- matrix(0, nrow = b, ncol = T/delta + 1)
    S[,1] <- S0
    
    for (i in 1:(T/delta)) {
      randoms <- rnorm(b, 0, sigma * sqrt(delta))
      for (j in 1:b) {
        next_row <- (j %% b) + 1
        S[next_row,i+1] <- S[j,i] * exp((r - 0.5 * sigma^2) * delta + randoms[j])
      }
    }
    
    V <- matrix(0, nrow = b, ncol = T/delta + 1)
    V[,ncol(V)] <- h(T, S[,ncol(S)])
    
    for (i in ((T/delta - 1):0)) {
      for (k in 1:b) {
        c_k <- exp(-r * delta) * sum(V[-k, i+1]) / (b - 1)
        V[k, i] <- ifelse(c_k <= h(i * delta, S[k, i]), h(i * delta, S[k, i]), exp(-r * delta) * V[k, i+1])
      }
    }
    low_estimator_sum <- low_estimator_sum + mean(V[,1])
  }
  
  low_estimator <- low_estimator_sum / n
  return(low_estimator)
}

S0 <- 100
K <- 100
sigma <- 0.2
r <- 0.01
T <- 2
delta <- 0.25
b <- 3
n <- 1000

low_estimator <- bermudan_call_low_estimator(S0, K, sigma, r, T, delta, b, n)
print(low_estimator)









































bermudan_call_option_low_estimator_and_random_trees <- function(K, S, sigma, r, T, dt, b, n) {
  intrinsic_value <- function(S_t, K) {
    return(max(S_t - K, 0))
  }
  
  option_value_low_estimator <- function(K, S_t, sigma, r, T, dt, b) {
    if (T <= 0) {
      return(intrinsic_value(S_t, K))
    }
    
    V <- numeric(b)
    for (k in 1:b) {
      W <- rnorm(b - 1, mean = 0, sd = sqrt(dt))
      S_t_next <- S_t * exp((r - 0.5 * sigma^2) * dt + sigma * W)
      V_excl_k <- sapply(S_t_next[-k], function(x) option_value_low_estimator(K, x, sigma, r, T - dt, dt, b))
      c_t <- exp(-r * dt) * mean(V_excl_k)
      
      if (c_t <= intrinsic_value(S_t, K)) {
        V[k] <- intrinsic_value(S_t, K)
      } else {
        V[k] <- c_t
      }
    }
    
    V_t <- mean(V)
    return(V_t)
  }
  
  option_values_low_estimator <- sapply(seq_len(n), function(i) option_value_low_estimator(K, S, sigma, r, T, dt, b))
  option_price_low_estimator <- mean(option_values_low_estimator)
  se_low_estimator <- sd(option_values_low_estimator) / sqrt(n)
  
  random_tree_option_value <- function(K, S_t, sigma, r, T, dt, b) {
    if (T <= 0) {
      return(intrinsic_value(S_t, K))
    }
    
    V <- numeric(b)
    for (k in 1:b) {
      W <- rnorm(b, mean = 0, sd = sqrt(dt))
      S_t_next <- S_t * exp((r - 0.5 * sigma^2) * dt + sigma * W)
      V[k] <- random_tree_option_value(K, S_t_next[k], sigma, r, T - dt, dt, b)
    }
    
    V_t <- mean(V)
    return(V_t)
  }
  
  option_values_random_trees <- sapply(seq_len(n), function(i) random_tree_option_value(K, S, sigma, r, T, dt, b))
  option_price_random_trees <- mean(option_values_random_trees)
  se_random_trees <- sd(option_values_random_trees) / sqrt(n)
  
  return(list(low_estimator = list(price = option_price_low_estimator, se = se_low_estimator),
              random_trees = list(price = option_price_random_trees, se = se_random_trees)))
}

# Parameters
K <- 100
S <- 100
sigma <- 0.20
r <- 0.01
T <- 2
dt <- 0.25
b <- 3
n <- 100

# Pricing the Bermudan call option with low estimator and random trees
result <- bermudan_call_option_low_estimator_and_random_trees(K, S, sigma, r, T, dt, b, n)
cat("Bermudan call option price (low estimator):", result$low_estimator$price, "\n")
cat("Standard error (low estimator):", result$low_estimator$se, "\n")
cat("Bermudan call option price (random trees):", result$random_trees$price, "\n")
cat("Standard error (random trees):", result$random_trees$se, "\n")

      
      
      
  



    




bermudan_call_option_low_estimator <- function(K, S, sigma, r, T, dt, b, n) {
  intrinsic_value <- function(S_t, K) {
    return(max(S_t - K, 0))
  }
  
  option_value <- function(K, S_t, sigma, r, T, dt, b) {
    if (T == 0) {
      return(intrinsic_value(S_t, K))
    }
    
    V <- rep(0, b)
    for (k in 1:b) {
      W <- rnorm(1, mean = 0, sd = 1)
      S_t_next <- S_t * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * W)
      V_excl_k <- option_value(K, S_t_next, sigma, r, T - dt, dt, b)[-k]
      c_t <- exp(-r * dt) * mean(V_excl_k)
      
      if (c_t <= intrinsic_value(S_t, K)) {
        V[k] <- intrinsic_value(S_t, K)
      } else {
        V[k] <- c_t
      }
    }
    
    V_t <- mean(V)
    return(V_t)
  }
  
  option_values <- rep(0, n)
  for (i in 1:n) {
    option_values[i] <- option_value(K, S, sigma, r, T, dt, b)
  }
  
  option_price <- mean(option_values)
  se <- sd(option_values) / sqrt(n)
  
  return(list(price = option_price, se = se))
}

# Parameters
K <- 100
S <- 100
sigma <- 0.20
r <- 0.01
T <- 2
dt <- 0.25
b <- 3
n <- 100

# Pricing the Bermudan call option with low estimator
result <- bermudan_call_option_low_estimator(K, S, sigma, r, T, dt, b, n)
cat("Bermudan call option price (low estimator):", result$price, "\n")
cat("Standard error:", result$se)




































bermudan_call_option_low_estimator <- function(K, S, sigma, r, T, dt, b, n) {
  intrinsic_value <- function(S_t, K) {
    return(max(S_t - K, 0))
  }
  
  option_value <- function(K, S_t, sigma, r, T, dt, b) {
    if (T == 0) {
      return(intrinsic_value(S_t, K))
    }
    
    V <- rep(0, b)
    for (k in 1:b) {
      W <- rnorm(1, mean = 0, sd = 1)
      S_t_next <- S_t * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * W)
      c_t <- exp(-r * dt) * mean(option_value(K, S_t_next, sigma, r, T - dt, dt, b)[-k])
      
      if (c_t <= intrinsic_value(S_t, K)) {
        V[k] <- intrinsic_value(S_t, K)
      } else {
        V[k] <- exp(-r * dt) * option_value(K, S_t_next, sigma, r, T - dt, dt, b)[k]
      }
    }
    
    V_t <- mean(V)
    return(V_t)
  }
  
  option_values <- rep(0, n)
  for (i in 1:n) {
    option_values[i] <- option_value(K, S, sigma, r, T, dt, b)
  }
  
  option_price <- mean(option_values)
  se <- sd(option_values) / sqrt(n)
  
  return(list(price = option_price, se = se))
}

# Parameters
K <- 100
S <- 100
sigma <- 0.20
r <- 0.01
T <- 2
dt <- 0.25
b <- 3
n <- 100

# Pricing the Bermudan call option with low estimator
result <- bermudan_call_option_low_estimator(K, S, sigma, r, T, dt, b, n)
cat("Bermudan call option price (low estimator):", result$price, "\n")
cat("Standard error:", result$se)










































































