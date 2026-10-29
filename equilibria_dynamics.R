
library(tidyverse)

# 1 - Change in growth rate over N

# Define an equation
eq <- function(x) x*(10-x)*(20-x)

# Plot the x values and y values
tibble(x = seq(from = -10, to = 30, by = 1),
       y = sapply(x, function(x) eq(x))) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_hline(linetype = "dashed", yintercept = 0) +
  geom_line()

# Define the logistic growth equation
log_growth <- function(N, rmax, K) rmax*N*(1-N/K)

# Define rates/state variables
Nt <- seq(from = 0, to = 50, by = 5)
K = 45
rmax = 2.2

# Plot the growth rate over N
tibble(N = Nt,
       dNdt = sapply(Nt, function(x) log_growth(N = x, K = K, rmax = rmax))) %>%
  ggplot(aes(x = N, y = dNdt)) + 
  geom_hline(linetype = "dashed", yintercept = 0) +
  geom_line()

# 2- Complex dynamics in discrete growth over time

# Function to estimate growth over time
log_disc_growth <- function(N0, t, rd, K) {
  
  pop_sizes <- vector(mode = "numeric", length = t +1)
  pop_sizes[1] <- N0
  
  for(i in 2:length(pop_sizes)) {
    Nt <- pop_sizes[i-1] + pop_sizes[i-1]*rd*(1-pop_sizes[i-1]/K)
    pop_sizes[i] <- Nt
  }
  
  return(pop_sizes)
  
}

# Set rates and state variables
N0 = 1
t = 50
rd = 2.2
K = 60

# Plot the population size over time
tibble(time = seq(from = 0, to = t, length.out = t +1),
       N = log_disc_growth(N0 = N0, t = t, K = K, rd = rd)) %>%
  ggplot(aes(x = time, y = N)) + 
  geom_line()

# 2- Complex dynamics - graphing Nt+1 over Nt
tibble(replacement = log_disc_growth(N0 = N0, t = t, K = K, rd = rd)[-t+1],
       Nt = log_disc_growth(N0 = N0, t = t, K = K, rd = rd)[-t+1],
       Nt_plus1 = log_disc_growth(N0 = N0, t = t, K = K, rd = rd)[-1]) %>%
  ggplot(aes(x = Nt, y = Nt_plus1)) +
  geom_smooth(se = F, colour = "black", lineend = "round", linewidth = 1.5)
