
# Load packages
library(tidyverse)
library(deSolve) # Package for solving differential equations

# Define rates
b <- 0.8
a <- 0.05 
e <- 0.4 
d <- 0.2

# 1 - Plot the isoclines

plot_isoclines <- function(e, a, d, b) {
  
  # Prey isocline
  P <- b/a
  
  # Predator isocline
  N <- d/(e*a)
  
  # Plot the isoclines
  p <- ggplot() +
    geom_hline(yintercept = P, colour = "red") +
    geom_vline(xintercept = N, colour = "blue") +
    labs(x = "Prey population size (N)", y = "Predator population size (P)")
  
  return(p)
  
}

# Plot the isoclines
plot_isoclines(e = e, a = a, b = b, d = d)

# 2 - Perform the eigen-analysis

pred_prey_eigen <- function(e, b, d) {
  
  # Construct Jacobian matrix
  m <- matrix(c(0, -d/e, e*b, 0), nrow = 2)
  
  # Perform the eigen-analysis
  return(eigen(m)$values)
}

pred_prey_eigen(e = e, b = b, d = d)

# 3 - Solve the differential equations and plot

# Choose starting population sizes
N0 <- 20
P0 <- 20

# Make a time sequence
Time <- seq(0, 100, by = 0.1) 

# 3 a - Use the classic Lotka-Volterra formulation

# Define an ODE function for the classic Lotka-Volterra model
predpreyLV <- function(t, y, params) { 
  N <- y[1] 
  P <- y[2] 
  with(as.list(params), {
    dNdt <- b * N - a * P * N 
    dPdt <- e * a * P * N - d * P
    return(list(c(dNdt, dPdt)))
  })
}

# Re-define rates
b <- 0.8
a <- 0.05 
e <- 0.4 
d <- 0.2

# Make a vector of the Lotka-Volterra parameters
parmsLV <- c(b = b, a = a, d = d, e = e)

# Solve 
LV_out <- ode(c(N0 = N0, P0 = P0), Time, predpreyLV, parmsLV) %>%
  as_tibble()

# Plot the predator-prey trajectory
ggplot(LV_out) +
  geom_line(aes(x = time, y = P0), colour = "red") +
  geom_line(aes(x = time, y = N0), colour = "blue") +
  labs(x = "Time", y = "Population size")

# 3 b - Use the Rosenzweig-MacArthur formulation

# Define new rates
w <- 5
D <- 20
alpha <- 0.1

# Re-define rates
b <- 0.8
a <- 0.05 
e <- 0.4 
d <- 0.2

# Define an ODE function for the Rosenzweig-MacArthur model
predpreyRM <- function(t, y, params) { 
  N <- y[1] 
  P <- y[2] 
  with(as.list(params), { 
    dNdt <- b * N * (1 - alpha * N) - w * P * N/(D + N) 
    dPdt <- e * w * P * N/(D + N) - d * P 
    return(list(c(dNdt, dPdt))) 
  }) 
}

# Make a vector of the Rosenzweig-MacArthur parameters
parmsRM <- c(b = b, a = a, d = d, e = e, w = w, D = D, alpha = alpha)

# Solve 
RM_out <- ode(c(N0 = N0, P0 = P0), Time, predpreyRM, parmsRM) %>%
  as_tibble()

# Plot the predator-prey trajectory
ggplot(RM_out) +
  geom_line(aes(x = time, y = P0), colour = "red") +
  geom_line(aes(x = time, y = N0), colour = "blue") +
  labs(x = "Time", y = "Population size")

