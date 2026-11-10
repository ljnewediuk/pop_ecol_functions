
library(tidyverse)

# Part 1 - Evaluate the Jacobian matrix ----

# Define function to evaluate the Jacobian

eval_J <- function(r1, r2, K1, K2, a12, a21) {
  
  # Equilibrium population sizes
  N1star <- (K1 - a12 * K2) / (1 - a12 * a21)
  N2star <- (K2 - a21 * K1) / (1 - a21 * a12)
  
  # Create the Jacobian matrix
  J <- matrix(
    nrow = 2,
    c(
      -r1 * N1star / K1,
      -r1 * a12 * N1star / K1,
      -r2 * a21 * N2star / K2,
      -r2 * N2star / K2
    )
  )
  
  # Evaluate and return the eigenvalues of the Jacobian
  return(eigen(J)$values)
  
}

# Evaluate stability, given population parameters
eval_J(r1 = 0.8, r2 = 0.8, K1 = 100, K2 = 105, a12 = 1.1, a21 = 0.9)

# Part 2 - Evaluate different scenarios ----

# Function to evaluate the invasion criterion
Bij <- function(aij, Kj, Ki) aij * Kj / Ki

# Function to plot isoclines
plot_isoclines <- function(K1, K2, a12, a21, N_max) {
  
  # Generate population sizes for plotting
  N <- 0:N_max
  
  # Calculate isoclines
  N2_species1 <- (K1 - N) / a12
  N2_species2 <- K2 - a21 * N
  
  # Create a tibble 
  isoclines <- tibble(
    N1 = rep(N, times = 2),
    N2 = c(N2_species1, N2_species2),
    Species = rep(c("Species 1", "Species 2"), each = length(N))
  )
  
  # Plot
  iso_plot <- ggplot(isoclines, aes(x = N1, y = N2, linetype = Species)) +
    geom_line(linewidth = 1) +
    coord_cartesian(xlim = c(0, N_max+N_max*.5), ylim = c(0, N_max+N_max*.5)) +
    labs(linetype = "ZNGI") 
  
  return(iso_plot)
  
}

# 1 - Species 2 wins (single stable equilibrium)

K1 <- 100
K2 <- 105

a12 <- 1.4
a21 <- 0.8

r1 <- 0.5
r2 <- 0.4

# Plot the isoclines
plot_isoclines(K1 = K1, K2 = K2, a12 = a12, a21 = a21, N_max = 100)

# Evaluate invasion criteria
# Invasion by species 2
Bij(aij = a12, Kj = K2, Ki = K1)
# Invasion by species 1
Bij(aij = a21, Kj = K1, Ki = K2)

# Evaluate stability, given population parameters
eval_J(r1 = r1, r2 = r1, K1 = K1, K2 = K2, a12 = a12, a21 = a21)

# 2 - Stable coexistence (single stable equilibrium)

K1 <- 100
K2 <- 105

a12 <- 0.7
a21 <- 0.6

r1 <- 0.5
r2 <- 0.4

# Plot the isoclines
plot_isoclines(K1 = K1, K2 = K2, a12 = a12, a21 = a21, N_max = 100)

# Evaluate invasion criteria
# Invasion by species 2
Bij(aij = a12, Kj = K2, Ki = K1)
# Invasion by species 1
Bij(aij = a21, Kj = K1, Ki = K2)

# Evaluate stability, given population parameters
eval_J(r1 = r1, r2 = r1, K1 = K1, K2 = K2, a12 = a12, a21 = a21)

# 3 - Unstable (multiple possible equilibria - depends on starting N1 and N2)

K1 <- 100
K2 <- 105

a12 <- 1.4
a21 <- 1.6

r1 <- 0.5
r2 <- 0.4

# Plot the isoclines
plot_isoclines(K1 = K1, K2 = K2, a12 = a12, a21 = a21, N_max = 100)

# Evaluate invasion criteria
# Invasion by species 2
Bij(aij = a12, Kj = K2, Ki = K1)
# Invasion by species 1
Bij(aij = a21, Kj = K1, Ki = K2)

# Evaluate stability, given population parameters
eval_J(r1 = r1, r2 = r1, K1 = K1, K2 = K2, a12 = a12, a21 = a21)
