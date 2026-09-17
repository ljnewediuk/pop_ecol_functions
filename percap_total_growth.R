library(tidyverse)

# Function to estimate total growth over time
total_growth <- function(N0, t, B, I, D, E) {
  
  pop_sizes <- vector(mode = "numeric", length = t +1)
  pop_sizes[1] <- N0
  
  for(i in 2:length(pop_sizes)) {
    Nt <- pop_sizes[i-1] + B + I - D - E
    pop_sizes[i] <- Nt
  }
  
  return(pop_sizes)
  
}

# Function to estimate per capita growth over time
percap_growth <- function(N0, t, B, I, D, E) {
  
  b <- B/N0
  d <- D/N0
  e <- E/N0
  im <- I/N0
  
  pop_sizes <- vector(mode = "numeric", length = t +1)
  pop_sizes[1] <- N0
  
  for(i in 2:length(pop_sizes)) {
    Nt <- pop_sizes[i-1] + (b*pop_sizes[i-1]) + (im*pop_sizes[i-1]) - (d*pop_sizes[i-1]) - (e*pop_sizes[i-1])
    pop_sizes[i] <- Nt
  }
  
  return(pop_sizes)
  
}

# Set parameters

N0 <- 100 # Initial population size
t <- 1000 # Time
B <- 20 # Number of births
D <- 10 # Number of deaths
I <- 10 # Number of immigrants
E <- 15 # Number of emigrants

# Plot total growth
tibble(time = 1:(t+1), N = total_growth(N0 = N0, t = t, B = B, D = D, I = I, E = E)) %>%
  ggplot(aes(x = time, y = N)) +
  geom_line()

# Plot per-capita growth
tibble(time = 1:(t+1), N = percap_growth(N0 = N0, t = t, B = B, D = D, I = I, E = E)) %>%
  ggplot(aes(x = time, y = N)) +
  geom_line()
