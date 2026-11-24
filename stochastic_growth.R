
library(tidyverse)

# Function to estimate growth over time with an alternating high/low growth rate
stoch_growth <- function(N0, t, growth_high, growth_low) {
  
  # Create population size vector, empty except for the starting size
  pop_sizes <- vector(mode = "numeric", length = t + 1)
  pop_sizes[1] <- N0
  
  # Create a vector of alternating high and low lambda of the same length
  lamb_vec <- rep(c(growth_high, growth_low), length.out = length(pop_sizes))
  
  # Recursively calculate the next population size in the time step
  for(i in 2:length(pop_sizes)) {
    Nt <- pop_sizes[i-1]*lamb_vec[i]
    pop_sizes[i] <- Nt
  }
  
  # Create a tibble with Time and Population size
  pop_df <- tibble(
    Time = 1:(t+1),
    N = pop_sizes
  )
  return(pop_df)
  
}

# Create three population tables with different alternating growth rates
# lambda = 0.95, 1.05
Nt1 <- stoch_growth(N0 = 20, t = 150, growth_high = 1.05, growth_low = 0.95) %>%
  mutate(lambda = "0.95, 1.05")
# lambda = 0.90, 1.10
Nt2 <- stoch_growth(N0 = 20, t = 150, growth_high = 1.10, growth_low = 0.90) %>%
  mutate(lambda = "0.90, 1.10")
# lambda = 0.85, 1.15
Nt3 <- stoch_growth(N0 = 20, t = 150, growth_high = 1.15, growth_low = 0.85) %>%
  mutate(lambda = "0.85, 1.15")

# Plot the population trajectories
bind_rows(Nt1, Nt2, Nt3) %>%
ggplot(aes(x = Time, y = N, colour = lambda)) +
  geom_hline(yintercept = 20) +
  geom_line(alpha = 0.3) +
  geom_smooth(se = F) + 
  ylim(0, 40)


