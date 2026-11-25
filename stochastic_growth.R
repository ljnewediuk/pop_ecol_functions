
library(tidyverse)

# 1 - Geometric mean vs. arithmetic mean

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
  mutate(lambda = "0.95, 1.05",
         mean_lambda = mean(c(0.95, 1.05))*20)
# lambda = 0.90, 1.10
Nt2 <- stoch_growth(N0 = 20, t = 150, growth_high = 1.10, growth_low = 0.90) %>%
  mutate(lambda = "0.90, 1.10",
         mean_lambda = mean(c(0.90, 1.10))*20)
# lambda = 0.85, 1.15
Nt3 <- stoch_growth(N0 = 20, t = 150, growth_high = 1.15, growth_low = 0.85) %>%
  mutate(lambda = "0.85, 1.15",
         mean_lambda = mean(c(0.85, 1.15))*20)

# Plot the population trajectories
bind_rows(Nt1, Nt2, Nt3) %>%
ggplot(aes(x = Time, y = N, colour = lambda)) +
  geom_hline(yintercept = 20, linetype = "dashed") +
  geom_line(aes(y = mean_lambda)) +
  geom_line(alpha = 0.3) +
  geom_smooth(se = F) + 
  ylim(0, 40)

# 2 - Monte Carlo simulations

# Function to simulate population trajectories

sim_pop_mc <- function(N0, mu, sigma, tmax, qextinct, times) {
  
  # Set initial iteration
  iteration <- 1
  
  repeat {
    
    # Break the loop if we hit the maximum number of iterations
    if(iteration > times) break
    
    # Create a vector for years 0 to max time
    years <- 0:tmax
    
    # Create an empty numeric vector to hold the Nts
    Nti <- numeric(length = length(years))
    
    # Randomly draw an rmax from the probability distribution
    rt <- rnorm(n = length(years), mean = mu, sd = sigma)
    
    # Set the initial population size to N0
    Nti[1] <- N0
    
    # Simulate forward in time using the random rts
    for(i in 2:length(years)) Nti[i] <- exp(rt[i-1]) * Nti[i-1]
    
    # Create a table for the simulation
    Nti_sim <- tibble(Year = years, Nt = Nti, Simulation = iteration) %>%
      # Add column to specify whether the population exceeded the extinction threshold
      mutate(Extinct = ifelse(any(Nt < qextinct), "yes", "no"))
    
    # Bind the simulations together, starting with just Nti if initial iteration
    if(iteration == 1) Nt_sim <- Nti_sim
    if(iteration > 1) Nt_sim <- bind_rows(Nt_sim, Nti_sim)
    
    iteration <- iteration + 1
    
  }
  
  return(Nt_sim)
  
}

# Set the parameters for the polar bear population 
# Population sizes from 1999 to 2006
N <- c(702, 821, 739, 828, 729, 874, 804, 716)

# Calculate annual lambda values
lambda <- N[-1] / N[-length(N)]

# Calculate annual log growth rates
rt <- log(lambda)

# Calculate sample mean and standard deviation
mu_rt <- mean(rt)
sd_rt <- sd(rt)

# Calculate the confidence intervals
mu_rt + (sd_rt / sqrt(length(n)))*1.96
mu_rt - (sd_rt / sqrt(length(n)))*1.96

# Set the extinction threshold
extinct <- 200

# Set parameters of simulation and run
sim_pops <- sim_pop_mc(mu = mu_rt, sigma = sd_rt, 
                       N0 = 702, tmax = 50, qextinct = extinct, times = 1000)

# Subset 30 simulations for plotting
sim_pops_subset <- sim_pops %>%
  filter(Simulation <= 30)

# Calculate the mean and confidence interval of the simulations
sim_pops_mean <- sim_pops %>%
  group_by(Year) %>%
  summarize(Mean_Nt = mean(Nt),
            Lower_Nt = mean(Nt) - (sd(Nt)/sqrt(length(Nt)))*1.96,
            Upper_Nt = mean(Nt) + (sd(Nt)/sqrt(length(Nt)))*1.96)

# Plot the simulations
ggplot() +
  geom_hline(yintercept = extinct, linetype = "dashed") +
  geom_line(data = sim_pops_subset, linewidth = 0.5,
            aes(x = Year, y = Nt, group = Simulation, colour = Extinct)) +
  geom_ribbon(data = sim_pops_mean, fill = "grey", alpha = 0.5,
              aes(x = Year, ymin = Lower_Nt, ymax = Upper_Nt)) +
  geom_line(data = sim_pops_mean, colour = "black", linewidth = 1.25,
            aes(x = Year, y = Mean_Nt)) +
  scale_colour_manual(values = c("lightblue", "darkred")) +
  labs(y = "Population size", x = "Time")

# Calculate the proportion of times the population drops below the threshold
cat("There is a ", 
    sum(sim_pops$Extinct == "yes")/nrow(sim_pops)*100, 
    "% chance the population will be extinct within 50 years.")

# 3 - The analytical probability of extinction

cat("There is a ",
    round((extinct / N[1])^(2 * mu_rt / sd_rt^2), 3)*100,
    "% chance that the population will be extinct at some point.")

