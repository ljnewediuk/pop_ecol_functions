library(tidyverse)

# 1 - Function to estimate population growth over time ----

# life_table = A life table (minimum required columns Age, Sa, and Fa)
# size_vec = vector of population sizes (must be same length as nrow(life_table))
asm <- function(life_table, size_vec, times) {
  
  iteration <- 0
  
  repeat{  
    
    iteration <- iteration + 1 
    size_vec_tplus1 <- c()
    
    for(i in 1:nrow(life_table)) {
      
      if(i == 1) {
        size_vec_tplus1[i] <- sum(size_vec * life_table$Fa)
      } else {
        size_vec_tplus1[i] <- size_vec[i-1] * life_table[i-1 ,]$Sa
        
      }
    }
    
    size_vec <- size_vec_tplus1
    
    if(iteration == times) break
    
  }
  
  return(size_vec)
  
}

# Life table for VI marmots
lt_marmots <- tibble(Age = 1:10,
                     Sa = c(0.5, rep(0.8, times = 8), 0),
                     Fa = c(rep(0, times = 3), 0.1, rep(2, times = 6))) %>%
  mutate(La = lag(cumprod(Sa), n = 1))
# Make the first La == 1
lt_marmots[1,]$La = 1


# Pop size vector for VI marmots
pop_size_marmots <- c(0, 20, rep(0, times = 8))

# Adjust 10-30 to show development of stable age structure/long-term growth rate
Times <- 10

asm_table <- tibble(
  Time = rep(1:Times, each = nrow(lt_marmots)),
  Age = factor(rep(1:nrow(lt_marmots), Times)), 
  Nta = lapply(1:Times, 
              function(x) asm(life_table = lt_marmots, 
                              size_vec = pop_size_marmots, 
                              times = x)) %>% 
    unlist())

# Plot all ages over time
ggplot(asm_table, aes(x = Time, y = Nta, group = Age)) +
  geom_line(aes(colour = Age))

# Estimate growth rates by time
Nt_totals <- asm_table %>% 
  group_by(Time) %>%
  summarize(Nt = sum(Nta)) %>%
  mutate(lambda_est = Nt / lag(Nt))

# 2 - Function to simulate age structure over time ----

# Simple life table
lt<- tibble(Age = c(1, 2, 3), 
            Sa = c(0.5, 0.5, 0),
            la = c(1, 0.5, 0.25),
            Fa = c(0, .25, 4))

# lt = life table (must have columns Age, Sa, Fa)
# Nts = length of population size vector (must be same as nrow(life table))
# reps = number of repeats of simulation
# t = number of time points
# Nt_out = return population sizes or growth rates?
sim_asm <- function(lt, Nts, reps, t, Nt_out = TRUE) {
  
  it <- 0
  Nt_asm <- tibble()
  growth_rates <- tibble()
  
  repeat{
    it <- it + 1
    
    size_vec <- sample(1:100, size = Nts)
    
    asm_table <- tibble(
      Time = rep(1:t, each = nrow(lt)),
      Age = factor(rep(1:nrow(lt), t)), 
      Nta = lapply(1:t, 
                   function(x) asm(life_table = lt, 
                                   size_vec = size_vec, 
                                   times = x)) %>% 
        unlist()) %>%
      mutate(iteration = it)
    
    Nt_asm <- bind_rows(Nt_asm, asm_table)
    
    Nt_totals <- asm_table %>% 
      group_by(Time) %>%
      summarize(Nt = sum(Nta)) %>%
      mutate(lambda_est = Nt / lag(Nt),
             iteration = it)
    
    growth_rates <- bind_rows(growth_rates, Nt_totals)
    
    if(it >= reps) break
  }
  if(isTRUE(Nt_out)){
    return(Nt_asm)
  } else {
    return(growth_rates)
  }
}

# Plot population sizes
sim_asm(lt = lt, Nts = 3, reps = 5, t = 100) %>%
ggplot(aes(x = Time, y = Nta)) +
  geom_line(aes(colour = Age, group = interaction(iteration, Age)))

# Plot growth rates
sim_asm(lt = lt, Nts = 3, reps = 10, t = 80, Nt_out = FALSE) %>%
  ggplot(aes(x = Time, y = lambda_est)) +
  geom_line(aes(group = factor(iteration), colour = factor(iteration))) +
  ylab("Lambda")

# The long-term growth rate and stable age structure will change if we adjust
# the Leslie matrix, as the eigenvalue and eigenvector are properties of the
# matrix.

# Modify the life table (increase fecundity of age class 2 from 0.25 to 3)
lt<- tibble(Age = c(1, 2, 3), 
            Sa = c(0.5, 0.5, 0),
            la = c(1, 0.5, 0.25),
            Fa = c(0, 3, 4))

# Plot growth rates
sim_asm(lt = lt, Nts = 3, reps = 10, t = 15, Nt_out = FALSE) %>%
  ggplot(aes(x = Time, y = lambda_est)) +
  geom_line(aes(group = factor(iteration), colour = factor(iteration))) +
  ylab("Lambda")

