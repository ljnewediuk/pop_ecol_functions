
library(tidyverse)

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

lt<- tibble(Age = c(1, 2, 3), 
            Sa = c(0.5, 0.5, 0),
            la = c(1, 0.5, 0.25),
            Fa = c(0, .25, 8))


Nt1 <- 20
Nt2 <- 14
Nt3 <- 9

# Adjust 10-100 to show development of stable age structure/long-term growth rate
Times <- 10

asm_table <- tibble(
  Time = rep(1:Times, each = nrow(lt)),
  Age = rep(1:nrow(lt), Times), 
  Nta = lapply(1:Times, 
              function(x) asm(life_table = lt, 
                              size_vec = c(Nt1, Nt2, Nt3), 
                              times = x)) %>% 
    unlist())

ggplot(asm_table, aes(x = Time, y = Nta, group = Age)) +
  geom_line(aes(colour = Age))

Nt_totals <- asm_table %>% 
  group_by(Time) %>%
  summarize(Nt = sum(Nta)) %>%
  mutate(lambda_est = Nt / lag(Nt))

