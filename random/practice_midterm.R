rm(list = ls())
library(ggplot2)
library(dplyr)

dir = "/Users/tombearpark/Documents/princeton/1st_year/ECO517/exercises/mid_term_prep/"

# Question 1
# Assume N = 100
# Assume sum x_i = 100
draws = data.frame(draws = rgamma(1000, shape = 100, rate = 100))
draws = draws %>% mutate(invert_draw= 1/draws)

ggplot(data = draws) +
  geom_density(aes(x =invert_draw))

# Question 2

library(ggplot2)
normal_normal = function(x){
  mu = 1
  sigma =1 
  p = (1 / (sigma * sqrt(2 * pi)) * exp((-1/2)*(x - mu)^2 )/(sigma^2))
  return(p)
}

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun = normal_normal) + xlim(-10,10)
p

normal_trans = function(x){
  mu = 1
  sigma = 1
  p = (1 / (sigma * sqrt(2 * pi)) * 
         exp((-1/2)*((1/x) - mu)^2 )/(sigma^2)) * 
            abs(1 / (x^(2)))
  return(p)
}
q = ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun = normal_trans) + xlim(-10,10) +
  stat_function(fun = normal_normal, color = "blue") + xlim(-4,4)+
  ggtitle("Normal, and inverse of normal")
q

ggsave(q, file = paste0(dir, "2_normal_and_inverse.png"), height = 4, width = 4)

# Get to the same thing using draws instead...
df = data.frame(norm = rnorm(100000, mean = 1, sd =1 )) %>% 
  mutate(inverse= 1/norm)

ggplot(data = df) +
  geom_density(aes(x = inverse), color = "red") +  xlim(-5,5)

mean(df$inverse)
mean(df$norm)

# Plot the integral...
int_above_0 = function(x) (1 / sqrt(2 * pi)) * exp(-0.5*((1/x)-1)^2) * (1/x)

ggplot(data = data.frame(x =0), mapping = aes(x = x)) +
  stat_function(fun = int_above_0) +xlim(0,100)

integrate (1 / sqrt(2 * pi)) * exp(-0.5*((1/x)-1)^2) * (1/x)