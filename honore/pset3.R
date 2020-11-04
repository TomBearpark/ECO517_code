rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(sandwich)
library(purrr)
theme_set(theme_bw())
set.seed(1)
out = "/Users/tombearpark/Documents/princeton/1st_year/ECO517/honore/assignments/pr3/"

##############################################
# Problem 1
##############################################

return_t_tests = function(i, N, beta, beta_null)
{
  
  df = data.frame(x = runif(N), v = rnorm(N)) %>% 
    mutate(epsilon = x * v) %>% 
    mutate(y = beta * x + epsilon)
  
  lm1 = lm(y ~ 0 + x, data = df)
  
  # using homoskedastic error assumption
  d = linearHypothesis(lm1, c("x = 1"))
  
  # using robust vcv matrix
  d1 = linearHypothesis(lm1, c("x = 1"), vcov=hccm(lm1, type = "hc0"))
  
  return(data.frame(p = d$`Pr(>F)`[2], pR = d1$`Pr(>F)`[2]))  
}

# Run 1000 times, check whether test is consistent 

N = 200
beta = 1
beta_null = 1

df = map_dfr(seq(1,1000), return_t_tests, 
             N = N, beta = beta, beta_null = 1) %>% 
  mutate(sig = ifelse(p<0.05,1,0), 
    sigR = ifelse(pR<0.05,1,0))

# Check fraction of times 
mean(df$sig)
mean(df$sigR)

plot_df = df %>% select(c("p", "pR")) %>% 
  mutate(n = row_number()) %>% 
  pivot_longer(cols = c("p", "pR"), names_to= "SE_type") %>% 
  mutate(SE_type = ifelse(SE_type == "p", "homoskedastic", "robust"))

ggplot(data = plot_df, aes(group = SE_type)) + 
  geom_density(aes(x = value, fill = SE_type), alpha = 0.2) + 
  ggtitle("P value density functions: 1000 reps") 
ggsave(paste0(out, "pval_density.png"))


##############################################
# Problem 2
##############################################

df = data.frame(x = rnorm(100), 
                epsilon = rnorm(100)) 

prob_2_sim = function(j, df){
  print(j)
  if(j != 0) {
    df$x[1] = j 
  }
  df = mutate(df, y = 1+ x + epsilon)

  lm1 = lm(y ~ x, df)

  # using homoskedastic error assumption
  d = linearHypothesis(lm1, c("x = 1"))$F[2]
  # using robust vcv matrix
  d1 = linearHypothesis(lm1, c("x = 1"), vcov=hccm(lm1, type = "hc0"))$F[2]
  
  return(data.frame(j = j, Homoskedastic = d, Robust = d1))
}

res = map_dfr(c(0, 1, 10, 100, 1000, 10000), prob_2_sim, df = df) %>% 
  pivot_longer(cols = c("Homoskedastic", "Robust")) %>% 
  mutate(t = sqrt(value))

ggplot(res) + 
  geom_point(aes(x = j, y = t, color = name)) +
  geom_line(aes(x = j, y = t, color = name), alpha = 0.5)
ggsave(paste0(out, "t_stat_as_changes.png"))

  












