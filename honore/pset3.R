rm(list = ls())
library(dplyr)
library(ggplot2)
library(car)
library(sandwich)
library(purrr)

set.seed(1)

##############################################
# Problem 1
##############################################

return_t_tests = function(i, N, beta, beta_null)
{
  
  df = data.frame(x = runif(N), v = rnorm(N)) %>% 
    mutate(epsilon = x * v) %>% 
    mutate(y = beta * x + epsilon)
  
  lm1 = lm(y ~ x, data = df)
  
  linearHypothesis(lm1, c("x = 1"))
  linearHypothesis(lm1, c("x = 1"), white.adjust = TRUE)
  
  # Standard values
  t = as.numeric((coef(lm1)["x"] - beta_null) / 
                   coef(summary(lm1))[,"Std. Error"]["x"])
  p = 2 * pt(abs(t), df = df.residual(lm1), lower.tail = FALSE)
  
  # HAC SEs
  robustSE <- sqrt(vcovHC(lm1, type = "HC1")[2,2])
  tR  = (coef(lm1)["x"] - beta_null) / robustSE
  pR = 2 * pt(abs(tR), df = df.residual(lm1), lower.tail = FALSE)
  
  return(data.frame(
    t = t, p = p, sig = ifelse(p<0.05,1,0), 
    tR = tR, pR = pR, sigR = ifelse(pR<0.05,1,0)))  
}

# Run 100000 times, check whether test is consistent 

N = 200
beta = 1
beta_null = 1

df = map_dfr(seq(1,1000), return_t_tests, 
             N = N, beta = beta, beta_null = 1)

# Check fraction of times 
mean(df$sig)
mean(df$sigR)


##############################################
# Problem 2
##############################################

df = data.frame(x = rnorm(100), 
                epsilon = rnorm(100)) %>% 
  mutate(y = 1+ x + epsilon)

lm(y ~ x, df)
  


df = data.frame(x = 1:100000)
  
microbenchmark("piping" = { df %>% mutate(y = x + 1)},
               "pure" = {mutate(df, y = x + 1)},
               "base" = {})
  
  
  
  
  












