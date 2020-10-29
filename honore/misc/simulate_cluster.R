# Replicate Bo's stata simulation from Chp2 slides 
set.seed(1)

library(ggplot2)
library(dplyr)
library(sandwich)
library(MASS)

simmou = function(i){
  n = 15000
  df = data.frame(x = seq(1, n))
  
  # Create four independent normals
  for (i in 1:4){
    df[paste0("x", i)] = qnorm(runif(n))
  }
  
  # Create an error term
  df$e = qnorm(runif(n))
  
  # Allow clustering at the state level - randomly group into sets of 50
  df$state = round(50* runif(n) + 0.5)
  
  # 
  df$z1 = qnorm(runif(n))
  
  # normal evaluated at state
  df = df %>% group_by(state) %>% 
    mutate(e1 = qnorm(runif(1)), 
           z2 = qnorm(runif(1))) %>% 
    ungroup()

  # Create combined error term
  df= df %>% mutate(x1 = x1 + z2)
  
  # generate outcome
  df = mutate(df, y = -1 + 0*x1 - 0.1 * x2 + x3 + 0.4* x4 + e1 + e)
  
  # Clean up df
  df = df %>% 
    dplyr::select(-c("e", "e1", "z1", "z2"))
  
  # Regression
  lm1 = lm(data =df, y ~ x1 + x2 + x3 + x4)
  
  # Heteroskedastic only 
  coefsHC = coeftest(lm1, vcov. = vcovHC)
  p1 = coefsHC[2,4] %>% as.numeric()
  c1 = ifelse(p1 < 0.05, 1, 0)
  # Robust to clusering 
  coefsCL = coeftest(lm1, function(x) vcovCL(x, cluster = df$state, type = 'HC1'))
  p2 = coefsCL[2,4] %>% as.numeric()
  c2 = ifelse(p2 < 0.05, 1, 0)
  
  # Save significance of x1 coeficient 
  return(data.frame(
    p1 = p1, c1 = c1, p2 = p2, c2 = c2
  ))
  
}
df = lapply(
  seq(1,1000), simmou) %>% bind_rows()

mean(df$p1)
mean(df$p2)


