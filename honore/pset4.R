# Honore - assignment 4 code 

rm(list = ls())
library(AER)
library(ggplot2)
library(tidyverse)
library(xtable)

theme_set(theme_bw())
set.seed(1)
out = paste0("/Users/tombearpark/Documents/princeton/1st_year/", 
        "ECO517/honore/assignments/pr4/")

####################################################
# Functions
####################################################
# Generate data and run IV
return_coef = function(j, N, d0, d1, a1, a2, q) 
{
  # Generate data
  df = data.frame(
    v1 = rnorm(N), v2 = rnorm(N), v3 = rnorm(N)
  )
  for (i in seq(1,100)){
    df[paste0("x",i)] = rnorm(N)
  }
  df = mutate(df, j = j, 
              z = a1 * x1 + a2 * x2 + v1 + v3,
              y = d0 + d1 * z + v1 + v2)
  # Run and return IV coefficient
  if (q == 1) formula = "y ~ z | 1+ x1 + x2"
  if (q == 2) {
    formula = "y ~ z | 1 "
      for (i in seq(1,100)) formula = paste0(formula, " + x", i)
  }
  iv_c = coef(ivreg(as.formula(formula), data = df))[2]
  ols = coef(lm(y ~ z, data = df))[2]
  return(data.frame(coef = iv_c, ols = ols, j = j, N = N))
}
# Function for running IV on 500 datasets, for 3 different N
run_loops = function(N1, N2, N3, q, a = 1)
{
  return(
    map_dfr(seq(1,500), return_coef, 
          N = N1, d0 = 1, d1 = 1, a1 = a, a2 = a, q = q) %>% 
    bind_rows(
      map_dfr(seq(1,500), return_coef, 
              N = N2, d0 = 1, d1 = a, a1 = a, a2 = 1, q = q)) %>% 
    bind_rows(
      map_dfr(seq(1,500), return_coef, 
              N = N3, d0 = 1, d1 = a, a1 = a, a2 = 1, q = q))
  )
}
# Returns and saves a latex table of summary stats
return_table = function(df, out, part) 
{
  out1 = df %>% group_by(N) %>% 
    summarise(mean = mean(coef), median = median(coef), 
              sd = sd(coef), iqr = IQR(coef))
  
  print(xtable(out1, type = "latex"), file = paste0(out, "q3_p", part,".tex"))
  return(out1)
}

####################################################
# Question 1
####################################################

# Generate IV estimates for each of 500 datasets
df1 = run_loops(20, 200, 2000, 1)

# Show comparison to naive ols estimator... 
p1 = ggplot(data = df1, aes(group = N)) + 
  geom_density(aes(x = coef)) + 
  geom_density(aes(x = ols), color = "steelblue") + 
  facet_wrap(~N, scales = "free") + 
  geom_vline(xintercept = 1,color = "red", alpha = 0.5)  + 
  ggtitle("Model working well")
p1
ggsave(p1, file= paste0(out, "q3_p1.png"))

# output means / sd / etc
return_table(df, out, 1)

####################################################
# Question 2
####################################################

# Generate IV estimates for each of 500 datasets
df2 = run_loops(200, 2000, 20000, 2)

# Show comparison to naive ols estimator... 
p2 = ggplot(data = df2, aes(group = N)) + 
  geom_density(aes(x = coef)) + 
  geom_density(aes(x = ols), color = "steelblue") + 
  facet_wrap(~N, scales = "free") + 
  geom_vline(xintercept = 1,color = "red", alpha = 0.5) + 
  ggtitle("Many instruments")

ggsave(p2, file= paste0(out, "q3_p2.png"))
p2
# output means / sd / etc
return_table(df2, out, 2)

####################################################
# Question 3
####################################################

# Generate IV estimates for each of 500 datasets
df3 = run_loops(20, 200, 2000, 2, a = 0.01)

# Show comparison to naive ols estimator... 
p3 = ggplot(data = df3, aes(group = N)) + 
  geom_density(aes(x = coef)) + 
  geom_density(aes(x = ols), color = "steelblue") + 
  facet_wrap(~N, scales = "free") + 
  geom_vline(xintercept = 1,color = "red", alpha = 0.5)  + 
  ggtitle("Weak instruments")
p3
ggsave(p3, file= paste0(out, "q3_p3.png"))

# output means / sd / etc
return_table(df3, out, 3)
