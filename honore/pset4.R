# Honore - assignment 4 code 

rm(list = ls())

library(AER)
library(furrr)
library(ggplot2)
library(tidyverse)
library(xtable)
library(ivmodel)

theme_set(theme_bw())
set.seed(1)
out = paste0("/Users/tombearpark/Documents/princeton/1st_year/", 
        "ECO517/honore/assignments/pr4/")

####################################################
# 0. Functions
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
              y = d0 + d1 * z + v1 + v2, ones = 1)
  
  # Set up data for model runs
  if (q == 1) {
    formula = "y ~ z | 1 + x1 + x2"
    INSTRUMENTS = c("ones", "x1", "x2")
  }
  if (q == 2) {
    INSTRUMENTS = c("ones")
    formula = "y ~ z | 1 "
      for (i in seq(1,100)) {
        formula = paste0(formula, " + x", i)
        INSTRUMENTS = c(INSTRUMENTS, paste0("x", i))
      }
  }
  Y = df[,"y"]
  D = df[,"z"]
  Z = df[,INSTRUMENTS]
  
  # Run the three models... 
  iv_c = coef(ivreg(as.formula(formula), data = df))[2]
  liml = ivmodel(Y=Y,D=D,Z=Z)
  ols = coef(lm(y ~ z, data = df))[2]
  return(data.frame(iv = iv_c, 
                    ols = ols, 
                    liml = as.numeric(LIML(liml)[1]), 
                    j = j, N = N))
}

# Function for running IV on 500 datasets, for 3 different N
run_loops = function(N1, N2, N3, q, a1 = 1, a2 = 1)
{
  return(
    future_map_dfr(seq(1,500), return_coef, 
          N = N1, d0 = 1, d1 = 1, a1 = a1, a2 = a2, q = q) %>% 
    bind_rows(
      future_map_dfr(seq(1,500), return_coef, 
          N = N2, d0 = 1, d1 = 1, a1 = a1, a2 = a2, q = q)) %>% 
    bind_rows(
      future_map_dfr(seq(1,500), return_coef, 
          N = N3, d0 = 1, d1 = 1, a1 = a1, a2 = a2, q = q))
  )
}
# Returns and saves a latex table of summary stats
return_table = function(df, out, part) 
{
  out1 = df %>% group_by(N, estimator) %>% 
    summarise(Mean = mean(value), Median = median(value), 
              S.D. = sd(value), IQR = IQR(value)) %>% 
    group_by(estimator) %>% 
    arrange(N, .by_group = TRUE) %>% 
    mutate(N = round(N)) %>% as.data.frame() %>% 
    mutate(estimator =toupper(estimator))
  
  print(xtable(out1, type = "latex"), 
        file = paste0(out, "PART2_p", part,".tex"))
  return(out1)
}

# Parallel computing...
no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)

####################################################
# Question 1
####################################################

# Generate IV estimates for each of 500 datasets
df1 = run_loops(20, 200, 2000, 1)

plot_df1 = df1 %>% 
  pivot_longer(cols = c("iv", "ols", "liml"), names_to = "estimator")

# Show comparison to naive ols estimator... 
p1 = ggplot(data = plot_df1) + 
  geom_density(aes(x = value, color = estimator)) + 
  facet_wrap(~N, scales = "free") + 
  geom_vline(xintercept = 1,color = "red", alpha = 0.5)  + 
  ggtitle("Model working well")
p1
ggsave(p1, file= paste0(out, "PART2_p1.png"), height = 7, width = 10)

# output means / sd / etc
return_table(plot_df1, out, 1) 

####################################################
# Question 2
####################################################

# Generate IV estimates for each of 500 datasets
# note - this takes a long time to run- this is running a lot of models
df2 = run_loops(200, 2000, 20000, 2)
plot_df2 = df2 %>% 
  pivot_longer(cols = c("iv", "ols", "liml"), names_to = "estimator")

# Show comparison to naive ols estimator... 
p2 = ggplot(data = plot_df2 %>% filter(estimator != "ols")) + 
  geom_density(aes(x = value, color = estimator)) +  
  facet_wrap(~N, scales = "free") + 
  geom_vline(xintercept = 1,color = "red", alpha = 0.5) + 
  ggtitle("Many instruments")
p2
ggsave(p2, file= paste0(out, "PART2_p2.png"), height = 7, width = 10)

# output means / sd / etc
return_table(plot_df2, out, 2)

####################################################
# Question 3
####################################################

# Generate IV estimates for each of 500 datasets
df3 = run_loops(20, 200, 2000, 1, a1 = 0.1, a2 = 0.1)
plot_df3 = df3 %>% 
  pivot_longer(cols = c("iv", "ols", "liml"), names_to = "estimator")

# Show comparison to naive ols estimator... 
p3 = ggplot(data = plot_df3%>% filter(estimator != "ols")) + 
  geom_density(aes(x = value, color = estimator)) + 
  facet_wrap(~N, scales = "free") + 
  geom_vline(xintercept = 1,color = "red", alpha = 0.5)  + 
  ggtitle("Weak instruments")
p3
ggsave(p3, file= paste0(out, "PART2_p3.png"), height = 7, width = 10)

# output means / sd / etc
return_table(plot_df3, out, 3)

