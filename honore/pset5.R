# Part 2, Bo Honore Metric Pset 5

rm(list = ls())
library(tidyverse)
library(ggplot2)
# library(gmm)- this doesn't work for some reason

# Data set properties 
M = 3
n = 1000

# Set parameters
d0 = c(1,1,1)
d1 = c(1,1,1)
a1 = c(1,1,1)
a2 = c(1,1,1)
b = c(0,0,0)
a3 = c(0,0,0)

# Generate dataset

# Variables common across equations 
df = data.frame(
                u1 = rnorm(n), 
                u2 = rnorm(n),
                w1 = rnorm(n), 
                x3_1 = runif(n, 0,3),
                x3_2 = runif(n, 0,3),
                x3_3 = runif(n, 0,3))

# Variables that are specific to each equation, m
for (m in 1:M) {
  for (i in 1:3) df[paste0("v",i,"_m",m)] = rnorm(n) 
  df[paste0("w2_m", m)] = rnorm(n)
  df[paste0("x1_m", m)] = rnorm(n)
  df[paste0("x2_m", m)] = df[paste0("w1")] + df[paste0("w2_m", m)]
}
# Specify alpha vectors

# Generate the dependent variables
for (m in 1:M) {
  # Generate z
  df[paste0("z_m", m)] = df[paste0("x1_m", m)] * a1[m] + 
    df[paste0("x2_m", m)] * a2[m] + 
    (df["x3_1"] + df["x3_1"] + df["x3_1"]) * a3[m] + 
    df[paste0("v1_m",m)] + df[paste0("v3_m",m)] + df["u2"]
  
  # Generate y
  df[paste0("y_m", m)] = d0[m] + df[paste0("z_m", m)] * d1[m] + 
    (df["x3_1"] + df["x3_1"] + df["x3_1"]) * b[m] + 
    df[paste0("v1_m",m)] + df[paste0("v2_m",m)] + df["u1"]
}

# Optimise the moment condition

moment_condition = function(df, par, m)
{
  d0 = par[1]
  d1 = par[2]
  b = c(par[3], par[4], par[5])
  
  y   = df[paste0("y_m", m)]
  z   = df[paste0("z_m", m)]
  x1  = df[paste0("x1_m", m)]
  x2  = df[paste0("x2_m", m)]
  x31 = df[paste0("x3_1")]
  x32 = df[paste0("x3_2")]
  x33 = df[paste0("x3_3")]
  
  m1 <-  y - (d0 + z * d1 + x31* b[1] + x32 * b[2] + x33 * b[3])
  m2 <- (y - (d0 + z * d1 + x31* b[1] + x32 * b[2] + x33 * b[3])) * x1
  m3 <- (y - (d0 + z * d1 + x31* b[1] + x32 * b[2] + x33 * b[3])) * x2
  m4 <- (y - (d0 + z * d1 + x31* b[1] + x32 * b[2] + x33 * b[3])) * x31
  m5 <- (y - (d0 + z * d1 + x31* b[1] + x32 * b[2] + x33 * b[3])) * x32
  m6 <- (y - (d0 + z * d1 + x31* b[1] + x32 * b[2] + x33 * b[3])) * x33
  # browser
  
  f =  data.frame(m1 = mean(m1[[1]]), m2 = mean(m2[[1]]), m3 = mean(m3[[1]]), 
                  m4 = mean(m4[[1]]), m5 = mean(m5[[1]]), m6 = mean(m6[[1]])) 
    
  return(f)
}
# Next step - run optimization function on this to minimize empirical moments
result = optim(
  par = c(0,0,0,0,0), fn =moment_condition,  df = df, m = 1
)






