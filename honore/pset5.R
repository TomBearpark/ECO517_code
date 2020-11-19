# Part 2, Bo Honore Metric Pset 5

rm(list = ls())
library(tidyverse)
library(ggplot2)

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

head(df)







