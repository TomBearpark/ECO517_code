# Part 2, Bo Honore Metric Pset 5

rm(list = ls())
library(tidyverse)
library(ggplot2)

M = 3
n = 1000
bm = c(0,0,0)

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
  for (i in 0:1) df[paste0("d",i,"_m",m)] = rep(1, n)
  for (i in 1:2) df[paste0("a",i,"_m",m)] = rep(1, n)
  df[paste0("x3_pr_beta", m)] = df[paste0("x3_", m)] * bm[m]
  df[paste0("w2_m", m)] = rnorm(n)
  df[paste0("x2_m", m)] = df[paste0("x3_", m)] * bm[m]
  df[paste0("x1_m", m)] = rnorm(n)
}
# Specify alpha vectors
a1 = c(1,1,1)
a2 = c(1,1,1)
# Generate the dependent variables
for (m in 1:M) {
  df[paste0("z_m", m)] = 
  df[paste0("y_m", m)] = df[paste0("d0_m", m)] + 
        
}