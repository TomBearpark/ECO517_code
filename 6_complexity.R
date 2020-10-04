# Code for Exercise 5, ECO517 at Princeton

# note: - make sure you have dplyr, ggplot2, tidyr
#       packages up to date and installed to run this code
#       - also, change the "dir" string to where you want to save outputs

# Contents: 
# 0. Set up environment and libraries 
# 1 Implement method 1
# 2 Implement method 2
# extension - implement method 2 directly as a t-distribution
# 3 run regression models extending AK analysis



##################################
# 0. Set up environment and libraries 

rm(list = ls())
library(dplyr) # data manipulation, and piping
library(ggplot2) # plots
library(tidyr) # reshaping data

set.seed(1) # Make random numbers replicable
theme_set(theme_bw()) # ggplot theme

# Output location string
dir = "/Users/tombearpark/Documents/princeton/1st_year/ECO517/exercises/5_week/"

# Load in the data
load(url("http://sims.princeton.edu/yftp/emet1_2020/kmeans/akdata.RData"))
df = akdataf 

