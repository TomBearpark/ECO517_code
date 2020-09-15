## Code for plots used in Exercise 2, ECO517 Course at Princeton

# Note - to run this code if you are not Tom, make sure you have the 
# dplyr, ggplot2

# Load environment. 
rm(list = ls())
library(dplyr) # Data manipulation
library(ggplot2) # Plotting

theme_set(theme_bw())

# Output root location
dir = '/Users/tombearpark/Documents/princeton/1st_year/ECO517/exercises/'

# Load in the data, subset to the variables we want
load(
  url("http://sims.princeton.edu/yftp/emet1_2020/kmeans/akdata.RData")
)
df <- akdataf[ , 1:2]


######################################
# Question 1
