# Code for Exercise 5, ECO517 at Princeton

# note: - make sure you have dplyr, ggplot2, data.table, testtha, patchwork
#       packages up to date and installed to run this code
#       - also, change the "dir" string to where you want to save outputs

# Contents: 
# 0. Set up environment and libraries 


##################################
# 0. Set up environment and libraries 

rm(list = ls())
library(dplyr) # data manipulation, and piping
library(ggplot2) # plots
# library(data.table) # fread command to easily read in this nasty asc file
# library(testthat) # Assert statements
# library(patchwork) # Combining ggplot objects
set.seed(1) # Make random numbers replicable
theme_set(theme_bw()) # ggplot theme

# Output location string
dir = "/Users/tombearpark/Documents/princeton/1st_year/ECO517/exercises/5_week/"

# Load in the data
load(url("http://sims.princeton.edu/yftp/emet1_2020/kmeans/akdata.RData"))
df = akdataf 



# Check the data out...
# ggplot(data = df, aes(x = educ, y = logwage)) +
#     geom_point() + 
#     geom_smooth(method = "lm")



##################################
# Method 1
# Assume that we know the se of means is the sigma

# Find standard errors of conditional means
df_a = df %>% 
    group_by(educ) %>% 
    summarise(mu_s = mean(logwage), 
                n = n(), 
                sd = sd(logwage), 
                se = sd / sqrt(n))
# Plot output
ggplot(data = df_a) +
    geom_point(aes(x = educ, y = mu_s), color = "red") +
    geom_errorbar(aes(x = educ, 
                      ymin = mu_s - 1.96 * se, ymax = mu_s + 1.96 * se)) +
    ggtitle("Conditional means with +- 1.96 SE errorbars")

ggsave(paste0(dir, "conditional_means.pdf"))

# Check out the n per educ group
ggplot(data = df_a) + 
    geom_point(aes(x = educ, y = n))

# Set parameters for our draws
mu = df_a$mu_s
sigma = df_a$se

# Function for taking draws
draw = function(mu, sigma) rnorm(length(mu), mean = mu, sd = sigma)

# Take draws
num_draws = 1000




d =draw(mu = mu, sigma = sigma)

plot(d)
unique(df$educ)


head(df)

