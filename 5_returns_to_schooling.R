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
# library(tidyr) # pivot data from long to wide

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


##################################
# Method 1
# Assume that we know the se of means is the sigma

# Find standard errors of conditional means
df_a = df %>% 
    group_by(educ) %>% 
    summarise(mu_s = mean(logwage), 
                n = n(), 
                sd = sd(logwage), 
                se = sd(logwage) / sqrt(n))
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
draw_and_test = function(i, mu, sigma) {
    
    # Take draws. Note, we drop the zero educ draw to follow pset 
    draw = rnorm(length(mu), 
                 mean = mu, sd = sigma)[-1]
    
    # Do tests and collect results
    df = data.frame(
        # Draw indicator
        draw = i, 
        # Check 1: monotone
        check1 = 1*all(diff(draw)>0), 
        # Check 2: 
        check2 = 1*(draw[12] - draw[11] >  draw[11] - draw[10]),
        # Check 3: monotone 5 to 16
        check3 = 1*all(diff(draw[seq(5,16)])>0), 
        # Check 4: monotone 17 to 20
        check4 = 1*all(diff(draw[seq(17,20)])>0), 
        # Check 5: 
        check5 = 1*(draw[16] - draw[12] >  draw[12] - draw[8])
    )
    
    return(df)
}

# Take draws
num_draws = 1000

df_a = lapply(seq(1, num_draws), 
    draw_and_test, mu = mu, sigma = sigma) %>% 
    bind_rows()

for (i in 1:5){
    print(paste0("Num draws satisfying test ", i, ": ",
                 sum(df_a[paste0("check", i)])))
}


##################################
# Method 2

# Use the full Normal-inverse-gamma: For each draw of the vector µ, first draw 
# a vector of σS values from their respective inverse-gamma distributions, 
# then use those σS values to generate draws from the corresponding conditional 
# normal joint distributions for µ.


# draw vector of sigma_s
rgamma()









# https://www.jstor.org/stable/pdf/2937954.pdf?refreqid=excelsior%3A875da335947556d4de1c669ef34fbba5

# 1
# replication of AK results
df_ak = df %>% mutate(y_and_q = yob + 0.25 * qob) %>% 
    group_by(y_and_q) %>% summarise(years_completed = mean(educ))
# fig1
ggplot(data = df_ak) +
    geom_line(aes(x = y_and_q,y=years_completed))
# Table five, column 1
lm(logwage ~ educ + as.factor(yob), data = df)

# 2 Look for some non-linearity... 
LMs = c()
col = c()
for (i in seq(0,18,3)){
    print(i)
    j = i+3
    LMs = c(LMs,
        lm(logwage~educ +  as.factor(yob), 
            data = df %>% 
               filter(educ >= !!i,
                   educ < !!j))$coefficients["educ"])
    col = c(col, paste0(i, " to ", j, " years educ"))
    
}
replic_df = data.frame(coefficient = LMs, id = as.factor(col)) %>% 
    mutate(num_id = row_number() * 3)

ggplot(replic_df) +
    geom_point(aes(x = num_id, y = coefficient))




