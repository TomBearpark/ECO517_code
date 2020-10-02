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


##################################
# Method 1
# Assume that we know the se of means is the sigma

# Find standard errors of conditional means
df_cond = df %>% 
    group_by(educ) %>% 
    summarise(mu_s = mean(logwage), 
                n = n(), 
                sd = sd(logwage), 
                se = sd(logwage) / sqrt(n))
# Plot output
ggplot(data = df_cond) +
    geom_point(aes(x = educ, y = mu_s), color = "red") +
    geom_errorbar(aes(x = educ, 
                      ymin = mu_s - 1.96 * se, ymax = mu_s + 1.96 * se)) +
    ggtitle("Conditional means with +- 1.96 SE errorbars")

ggsave(paste0(dir, "conditional_means.pdf"), height=6, width = 7)

# Check out the n per educ group
ggplot(data = df_cond) + 
    geom_point(aes(x = educ, y = n))

# Set parameters for our draws
mu = df_cond$mu_s
sigma = df_cond$se


# Function for doing the checks asked for in the pset
return_tests = function(draw, i){
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
        check5 = 1*(draw[16] - draw[12] >  draw[12] - draw[8]),
        
        # Also return some draws we want to plot...
        mu18 = draw[18], 
        mu19 = draw[19]
        
    )
    return(df)
}

# Function for taking draws according to method a, and testing. Returns test 
# results for a given draw.
draw_and_test_a = function(i, mu, sigma) {
    
    # Take draws. Note, we drop the zero educ draw to follow pset notation
    draw = rnorm(length(mu), 
                 mean = mu, sd = sigma)[-1]
    
    df = return_tests(draw, i = i)
    
    return(df)
}

# Take draws
num_draws = 1000

df_a = lapply(seq(1, num_draws), 
    draw_and_test_a, mu = mu, sigma = sigma) %>% 
    bind_rows()

# Print results
for (i in 1:5){
    print(paste0("Num draws satisfying test ", i, ": ",
                 sum(df_a[paste0("check", i)])))
}


# Extra intuition plot... comparing mu18 and mu19 distributions

vals = data.frame(min_18 = min(df_a$mu18), 
max_19 = max(df_a$mu19))
df_a_plot = df_a %>% 
    select(c("draw", "mu18", "mu19")) %>% 
    pivot_longer(c("mu18", "mu19"))

ggplot() +
    geom_histogram(data = df_a_plot, aes(x = value, fill = name), alpha = 0.4) + 
    geom_vline(data = vals, aes(xintercept = min_18), color = "red") +
    geom_vline(data = vals, aes(xintercept = max_19), color = "blue") + 
    geom_hline(yintercept = 0, color = "black")

ggsave(paste0(dir, "mu_19and18.pdf"), height=6, width = 7)

##################################
# Method 2

# Use the full Normal-inverse-gamma: For each draw of the vector µ, first draw 
# a vector of σS values from their respective inverse-gamma distributions, 
# then use those σS values to generate draws from the corresponding conditional 
# normal joint distributions for µ.

# why aren't we just drawing from a t distribution?

# draw vector of sigma_s
# 1 / \sigma^2 is Gamma((N + 1)/2, N s2/2)
# because of jacobian terms,
# 1 / \sigma^2 is Gamma((N - 3)/2, N s2/2)

# 1 Get input vectors
shape_vect = (df_cond$n - 3) / 2
scale_vect  = (df_cond$n * df_cond$sd^2) / 2

data.frame(educ = df_cond$educ, shape_vect = shape_vect, scale_vect = scale_vect)

# Take a draw, return checks for a given draw
draw_and_test_b = function(i, shape_vect, scale_vect, mu, n_vect){
    
    # Take sigma draws, as 1 / gamma draw
    sigma_draw = sqrt(1 / rgamma(n = length(shape_vect), 
                                 shape = shape_vect, rate = scale_vect))
    
    se = sigma_draw / sqrt(n_vect)
    # Use these to draw from normal 
    draw = rnorm(length(mu), 
                 mean = mu, sd = se)[-1]
    
    # Do tests and collect results
    df = return_tests(draw, i = i)
    
    return(df)
}
num_draws =1000
df_b = lapply(seq(1, num_draws), 
              draw_and_test_b, 
              mu = mu, shape_vect = shape_vect, scale_vect = scale_vect, n_vect = df_cond$n) %>% 
    bind_rows()

for (i in 1:5){
    print(paste0("Num draws satisfying test ", i, ": ",
                 sum(df_b[paste0("check", i)])))
}

# Extention: T distribution version... 	df_18 = data.frame()
# We dont' bother renormalising, since the tests are all just on relative 	
# magnitudes 	

t_df = df_cond$n - 3	
scale_t = df_cond$sd / sqrt(df_cond$n - 3)	

draw_and_test_t = function(i, t_df, scale_t){	
    
    draw = rt(length(t_df), df = t_df, ncp = scale_t)	
    
    # Do tests and collect results	
    df = return_tests(draw, i = i)	
    
    return(df)	
}	

df_t = lapply(seq(1, num_draws), 	
              draw_and_test_t, 	
              t_df = t_df, scale_t = scale_t) %>% 	
    bind_rows()	

for (i in 1:5){	
    print(paste0("Num draws satisfying test ", i, ": ",	
                 sum(df_b[paste0("check", i)])))	
}


#########################################
# 3. Further AK analysis

# see paper at this link...
# https://www.jstor.org/stable/pdf/2937954.pdf?refreqid=excelsior%3A875da335947556d4de1c669ef34fbba5

# 1 - Basic replication of one of their results

# replication of AK results fig 1
df %>% mutate(y_and_q = yob + 0.25 * qob) %>% 
    group_by(y_and_q) %>% summarise(years_completed = mean(educ)) %>% 
    ggplot() +
        geom_line(aes(x = y_and_q,y=years_completed))

# 2.0 Replicate Table five, column 1
c = lm(logwage ~ educ + as.factor(yob), data = df)
summary(c)


# 2.1 Look for some non-linearity, include cubic terms
df = df %>% mutate(educ_2 = educ^2, educ_3 = educ^3)

cubic.lm = lm(logwage ~ educ + educ_2 + educ_3 + as.factor(yob), data = df)
summary(cubic.lm)

# 2.2 Use k-means to do some clustering. Run separate regressions in each cluster

# Split observations into their clusters. Identify with a Dummy
df$cluster_scaled = kmeans(df[c(1,2)] %>% 
                               mutate(logwage = logwage / sd(akdataf$logwage), 
                                      educ = educ / sd(akdataf$educ)) , 
                           2, iter.max=100)$cluster

df$cluster_unscaled = kmeans(df[c(1,2)], 
                    2, iter.max=100)$cluster
    
df = df %>% 
    mutate(cluster_scaled = ifelse(cluster_scaled == 1, 0, 1)) %>% 
    mutate(educ.cluster = educ * cluster_scaled, 
           educ.cluster_unscaled = educ*cluster_unscaled)

# Check it's what we were expecting
ggplot(data = df) +
    geom_point(aes(x = educ, y = logwage, color = cluster_scaled))

# Run regressoins, including a k-means cluster interaction term
kmeans.lm = lm(logwage ~ educ + educ.cluster + as.factor(yob), data = df)
summary(kmeans.lm)
kmeans.us.lm = lm(logwage ~ educ + educ.cluster_unscaled + as.factor(yob), data = df)
summary(kmeans.us.lm)


# Sandwhich estimator test...
# install.packages("sandwich")
library(sandwich)
vcovHC(c, type = "HC")
sandwich_se = diag(vcovHC(c, type = "HC"))^0.5
coef(c)-1.96*sandwich_se
# Doesn't really do much to our results here...
sandwich(c)





