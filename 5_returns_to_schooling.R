# Code for Exercise 5, ECO517 at Princeton

# note: - make sure you have dplyr, ggplot2, 
#       packages up to date and installed to run this code
#       - also, change the "dir" string to where you want to save outputs

# Contents: 
# 0. Set up environment and libraries 
# 1 Implement method 1


##################################
# 0. Set up environment and libraries 

rm(list = ls())
library(dplyr) # data manipulation, and piping
library(ggplot2) # plots

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

ggsave(paste0(dir, "conditional_means.pdf"))

# Check out the n per educ group
ggplot(data = df_cond) + 
    geom_point(aes(x = educ, y = n))

# Set parameters for our draws
mu = df_cond$mu_s
sigma = df_cond$se


# Function for doing the checks asked for in the pset
return_tests = function(draw){
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

# Function for taking draws according to method a, and testing. Returns test 
# results for a given draw.
draw_and_test_a = function(i, mu, sigma) {
    
    # Take draws. Note, we drop the zero educ draw to follow pset 
    draw = rnorm(length(mu), 
                 mean = mu, sd = sigma)[-1]
    
    df = return_tests(draw)
    
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

# Take a dreaw, return checks for a given draw
draw_and_test_b = function(i, shape_vect, scale_vect, mu){
    # Take sigma draws, as 1 / gamma draw
    sigma_draw = sqrt(1 / rgamma(n = length(shape_vect), 
                                 shape = shape_vect, scale = scale_vect))
    
    # Use these to draw from normal 
    draw = rnorm(length(mu), 
                 mean = mu, sd = sigma_draw)[-1]
    
    # Do tests and collect results
    df = return_tests(draw)
    
    return(df)
}
df_b = lapply(seq(1, num_draws), 
              draw_and_test_b, 
              mu = mu, shape_vect = shape_vect, scale_vect = scale_vect) %>% 
    bind_rows()

for (i in 1:5){
    print(paste0("Num draws satisfying test ", i, ": ",
                 sum(df_b[paste0("check", i)])))
}

#########################################
# Further AK analysis

# see paper at this link...
# https://www.jstor.org/stable/pdf/2937954.pdf?refreqid=excelsior%3A875da335947556d4de1c669ef34fbba5

# 1 - Basic replication of one of their results

# replication of AK results fig 1
df %>% mutate(y_and_q = yob + 0.25 * qob) %>% 
    group_by(y_and_q) %>% summarise(years_completed = mean(educ)) %>% 
    ggplot() +
        geom_line(aes(x = y_and_q,y=years_completed))

# Table five, column 1
c = lm(logwage ~ educ + as.factor(yob), data = df)$coefficients["educ"]

# 2.1 Look for some non-linearity, three year subsets

LMs = c()
col = c()
for (i in seq(0,18,3)){
    print(i)
    j = i+3
    LMs = c(LMs,
        lm(logwage ~ educ +  as.factor(yob), 
            data = df %>% 
               filter(educ >= !!i,
                   educ < !!j))$coefficients["educ"])
    col = c(col, paste0(i, " to ", j, " years educ"))
    
}



replic_df = data.frame(coefficient = LMs, id = as.factor(col)) %>% 
    mutate(num_id = row_number() * 3)

# Coefficients for each three year group
ggplot(replic_df) +
    geom_point(aes(x = num_id, y = coefficient)) + 
    geom_hline(yintercept = c, color = "red", alpha = 0.5)

# 2.2 Look for some non-linearity, split sample on low / high wage

# Split low and high wage people by the median wage. Check coefficients in each 
# portion
med_lw = median(df$logwage)
df = df %>% mutate(highwage = ifelse(logwage >= med_lw, 1,0))

split.lm.high = lm(logwage ~ educ + as.factor(yob), 
                  data = df %>% filter(highwage == 1))

split.lm.low = lm(logwage ~ educ + as.factor(yob), 
                  data = df %>% filter(highwage == 0))

summary(split.lm.high)
summary(split.lm.low)

# 2.3 Use k-means to do some clustering. Run separate regressions in each cluster

# On the unscaled version

df$cluster_unscaled = kmeans(df[c(1,2)], 2, iter.max=100)$cluster

split.lm.high_cu = lm(logwage ~ educ + as.factor(yob), 
                   data = df %>% filter(cluster_unscaled == 1))

split.lm.low_cu = lm(logwage ~ educ + as.factor(yob), 
                  data = df %>% filter(cluster_unscaled == 2))

summary(split.lm.high_cu)
summary(split.lm.low_cu)

# On the scaled version

df$cluster_scaled = kmeans(df[c(1,2)] %>% 
                               mutate(logwage = logwage / sd(akdataf$logwage), 
                                      educ = educ / sd(akdataf$educ)) , 
                           2, iter.max=100)$cluster
ggplot(data = df) +
    geom_point(aes(x = educ, y = logwage, color = cluster_scaled))

split.lm.high_cs = lm(logwage ~ educ + as.factor(yob), 
                      data = df %>% filter(cluster_scaled == 1))

split.lm.low_cs = lm(logwage ~ educ + as.factor(yob), 
                     data = df %>% filter(cluster_scaled == 2))

summary(split.lm.high_cs)
summary(split.lm.low_cs)

# These results are nonsence i think. Removing legit y variation... 


# 2.4 Look for some non-linearity, include cubic terms
df = df %>% mutate(educ_2 = educ^2, educ_3 = educ^3)

cubic.lm = lm(logwage ~ educ + educ_2 + educ_3 + as.factor(yob), data = df)
summary(cubic.lm)



