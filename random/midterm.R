# Code used in midterm calculations. 
# Run notes:
  # To run, make sure you have required pacakges installed and up to date
  # Also, change the dir string to the location you want to save your outputs

# Contents
# 0. Set up and load environment
# 1. Code used in question 2
# 2. Code used in question 3

####################################################
# 0. Set up and load environment
####################################################

rm(list = ls())
library(ggplot2) # plotting 
library(dplyr) # data manipulation
library(HDInterval) # calculating HPD credible set
library(car) # anova

dir = "/Users/tombearpark/Documents/princeton/1st_year/ECO517/exercises/mid_term/"
dir.create(dir, showWarnings = FALSE)
set.seed(1) # Make random numbers replicable
theme_set(theme_bw()) # ggplot theme

####################################################
# 1. Code used in question 2
####################################################
values = c(rep(1,7), rep(0,13))
# Question 2a
# set number of draws, and take them from a beta distribution
N = 1000000
draws = rbeta(N, 8, 14)
# Function for calculating HPD set
hpd = hdi(draws)
# Plot results
ggplot(data = data.frame(draws)) + 
  geom_density(aes(x = draws)) +
  geom_vline(xintercept = as.numeric(hpd[1]) , color = "red") + 
  geom_vline(xintercept = as.numeric(hpd[2]) , color = "red") + 
  xlim(0,1) + xlab("p") + ylab("")
ggsave(file = paste0(dir, "2a_hpd_beta.png"), height = 5, width = 5)
# Sense check we have 5% outside the credible set... 
df = data.frame(draws)
length(df$draws[df$draws > as.numeric(hpd[2]) | 
                  df$draws < as.numeric(hpd[1])]) / N

# Question 2b
mean = mean(values)
sd = sd(values) * (20-1)/20
se = sd / sqrt(20)
ci = c(mean - 1.96*se, mean+1.96*se)

# What would the exact values be?
binom.exact(7, 20, conf.level = 0.95)
# binom.confint(7, 20, conf.level = 0.95, methods = "all")
####################################################
# 2. Code used in question 3
####################################################

# Load in the data
load(url("http://sims.princeton.edu/yftp/emet1_2020/kmeans/akdata.RData"))
df = akdataf %>% 
  mutate(constant = 1)

# Inital plot - get means and standard errors of the means
plot_df  = df %>% 
  group_by(yob) %>% 
  summarise(mean_educ = mean(educ), 
            n = n(), 
            sd = sd(educ), 
            se = sd(educ) / sqrt(n))

# Plot output conditional means
ggplot(data = plot_df) +
  geom_point(aes(x = yob, y = mean_educ), color = "red") +
  geom_errorbar(aes(x = yob, 
                    ymin = mean_educ - 1.96 * se, ymax = mean_educ + 1.96 * se)
                ) +
  ggtitle("Conditional means with +- 1.96 SE errorbars") +
  geom_hline(yintercept = mean(df$educ), color = "blue")

ggsave(paste0(dir, "conditional_means.pdf"), height=6, width = 7)

# See how normal our errors look... 
qqnorm(df$educ)

N =length(df$educ)
# replicate values presented in the question
lm_R = lm(educ ~1, data = df)
summary(lm_R)
Anova(lm_R)

lm_U = lm(educ ~ as.factor(yob), data= df)
summary(lm_U)
Anova(lm_U)
deviance(lm_U)

# F-test
RSSR = 3547667.7
RSSU = 3537568.4
k = length(lm_U$coefficients) - length(lm_R$coefficients) 

qf(.99, df1=k, df2=N-k) 

# Run calculation
((RSSR - RSSU) / RSSU) * (N - k)/k

# Calculate BICs
bic <- function(n,rss,k) { 
  return(n*log(rss/n) + k*log(n))
}
bic(N, RSSR, length(lm_R$coefficients))
bic(N, RSSU, length(lm_U$coefficients))


####################################################
# 2. Code used in question 4
####################################################

# Visualise the sitution we are trying to work from - using 
# an example where \mu = 1

x = rnorm(10000, 1, 1)

ggplot(data = data.frame(x)) + 
  geom_density(aes(x = x))

# Create truncated version where values smaller than zero set to zero
x_tr = x
x_tr[x_tr <= 0] =0

ggplot(data = data.frame(x_tr)) + 
  geom_density(aes(x = x_tr))






