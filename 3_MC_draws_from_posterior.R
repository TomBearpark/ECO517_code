## Code for plots used in Exercise 3, ECO517 Course at Princeton

# Note - to run this code if you are not Tom, make sure you have the 
# dplyr, ggplot2, patchwork libraries loaded and up to date

# contents:
# Question 2
# 2.1 - Initial data cleaning and variable construction
# 2.2 - Plotting the proportions, visualisations
# 2.3 - Draws from dirichlets, using three different specifications described in 
    # the question

# Question 1
# This is a simulation to help with intuition for question 1
  # Its after question 2 in the code because it uses  a function developed 
  # as part of question 2

# Load environment. 
rm(list = ls())
library(dplyr) # Data manipulation
library(ggplot2) # Plotting
library(patchwork) # Combining ggplot objects
theme_set(theme_bw())

# Forcing the version of dplyr, since we use the across function from a recent
# release
if(packageVersion("dplyr") != "1.0.2"){
  install.packages("dplyr", version = "1.0.2", 
                  repos = "http://cran.us.r-project.org")
}

# Output root location
dir = '/Users/tombearpark/Documents/princeton/1st_year/ECO517/exercises/'
# Load in the data
load(url("http://sims.princeton.edu/yftp/emet1_2020/kmeans/akdata.RData"))
df = akdataf 

######################################
# Question 2

# 2.1 Initial data cleaning / variable construction

# Create a variable for cohort, identifying each two year group. Include both a 
# numerical version (for analysis) and a categorical one (for plotting)
for (i in seq(min(akdataf$yob), max(akdataf$yob), 2)){
  list = c(i, i+1)
  df$cohort[df$yob %in% list] <- i
  df$cohort_tag[df$yob %in% list] <- paste0(i, " to ", i+1)
}

# Get a dataframe of the number of people with zero education in each cohort
zero_educ = df %>% 
  mutate(zero = ifelse(educ == 0, 1, 0)) %>%
  group_by(cohort, cohort_tag) %>% 
  summarise(num_zeros = sum(zero))

# Join information on zero education, with number of people in each cohort
zero_educ = left_join(
  zero_educ, df %>% group_by(cohort) %>% tally(), 
  by = "cohort") %>% 
  mutate(num_zeros_prop_cohort = num_zeros / n)

# 2.2 PLotting

# Plot the zeros as a proportion of the cohort size, and as proportion of the total 
# population
p = ggplot(data = zero_educ) +
  geom_point(aes(x = cohort_tag, y = num_zeros)) +
  ylab("No. People with Education = 0") + xlab("Cohort")
ggsave(paste0(dir, 
              '/3_week/scatter_visual_zeroeduc_by_cohort.png'))

q = ggplot(data = zero_educ) +
  geom_point(aes(x = cohort_tag, y = num_zeros_prop_cohort)) +
  ylab("Proportion of the Cohort with Education = 0") + xlab("Cohort")

r =  p + q
ggsave(r, file = paste0(dir, 
              '/3_week/scatter_visual_percent_zeroeduc_by_cohort.png'))

# 2.3 - Dirichlet Time!

# Function for returning a dataframe, where each row is a draw 
# Return n draws. Input parameters are alpha
# Modified version of Prof Sims one, to use dplyr, and to add 
# identifier for drift

rdirichlet_tom <- function(n, alpha) {
  set.seed(123)
  m <- length(alpha)
  outmat <- matrix(0, n, m)
  for (ic in 1:m) {
    outmat[ , ic] <- rgamma(n, alpha[ic]) 
  }
  outmat= as.data.frame(outmat)
  names = c()
  for (n in 1:m) names = c(names, paste0("D", n))
  names(outmat) = names
  outmat = outmat %>%
    rowwise() %>%
    mutate(norm = sum(c_across(starts_with("D"))))
  for (n in names){
    outmat[n] = outmat[n] / outmat$norm
  }
  outmat = select(outmat, -norm)
  return(outmat)
}

# Method 1
# Each bin is a count
method_1_df = rdirichlet_tom(n = 1000, 
        alpha = as.vector(zero_educ$num_zeros)) %>% 
  mutate( 
    decreasing= ifelse(
      (D1 > D2) & (D2 > D3) & (D3 > D4) & (D4 > D5), 1, 0 ))

length(method_1_df$decreasing[method_1_df$decreasing == 1]) / 1000


# Method 2
# Account for the fact that cohorts are changing over time
zero_educ = zero_educ %>% 
  mutate(num_educated = n - num_zeros)

# Quick plot - to show trend over time
s = ggplot(data = zero_educ) + 
  geom_point(aes(x = cohort_tag, y = n)) + 
  ggtitle("Size of cohorts") +   
  ylab("No. People in Cohort") + xlab("Cohort")
ggsave(s, file = paste0(dir, 
              '/3_week/cohort_size_over_time.png'),height = 5, width = 5)

# Draw bins for the educated 
method_2_df = rdirichlet_tom(n = 1000, 
                      alpha = as.vector(zero_educ$num_educated))
names(method_2_df) = paste0(names(method_2_df), "_educated")

# join to info
method_2_df = bind_cols(method_2_df, select(method_1_df, -decreasing))
# Calculate ratios of non-educted to educated for each bin
for (i in 1:5){
  method_2_df[paste0("ratio_c_", i)] = 
    method_2_df[paste0("D", i)] / method_2_df[paste0("D", i, "_educated")]
}
method_2_df = method_2_df %>% 
  mutate( 
    decreasing= ifelse(
      (ratio_c_1 > ratio_c_2) & (ratio_c_2 > ratio_c_3) & 
        (ratio_c_3 > ratio_c_4) & (ratio_c_4 > ratio_c_5), 1, 0 ))

length(method_2_df$decreasing[method_2_df$decreasing == 1]) / 1000


# Method 3.
# Treat cohort size as fixed. Scale the educ ==0 probabilities by cohort size
method_3_df = select(method_1_df, -decreasing)

for (i in 1:5){
  method_3_df[paste0("D",i)] =  method_3_df[paste0("D",i)] / zero_educ$n[i]
}
method_3_df = method_3_df %>% 
  mutate( 
    decreasing= ifelse(
      (D1 > D2) & (D2 > D3) & (D3 > D4) & (D4 > D5), 1, 0 ))

length(method_3_df$decreasing[method_3_df$decreasing == 1]) / 1000
 
######################################
# Question 1
# Simulate to get intuition described in question 

# Set parameter vector
a1 = 2
a2 = 3
a3 = 4

# Draw from 3D dirichlet
alpha1 = c(a1,a2,a3)
vals1 = rdirichlet_tom(n = 10000, alpha = alpha1) %>% 
  mutate(p1_norm =D1 / (D1+D2), 
         p2_norm = D2 / (D1+D2))

# Draw from associated 2D, using transformation from the pset
alpha2 = c(a1, a2)
vals2 = rdirichlet_tom(n = 10000, alpha = alpha2)

# Clean up and put in a dataframe for plotting 
df = as.data.frame(c(
  vals1 %>% select(c("p1_norm", "p2_norm")), 
  vals2 %>% select(c("D1", "D2"))
))

# Compare outputs
p1 = ggplot(data = df) +
  geom_density2d_filled(aes(x = p1_norm, y = p2_norm)) +
  ggtitle("Joint Density of Marginal from 3D Dirichlet") 
q1 = ggplot(data = df) +
  geom_density2d_filled(aes(x = D1, y = D2)) +
  ggtitle("Join Density of 2D Dirichlet")

p1+q1
ggsave(p1+q1, file = paste0(dir, 
                          '/3_week/simulated_dirichlet_Q1.png'))
