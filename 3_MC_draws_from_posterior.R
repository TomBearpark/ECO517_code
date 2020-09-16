## Code for plots used in Exercise 2, ECO517 Course at Princeton

# Note - to run this code if you are not Tom, make sure you have the 
# dplyr, ggplot2

# Load environment. 
rm(list = ls())
library(dplyr) # Data manipulation
library(ggplot2) # Plotting
library(patchwork)

theme_set(theme_bw())

# Output root location
dir = '/Users/tombearpark/Documents/princeton/1st_year/ECO517/exercises/'

# Load in the data, subset to the variables we want
load(url("http://sims.princeton.edu/yftp/emet1_2020/kmeans/akdata.RData"))

######################################
# Question 1 - Maths

######################################
# Question 2

# Create a variable for cohort
df = akdataf %>%
  mutate(cohort = 0)

# Generate cohort identifier
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

# Plot this, to visualise the drift
ggplot(data = zero_educ) +
  geom_point(aes(x = cohort_tag, y = num_zeros)) +
  ylab("No. People with Education = 0") + xlab("Cohort")
ggsave(paste0(dir, 
              '/3_week/scatter_visual_zeroeduc_by_cohort.png'))

# Want to find the posterior probability that the population proportion of 
# people who have zero years of schooling declines in each cohort. 


# "To compute this, draw a sample of, say, 1000 draws from the posterior 
# distribution of the relevant population proportions,
# and count what fraction of them satisfy the condition."

# 2.1 - Most simple approach

# "The most straightforward approach is to use the fact that the population 
# probabilities of the 5 cohort/zero education bins, 
# normalized to sum to one across cohorts, are jointly 5-dimensional Dirichlet. 
# Then it’s just a matter of drawing a large number of 5-dimensional
# vectors and seeing how many of them satisfy all(diff(p) < 0)"

# We already got the number of zero education people in each cohort. 
# Use this and the number of people in each bin to get a df for proportions with
# zero eduction in each bin

# Get total 
total_n = sum(zero_educ_Prob$n)

zero_educ_Prob = left_join(
  zero_educ, df %>% group_by(cohort) %>% tally(), 
  by = "cohort") %>% 
  mutate(num_zeros_cohort_proportion = num_zeros / n, 
         num_zeros_population_proportion = num_zeros / total_n)


# Plot the zeros as a proportion of the cohort size, and as proportion of the total 
# population
p = ggplot(data = zero_educ_Prob) +
  geom_point(aes(x = cohort_tag, y = num_zeros_cohort_proportion)) +
  ylab("Proportion of Cohort with Education = 0") + xlab("Cohort")

q = ggplot(data = zero_educ_Prob) +
  geom_point(aes(x = cohort_tag, y = num_zeros_population_proportion)) +
  ylab("Proportion of the Sample with Education = 0") + xlab("Cohort")

r = q + p
ggsave(r, file = paste0(dir, 
              '/3_week/scatter_visual_percent_zeroeduc_by_cohort.png'))

# Dirichlet Time!

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
  if(m == 5){
    outmat = outmat %>% 
      mutate( 
        decreasing= ifelse(
          (D1 > D2) & (D2 > D3) & (D3 > D4) & (D4 > D5), 1, 0 ))
  }else{
    print("Drift checked functionality only implemented for five cohorts")
  }
  
  return(outmat)
}

# Normalise the alpha vectors, so they sum to one
norm_population = sum(zero_educ_Prob$num_zeros_population_proportion)
norm_cohort = sum(zero_educ_Prob$num_zeros_cohort_proportion)


zero_educ_prob_norm = zero_educ_Prob %>% 
  mutate(num_zeros_pop_norm = num_zeros_population_proportion / norm_population, 
         num_zeros_cohort_norm = num_zeros_cohort_proportion/ norm_cohort)


# Take a thousand draws
vals = rdirichlet_tom(n = 1000, 
                      alpha = as.vector(zero_educ_prob_norm$num_zeros_pop_norm))

p=length(vals$decreasing[vals$decreasing == 1]) / 1000
print(p)

vals1 = rdirichlet_tom(n = 1000, 
                      alpha = as.vector(zero_educ_prob_norm$num_zeros_cohort_norm))

p1=length(vals$decreasing[vals$decreasing == 1]) / 1000
print(p1)

# dataframe of alpha values for copying into latex:
zero_educ_prob_norm[c("cohort_tag", "num_zeros_pop_norm", "num_zeros_cohort_norm")]




