# Note: to run this if you aren't Tom, change the "dir" string to the location
# of the input data on your machine, and make sure you have 
# dplyr and ggplot2 libraries
# The fuzzy kmeans analysis at the end also requires the "e1071" library
# I use dplyr and ggplot2 rather than the base R version provided by the professor
# due to improved plot aesthetics

# Contents: 
# 1. Just running the code that the prof put on his website.
# 2. Replicate using dplyr and ggplot, plot graphs asked for by exercise sheet.
# 2.1 Density plot
# 2.2 Kmeans analysis
# 2.3 Unscaled Kmeans analysis
# 2.4 Extensions: Fuzzy kmeans and sum of squares as function of K

rm(list = ls())
dir = '/Users/tombearpark/Documents/princeton/1st_year/ECO517/exercises/'
load(paste0(dir, "data/asciiqob.rdata"))

########################################
# Version 1 
# Doing this using base R, since that's what the exercise pdf does

# Subset data to just log wage and education
akv1 <- as.matrix(akdataf[ , 1:2])

# Get standard deviations of each variable
sigakv1 <- apply(akv1, 2, sd)

# Rescale variables so the variances are the same 
akv1 <- akv1 %*% diag(1/sigakv1)

# Plot
kout <- kmeans(akv1, 2, iter.max=100)
plot(akv1[ , 2], akv1[ , 1], col= kout$cluster)

# recover variable centers (unscaled)
kout$centers %*% diag(sigakv1)


########################################
# Version 2 

# Tidyverse version
library(dplyr)
library(ggplot2)

# 2.1 Density plot

# Rescale by standard deviation for vars of interest
df = akdataf %>% 
  mutate(logwage_scaled = logwage / sd(akdataf$logwage), 
         educ_scaled = educ / sd(akdataf$educ)) 
  
# Check out a density plot of the situation - just the raw values to start with
ggplot(data = df) +
  geom_bin2d(aes(x = educ_scaled, y = logwage_scaled), alpha = 0.7) +
  scale_fill_continuous(type = "viridis", trans = "log", guide = FALSE) +
  theme_bw() 
  
ggsave(filename = paste0(dir, "/1_week/raw_data_heat_map.png"))


# 2.2 Kmeans analysis
# Function for adding a column with contains the cluster allocated by kmean 
get_plot_df = function(data, k){
  
  data_subset = data %>% 
    select(c(logwage_scaled, educ_scaled)) 
  
  data_subset = data_subset %>%
    mutate(cluster = 
           as.factor(kmeans(data_subset, k, iter.max=100)$cluster)) %>% 
    mutate(num_clusters = as.factor(k))
  
  return(data_subset)
}

# Get a long dataframe for plotting, with different cluster numbers as id
plot_df = lapply(
                c(2,3,4,5),
                 get_plot_df, data = df) %>% 
  bind_rows()

# Plot each scatter as a separate facet
ggplot(data = plot_df) +
  geom_point(aes(x = educ_scaled, y = logwage_scaled, 
                 col = cluster), alpha = 0.7) +
  theme_bw() +
  facet_wrap(~num_clusters) 

ggsave(filename = paste0(dir, "/1_week/kmeans_log.png"))


# 2.3 Unscaled Kmeans analysis

# Try a version without using scaling of logwage/education 

plot_df_raw = df %>%
  select(c(logwage, educ)) 

plot_df_raw = plot_df_raw %>%
  mutate(cluster = 
           as.factor(kmeans(plot_df_raw, 3, iter.max=100)$cluster)) %>% 
  mutate(num_clusters = as.factor(2))

ggplot(data = plot_df_raw) +
  geom_point(aes(x = educ, y = logwage, col = cluster), alpha = 0.7) +
  theme_bw() 
ggsave(filename = paste0(dir, "/1_week/kmeans_wage_not_logged.png"))


# 2.4 Extensions: Fuzzy kmeans and sum of squares as function of K

# 2.4.1 Try fuzzy clustering algorithm

library(e1071)
data_subset = df %>% 
  select(c(logwage_scaled, educ_scaled)) 
cm = cmeans(data_subset, 2)

# Create an index of certainty. This is somewhat adhoc
plot_cm = cbind(data_subset, cm$membership) %>% 
  mutate(group = ifelse(`1` > 0.5,1,2)) %>% 
  mutate(Certainty_level = `1`-0.5)

ggplot(data = plot_cm) +
  geom_point(aes(x = educ_scaled, y = logwage_scaled, 
                 color = Certainty_level)) +
  theme_bw()+
  scale_colour_gradient2()
ggsave(filename = paste0(dir, "/1_week/fuzzy_kmeans.png"))

# 2.4.2. Is there any big jumps in model fit with increases in k?
get_ss_info = function(data, k){
  
  data_subset = data %>% 
    select(c(logwage_scaled, educ_scaled)) 
  
  info = as.data.frame(kmeans(data_subset, k, iter.max=100)$tot.withinss)
  info$k = k
  names(info) = c("total_wss", "k")
  
  return(info)
}
w_vals = lapply(
  c(1,2,3,4,5,6,7),
  get_ss_info, data = df) %>% 
  bind_rows()

ggplot(data = w_vals) +
  geom_point(aes(x= k, y = total_wss)) +
  theme_bw()

ggsave(filename = paste0(dir, "/1_week/kmeans_within_cluster_by_K.png"))



