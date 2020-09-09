## Code for plots used in Exercise 2, ECO517 Course at Princeton

# Note - to run this code if you are not Tom, change the 'dir' string to the 
# location of the AK data on your machine, and make sure you have the 
# dplyr, ggplot2, patchwork, and ggrepel packages installed.

# Load environment. 
rm(list = ls())
library(dplyr) # Data manipulation
library(ggplot2) # Plotting
library(patchwork) # Combining ggplot objects
library(ggrepel) # Making text on scatter plots nicely spaced

theme_set(theme_bw())
dir = '/Users/tombearpark/Documents/princeton/1st_year/ECO517/exercises/'

######################################
# Question 1

# Create dataframe of the utility points
utility = data.frame(
  umbrella = c("A", "B", "C", "D", "E", "F", "none"), 
  rain = c(1, 0.6, 0.5, 0.3, 0.25, 0.1, -1), 
  sun = c(1,2,2.6, 3,3,4,6)
)

# get coordinates for convex hull
con.hull.pos=chull(utility[c(2,3)])
con.hull <- rbind(utility[con.hull.pos,],utility[con.hull.pos[1],]) 
utility$interior = ifelse(
  utility$umbrella %in% utility[con.hull.pos,]$umbrella, 
  "Frontier","Not-Frontier")

# Plot the points, and the polygon of the hull
ggplot() +
  geom_point(data = utility, aes(x = rain, y = sun, 
                                 color = interior)) +
  geom_polygon(data = con.hull, aes(x = rain, y = sun), 
               alpha = 0.1, fill = "blue") +
  xlab("Utility if it rains") +
  ylab("Utility if it is sunny") +
  theme(legend.title=element_blank()) + 
  geom_text_repel(data = utility, aes(x = rain, y = sun, label = umbrella))

ggsave(filename = paste0(dir, "/2_week/admissability.png"), 
       height = 5, width = 10)

######################################
# Question 2

# Load in the data, subset to the variables we want
load(paste0(dir, "data/asciiqob.rdata"))
df <- akdataf[ , 1:2]

# Take deciles of each variable
df_deciles = df %>%
  mutate(decile_logwage = ntile(logwage, 10), 
         decile_educ = ntile(educ, 10)) 

# 1 Conditional Mean of logwage by Decile of Education

df_dec_educ = df_deciles %>% 
  group_by(decile_educ) %>% 
  summarise(cond_expt_logwage = mean(logwage))

p = ggplot(data = df_dec_educ) +
  geom_point(aes(x= as.factor(decile_educ), y = cond_expt_logwage)) +
  ggtitle("Conditional Mean of Logwage by Decile of Education") +
  xlab("Decile of Education") + ylab("Logwage Predicted Value")

ggsave(p, filename = paste0(dir, "/2_week/cond_mean_logwage_by_educ.png"))

# 2 Conditional Mean of logwage by decile of Education

df_dec_logwage = df_deciles %>% 
  group_by(decile_logwage) %>% 
  summarise(cond_expt_educ = mean(educ))

q = ggplot(data = df_dec_logwage) +
  geom_point(aes(x= as.factor(decile_logwage), y = cond_expt_educ)) +
  ggtitle("Conditional Mean of Education by Decile of Logwage") +
  xlab("Decile of Logwage") + ylab("Education Predicted Value")

ggsave(q, filename = paste0(dir, "/2_week/cond_mean_educ_by_logwage.png"))

# Further exploration... Let's add spreads to the plots 

df_plot_educ = 
  df_deciles %>% 
    left_join(df_dec_educ, by = "decile_educ") %>% 
    mutate(decile_educ = as.factor(decile_educ))

df_plot_wage =
  df_deciles %>% 
    left_join(df_dec_logwage, by = "decile_logwage") %>% 
    mutate(decile_logwage = as.factor(decile_logwage))

r1 = ggplot(data = df_plot_educ) +
  geom_point(aes(x = decile_educ, y = logwage), alpha = 0.02, 
             color = "blue", size = 0.01) + 
  geom_point(aes(x= decile_educ, y = cond_expt_logwage), 
            color = "red", size = 1) +
  xlab("Decile of Education") + ylab("Logwage")

r2 = ggplot(data = df_plot_wage) +
  geom_point(aes(x = decile_logwage, y = educ), alpha = 0.02, 
             color = "blue", size = 0.01) + 
  geom_point(aes(x= decile_logwage, y = cond_expt_educ), 
             color = "red", size = 1)   +
  xlab("Decile of Logwage") + ylab("Education")

# Combine and save
r = r1 + r2
ggsave(r, 
       filename = 
         paste0(dir, "/2_week/combined_conditional_means_w_spread.png"))

