
# Load environment. Dplyr package for data manipulation, ggplot for plots
rm(list = ls())
library(dplyr)
library(ggplot2)

# Load in the data, subset to the variables we want
dir = '/Users/tombearpark/Documents/princeton/1st_year/ECO517/exercises/'
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

ggplot(data = df_dec_educ) +
  geom_point(aes(x= decile_educ, y = cond_expt_logwage)) +
  theme_bw() +
  ggtitle("Conditional Mean of Logwage by Decile of Education")

# 2 Conditional Mean of logwage by decile of Education

df_dec_logwage = df_deciles %>% 
  group_by(decile_logwage) %>% 
  summarise(cond_expt_educ = mean(educ))

ggplot(data = df_dec_logwage) +
  geom_point(aes(x= decile_logwage, y = cond_expt_educ)) +
  theme_bw() +
  ggtitle("Conditional Mean of Education by Decile of Logwage")







# Version 2 - Using Regression to get SE
# This is probably not useful and should be deleted

get_df_by_educ = function(decile){
  
  d = df_deciles %>% 
    filter(decile_educ == !!decile)
  
  m = lm(logwage ~ decile_educ, data = d)
  val = coef(summary(m))[, "Estimate"] 
  se = coef(summary(m))[, "Std. Error"] 

  df = data.frame(decile = decile, val = val, se = se)
  return(df)

}

x_df_decile_educ = lapply(seq(1,10), get_df) %>% 
  bind_rows() %>% 
  mutate(max = val + 1.96 * se, 
         min = val - 1.96 * se)

ggplot(data = x_df_decile_educ) +
  geom_point(stat="identity", color="black", position=position_dodge(.9), 
           aes(x = decile, y = val)) +
  geom_errorbar(aes(x = decile, ymin=min, ymax=max), width=.2,
             position=position_dodge(.9)) +
  theme_bw()
















